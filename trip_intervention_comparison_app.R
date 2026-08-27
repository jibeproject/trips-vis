# To run
# shiny::runApp("D:/projects/jibe/trips-vis/trip_intervention_comparison_app.R")

library(shiny)
library(tidyverse)
library(ggplot2)
library(dplyr)
library(data.table)
library(gt)
library(gridExtra)
library(qs)

# ############################ Configuration ################################

# Only these columns of trips.csv are used downstream; reading the full 22
# columns of a ~10GB file wastes most of the parse time on the four
# full-precision coordinate doubles (originX/Y, destinationX/Y) that we discard.
MODEL_TRIP_COLS <- c("p.ID", "t.purpose", "mode",
                     "t.distance_walk", "t.distance_bike", "t.distance_auto",
                     "time_auto", "time_pt", "time_walk", "time_bike")

HOME_BASED_PURPOSES <- c("HBW", "HBE", "HBA", "HBS", "HBR", "HBO", "RRT")

MODE_LEVELS <- c("Driving Car", "Car Passenger", "Public Transport",
                 "Walking", "Cycling", "Other")

PURPOSE_LEVELS <- c("Home-based-work", "Home-based-education",
                    "Home-based-accompanying", "Home-based-shopping",
                    "Home-based-recreation", "Home-based-other",
                    "Non-home-based-other", "Non-home-based-work",
                    "Recreational round trip")

GREATER_MELBOURNE_SA4 <- c("Melbourne - Inner", "Melbourne - Inner East",
                           "Melbourne - Inner South", "Melbourne - North East",
                           "Melbourne - North West", "Melbourne - Outer East",
                           "Melbourne - South East", "Melbourne - West",
                           "Mornington Peninsula")

# One palette covering every scenario that can be present. A two-value manual
# scale errors ("Insufficient values in manual scale") as soon as VISTA is added.
SCENARIO_COLOURS <- c("Base"           = "#4A90E2",
                      "Intervention"   = "#2ECC71",
                      "VISTA"          = "#555555",
                      "VISTA_noWeight" = "#AAAAAA")

# Maximum trips per scenario x mode fed to the density estimator.
DENSITY_SAMPLE_N <- 200000

CACHE_DIR <- file.path(".cache")

# ############################ Data preparation #############################

#' Recode a raw MITO trips.csv into the common analysis schema.
#'
#' Kept as one function because Base and Intervention differ only by label. All
#' transforms are done in place with data.table `:=` - chained dplyr mutate()
#' on a 40M-row table copies the whole frame at every step.
prepare_model_trips <- function(path, scenario_label) {
  dt <- fread(file = path, select = MODEL_TRIP_COLS)

  dt[, mode := fcase(
    mode == "autoDriver",    "Driving Car",
    mode == "autoPassenger", "Car Passenger",
    mode == "pt",            "Public Transport",
    mode == "walk",          "Walking",
    mode == "bicycle",       "Cycling",
    default = "Other")]

  # Assign time (hours) and distance (km) per mode, touching each row once
  # rather than dividing all four time columns across every row and then
  # discarding three of them. Note PT distance deliberately uses the *auto*
  # routed distance: MATSim routes PT by time, not distance.
  dt[, `:=`(time = NA_real_, dist = NA_real_)]
  dt[mode == "Walking",          `:=`(time = time_walk / 60, dist = t.distance_walk)]
  dt[mode == "Cycling",          `:=`(time = time_bike / 60, dist = t.distance_bike)]
  dt[mode == "Public Transport", `:=`(time = time_pt   / 60, dist = t.distance_auto)]
  dt[mode %chin% c("Driving Car", "Car Passenger"),
                                 `:=`(time = time_auto / 60, dist = t.distance_auto)]

  # Home-based trips and RRT imply a return leg.
  dt[, t.factor := fifelse(t.purpose %chin% HOME_BASED_PURPOSES, 2, 1)]
  dt[, `:=`(
    scenario      = scenario_label,
    time_factored = time * t.factor,
    dist_factored = dist * t.factor,
    t.weight      = 1,
    p.weight      = 1
  )]

  # p.ID stays integer. It is only ever used in group_by(p.ID, mode, scenario),
  # and scenario is always part of that grouping, so IDs never need to be unique
  # across scenarios. Coercing 40M integers to character costs both time and a
  # large amount of memory via R's global string cache.
  dt[, p.ID := as.integer(p.ID)]

  # Filter before binding so the combined table is as small as possible.
  dt <- dt[dist > 0 & time > 0 & is.finite(time) & is.finite(dist)]

  dt[, .(p.ID, t.purpose, time, time_factored, dist, dist_factored, mode,
         scenario, t.factor, t.weight, p.weight)]
}

#' Resolve the VISTA weight and area column names for the loaded vintage.
#'
#' The full VISTA 2012-20 extract carries SA3 weights and lower-case SA4 name
#' fields; the open LGA sample carries LGA weights and mixed-case SA4 fields.
resolve_vista_fields <- function(trips_vista, pp_vista) {
  if (all(c("wdtripwgt_sa3", "wetripwgt_sa3") %in% names(trips_vista)) &&
      all(c("wdperswgt_sa3", "weperswgt_sa3") %in% names(pp_vista))) {
    list(wdtripwgt = "wdtripwgt_sa3", wetripwgt = "wetripwgt_sa3",
         wdperswgt = "wdperswgt_sa3", weperswgt = "weperswgt_sa3",
         origin_area = "origsa4_name", destination_area = "destsa4_name")
  } else {
    list(wdtripwgt = "wdtripwgt_LGA", wetripwgt = "wetripwgt_LGA",
         wdperswgt = "wdperswgt_LGA", weperswgt = "weperswgt_LGA",
         origin_area = "origSA4_name", destination_area = "destSA4_name")
  }
}

#' Recode VISTA survey trips into the common analysis schema.
#'
#' Ported from trip_calibration_comparison.Rmd (Process VISTA Data), which is
#' the most recent version of this logic.
prepare_vista <- function(trips_vista, pp_vista, fields) {
  trips_scenario_vista <- trips_vista %>%
    mutate(scenario = "VISTA")

  # Exclude trips with an origin or destination outside Greater Melbourne.
  trips_scenario_vista <- trips_scenario_vista %>%
    filter(!!sym(fields$origin_area) %in% GREATER_MELBOURNE_SA4 &
             !!sym(fields$destination_area) %in% GREATER_MELBOURNE_SA4)

  # Clean trip purposes, by replacing purpose/place with 'work related' where
  # one is specified and the other is missing.
  trips_scenario_vista <- trips_scenario_vista %>%
    mutate(origplace1 = ifelse((is.na(origplace1) | str_detect(origplace1, "Unknown")) &
                                 origpurp1 == "Work Related", "Workplace", origplace1),
           destplace1 = ifelse((is.na(destplace1) | str_detect(destplace1, "Unknown")) &
                                 destpurp1 == "Work Related", "Workplace", destplace1),
           origpurp1 = ifelse((is.na(origpurp1) | str_detect(origpurp1, "Unknown")) &
                                origplace1 == "Workplace", "Work Related", origpurp1),
           destpurp1 = ifelse((is.na(destpurp1) | str_detect(destpurp1, "Unknown")) &
                                destplace1 == "Workplace", "Work Related", destpurp1))

  # Assign purpose (see https://github.com/jibeproject/Melbourne-Mode-Choice)
  trips_scenario_vista <- trips_scenario_vista %>%
    mutate(purpose = case_when(
      origpurp2 == "Employer's Business" | destpurp2 == "Employer's Business" ~ "business",
      origpurp1 %in% c("Unknown Purpose (at start of day)", "Not Stated") |
        destpurp1 %in% c("NA", "Not Stated")                ~ "unknown",
      origpurp1 == "At Home" & destpurp1 == "At or Go Home" ~ "RRT",
      # trips from home are HBx, with 'x' based on destination
      origpurp1 == "At Home" ~ case_when(
        destpurp1 == "Work Related"                         ~ "HBW",
        destpurp1 == "Education"                            ~ "HBE",
        destpurp1 == "Buy Something"                        ~ "HBS",
        destpurp1 == "Recreational"                         ~ "HBR",
        destpurp1 == "Other Purpose"                        ~ "HBO",
        destpurp1 %in% c("Accompany Someone", "Pick-up or Drop-off Someone") ~ "HBA",
        # classify remaining trips from home using place information if clearer
        destplace1 == "Workplace"                           ~ "HBW",
        destplace1 == "Place of Education"                  ~ "HBE",
        destplace1 == "Shops"                               ~ "HBS",
        destplace1 %in% c("Recreational Place", "Natural Feature", "Social Place") ~ "HBR",
        TRUE                                                ~ "HBO"
      ),
      # trips to home are NA
      destpurp1 == "At or Go Home"                          ~ "NA",
      origpurp1 == "Work Related" | destpurp1 == "Work Related" ~ "NHBW",
      TRUE                                                  ~ "NHBO"))

  trips_scenario_vista <- trips_scenario_vista %>%
    left_join(pp_vista, by = "persid") %>%
    filter(!purpose %in% c("NA", "business", "unknown")) %>%
    mutate(
      mode = case_when(
        linkmode == "Vehicle Driver"    ~ "Driving Car",
        linkmode == "Vehicle Passenger" ~ "Car Passenger",
        linkmode %in% c("Public Bus", "School Bus", "Train", "Tram") ~ "Public Transport",
        linkmode == "Walking"           ~ "Walking",
        linkmode == "Bicycle"           ~ "Cycling",
        # 'Other' covers Jogging, Mobility Scooter, Motorcycle, Other and Taxi
        TRUE                            ~ "Other"),
      # x2 for home-based/RRT return legs, x7 because VISTA is a one-day diary
      # and the model reports a full week.
      t.factor = if_else(purpose %in% HOME_BASED_PURPOSES, 2 * 7, 1 * 7),
      time = triptime / 60,  # minutes to hours
      dist = cumdist,        # km
      time_factored = time * t.factor,
      dist_factored = dist * t.factor,
      # Weekday and weekend weights are mutually exclusive: take whichever is
      # populated for the surveyed day.
      t.weight = ifelse(is.na(.data[[fields$wdtripwgt]]) | .data[[fields$wdtripwgt]] == "",
                        .data[[fields$wetripwgt]], .data[[fields$wdtripwgt]]),
      p.weight = ifelse(is.na(.data[[fields$wdperswgt]]) | .data[[fields$wdperswgt]] == "",
                        .data[[fields$weperswgt]], .data[[fields$wdperswgt]]),
      t.weight = as.numeric(gsub(",", "", t.weight)),
      p.weight = as.numeric(gsub(",", "", p.weight)),
      # See prepare_model_trips(): p.ID only groups within a scenario, so codes
      # need not be unique across scenarios.
      p.ID = as.integer(factor(persid))) %>%
    filter(!mode %in% c("Other")) %>%
    select(p.ID, t.purpose = purpose, time, time_factored, dist,
           dist_factored, mode, scenario, t.factor, t.weight, p.weight) %>%
    filter(dist > 0, time > 0, is.finite(time), is.finite(dist), !is.na(t.weight)) %>%
    as.data.table()

  vista_no_weight <- copy(trips_scenario_vista)[
    , `:=`(t.weight = 1, p.weight = 1, scenario = "VISTA_noWeight")]

  list(weighted = trips_scenario_vista, unweighted = vista_no_weight)
}

#' Sum VISTA person weights to a population estimate.
parse_vista_weights <- function(x) {
  sum(as.numeric(gsub(",", "", x)), na.rm = TRUE)
}

#' Apply the shared factor levels to a combined trips table.
finalise_combined <- function(trips, scenario_levels) {
  trips[, scenario := factor(scenario, levels = scenario_levels)]
  trips[, mode := factor(mode, levels = MODE_LEVELS)]
  trips[, t.purpose := fcase(
    t.purpose == "HBW",  "Home-based-work",
    t.purpose == "HBE",  "Home-based-education",
    t.purpose == "HBA",  "Home-based-accompanying",
    t.purpose == "HBS",  "Home-based-shopping",
    t.purpose == "HBR",  "Home-based-recreation",
    t.purpose == "HBO",  "Home-based-other",
    t.purpose == "NHBO", "Non-home-based-other",
    t.purpose == "NHBW", "Non-home-based-work",
    t.purpose == "RRT",  "Recreational round trip",
    default = NA_character_)]
  trips[, t.purpose := factor(t.purpose, levels = PURPOSE_LEVELS)]
  trips[]
}

#' Cache key from the input paths plus each file's size and mtime, so the cache
#' invalidates whenever any input file changes.
cache_key <- function(paths) {
  paths <- paths[!is.na(paths) & nzchar(paths)]
  info <- file.info(paths)
  substr(rlang::hash(list(paths, info$size, info$mtime)), 1, 16)
}

scenario_scale <- function(aes_type = c("fill", "colour")) {
  aes_type <- match.arg(aes_type)
  if (aes_type == "fill") {
    scale_fill_manual(values = SCENARIO_COLOURS, drop = FALSE)
  } else {
    scale_colour_manual(values = SCENARIO_COLOURS, drop = FALSE)
  }
}

# ############################### UI ########################################

ui <- fluidPage(
  titlePanel("Trip Scenario Comparison: Base vs Intervention"),

  sidebarLayout(
    sidebarPanel(
      h3("Data Loading"),

      textInput("base_path",
                "Base Scenario trips.csv Path:",
                value = "",
                placeholder = "C:/path/to/base/trips.csv"),

      textInput("intervention_path",
                "Intervention Scenario trips.csv Path:",
                value = "",
                placeholder = "C:/path/to/intervention/trips.csv"),

      textInput("persons_path",
                "Persons file Path (pp_*.csv):",
                value = "",
                placeholder = "C:/path/to/pp_*.csv"),

      h4("Analysis Options:"),
      checkboxInput("show_vista", "Include VISTA Reference Data", value = FALSE),

      conditionalPanel(
        condition = "input.show_vista == true",
        textInput("vista_trips_path",
                  "VISTA trips.csv Path:",
                  placeholder = "C:/path/to/T_VISTA_*.csv"),
        textInput("vista_persons_path",
                  "VISTA persons.csv Path:",
                  placeholder = "C:/path/to/P_VISTA_*.csv")
      ),

      checkboxInput("use_cache", "Use cached data if available", value = TRUE),

      actionButton("load_data", "Load Data", class = "btn-primary"),
      actionButton("rebuild_cache", "Rebuild cache"),

      hr(),

      h4("Data Status:"),
      textOutput("data_status")
    ),

    mainPanel(
      tabsetPanel(
        tabPanel("Overview",
                 h3("Key Population Figures"),
                 gt_output("key_figures_table"),
                 hr(),
                 h3("Data Quality Summary"),
                 gt_output("diagnostics_table")
        ),

        tabPanel("Mode Share",
                 h3("Mode Share Comparison"),
                 gt_output("mode_share_table"),
                 plotOutput("mode_share_plot", height = "600px")
        ),

        tabPanel("Trip Distribution",
                 h3("Trip Length Distribution by Mode"),
                 plotOutput("trip_length_plot", height = "1000px"),
                 hr(),
                 h3("Median Distance Comparison"),
                 gt_output("median_distance_table"),
                 plotOutput("median_distance_plot", height = "600px")
        ),

        tabPanel("Weekly Patterns",
                 h3("Average Weekly Distance and Time Per Person"),
                 plotOutput("weekly_plots", height = "800px")
        ),

        tabPanel("Trip Purpose",
                 h3("Trip Purpose Distribution"),
                 plotOutput("purpose_plot", height = "600px")
        ),

        tabPanel("Summary",
                 h3("Intervention Impact Summary"),
                 verbatimTextOutput("summary_text"),
                 hr(),
                 h3("Key Findings"),
                 htmlOutput("key_findings")
        )
      )
    )
  )
)

# ############################## Server #####################################

server <- function(input, output, session) {

  data <- reactiveValues(
    trips = NULL,
    populations = NULL,
    loaded = FALSE,
    status = "No data loaded"
  )

  load_all <- function(force_rebuild = FALSE) {

    req(input$base_path, input$intervention_path, input$persons_path)

    include_vista <- isTRUE(input$show_vista)
    if (include_vista) {
      req(input$vista_trips_path, input$vista_persons_path)
    }

    withProgress(message = 'Loading data...', value = 0, {

      tryCatch({

        paths <- c(input$base_path, input$intervention_path, input$persons_path)
        if (include_vista) {
          paths <- c(paths, input$vista_trips_path, input$vista_persons_path)
        }

        if (!all(file.exists(paths))) {
          stop("File(s) not found: ",
               paste(paths[!file.exists(paths)], collapse = "; "))
        }

        key <- cache_key(paths)
        cache_file <- file.path(CACHE_DIR, paste0("trips_", key, ".qs"))
        use_cache <- isTRUE(input$use_cache) && !force_rebuild

        if (use_cache && file.exists(cache_file)) {
          incProgress(0.5, detail = "Reading cached data...")
          cached <- qs::qread(cache_file)
          data$trips <- cached$trips
          data$populations <- cached$populations
          data$loaded <- TRUE
          data$status <- paste0(cached$status, " [from cache]")
          incProgress(1.0, detail = "Loaded from cache")
          return(invisible(NULL))
        }

        incProgress(0.05, detail = "Loading persons file...")
        # Only the row count is needed; read a single column by position so no
        # assumption is made about the persons file's column names.
        pop_baseline <- nrow(fread(file = input$persons_path, select = 1L))

        incProgress(0.10, detail = "Loading and processing base scenario...")
        trips_base <- prepare_model_trips(input$base_path, "Base")
        n_base <- nrow(trips_base)

        incProgress(0.35, detail = "Loading and processing intervention scenario...")
        trips_intervention <- prepare_model_trips(input$intervention_path, "Intervention")
        n_intervention <- nrow(trips_intervention)

        scenario_levels <- c("Base", "Intervention")
        populations <- c(Base = pop_baseline, Intervention = pop_baseline)
        parts <- list(trips_base, trips_intervention)
        vista_status <- ""

        if (include_vista) {
          incProgress(0.60, detail = "Loading VISTA reference data...")
          trips_vista <- fread(file = input$vista_trips_path)
          pp_vista <- fread(file = input$vista_persons_path)

          fields <- resolve_vista_fields(trips_vista, pp_vista)

          incProgress(0.70, detail = "Processing VISTA data...")
          vista <- prepare_vista(trips_vista, pp_vista, fields)

          populations <- c(populations,
                           VISTA = parse_vista_weights(pp_vista[[fields$wdperswgt]]) +
                                   parse_vista_weights(pp_vista[[fields$weperswgt]]),
                           VISTA_noWeight = nrow(pp_vista))
          scenario_levels <- c(scenario_levels, "VISTA", "VISTA_noWeight")
          parts <- c(parts, list(vista$weighted, vista$unweighted))

          vista_status <- sprintf(", %s VISTA trips",
                                  format(nrow(vista$weighted), big.mark = ","))
          rm(trips_vista, pp_vista, vista)
        }

        incProgress(0.85, detail = "Combining data...")
        trips <- rbindlist(parts, use.names = TRUE)
        rm(parts, trips_base, trips_intervention)
        gc(verbose = FALSE)

        trips <- finalise_combined(trips, scenario_levels)

        status <- sprintf("Data loaded: %s Base trips, %s Intervention trips%s, %s persons",
                          format(n_base, big.mark = ","),
                          format(n_intervention, big.mark = ","),
                          vista_status,
                          format(pop_baseline, big.mark = ","))

        data$trips <- trips
        data$populations <- populations
        data$loaded <- TRUE
        data$status <- status

        incProgress(0.95, detail = "Writing cache...")
        tryCatch({
          if (!dir.exists(CACHE_DIR)) dir.create(CACHE_DIR, recursive = TRUE)
          qs::qsave(list(trips = trips, populations = populations, status = status),
                    cache_file)
        }, error = function(e) {
          showNotification(paste("Could not write cache:", e$message),
                           type = "warning", duration = 5)
        })

        incProgress(1.0, detail = "Data loaded successfully!")

      }, error = function(e) {
        data$status <- paste("Error loading data:", e$message)
        showNotification(paste("Error:", e$message), type = "error", duration = 10)
      })
    })
  }

  observeEvent(input$load_data, load_all(force_rebuild = FALSE))
  observeEvent(input$rebuild_cache, load_all(force_rebuild = TRUE))

  output$data_status <- renderText({
    data$status
  })

  # ---- Cached aggregates -------------------------------------------------
  # Each of these is a full pass over the combined table (up to ~80M rows).
  # Computing them in reactive()s means they run once per load and are shared
  # between the tables and plots, rather than being recomputed by each output
  # and re-run on every window resize.

  scenarios_present <- reactive({
    req(data$loaded)
    levels(data$trips$scenario)
  })

  mode_summary <- reactive({
    req(data$loaded)
    out <- data$trips[, .(count = sum(t.factor * t.weight, na.rm = TRUE)),
                      by = .(scenario, mode)]
    out[, percent := count / sum(count) * 100, by = scenario]
    as_tibble(out)
  })

  mode_comparison <- reactive({
    mode_summary() %>%
      select(scenario, mode, percent) %>%
      pivot_wider(names_from = scenario, values_from = percent) %>%
      mutate(change = Intervention - Base,
             pct_change = (change / Base) * 100)
  })

  median_summary <- reactive({
    req(data$loaded)
    as_tibble(data$trips[, .(median_dist_km = median(dist, na.rm = TRUE)),
                         by = .(scenario, mode)])
  })

  diagnostics_summary <- reactive({
    req(data$loaded)
    as_tibble(data$trips[, .(
      n = .N,
      mean_dist = mean(dist, na.rm = TRUE),
      median_dist = median(dist, na.rm = TRUE),
      # time is stored in hours; report minutes
      mean_time = mean(time, na.rm = TRUE) * 60,
      median_time = median(time, na.rm = TRUE) * 60
    ), by = .(scenario, mode)][order(scenario, mode)])
  })

  key_figures <- reactive({
    req(data$loaded)
    pops <- data$populations
    out <- data$trips[, .(trips = sum(t.factor * t.weight, na.rm = TRUE)),
                      by = scenario][order(scenario)]
    out[, pop := unname(pops[as.character(scenario)])]
    out[, avgTrip := trips / pop]
    as_tibble(out)
  })

  weekly_summary <- reactive({
    req(data$loaded)
    pops <- data$populations

    per_person <- data$trips[, .(dist = sum(dist_factored * p.weight, na.rm = TRUE),
                                 time = sum(time_factored * p.weight, na.rm = TRUE)),
                             by = .(p.ID, mode, scenario)]

    out <- per_person[, .(totalDist = sum(dist), totalTime = sum(time)),
                      by = .(mode, scenario)]
    out[, pop := unname(pops[as.character(scenario)])]
    out[, `:=`(avgDist = totalDist / pop, avgTime = totalTime / pop * 60)]
    as_tibble(out)
  })

  purpose_summary <- reactive({
    req(data$loaded)
    out <- data$trips[, .(count = sum(t.factor * t.weight, na.rm = TRUE)),
                      by = .(scenario, t.purpose)]
    out[, percent := count / sum(count) * 100, by = scenario]
    as_tibble(out)
  })

  # Density estimation over ~80M rows is by far the slowest output, and
  # renderPlot re-runs on every resize. Sample per scenario x mode instead; the
  # curve is visually indistinguishable and the sample size is disclosed below.
  density_sample <- reactive({
    req(data$loaded)
    trips <- data$trips
    idx <- trips[, .I[sample(.N, min(.N, DENSITY_SAMPLE_N))],
                 by = .(scenario, mode)]$V1
    list(data = trips[idx], n_sampled = length(idx), n_total = nrow(trips))
  })

  # ---- Outputs -----------------------------------------------------------

  output$key_figures_table <- render_gt({
    key_figures() %>%
      gt() %>%
      tab_header(title = "Key Figures by Scenario") %>%
      cols_label(
        scenario = "Scenario",
        trips = "Total Trips per Week",
        pop = "Total Population",
        avgTrip = "Weekly Trip Rate Per Person"
      ) %>%
      fmt_number(columns = c(trips, pop), decimals = 0, use_seps = TRUE) %>%
      fmt_number(columns = avgTrip, decimals = 2)
  })

  output$diagnostics_table <- render_gt({
    diagnostics_summary() %>%
      gt() %>%
      tab_header(title = "Summary Statistics by Mode and Scenario") %>%
      cols_label(
        n = "Total Trips",
        mean_dist = "Mean Dist (km)",
        median_dist = "Median Dist (km)",
        mean_time = "Mean Time (min)",
        median_time = "Median Time (min)"
      ) %>%
      fmt_number(columns = c(mean_dist, median_dist), decimals = 2) %>%
      fmt_number(columns = c(mean_time, median_time), decimals = 1) %>%
      fmt_number(columns = n, decimals = 0, use_seps = TRUE)
  })

  output$mode_share_table <- render_gt({
    comparison <- mode_comparison()
    value_cols <- intersect(scenarios_present(), names(comparison))

    comparison %>%
      select(mode, all_of(value_cols), change, pct_change) %>%
      gt() %>%
      tab_header(title = "Mode Share Comparison (%)",
                 subtitle = "Impact of the intervention on mode choice") %>%
      cols_label(mode = "Mode", change = "Change (%)", pct_change = "% Change") %>%
      fmt_number(columns = c(all_of(value_cols), change), decimals = 2) %>%
      fmt_number(columns = pct_change, decimals = 1, pattern = "{x}%") %>%
      tab_style(
        style = cell_fill(color = "lightgreen"),
        locations = cells_body(columns = change, rows = change > 0.5)
      ) %>%
      tab_style(
        style = cell_fill(color = "lightcoral"),
        locations = cells_body(columns = change, rows = change < -0.5)
      )
  })

  output$mode_share_plot <- renderPlot({
    ggplot(mode_summary(), aes(x = mode, y = percent, fill = scenario)) +
      geom_col(position = "dodge") +
      geom_text(aes(label = paste0(round(percent, 1), "%")),
                position = position_dodge(width = 0.9),
                vjust = -0.3, size = 3.5) +
      labs(title = "Mode Share Comparison: Base vs Intervention",
           x = "Transport Mode", y = "Percentage (%)", fill = "Scenario") +
      theme_minimal(base_size = 14) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            legend.position = "bottom") +
      scenario_scale("fill")
  })

  output$trip_length_plot <- renderPlot({
    sampled <- density_sample()

    ggplot(sampled$data, aes(x = dist * 1000, color = scenario)) +
      geom_density(alpha = 0.7, linewidth = 1.2) +
      scale_x_continuous(
        trans = "log2",
        breaks = c(250, 500, 1000, 2000, 5000, 10000, 20000, 50000, 100000, 200000),
        labels = c("250", "500", "1,000", "2,000", "5,000", "10,000", "20,000",
                   "50,000", "100,000", "200,000"),
        limits = c(100, 200000)
      ) +
      labs(
        title = "Trip Length Distribution: Base vs Intervention",
        x = "Distance (metres, log2 scale)",
        color = "Scenario",
        caption = sprintf(
          "Density estimated from a stratified random sample of up to %s trips per scenario x mode (n = %s of %s).",
          format(DENSITY_SAMPLE_N, big.mark = ","),
          format(sampled$n_sampled, big.mark = ","),
          format(sampled$n_total, big.mark = ","))
      ) +
      theme_minimal(base_size = 14) +
      theme(legend.position = "bottom") +
      facet_grid(mode ~ ., scales = "free_y", switch = "both") +
      theme(strip.placement = "outside") +
      scenario_scale("colour")
  })

  output$median_distance_table <- render_gt({
    comparison <- median_summary() %>%
      pivot_wider(names_from = scenario, values_from = median_dist_km) %>%
      mutate(change = Intervention - Base,
             pct_change = (change / Base) * 100)

    value_cols <- intersect(scenarios_present(), names(comparison))

    comparison %>%
      select(mode, all_of(value_cols), change, pct_change) %>%
      gt() %>%
      tab_header(
        title = "Median Trip Distance Comparison (km)",
        subtitle = "Change from base to intervention"
      ) %>%
      cols_label(mode = "Mode", change = "Change (km)", pct_change = "% Change") %>%
      fmt_number(columns = c(all_of(value_cols), change), decimals = 2) %>%
      fmt_number(columns = pct_change, decimals = 1, pattern = "{x}%") %>%
      tab_style(
        style = cell_fill(color = "lightgreen"),
        locations = cells_body(columns = change, rows = change > 0.1)
      ) %>%
      tab_style(
        style = cell_fill(color = "lightcoral"),
        locations = cells_body(columns = change, rows = change < -0.1)
      )
  })

  output$median_distance_plot <- renderPlot({
    ggplot(median_summary(), aes(x = mode, y = median_dist_km, fill = scenario)) +
      geom_col(position = "dodge") +
      geom_text(
        aes(label = round(median_dist_km, 2)),
        position = position_dodge(width = 0.9),
        vjust = -0.3,
        size = 3.5
      ) +
      labs(
        title = "Median Trip Distance by Mode and Scenario",
        x = "Transport Mode",
        y = "Median Distance (km)",
        fill = "Scenario"
      ) +
      theme_minimal(base_size = 14) +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom"
      ) +
      scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
      scenario_scale("fill")
  })

  output$weekly_plots <- renderPlot({
    summary_data <- weekly_summary()

    p1 <- ggplot(summary_data, aes(x = mode, y = avgDist, fill = scenario)) +
      geom_col(position = "dodge2") +
      geom_text(aes(label = round(avgDist, 1)),
                position = position_dodge(width = 0.9),
                hjust = -0.2, vjust = 0.5, size = 3.5) +
      labs(title = "Average Weekly Distance (km) by Mode Per Person",
           fill = "Scenario", x = "", y = "km") +
      coord_flip() +
      theme_minimal(base_size = 13) +
      theme(legend.position = "bottom") +
      scenario_scale("fill")

    p2 <- ggplot(summary_data, aes(x = mode, y = avgTime, fill = scenario)) +
      geom_col(position = "dodge2") +
      geom_text(aes(label = round(avgTime, 1)),
                position = position_dodge(width = 0.9),
                hjust = -0.2, vjust = 0.5, size = 3.5) +
      labs(title = "Average Weekly Time (minutes) by Mode Per Person",
           fill = "Scenario", x = "", y = "minutes") +
      coord_flip() +
      theme_minimal(base_size = 13) +
      theme(legend.position = "bottom") +
      scenario_scale("fill")

    grid.arrange(p1, p2, ncol = 1)
  })

  output$purpose_plot <- renderPlot({
    ggplot(purpose_summary(), aes(x = scenario, y = percent, fill = t.purpose)) +
      geom_bar(stat = "identity", position = "fill") +
      geom_text(
        aes(label = ifelse(percent > 2, paste0(round(percent, 1), "%"), "")),
        position = position_fill(vjust = 0.5),
        color = "white", size = 3.5
      ) +
      labs(
        title = "Trip Purpose Distribution",
        y = "Proportion (%)", x = "Scenario", fill = "Purpose"
      ) +
      theme_minimal(base_size = 14) +
      theme(
        axis.text.x = element_text(face = "bold"),
        legend.position = "bottom"
      ) +
      guides(fill = guide_legend(nrow = 3)) +
      coord_flip() +
      scale_y_reverse()
  })

  output$summary_text <- renderText({
    comparison <- mode_comparison()

    cycling_mode_change <- comparison %>%
      filter(mode == "Cycling") %>%
      pull(change)

    modes_increased <- comparison %>%
      filter(change > 0.5) %>%
      pull(mode)

    modes_decreased <- comparison %>%
      filter(change < -0.5) %>%
      pull(mode)

    vista_note <- if ("VISTA" %in% scenarios_present()) {
      vista_cycling <- comparison %>% filter(mode == "Cycling") %>% pull(VISTA)
      paste0("\n\nVISTA (observed) cycling mode share: ",
             sprintf("%.2f%%", vista_cycling))
    } else {
      ""
    }

    paste0(
      "Intervention Impact Summary\n\n",
      "Cycling mode share change: ", sprintf("%+.2f%%", cycling_mode_change), "\n\n",
      "Modes that increased: ", paste(modes_increased, collapse = ", "), "\n\n",
      "Modes that decreased: ", paste(modes_decreased, collapse = ", "),
      vista_note
    )
  })

  output$key_findings <- renderUI({
    req(data$loaded)

    vista_block <- if ("VISTA" %in% scenarios_present()) {
      "<h5>VISTA Reference Comparison</h5>
       <p>VISTA columns show the observed Melbourne travel survey, weighted to population
       (<em>VISTA</em>) and as raw respondent counts (<em>VISTA_noWeight</em>). Use these to
       judge whether each model scenario is plausible against observed behaviour.</p>"
    } else {
      "<h5>VISTA Reference Comparison</h5>
       <p>Tick <em>Include VISTA Reference Data</em> and supply the VISTA trips and persons
       files to compare both scenarios against observed travel survey data.</p>"
    }

    HTML(paste0("
      <h4>Impact of the Intervention</h4>
      <p>This analysis compares travel patterns between the base scenario and the intervention scenario.</p>

      <h5>Mode Share Changes</h5>
      <p>Review the Mode Share tab to see how the intervention affected mode choice across all transport modes.</p>

      <h5>Trip Distribution</h5>
      <p>Examine the Trip Distribution tab to understand how trip distances changed for each mode under the intervention.</p>

      <h5>Travel Behavior</h5>
      <p>The Weekly Patterns and Trip Purpose tabs show broader changes in travel behavior and trip-making patterns.</p>
      ", vista_block))
  })
}

# Run the application
shinyApp(ui = ui, server = server)
