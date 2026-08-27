# To run
# shiny::runApp("D:/projects/jibe/trips-vis/trip_intervention_comparison_app.R")

library(shiny)
library(tidyverse)
library(ggplot2)
library(dplyr)
library(data.table)
library(gt)
library(gridExtra)

# Define UI
ui <- fluidPage(
  titlePanel("Trip Intervention Comparison: Base vs Cycling Scenario"),
  
  sidebarLayout(
    sidebarPanel(
      h3("Data Loading"),
      
      textInput("base_path", 
                "Base Scenario trips.csv Path:",
                value = "",
                placeholder = "C:/path/to/base/trips.csv"),
      
      textInput("cycling_path", 
                "Cycling Scenario trips.csv Path:",
                value = "",
                placeholder = "C:/path/to/cycling/trips.csv"),
      
      textInput("persons_path", 
                "Persons file Path (pp_*.csv):",
                value = "",
                placeholder = "C:/path/to/pp_*.csv"),
      
      actionButton("load_data", "Load Data", class = "btn-primary"),
      
      hr(),
      
      h4("Data Status:"),
      textOutput("data_status"),
      
      hr(),
      
      h4("Analysis Options:"),
      checkboxInput("show_vista", "Include VISTA Reference Data", value = FALSE),
      
      conditionalPanel(
        condition = "input.show_vista == true",
        textInput("vista_trips_path", 
                  "VISTA trips.csv Path:",
                  placeholder = "C:/path/to/T_VISTA_*.csv"),
        textInput("vista_persons_path", 
                  "VISTA persons.csv Path:",
                  placeholder = "C:/path/to/P_VISTA_*.csv"),
        textInput("vista_households_path", 
                  "VISTA households.csv Path:",
                  placeholder = "C:/path/to/H_VISTA_*.csv")
      ),
      
      hr(),
      
      downloadButton("download_report", "Download Report")
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

# Define server logic
server <- function(input, output, session) {
  
  # Reactive values to store loaded data
  data <- reactiveValues(
    trips = NULL,
    pp_ref = NULL,
    loaded = FALSE,
    status = "No data loaded"
  )
  
  # Load data when button is clicked
  observeEvent(input$load_data, {
    
    req(input$base_path, input$cycling_path, input$persons_path)
    
    withProgress(message = 'Loading data...', value = 0, {
      
      tryCatch({
        incProgress(0.1, detail = "Loading base scenario...")
        trips_base <- fread(file = input$base_path)
        
        incProgress(0.2, detail = "Loading cycling scenario...")
        trips_cycling <- fread(file = input$cycling_path)
        
        incProgress(0.3, detail = "Loading persons file...")
        pp_ref <- fread(file = input$persons_path)
        
        incProgress(0.4, detail = "Processing base scenario...")
        trips_base <- trips_base %>% 
          mutate(scenario = "Base") %>%
          mutate(mode = case_when(
            mode == "autoDriver" ~ "Driving Car",
            mode == "autoPassenger" ~ "Car Passenger",
            mode == "pt" ~ "Public Transport",
            mode == "walk" ~ "Walking",
            mode == "bicycle" ~ "Cycling",
            TRUE ~ "Other"),
            t.factor = if_else(t.purpose %in% c("HBW", "HBE", "HBA", "HBS", "HBR", "HBO", "RRT"), 2, 1),
            time_walk = time_walk/60,
            time_bike = time_bike/60,
            time_auto = time_auto/60,
            time_pt = time_pt/60,
            time = case_when(mode=="Cycling"~time_bike,
                             mode=="Walking"~time_walk,
                             mode=="Public Transport"~time_pt,
                             mode=="Driving Car"~time_auto,
                             mode=="Car Passenger"~time_auto),
            dist = case_when(mode=="Cycling"~t.distance_bike,
                             mode=="Walking"~t.distance_walk,
                             mode=="Public Transport"~t.distance_auto,
                             mode=="Driving Car"~t.distance_auto,
                             mode=="Car Passenger"~t.distance_auto),
            time_factored = time * t.factor,
            dist_factored = dist * t.factor,
            t.weight = 1,
            p.weight = 1,
            p.ID = as.character(p.ID)) %>% 
          select(p.ID, t.purpose, time, time_factored, dist, dist_factored, mode,
                 scenario, t.factor, t.weight, p.weight)
        
        incProgress(0.6, detail = "Processing cycling scenario...")
        trips_cycling <- trips_cycling %>% 
          mutate(scenario = "Cycling") %>%
          mutate(mode = case_when(
            mode == "autoDriver" ~ "Driving Car",
            mode == "autoPassenger" ~ "Car Passenger",
            mode == "pt" ~ "Public Transport",
            mode == "walk" ~ "Walking",
            mode == "bicycle" ~ "Cycling",
            TRUE ~ "Other"),
            t.factor = if_else(t.purpose %in% c("HBW", "HBE", "HBA", "HBS", "HBR", "HBO", "RRT"), 2, 1),
            time_walk = time_walk/60,
            time_bike = time_bike/60,
            time_auto = time_auto/60,
            time_pt = time_pt/60,
            time = case_when(mode=="Cycling"~time_bike,
                             mode=="Walking"~time_walk,
                             mode=="Public Transport"~time_pt,
                             mode=="Driving Car"~time_auto,
                             mode=="Car Passenger"~time_auto),
            dist = case_when(mode=="Cycling"~t.distance_bike,
                             mode=="Walking"~t.distance_walk,
                             mode=="Public Transport"~t.distance_auto,
                             mode=="Driving Car"~t.distance_auto,
                             mode=="Car Passenger"~t.distance_auto),
            time_factored = time * t.factor,
            dist_factored = dist * t.factor,
            t.weight = 1,
            p.weight = 1,
            p.ID = as.character(p.ID)) %>% 
          select(p.ID, t.purpose, time, time_factored, dist, dist_factored, mode,
                 scenario, t.factor, t.weight, p.weight)
        
        incProgress(0.8, detail = "Combining data...")
        trips <- bind_rows(trips_base, trips_cycling) %>%
          mutate(scenario = factor(scenario, levels = c("Base", "Cycling")),
                 mode = factor(mode, levels = c("Driving Car", "Car Passenger", "Public Transport", 
                                                 "Walking", "Cycling", "Other")),
                 t.purpose = case_when(
                   t.purpose == "HBW" ~ "Home-based-work",
                   t.purpose == "HBE" ~ "Home-based-education",
                   t.purpose == "HBA" ~ "Home-based-accompanying",
                   t.purpose == "HBS" ~ "Home-based-shopping",
                   t.purpose == "HBR" ~ "Home-based-recreation",
                   t.purpose == "HBO" ~ "Home-based-other",
                   t.purpose == "NHBO" ~ "Non-home-based-other",
                   t.purpose == "NHBW" ~ "Non-home-based-work",
                   t.purpose == "RRT" ~ "Recreational round trip",
                   TRUE ~ NA_character_),
                 t.purpose = factor(t.purpose, levels = c("Home-based-work", "Home-based-education", 
                                                           "Home-based-accompanying", "Home-based-shopping", 
                                                           "Home-based-recreation", "Home-based-other", 
                                                           "Non-home-based-other", "Non-home-based-work",
                                                           "Recreational round trip")))
        
        # Filter out problem records
        trips <- trips %>%
          filter(
            dist > 0,
            time > 0,
            !is.infinite(time),
            !is.infinite(dist),
            !is.na(t.weight)
          )
        
        incProgress(1.0, detail = "Data loaded successfully!")
        
        data$trips <- trips
        data$pp_ref <- pp_ref
        data$loaded <- TRUE
        data$status <- sprintf("Data loaded: %s Base trips, %s Cycling trips, %s persons", 
                               format(nrow(trips_base), big.mark=","),
                               format(nrow(trips_cycling), big.mark=","),
                               format(nrow(pp_ref), big.mark=","))
        
      }, error = function(e) {
        data$status <- paste("Error loading data:", e$message)
        showNotification(paste("Error:", e$message), type = "error", duration = 10)
      })
    })
  })
  
  # Data status output
  output$data_status <- renderText({
    data$status
  })
  
  # Key figures table
  output$key_figures_table <- render_gt({
    req(data$loaded)
    
    pop_baseline <- nrow(data$pp_ref)
    
    summary_data <- data$trips %>% 
      group_by(scenario) %>% 
      summarise(trips = sum(t.factor * t.weight, na.rm = TRUE), .groups = 'drop') %>%
      mutate(pop = pop_baseline,
             avgTrip = trips/pop)
    
    summary_data %>%
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
  
  # Diagnostics table
  output$diagnostics_table <- render_gt({
    req(data$loaded)
    
    diagnostics <- data$trips %>%
      group_by(scenario, mode) %>%
      summarise(
        n = n(),
        mean_dist = mean(dist, na.rm = TRUE),
        median_dist = median(dist, na.rm = TRUE),
        mean_time = mean(time, na.rm = TRUE),
        median_time = median(time, na.rm = TRUE),
        .groups = 'drop'
      )
    
    diagnostics %>%
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
  
  # Mode share table
  output$mode_share_table <- render_gt({
    req(data$loaded)
    
    mode_summary <- data$trips %>%
      group_by(scenario, mode) %>%
      summarise(count = sum(t.factor * t.weight, na.rm = TRUE), .groups = 'drop') %>%
      group_by(scenario) %>%
      mutate(percent = count / sum(count) * 100)
    
    mode_comparison <- mode_summary %>%
      select(scenario, mode, percent) %>%
      pivot_wider(names_from = scenario, values_from = percent) %>%
      mutate(
        change = Cycling - Base,
        pct_change = (change / Base) * 100
      )
    
    mode_comparison %>%
      gt() %>%
      tab_header(title = "Mode Share Comparison (%)", 
                 subtitle = "Impact of cycling intervention on mode choice") %>%
      fmt_number(columns = c(Base, Cycling, change), decimals = 2) %>%
      fmt_number(columns = pct_change, decimals = 1, suffix = "%") %>%
      tab_style(
        style = cell_fill(color = "lightgreen"),
        locations = cells_body(
          columns = change,
          rows = change > 0.5
        )
      ) %>%
      tab_style(
        style = cell_fill(color = "lightcoral"),
        locations = cells_body(
          columns = change,
          rows = change < -0.5
        )
      )
  })
  
  # Mode share plot
  output$mode_share_plot <- renderPlot({
    req(data$loaded)
    
    mode_summary <- data$trips %>%
      group_by(scenario, mode) %>%
      summarise(count = sum(t.factor * t.weight, na.rm = TRUE), .groups = 'drop') %>%
      group_by(scenario) %>%
      mutate(percent = count / sum(count) * 100)
    
    ggplot(mode_summary, aes(x = mode, y = percent, fill = scenario)) +
      geom_col(position = "dodge") +
      geom_text(aes(label = paste0(round(percent, 1), "%")),
                position = position_dodge(width = 0.9),
                vjust = -0.3, size = 4) +
      labs(title = "Mode Share Comparison: Base vs Cycling Intervention",
           x = "Transport Mode", y = "Percentage (%)", fill = "Scenario") +
      theme_minimal(base_size = 14) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            legend.position = "bottom") +
      scale_fill_manual(values = c("Base" = "#4A90E2", "Cycling" = "#2ECC71"))
  })
  
  # Trip length distribution plot
  output$trip_length_plot <- renderPlot({
    req(data$loaded)
    
    ggplot(data$trips, aes(x = dist * 1000, color = scenario)) +
      geom_density(alpha = 0.7, size = 1.2) +
      scale_x_continuous(
        trans = "log2",
        breaks = c(250, 500, 1000, 2000, 5000, 10000, 20000, 50000, 100000, 200000),
        labels = c("250", "500", "1,000", "2,000", "5,000", "10,000", "20,000", "50,000", "100,000", "200,000"),
        limits = c(100, 200000)
      ) +
      labs(
        title = "Trip Length Distribution: Base vs Cycling Intervention",
        x = "Distance (metres, log2 scale)",
        color = "Scenario"
      ) +
      theme_minimal(base_size = 14) +
      theme(legend.position = "bottom") + 
      scale_y_continuous(limits = c(0, 1)) +
      facet_grid(mode ~ ., scales = "free_y", switch = "both") +
      theme(strip.placement = "outside") +
      scale_color_manual(values = c("Base" = "#4A90E2", "Cycling" = "#2ECC71"))
  })
  
  # Median distance table
  output$median_distance_table <- render_gt({
    req(data$loaded)
    
    median_summary <- data$trips %>%
      group_by(scenario, mode) %>%
      summarise(median_dist_km = median(dist, na.rm = TRUE), .groups = 'drop')
    
    median_comparison <- median_summary %>%
      pivot_wider(names_from = scenario, values_from = median_dist_km) %>%
      mutate(
        change = Cycling - Base,
        pct_change = (change / Base) * 100
      )
    
    median_comparison %>%
      gt() %>%
      tab_header(
        title = "Median Trip Distance Comparison (km)",
        subtitle = "Change from base to cycling intervention"
      ) %>%
      cols_label(
        mode = "Mode",
        Base = "Base",
        Cycling = "Cycling",
        change = "Change (km)",
        pct_change = "% Change"
      ) %>%
      fmt_number(columns = c(Base, Cycling, change), decimals = 2) %>%
      fmt_number(columns = pct_change, decimals = 1, suffix = "%") %>%
      tab_style(
        style = cell_fill(color = "lightgreen"),
        locations = cells_body(
          columns = change,
          rows = change > 0.1
        )
      ) %>%
      tab_style(
        style = cell_fill(color = "lightcoral"),
        locations = cells_body(
          columns = change,
          rows = change < -0.1
        )
      )
  })
  
  # Median distance plot
  output$median_distance_plot <- renderPlot({
    req(data$loaded)
    
    median_summary <- data$trips %>%
      group_by(scenario, mode) %>%
      summarise(median_dist_km = median(dist, na.rm = TRUE), .groups = 'drop')
    
    ggplot(median_summary, aes(x = mode, y = median_dist_km, fill = scenario)) +
      geom_col(position = "dodge") +
      geom_text(
        aes(label = round(median_dist_km, 2)),
        position = position_dodge(width = 0.9),
        vjust = -0.3,
        size = 4
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
      scale_fill_manual(values = c("Base" = "#4A90E2", "Cycling" = "#2ECC71"))
  })
  
  # Weekly patterns plot
  output$weekly_plots <- renderPlot({
    req(data$loaded)
    
    pop_baseline <- nrow(data$pp_ref)
    
    dist_summary <- data$trips %>%
      group_by(p.ID, mode, scenario) %>%
      summarise(dist = sum(dist_factored * p.weight, na.rm = TRUE), .groups = 'drop') %>%
      group_by(mode, scenario) %>%
      summarise(totalDist = sum(dist), .groups = 'drop') %>% 
      mutate(avgDist = totalDist / pop_baseline)
    
    time_summary <- data$trips %>%
      group_by(p.ID, mode, scenario) %>%
      summarise(time = sum(time_factored * p.weight, na.rm = TRUE), .groups = 'drop') %>%
      group_by(mode, scenario) %>%
      summarise(totalTime = sum(time), .groups = 'drop') %>% 
      mutate(avgTime = totalTime / pop_baseline * 60)
    
    p1 <- ggplot(dist_summary, aes(x = mode, y = avgDist, fill = scenario)) +
      geom_col(position = "dodge2") +
      geom_text(aes(label = round(avgDist, 1)), 
                position = position_dodge(width = 0.9), 
                hjust = -0.2, vjust = 0.5, size = 4) +
      labs(title = "Average Weekly Distance (km) by Mode Per Person",
           fill = "Scenario", x = "", y = "km") +
      coord_flip() +
      theme_minimal(base_size = 13) +
      theme(legend.position = "bottom") +
      scale_fill_manual(values = c("Base" = "#4A90E2", "Cycling" = "#2ECC71"))
    
    p2 <- ggplot(time_summary, aes(x = mode, y = avgTime, fill = scenario)) +
      geom_col(position = "dodge2") +
      geom_text(aes(label = round(avgTime, 1)), 
                position = position_dodge(width = 0.9), 
                hjust = -0.2, vjust = 0.5, size = 4) +
      labs(title = "Average Weekly Time (minutes) by Mode Per Person",
           fill = "Scenario", x = "", y = "minutes") +
      coord_flip() +
      theme_minimal(base_size = 13) +
      theme(legend.position = "bottom") +
      scale_fill_manual(values = c("Base" = "#4A90E2", "Cycling" = "#2ECC71"))
    
    grid.arrange(p1, p2, ncol = 1)
  })
  
  # Trip purpose plot
  output$purpose_plot <- renderPlot({
    req(data$loaded)
    
    purpose_summary <- data$trips %>%
      group_by(scenario, t.purpose) %>%
      summarise(count = sum(t.factor * t.weight, na.rm = TRUE), .groups = 'drop') %>%
      group_by(scenario) %>%
      mutate(percent = count / sum(count) * 100)
    
    ggplot(purpose_summary, aes(x = scenario, y = percent, fill = t.purpose)) +
      geom_bar(stat = "identity", position = "fill") +
      geom_text(
        aes(label = ifelse(percent > 2, paste0(round(percent, 1), "%"), "")),
        position = position_fill(vjust = 0.5), 
        color = "white", size = 4
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
  
  # Summary text
  output$summary_text <- renderText({
    req(data$loaded)
    
    mode_summary <- data$trips %>%
      group_by(scenario, mode) %>%
      summarise(count = sum(t.factor * t.weight, na.rm = TRUE), .groups = 'drop') %>%
      group_by(scenario) %>%
      mutate(percent = count / sum(count) * 100)
    
    mode_comparison <- mode_summary %>%
      select(scenario, mode, percent) %>%
      pivot_wider(names_from = scenario, values_from = percent) %>%
      mutate(change = Cycling - Base)
    
    cycling_mode_change <- mode_comparison %>% 
      filter(mode == "Cycling") %>% 
      pull(change)
    
    modes_increased <- mode_comparison %>% 
      filter(change > 0.5) %>% 
      pull(mode)
    
    modes_decreased <- mode_comparison %>% 
      filter(change < -0.5) %>% 
      pull(mode)
    
    paste0(
      "Intervention Impact Summary\n\n",
      "Cycling mode share change: ", sprintf("%+.2f%%", cycling_mode_change), "\n\n",
      "Modes that increased: ", paste(modes_increased, collapse = ", "), "\n\n",
      "Modes that decreased: ", paste(modes_decreased, collapse = ", ")
    )
  })
  
  # Key findings
  output$key_findings <- renderUI({
    req(data$loaded)
    
    HTML("
      <h4>Impact of Cycling Intervention</h4>
      <p>This analysis compares travel patterns between the base scenario and a cycling intervention scenario.</p>
      
      <h5>Mode Share Changes</h5>
      <p>Review the Mode Share tab to see how the intervention affected mode choice across all transport modes.</p>
      
      <h5>Trip Distribution</h5>
      <p>Examine the Trip Distribution tab to understand how trip distances changed for each mode under the intervention.</p>
      
      <h5>Travel Behavior</h5>
      <p>The Weekly Patterns and Trip Purpose tabs show broader changes in travel behavior and trip-making patterns.</p>
    ")
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
