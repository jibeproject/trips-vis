require(tidyverse)
require(sf)
require(gt)
require(ggplot2)
require(dplyr)
require(stringr)
require(arrow)
require(plotly)

# Change the path as needed, currently set to Manchester folder
dir_path <- ""

zone <- read_csv(paste0(dir_path, "synpop/sp_2021/zoneSystem.csv"))

trips_ref <- read_csv(paste0(dir_path, "simulationResults/ForUrbanTransition/reference/travel_demand_mito/trips.csv"))

trips_cint <- read_csv(paste0(dir_path, "simulationResults/ForUrbanTransition/cycleIntervention/travel_demand_mito/trips.csv"))

pp <- read_csv(paste0(dir_path, "synpop/sp_2021/pp_2021.csv"))


## Data Preparation
# ################### Data Preparation ########################

trips_ref <- trips_ref %>%
  mutate(scenario = "Reference")

trips_cint <- trips_cint %>%
  mutate(scenario = "Cycling Intervention")

trips <- bind_rows(trips_ref, trips_cint)

#rm(trips_ref, trips_cint)

# ################### Adding Zone and Demographics ########################
# 
trips <- trips %>%
  left_join(zone %>%
              select(LAD_origin = ladnm,imd_origin = imd10, oaID),
            by = c("origin" = "oaID")) %>% 
  left_join(zone %>% 
              select(LAD_destination = ladnm, imd_destination = imd10, oaID),
            by = c("destination" = "oaID")) %>% 
  left_join(pp%>%
              select(id,age,gender,occupation), by = c("p.ID" = "id"))

rm(pp, zone)

trips <- trips %>%
  #select(p.ID,t.distance_walk,t.distance_bike,t.distance_auto,t.distance_auto,time_auto,time_pt,mode,scenario,LAD_origin, imd_origin) %>%
  mutate(time_walk = t.distance_walk/2.92,
         time_bike = t.distance_bike/10.44,
         time_pt = as.numeric(time_pt),
         time_auto = time_auto/60,
         time_pt = time_pt/60,
         mode = case_when(
           mode == "autoDriver" ~ "Driving Car",
           mode == "autoPassenger" ~ "Car Passenger",
           mode == "pt" ~ "Public Transport",
           mode == "walk" ~ "Walking",
           mode == "bicycle" ~ "Cycling",
           TRUE ~ "Other"),
         mode = factor(mode, levels = c("Driving Car",
                                        "Car Passenger",
                                        "Public Transport",
                                        "Walking",
                                        "Cycling",
                                        "Other")),
         scenario = factor(scenario, levels = c("Reference",
                                                "Cycling Intervention")))
# 
# ################### write out into Parquet file ########################
arrow::write_parquet(trips, "D:/Users/aa797/RMIT University/JIBE working group - General/manchester/simulationResults/ForUrbanTransition/visualization/trips.parquet")

