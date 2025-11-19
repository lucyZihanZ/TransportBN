# 
library(tidyverse)
library(dplyr)
library(stringr)
# Here we analyze three different parts 
casualties = read.csv("/Users/zihanzhao/Documents/fall/BN-Benchmark/datasets/casualty_2024.csv")
vehicles = read.csv("/Users/zihanzhao/Documents/fall/BN-Benchmark/datasets/vehicle_2024.csv")
collision = read.csv("/Users/zihanzhao/Documents/fall/BN-Benchmark/datasets/collision_2024.csv")
# 
unique(vehicles$vehicle_type)

# preprocessed:
vehicle_grouped <- vehicles %>%
  mutate(
    vehicle_group = case_when(
      # --- Car types ---
      str_detect(vehicle_type, regex("car|taxi|minicab|estate|saloon|hatchback|MPV|van", ignore_case = TRUE)) ~ "car",
      
      # --- Bus / Truck types ---
      str_detect(vehicle_type, regex("bus|coach|goods|lorry|truck|HGV|LGV", ignore_case = TRUE)) ~ "bus_truck",
      
      # --- Bicycle ---
      str_detect(vehicle_type, regex("pedal cycle|bicycle", ignore_case = TRUE)) ~ "bicycle",
      
      # --- Motorcycle ---
      str_detect(vehicle_type, regex("motorcycle|moped|scooter", ignore_case = TRUE)) ~ "motorcycle",
      
      # --- Everything else ---
      TRUE ~ "other"
    )
  ) %>%
  # Count per collision
  count(vehicle_group) %>%
  # Wide format: one column for each category
  tidyr::pivot_wider(
    names_from = vehicle_group,
    values_from = n,
    values_fill = 0
  )
# 
unique(collision$light_conditions)

# casualties:
severity_order <- c("Slight", "Serious", "Fatal")

casualty_agg <- casualties %>%
  mutate(
    severity = factor(casualty_severity, levels = severity_order, ordered = TRUE)
  ) %>%
  group_by(collision_index) %>%
  summarise(
    casualty_severity = max(severity),          # pick most severe
    casualty_avg_age = mean(age_of_casualty, na.rm = TRUE),
    casualty_count = n()
  )
# vehicle:
vehicle_agg <- vehicles %>%
  mutate(
    vehicle_group = case_when(
      str_detect(vehicle_type, regex("car|taxi|minicab|estate|saloon|hatchback|MPV|van", ignore_case = TRUE)) ~ "car",
      str_detect(vehicle_type, regex("bus|coach|goods|lorry|truck|HGV|LGV", ignore_case = TRUE)) ~ "bus_truck",
      str_detect(vehicle_type, regex("pedal cycle|bicycle", ignore_case = TRUE)) ~ "bicycle",
      str_detect(vehicle_type, regex("motorcycle|moped|scooter", ignore_case = TRUE)) ~ "motorcycle",
      TRUE ~ "other"
    )
  ) %>%

  mutate(
    junction_group = case_when(
      # --- No junction ---
      str_detect(junction_location, regex("Not at", ignore_case = TRUE)) ~ "No junction",
      
      # --- Roundabout-related ---
      str_detect(junction_location, regex("roundabout", ignore_case = TRUE)) ~ "Roundabout",
      
      # --- T-Junction-related ---
      str_detect(junction_location, regex("junction", ignore_case = TRUE)) ~ "T-Junction",
      
      # --- Everything else ---
      TRUE ~ "Other junction"
    )
  ) %>%
  group_by(collision_index, vehicle_group, junction_group) %>%
  summarise(n = n(), .groups = "drop") %>%  
  tidyr::pivot_wider(
    names_from = vehicle_group,
    values_from = n,
    values_fill = 0
  ) %>%
  mutate(
    car        = coalesce(car, 0),
    bus_truck  = coalesce(bus_truck, 0),
    motorcycle = coalesce(motorcycle, 0),
    bicycle    = coalesce(bicycle, 0),
    other      = coalesce(other, 0),
    # total number
    total_vehicles = car + bus_truck + motorcycle + bicycle + other
  )
# TODO: combine the junction into vehicle_agg.
# TODO: combine casualty_agg and vehicle_agg into the collision(smallest number in this set).
# TODO: need BN methods.

  

