library(stats19)
library(bnlearn)
library(Rgraphviz)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(stringr)

#collision1 <- get_stats19(year = 2024, type = "collision", ask = FALSE)
#casualties <- get_stats19(year = 2024, type = "casualty", ask = FALSE)
#vehicles <- get_stats19(year = 2024, type = "vehicle", ask = FALSE)

collision <- read.csv('collision_2024.csv')
vehicles <- read.csv('vehicle_2024.csv')
casualties <- read.csv('casualty_2024.csv')

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
    imd_num = case_when(
      str_detect(casualty_imd_decile, "Least deprived 10%") ~ 5,
      str_detect(casualty_imd_decile, "Less deprived 10-20%") ~ 15,
      str_detect(casualty_imd_decile, "Less deprived 20-30%") ~ 25,
      str_detect(casualty_imd_decile, "Less deprived 30-40%") ~ 35,
      str_detect(casualty_imd_decile, "Less deprived 40-50%") ~ 45,
      str_detect(casualty_imd_decile, "More deprived 10-20%") ~ 15,  # check if label is inverted
      str_detect(casualty_imd_decile, "More deprived 20-30%") ~ 25,
      str_detect(casualty_imd_decile, "More deprived 30-40%") ~ 35,
      str_detect(casualty_imd_decile, "More deprived 40-50%") ~ 45,
      str_detect(casualty_imd_decile, "Most deprived 10%") ~ 95,
      TRUE ~ NA_real_
    )) %>%
  mutate(
    severity = factor(casualty_severity, levels = severity_order, ordered = TRUE)
  ) %>%
  group_by(collision_index) %>%
  summarise(
    mean_imd = mean(imd_num, na.rm = TRUE),
    pedestrian = ifelse(pedestrian_movement == 'Not a Pedestrian', 0, 1),
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
  group_by(collision_index, vehicle_group) %>%
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

# vehicle 2:
vehicle_agg2 <- vehicles %>%
  group_by(collision_index) %>%
  summarise(veh_age = max(age_of_vehicle))

junction_agg <- vehicles %>%
  mutate(
    junction_group = case_when(
      str_detect(junction_location, regex("Not at", ignore_case = TRUE))   ~ "No junction",
      str_detect(junction_location, regex("roundabout", ignore_case = TRUE)) ~ "Roundabout",
      str_detect(junction_location, regex("junction", ignore_case = TRUE))  ~ "T-Junction",
      TRUE ~ "Other junction"
    )
  ) %>%
  group_by(collision_index) %>%
  summarise(
    # final label: "Mixed" when more than one distinct junction_group appears in this collision,
    # otherwise the single junction_group value
    junction_type = if (n_distinct(junction_group) > 1) "Mixed" else first(junction_group),
    # optional debugging / info columns:
    junction_types = list(sort(unique(junction_group))),  # list of types (can be removed)
    n_types = n_distinct(junction_group),
    .groups = "drop"
  )

vehicle_final <- vehicle_agg %>%
  left_join(junction_agg, by = "collision_index") %>%
  left_join(vehicle_agg2, by = "collision_index")


collision_final <- collision %>%
  full_join(casualty_agg, by = "collision_index") %>%
  full_join(vehicle_final, by = "collision_index")

collision_final <- collision_final %>%
  mutate(
    total_vehicles_cat = case_when(
      total_vehicles == 1 ~ "Single",
      total_vehicles == 2 ~ "Double",
      total_vehicles >= 3 ~ "Multiple"
    )
  )

collision_final$number_of_casualties <- as.numeric(collision_final$number_of_casualties)

quantile(collision_final$number_of_casualties, c(0,.1,.2,.3,.4, .5, .6, .7, .8, .99))
#unique(quantile(data_bn$casualty_count, probs = seq(0,1, length.out = 4), na.rm=TRUE))
collision_final2 <- collision_final[collision_final$number_of_casualties >= 7,]
#collision_final$mean_imd
ggplot(data = collision_final, aes(mean_imd, number_of_casualties)) +
  geom_point() + geom_smooth(method = 'lm')
colnames(collision_final)
model <- lm(number_of_casualties ~ mean_imd + veh_age + day_of_week,
            data = collision_final)
summary(model)



collision_final <- collision_final %>%
  filter(enhanced_severity_collision != -1) %>%   # drop missing rows
  mutate(
    enhanced_severity_collision = factor(
      enhanced_severity_collision,
      levels = c(1, 5, 6, 7, 3),
      labels = c(
        "Fatal",
        "Very serious",
        "Moderately serious",
        "Less serious",
        "Slight"
      ),
      ordered = TRUE   # optional, if severity should be ordinal
    )
  )

df <- collision_final  # your data.frame

# 1A: convert any character columns to factor
char_cols <- names(df)[sapply(df, is.character)]
df[char_cols] <- lapply(df[char_cols], function(x) factor(x))

# 1B: vehicle-presence binary factors (if your columns are counts)
vehicle_cols <- c("car","motorcycle","bus_truck","bicycle","other")
for (v in vehicle_cols) {
  if (v %in% names(df)) {
    df[[paste0(v, "_present")]] <- factor(ifelse(is.na(df[[v]]), NA,
                                                 ifelse(df[[v]] > 0, "yes", "no")),
                                          levels = c("no","yes"))
  }
}

# 1C: convert existing total_junctions / total_vehicles (if numeric) into small bins
df$total_vehicles_num <- as.numeric(as.character(df$total_vehicles))  # if it was character
df$total_vehicles_cat <- cut(df$total_vehicles_num,
                             breaks = c(-Inf,1,2,3,Inf),
                             labels = c("0","1","2","3+"), right = TRUE)

# 1D: casualty count -> ordered factor
df$casualty_count_num <- as.numeric(as.character(df$casualty_count))
df$casualty_count_cat <- cut(df$casualty_count_num,
                             breaks = c(-Inf,0,1,2,Inf),
                             labels = c("0","1","2","3+"), ordered_result = TRUE)
#df$casualty_count_cat <- cut(
#  df$casualty_count,
#  breaks = c(-Inf, 0, 2, 5, Inf),
#  labels = c("0", "1-2", "3-5", "6+"),
#  right = TRUE
#)
# 1E: pedestrian indicator (you said a binary variable if any casualty is a pedestrian)
if ("pedestrian" %in% names(df)) {
  df$pedestrian_any <- factor(ifelse(df$pedestrian > 0, "yes", "no"),
                              levels = c("no","yes"))
}

# 1F: vehicle age -> bins (example)
df$veh_age_num <- as.numeric(as.character(df$veh_age))
df$veh_age_cat <- cut(df$veh_age_num, breaks = c(-Inf,5,10,15,Inf),
                      labels = c("0-5","6-10","11-15","16+"), right = TRUE)

# 1G: lighting, road surface, weather, urban_or_rural — ensure they're factors
for (c in c("light_conditions","road_surface_conditions","weather_conditions","urban_or_rural_area")) {
  if (c %in% names(df)) df[[c]] <- factor(df[[c]])
}


# tertiles (low / mid / high)
df$imd_cat_3 <- cut(df$mean_imd,
                    breaks = unique(quantile(df$mean_imd, probs = seq(0,1, length.out=4), na.rm=TRUE)),
                    include.lowest = TRUE,
                    labels = c("low","mid","high"))

# deciles 
dec_breaks <- unique(quantile(df$mean_imd, probs = seq(0,1, by=0.2), na.rm=TRUE))
df$imd_cat_5 <- cut(df$mean_imd, breaks = dec_breaks, include.lowest = TRUE,
                    labels = paste0("d",1:(length(dec_breaks)-1)))

df$imd_cat_3 <- df$imd_cat_5
df$casualty_count_cat


ggplot(df, aes(casualty_severity, mean_imd)) +
  geom_boxplot() 


#collision_final <- df$casualty_count_cat
colnames(collision_final)
model <- lm(number_of_casualties ~ mean_imd + veh_age,
            data = collision_final)
summary(model)

df$casualty_count_cat <- df$enhanced_severity_collision

# select a working dataset with only variables you want in BN
vars_to_keep <- c("imd_cat_3", "casualty_count_cat", "pedestrian_any",
                  'junction_type', 'day_of_week',
                  "total_vehicles_cat", "total_junctions_cat", "veh_age_cat",
                  "urban_or_rural_area", "light_conditions", "road_surface_conditions",
                  paste0(vehicle_cols, "_present"))  # adjust to your columns
data_bn <- df %>% select(all_of(intersect(vars_to_keep, names(df)))) %>% na.omit()


df <- data_bn

if (! "pedestrian_any" %in% names(df) && "pedestrian" %in% names(df)) {
  # handle pedestrian being count or factor/character
  ped_vec <- df$pedestrian
  # try to coerce numeric when possible, otherwise treat character > "0"
  ped_num <- suppressWarnings(as.numeric(as.character(ped_vec)))
  df$pedestrian_any <- factor(ifelse(!is.na(ped_num) & ped_num > 0, "yes",
                                     if (!is.na(ped_num)) "no"
                                     else ifelse(as.character(ped_vec) %in% c("yes","Yes","YES","1"), "yes",
                                                 ifelse(as.character(ped_vec) %in% c("no","No","NO","0"), "no", NA))),
                              levels = c("no","yes"))
  message("Created pedestrian_any from pedestrian column.")
}

# Define the set of candidate nodes you intended for the DAG
candidate_nodes <- c("imd_cat_3", "urban_or_rural_area", "veh_age_cat",
                     'day_of_week',
                     "road_surface_conditions", "light_conditions", "car_present", "motorcycle_present",
                     "bicycle_present", "bus_truck_present", "other_present",
                     "junction_type", "casualty_count_cat", "pedestrian_any",
                     'casualty_severity')

# show which of those exist
available <- intersect(candidate_nodes, names(df))
available
missing_nodes <- setdiff(candidate_nodes, available)
cat("Available nodes:\n"); print(available)
cat("Missing candidate nodes (will be ignored unless created):\n"); print(missing_nodes)

# Define your desired arcs as pairs (from -> to) in a list
desired_arcs <- list(
  c("imd_cat_3","urban_or_rural_area"),
  c("imd_cat_3","veh_age_cat"),
  c("imd_cat_3","junction_type"),
  c("imd_cat_3","light_conditions"),
  c("urban_or_rural_area","light_conditions"),
  c("urban_or_rural_area","junction_type"),
  # casualty drivers (many parents -> casualty)
  c("car_present","casualty_count_cat"),
  c("motorcycle_present","casualty_count_cat"),
  c("bicycle_present","casualty_count_cat"),
  c("bus_truck_present","casualty_count_cat"),
  c("other_present","casualty_count_cat"),
  c("junction_type","casualty_count_cat"),
  c("road_surface_conditions","casualty_count_cat"),
  c("light_conditions","casualty_count_cat"),
  c("veh_age_cat","casualty_count_cat"),
  c("pedestrian_any","casualty_count_cat"),
  
  c("imd_cat_3","car_present"),
  c("imd_cat_3","motorcycle_present"),
  c("imd_cat_3","bicycle_present"),
  c("imd_cat_3","bus_truck_present"),
  
  c("imd_cat_3","pedestrian_any"),
  
  c("imd_cat_3","junction_type"),
  c("day_of_week","casualty_count_cat")
)

#Filter the arcs so we only include arcs where both nodes are available
valid_arcs_list <- lapply(desired_arcs, function(a) {
  if (all(a %in% available)) return(a) else return(NULL)
})
# remove NULLs
valid_arcs_list <- Filter(Negate(is.null), valid_arcs_list)

if (length(valid_arcs_list) == 0) stop("No valid arcs found: none of the candidate nodes are present in df. Check your column names.")

# convert to a two-column matrix for bnlearn::arcs assignment
valid_arcs_mat <- do.call(rbind, valid_arcs_list)
colnames(valid_arcs_mat) <- c("from","to")
cat("Using these arcs (from -> to):\n"); print(valid_arcs_mat)

#  Create a DAG with the available nodes and add valid arcs
dag <- empty.graph(nodes = available)
arcs(dag) <- valid_arcs_mat

# sanity check: plot and/or print
print(dag)
#plot(dag)  # will pop up in R plotting device
library(Rgraphviz)
graphviz.plot(dag, shape = "ellipse")
# Fit the BN (only on rows without NAs for the chosen variables)
data_bn <- df[ , available, drop=FALSE ]
data_bn <- na.omit(data_bn)
data_bn <- as.data.frame(data_bn)

# Identify rows where any column contains the value
rows_to_remove <- apply(data_bn, 1, function(row) any(row == 'Data missing or out of range'))

# Remove the identified rows
data_bn2 <- data_bn[!rows_to_remove, ]

#fitted <- bn.fit(dag, data_bn, method = "bayes")  # this is bayes
fitted <- bn.fit(dag, data_bn)  # this is mle
