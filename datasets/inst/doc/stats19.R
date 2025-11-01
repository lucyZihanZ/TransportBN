## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  out.width = "100%",
  eval = curl::has_internet()
)

## ----eval=FALSE---------------------------------------------------------------
# # from CRAN
# install.packages("stats19")
# # you can install the latest development (discoraged) using:
# remotes::install_github("ITSLeeds/stats19")

## -----------------------------------------------------------------------------
library(stats19)

## ----dl2022-accidents---------------------------------------------------------
dl_stats19(year = 2022, type = "collision", ask = FALSE)

## ----dl2022-all, eval=FALSE---------------------------------------------------
# dl_stats19(year = 2022)

## ----dl2022-read--------------------------------------------------------------
crashes_2022_raw = get_stats19(year = 2022, type = "collision", format = FALSE)

## ----echo=FALSE---------------------------------------------------------------
# skip vignettes if resource unavailable
if(object.size(crashes_2022_raw) < 1000) {
  knitr::opts_chunk$set(eval = FALSE)
}

## ----crashes2022-class--------------------------------------------------------
class(crashes_2022_raw)
dim(crashes_2022_raw)

## ----read2022-raw-format------------------------------------------------------
crashes_2022_raw = read_collisions(year = 2022, format = FALSE)
crashes_2022 = format_collisions(crashes_2022_raw)
nrow(crashes_2022_raw)
ncol(crashes_2022_raw)
nrow(crashes_2022)
ncol(crashes_2022)

## ----crashes2022-columns------------------------------------------------------
names(crashes_2022_raw)
crashes_2022_raw[c(8, 18, 23, 25)]
crashes_2022[c(8, 18, 23, 25)]

## ----echo=FALSE, eval=FALSE---------------------------------------------------
# # commented out as confusing...
# key_patt = "severity|speed|light|human"
# key_vars = grep(key_patt, x = names(crashes_2022_raw), ignore.case = TRUE)
# random_n = sample(x = nrow(crashes_2022_raw), size = 3)
# crashes_2022_raw[random_n, key_vars]
# crashes_2022[random_n, key_vars]

## ----variables-and-schema-----------------------------------------------------
stats19_variables
stats19_schema

## ----format-col-names---------------------------------------------------------
format_column_names(stats19_variables$variable[1:3])

## ----format-main--------------------------------------------------------------
crashes_2022 = format_collisions(crashes_2022_raw)
# vehicle data for 2022
dl_stats19(year = 2022, type = "vehicle", ask = FALSE)
vehicles_2022_raw = read_vehicles(year = 2022, format = FALSE)
vehicles_2022 = format_vehicles(vehicles_2022_raw)

# casualties data for 2022
dl_stats19(year = 2022, type = "casualty", ask = FALSE)
casualties_2022 = read_casualties(year = 2022)

## ----summarise-stats19--------------------------------------------------------
summarise_stats19 = function(x) {
  data.frame(row.names = 1:length(x),
    name = substr(names(x), 1, 19),
    class = sapply(x, function(v) class(v)[1]),
    n_unique = sapply(x, function(v) length(unique(v))),
    first_label = sapply(x, function(v) substr(unique(v)[1], 1, 16)),
    most_common_value = sapply(x, function(v) 
      substr(names(sort(table(v), decreasing = TRUE)[1]), 1, 16)[1])
  )
}

## ----summarise-crashes--------------------------------------------------------
knitr::kable(summarise_stats19(crashes_2022), 
             caption = "Summary of formatted crash data.")

## ----summarise-vehicles-------------------------------------------------------
knitr::kable(summarise_stats19(vehicles_2022), 
             caption = "Summary of formatted vehicles data.")

## ----summarise-casualties-----------------------------------------------------
knitr::kable(summarise_stats19(casualties_2022), 
             caption = "Summary of formatted casualty data.")

## ----echo=FALSE, results='asis'-----------------------------------------------
key_patt = "severity|speed|light|human"
key_vars = grep(key_patt, x = names(stats19::accidents_sample_raw), ignore.case = TRUE)
knitr::kable(stats19::accidents_sample_raw[, key_vars])

## ----2022-cas-----------------------------------------------------------------
dl_stats19(year = 2022, type = "casualty", ask = FALSE)
casualties_2022 = read_casualties(year = 2022)
nrow(casualties_2022)
ncol(casualties_2022)

## ----2022-cas-columns---------------------------------------------------------
casualties_2022[c(4, 5, 6, 14)]

## ----2022-cas-columns-all-----------------------------------------------------
names(casualties_2022)

## ----dl2022-vehicles----------------------------------------------------------
dl_stats19(year = 2022, type = "vehicle", ask = FALSE)
vehicles_2022 = read_vehicles(year = 2022)
nrow(vehicles_2022)
ncol(vehicles_2022)

## ----2022-veh-columns---------------------------------------------------------
vehicles_2022[c(3, 14:16)]

## ----2022-veh-columns-all-----------------------------------------------------
names(vehicles_2022)

## ----eval=FALSE, echo=FALSE---------------------------------------------------
# # old code to be up-dated
# d14 = "Stats19_Data_2005-2014"
# crashes_2005_2014 = read_collisions(data_dir = d14)
# crashes_2005_2014_f = format_stats19_2005_2014_ac(crashes_2005_2014)
# d15 = "RoadSafetyData_2015"
# crashes_2015 = read_collisions(data_dir = d15, filename = "Accidents_2015.csv")
# crashes_2015_f = format_stats19_2015_ac(crashes_2015)
# d16 = "dftRoadSafety_Accidents_2016"
# crashes_2016 = read_collisions(data_dir = d16, filename = "dftRoadSafety_Accidents_2016.csv")
# crashes_2016_f = format_stats19_2016_ac(crashes_2016)
# all_crashes = rbind(crashes_2015_f, crashes_2016_f, crashes_2022_f)
# table(ac$collision_Severity)

## ----format-crashes-sf--------------------------------------------------------
crashes_sf = format_sf(crashes_2022)

## ----nfatalities--------------------------------------------------------------

library(sf)
library(dplyr)
# crashes_sf %>% 
#   filter(collision_severity == "Fatal") |> 
#   select(n_fatalities = collision_index) |> 
#   aggregate(by = stats19::police_boundaries, FUN = length)
#   plot()

## ----ukboundaries-------------------------------------------------------------
west_yorkshire =
  police_boundaries[police_boundaries$pfa16nm == "West Yorkshire", ]

## ----crashes-west_yorkshire---------------------------------------------------
crashes_wy = crashes_sf[west_yorkshire, ]
nrow(crashes_sf)
nrow(crashes_wy)

## ----table-join, message = FALSE----------------------------------------------
library(tidyr)
library(dplyr)
sel = casualties_2022$collision_index %in% crashes_wy$collision_index
casualties_wy = casualties_2022[sel, ]
table(casualties_wy$casualty_type)
cas_types = casualties_wy %>% 
  select(collision_index, casualty_type) %>% 
  group_by(collision_index) %>% 
  summarise(
    Total = n(),
    walking = sum(casualty_type == "Pedestrian"),
    cycling = sum(casualty_type == "Cyclist"),
    passenger = sum(casualty_type == "Car occupant")
    ) 
cj = left_join(crashes_wy, cas_types)
summary(cj)

## ----table-join-examples------------------------------------------------------
base::setdiff(names(cj), names(crashes_wy))

## ----out.width="90%", fig.show='hold'-----------------------------------------
plot(
  cj[cj$cycling > 0, "speed_limit", ],
  cex = cj$Total[cj$cycling > 0] / 3,
  main = "Speed limit (cycling)"
  )
plot(
  cj[cj$passenger > 0, "speed_limit", ],
  cex = cj$Total[cj$passenger > 0] / 3,
  main = "Speed limit (passenger)"
  )

## ----echo=FALSE---------------------------------------------------------------
knitr::include_graphics("wy-overview.jpg")

## ----sfplot, fig.show='hold', out.width="100%", fig.cap="Spatial distribution of serious and fatal crashes in West Yorkshire, for cycling, walking, being a car passenger and other modes of travel. Colour is related to the speed limit where the crash happened (red is faster) and size is proportional to the total number of people hurt in each crash (legend not shown).", fig.width=9, fig.height=7----
library(ggplot2)
crashes_types = cj %>% 
  filter(collision_severity != "Slight") %>% 
  mutate(type = case_when(
    walking > 0 ~ "Walking",
    cycling > 0 ~ "Cycling",
    passenger > 0 ~ "Passenger",
    TRUE ~ "Other"
  ))
crashes_types$speed_limit = as.integer(crashes_types$speed_limit)
table(crashes_types$speed_limit)
ggplot(crashes_types, aes(size = Total, colour = speed_limit)) +
  geom_sf(show.legend = "point", alpha = 0.3) +
  facet_grid(vars(type), vars(collision_severity)) +
  scale_size(
    breaks = c(1:3, 12),
    labels = c(1:2, "3+", 12)
    ) +
  scale_color_gradientn(colours = c("blue", "yellow", "red")) +
  theme(axis.text = element_blank(), axis.ticks = element_blank())

## ----ggplot-ped-severity, fig.height=5, fig.width=6---------------------------
table(cj$light_conditions)
cj$speed_limit = as.integer(cj$speed_limit)
cj %>% 
  filter(walking > 0) %>% 
  mutate(light = case_when(
    light_conditions == "Daylight" ~ "Daylight",
    light_conditions == "Darkness - lights lit" ~ "Lit",
    TRUE ~ "Other/Unlit"
  )) %>% 
  ggplot(aes(colour = speed_limit)) +
  geom_sf() +
  facet_grid(vars(light), vars(collision_severity)) +
  scale_color_continuous(low = "blue", high = "red") +
  theme(axis.text = element_blank(), axis.ticks = element_blank())

## ----crash-date-plot, fig.width=5, fig.height=5-------------------------------
crashes_dates = cj %>% 
  st_set_geometry(NULL) %>% 
  group_by(date) %>% 
  summarise(
    walking = sum(walking),
    cycling = sum(cycling),
    passenger = sum(passenger)
    ) %>% 
  gather(mode, casualties, -date)
ggplot(crashes_dates, aes(date, casualties)) +
  geom_smooth(aes(colour = mode), method = "loess") +
  ylab("Casualties per day")

## ----crash-time-plot, fig.width=5, fig.height=5-------------------------------
library(stringr)

crash_times = cj %>% 
  st_set_geometry(NULL) %>% 
  group_by(hour = as.numeric(str_sub(time, 1, 2))) %>% 
  summarise(
    walking = sum(walking),
    cycling = sum(cycling),
    passenger = sum(passenger)
    ) %>% 
  gather(mode, casualties, -hour)

ggplot(crash_times, aes(hour, casualties)) +
  geom_line(aes(colour = mode))

