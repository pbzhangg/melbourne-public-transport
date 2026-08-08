library(tidyverse)
library(sf)
library(readxl)
library(here)

train <- read.delim("raw_data/gtfs/2/google_transit/stops.txt", sep = ",") %>%
  mutate(stop_id = as.character(stop_id),
         parent_station = as.character(parent_station))
tram <- read.delim("raw_data/gtfs/3/google_transit/stops.txt", sep = ",") %>%
  mutate(stop_id = as.character(stop_id),
         parent_station = as.character(parent_station))
bus <- read.delim("raw_data/gtfs/4/google_transit/stops.txt", sep = ",") %>%
  mutate(stop_id = as.character(stop_id),
         parent_station = as.character(parent_station))

stops <- bind_rows(train, tram, bus) %>%
  select(stop_id, stop_lat, stop_lon)

stop_times <- bind_rows(
  read.delim("raw_data/gtfs/2/google_transit/stop_times.txt", sep = ","),
  read.delim("raw_data/gtfs/3/google_transit/stop_times.txt", sep = ","),
  read.delim("raw_data/gtfs/4/google_transit/stop_times.txt", sep = ",")
)

trips <- bind_rows(
  read.delim("raw_data/gtfs/2/google_transit/trips.txt", sep = ","),
  read.delim("raw_data/gtfs/3/google_transit/trips.txt", sep = ","),
  read.delim("raw_data/gtfs/4/google_transit/trips.txt", sep = ",")
)

calendar <- bind_rows(
  read.delim("raw_data/gtfs/2/google_transit/calendar.txt", sep = ","),
  read.delim("raw_data/gtfs/3/google_transit/calendar.txt", sep = ","),
  read.delim("raw_data/gtfs/4/google_transit/calendar.txt", sep = ",")
)

week_services <- calendar %>%
  filter(if_any(monday:friday, ~ .x == 1)) %>%
  select(service_id)

weekday_trips <- semi_join(trips, week_services, by = "service_id")

weekday_stop_times <- semi_join(stop_times, weekday_trips, by = "trip_id")

pop_est <- read_excel("raw_data/est_pop_sa2.xlsx", sheet = "Table 1", skip = 6)

pop <- pop_est %>%
  rename(
    SA2_CODE_2021 = "SA2 code",
    pop_2001 = "no....11",
    pop_2002 = "no....12",
    pop_2003 = "no....13",
    pop_2004 = "no....14",
    pop_2005 = "no....15",
    pop_2006 = "no....16",
    pop_2007 = "no....17",
    pop_2008 = "no....18",
    pop_2009 = "no....19",
    pop_2010 = "no....20",
    pop_2011 = "no....21",
    pop_2012 = "no....22",
    pop_2013 = "no....23",
    pop_2014 = "no....24",
    pop_2015 = "no....25",
    pop_2016 = "no....26",
    pop_2017 = "no....27",
    pop_2018 = "no....28",
    pop_2019 = "no....29",
    pop_2020 = "no....30",
    pop_2021 = "no....31",
    pop_2022 = "no....32",
    pop_2023 = "no....33",
    pop_2024 = "no....34",
    SA2_NAME_2021 = "SA2 name"
  ) %>%
  filter(`GCCSA name` == "Greater Melbourne") %>%
  mutate(SA2_CODE_2021 = as.character(SA2_CODE_2021),
         absolute_change = pop_2024 - pop_2001,
         percent_change = ifelse(pop_2001 <= 500, NA,
                                 round((absolute_change / pop_2001) * 100, 3))) %>%
  select(-`S/T code`, -`S/T name`, -`GCCSA name`, -`GCCSA code`, -`SA3 code`, -`SA3 name`, -`SA4 code`)

st_layers("raw_data/Geopackage_2021_G33_VIC_GDA2020/G33_VIC_GDA2020.gpkg")

metro_sa2 <- read_sf("raw_data/Geopackage_2021_G33_VIC_GDA2020/G33_VIC_GDA2020.gpkg", 
                     layer = "G33_SA2_2021_VIC") %>%
  semi_join(pop, by = "SA2_CODE_2021") %>%
  select(SA2_CODE_2021, SA2_NAME_2021, geom)

stops_sf <- st_as_sf(stops, coords = c("stop_lon", "stop_lat"), crs = 7844)

stops_sa2 <- st_join(stops_sf, metro_sa2, left = FALSE) %>%
  st_drop_geometry() %>%
  distinct(stop_id, SA2_CODE_2021, SA2_NAME_2021)

sa2_service <- weekday_stop_times %>%
  mutate(stop_id = as.character(stop_id)) %>%
  inner_join(stops_sa2, by = "stop_id") %>%
  count(SA2_CODE_2021, name = "service_intensity")

sa2_map <- metro_sa2 %>%
  left_join(sa2_service, by = "SA2_CODE_2021") %>%
  left_join(pop, by = "SA2_CODE_2021") %>%
  mutate(
    service_intensity = replace_na(service_intensity, 0),
    service_per_capita = ifelse(
      pop_2024 == 0, 
      0,
      service_intensity / pop_2024)
  ) %>%
  select(-SA2_NAME_2021.y) %>%
  rename(SA2_NAME_2021 = "SA2_NAME_2021.x")

choropleth_map_data <- sa2_map %>%
  pivot_longer(
    cols = starts_with("pop_"),
    names_to = "year",
    values_to = "population"
  ) %>%
  mutate(year = as.numeric(gsub("pop_", "", year))) %>%
  arrange(SA2_CODE_2021, year) %>%
  group_by(SA2_CODE_2021) %>%
  mutate(annual_change = population - lag(population)) %>%
  ungroup() %>%
  select(SA2_CODE_2021, SA2_NAME_2021, geom, 
         `SA4 name`, year, annual_change)

time_series_data <- sa2_map %>%
  st_drop_geometry() %>%
  pivot_longer(
    cols = starts_with("pop_"),
    names_to = "year",
    values_to = "population"
  ) %>%
  mutate(year = as.numeric(gsub("pop_", "", year))) %>%
  select(-absolute_change, -percent_change, -service_per_capita, -service_intensity)

rank_data <- sa2_map %>%
  st_drop_geometry() %>%
  arrange(desc(service_intensity)) %>%
  mutate(rank = row_number()) %>%
  select(SA2_CODE_2021, SA2_NAME_2021, service_intensity, service_per_capita, rank, `SA4 name`)

sa2_map <- st_transform(sa2_map, 4326)
choropleth_map_data <- st_transform(choropleth_map_data, 4326)

time_series_data$SA2_CODE_2021 <- as.character(time_series_data$SA2_CODE_2021)
rank_data$SA2_CODE_2021 <- as.character(rank_data$SA2_CODE_2021)
choropleth_map_data$SA2_CODE_2021 <- as.character(choropleth_map_data$SA2_CODE_2021)

write_rds(sa2_map, "clean_data/sa2_data.rds")
write_rds(choropleth_map_data, "clean_data/map_data.rds")
write.csv(time_series_data, "clean_data/time_data.csv")
write.csv(rank_data, "clean_data/rank_data.csv")
