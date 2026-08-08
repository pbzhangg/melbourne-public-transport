library(tidyverse)
library(sf)
library(readxl)
library(here)

train <- read.delim("data/gtfs/2/google_transit/stops.txt", sep = ",") %>%
  mutate(stop_id = as.character(stop_id),
         parent_station = as.character(parent_station))
tram <- read.delim("data/gtfs/3/google_transit/stops.txt", sep = ",") %>%
  mutate(stop_id = as.character(stop_id),
         parent_station = as.character(parent_station))
bus <- read.delim("data/gtfs/4/google_transit/stops.txt", sep = ",") %>%
  mutate(stop_id = as.character(stop_id),
         parent_station = as.character(parent_station))

stops <- bind_rows(train, tram, bus) %>%
  select(stop_id, stop_lat, stop_lon)

stop_times <- bind_rows(
  read.delim("data/gtfs/2/google_transit/stop_times.txt", sep = ","),
  read.delim("data/gtfs/3/google_transit/stop_times.txt", sep = ","),
  read.delim("data/gtfs/4/google_transit/stop_times.txt", sep = ",")
)

trips <- bind_rows(
  read.delim("data/gtfs/2/google_transit/trips.txt", sep = ","),
  read.delim("data/gtfs/3/google_transit/trips.txt", sep = ","),
  read.delim("data/gtfs/4/google_transit/trips.txt", sep = ",")
)

calendar <- bind_rows(
  read.delim("data/gtfs/2/google_transit/calendar.txt", sep = ","),
  read.delim("data/gtfs/3/google_transit/calendar.txt", sep = ","),
  read.delim("data/gtfs/4/google_transit/calendar.txt", sep = ",")
)

week_services <- calendar %>%
  filter(if_any(monday:friday, ~ .x == 1)) %>%
  select(service_id)

weekday_trips <- semi_join(trips, week_services, by = "service_id")

weekday_stop_times <- semi_join(stop_times, weekday_trips, by = "trip_id")

pop_est <- read_excel("data/est_pop_sa2.xlsx", sheet = "Table 1", skip = 6)

pop <- pop_est %>%
  rename(
    SA2_CODE_2021 = "SA2 code",
    pop_2001 = "no....11",
    pop_2024 = "no....34",
    SA2_NAME_2021 = "SA2 name"
  ) %>%
  filter(`GCCSA name` == "Greater Melbourne") %>%
  mutate(SA2_CODE_2021 = as.character(SA2_CODE_2021),
         absolute_change = pop_2024 - pop_2001,
         percent_change = ifelse(pop_2001 <= 500, NA,
                                 round((absolute_change / pop_2001) * 100, 3))) %>%
  select(SA2_CODE_2021, SA2_NAME_2021, pop_2001, pop_2024, absolute_change, percent_change)

st_layers("data/Geopackage_2021_G33_VIC_GDA2020/G33_VIC_GDA2020.gpkg")

metro_sa2 <- read_sf("data/Geopackage_2021_G33_VIC_GDA2020/G33_VIC_GDA2020.gpkg", 
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

top_growth <- sa2_map %>%
  filter(SA2_NAME_2021 %in% c("Mickleham - Yuroke", "Wollert", "Rockbank - Mount Cottrell",
                            "Tarneit - Central", "Beaconsfield - Officer", "Clyde North - South",
                            "Pakenham - South West", "Cranbourne East - North", "Melbourne CBD - North",
                            "Cranbourne West", "Taylors Hill", "Point Cook - East", "South Morang - North",
                            "Keysborough - South", "Epping (Vic.) - West", "Mernda - North", 
                            "Pakenham - North West", "Point Cook - North East", "Lynbrook - Lyndhurst", 
                            "Melbourne CBD - West", "Wyndham Vale - North")) %>%
  mutate(pop_int = service_intensity / absolute_change)

write_sf(sa2_map, "data/question_1.shp")
write_sf(top_growth, "data/top_sa2s.shp")
####################################################################################

full <- expand_grid(SA2_CODE_2021 = unique(metro_sa2$SA2_CODE_2021),
                    period = c("morning peak", "midday", "interpeak", "afternoon peak", "evening"),
                    day = c("weekday", "saturday", "sunday"))

full <- full %>%
  left_join(metro_sa2) %>%
  mutate(hours = case_when(
    period == "morning peak" ~ 4,
    period == "midday" ~ 4,
    period == "interpeak" ~ 2,
    period == "afternoon peak" ~ 2,
    period == "evening" ~ 4
  ))

period_hours <- tibble(
  period = c("morning peak", "midday", "interpeak", "afternoon peak", "evening"),
  hours = c(4, 4, 2, 2, 4)
)

weekday_service <- calendar %>%
  mutate(weekday_days = monday + tuesday + wednesday + thursday + friday) %>%
  filter(weekday_days > 0) %>%
  select(service_id, weekday_days)

weekday_trips <- trips %>%
  inner_join(weekday_service, by = "service_id")

weekday_data <- stop_times %>%
  inner_join(weekday_trips, by = "trip_id") %>%
  mutate(stop_id = as.character(stop_id)) %>%
  inner_join(stops_sa2 %>% st_drop_geometry(), by = "stop_id")

weekday_stop_times <- weekday_data %>%
  mutate(
    hour = as.numeric(substr(departure_time, 1, 2)),
    period = case_when(
      hour >= 6 & hour < 10 ~ "morning peak",
      hour >= 10 & hour < 14 ~ "midday",
      hour >= 14 & hour < 16 ~ "interpeak",
      hour >= 16 & hour < 18 ~ "afternoon peak",
      hour >= 18 & hour < 22 ~ "evening"
    )
  ) %>%
  filter(!is.na(period))

weekday_freq <- weekday_stop_times %>%
  group_by(SA2_CODE_2021, SA2_NAME_2021, period) %>%
  summarise(weighted_trips = sum(weekday_days), .groups = "drop") %>%
  left_join(period_hours, by = "period") %>%
  mutate(trips = weighted_trips / 5, 
         trips_per_hour = trips / hours,
         day = "weekday")

saturday_service <- calendar %>%
  filter(saturday == 1)

saturday_trips <- trips %>%
  filter(service_id %in% saturday_service$service_id)

saturday_data <- stop_times %>%
  inner_join(saturday_trips, by = "trip_id") %>%
  mutate(stop_id = as.character(stop_id)) %>%
  inner_join(stops_sa2 %>% st_drop_geometry(), by = "stop_id")

saturday_stop_times <- saturday_data %>%
  mutate(
    hour = as.numeric(substr(departure_time, 1, 2)),
    period = case_when(
      hour >= 6 & hour < 10 ~ "morning peak",
      hour >= 10 & hour < 14 ~ "midday",
      hour >= 14 & hour < 16 ~ "interpeak",
      hour >= 16 & hour < 18 ~ "afternoon peak",
      hour >= 18 & hour < 22 ~ "evening"
    )
  ) %>%
  filter(!is.na(period))

saturday_freq <- saturday_stop_times %>%
  group_by(SA2_NAME_2021, SA2_CODE_2021, period) %>%
  summarise(trips = n(), .groups = "drop") %>%
  left_join(period_hours, by = "period") %>%
  mutate(trips_per_hour = trips / hours,
         day = "saturday")


sunday_service <- calendar %>%
  filter(sunday == 1)

sunday_trips <- trips %>%
  filter(service_id %in% sunday_service$service_id)

sunday_data <- stop_times %>%
  inner_join(sunday_trips, by = "trip_id") %>%
  mutate(stop_id = as.character(stop_id)) %>%
  inner_join(stops_sa2 %>% st_drop_geometry(), by = "stop_id")

sunday_stop_times <- sunday_data %>%
  mutate(
    hour = as.numeric(substr(departure_time, 1, 2)),
    period = case_when(
      hour >= 6 & hour < 10 ~ "morning peak",
      hour >= 10 & hour < 14 ~ "midday",
      hour >= 14 & hour < 16 ~ "interpeak",
      hour >= 16 & hour < 18 ~ "afternoon peak",
      hour >= 18 & hour < 22 ~ "evening"
    )
  ) %>%
  filter(!is.na(period))

sunday_freq <- sunday_stop_times %>%
  group_by(SA2_NAME_2021, SA2_CODE_2021, period) %>%
  summarise(trips = n(), .groups = "drop") %>%
  left_join(period_hours, by = "period") %>%
  mutate(trips_per_hour = trips / hours,
         day = "sunday") 

all_freq <- bind_rows(weekday_freq, saturday_freq, sunday_freq)

freq <- full %>%
  left_join(all_freq, by = c("SA2_CODE_2021", "period", "day", "hours")) %>%
  select(-SA2_NAME_2021.y, -weighted_trips, -hours) %>%
  rename(SA2_NAME_2021 = "SA2_NAME_2021.x") %>%
  mutate(trips_per_hour = replace_na(trips_per_hour, 0),
         trips = replace_na(trips, 0),
         period = factor(period, levels = c("morning peak", "midday", "interpeak", "afternoon peak", "evening"),
                         labels = c("6AM - 10AM", "10AM - 2PM", "2PM - 4PM", "4PM - 6PM", "6PM - 10PM")))

calendar_long <- calendar %>%
  select(service_id, monday:friday) %>%
  pivot_longer(
    cols = monday:friday,
    names_to = "day",
    values_to = "active"
  ) %>%
  filter(active == 1)

weekday_stop_days <- stop_times %>%
  inner_join(trips, by = "trip_id") %>%
  inner_join(calendar_long, by = "service_id") %>%
  mutate(stop_id = as.character(stop_id)) %>%
  inner_join(stops_sa2, by = "stop_id")

weekday_stop_days <- weekday_stop_days %>%
  mutate(
    hour = as.numeric(substr(departure_time, 1, 2)),
    period = case_when(
      hour >= 6 & hour < 10 ~ "morning peak",
      hour >= 10 & hour < 14 ~ "midday",
      hour >= 14 & hour < 16 ~ "interpeak",
      hour >= 16 & hour < 18 ~ "afternoon peak",
      hour >= 18 & hour < 22 ~ "evening"
    )
  ) %>%
  filter(!is.na(period))

daily_coverage <- weekday_stop_days %>%
  distinct(SA2_CODE_2021, SA2_NAME_2021, day, stop_id, period) %>%
  count(SA2_CODE_2021, SA2_NAME_2021, day, period, name = "stops_active")

weekday_coverage <- daily_coverage %>%
  group_by(SA2_CODE_2021, SA2_NAME_2021, period) %>%
  summarise(active_stops = sum(stops_active) / 5, .groups = "drop") %>%
  mutate(day = "weekday")

saturday_coverage <- saturday_stop_times %>%
  group_by(SA2_CODE_2021, SA2_NAME_2021, period) %>%
  summarise(
    active_stops = n_distinct(stop_id),
    .groups = "drop"
  ) %>%
  mutate(day = "saturday")

sunday_coverage <- sunday_stop_times %>%
  group_by(SA2_CODE_2021, SA2_NAME_2021, period) %>%
  summarise(
    active_stops = n_distinct(stop_id),
    .groups = "drop"
  ) %>%
  mutate(day = "sunday")

all_coverage = bind_rows(weekday_coverage, saturday_coverage, sunday_coverage)

cov <- full %>%
  left_join(all_coverage, by = c("SA2_CODE_2021","SA2_NAME_2021", "period", "day")) %>%
  mutate(active_stops = replace_na(active_stops, 0),
         period = factor(period, levels = c("morning peak", "midday", "interpeak", "afternoon peak", "evening"),
                         labels = c("6AM - 10AM", "10AM - 2PM", "2PM - 4PM", "4PM - 6PM", "6PM - 10PM")),
         day = factor(day, levels = c("weekday", "saturday", "sunday"))) %>%
  select(-hours)

freq_cov <- freq %>%
  left_join(cov)

summary(aov(trips_per_hour ~ day, data = freq))
summary(aov(trips_per_hour ~ period, data = freq))
summary(aov(active_stops ~ day, data = cov))
summary(aov(active_stops ~ period, data = cov))

write_sf(freq_cov, "data/q2_freq_cov.shp")

################################# SEIFA

seifa_aus <- read_excel("data/Statistical Area Level 2, Indexes, SEIFA 2021.xlsx", sheet = "Table 1", skip = 5) %>%
  rename(SA2_CODE_2021 = `2021 Statistical Area Level 2  (SA2) 9-Digit Code`,
         irsd_score = "Score...3") %>%
  mutate(irsd_score = as.numeric(irsd_score),
         SA2_CODE_2021 = as.character(SA2_CODE_2021)) %>%
  select(SA2_CODE_2021, irsd_score)

seifa_vic <- metro_sa2 %>%
  left_join(seifa_aus, by = "SA2_CODE_2021")

pop_21 <- pop_est %>%
  rename(
    SA2_CODE_2021 = "SA2 code",
    pop_2021 = "no....31"
  ) %>%
  filter(`GCCSA name` == "Greater Melbourne") %>%
  mutate(SA2_CODE_2021 = as.character(SA2_CODE_2021)) %>%
  select(SA2_CODE_2021, pop_2021)

seifa_clean <- seifa_vic %>%
  left_join(pop_21, by = "SA2_CODE_2021") %>%
  filter(pop_2021 > 1000)

freq_sa2 <- freq %>%
  group_by(SA2_CODE_2021, SA2_NAME_2021) %>%
  summarise(
    avg_trips_per_hour = mean(trips_per_hour),
    .groups = "drop"
  ) %>%
  right_join(seifa_clean, by = "SA2_CODE_2021") %>%
  mutate(irsd_quartile = ntile(irsd_score, 4),
         irsd_quartile = factor(irsd_quartile,
                                labels = c("Q1 Most disadvantaged", "Q2", "Q3", "Q4 Least disadvantaged")))
freq_sa2 %>%
  ggplot(aes(x = factor(irsd_quartile), y = avg_trips_per_hour)) +
  geom_boxplot() +
  labs(title = "Public Transport Service Frequency by Socioeconomic Quartile",
       x = "IRSD Quartile", 
       y = "Average Trips per Hour") +
  theme_minimal()


cov_sa2 <- cov %>%
  group_by(SA2_CODE_2021, SA2_NAME_2021) %>%
  summarise(
    avg_stops = mean(active_stops),
    .groups = "drop"
  ) %>%
  right_join(seifa_clean, by = "SA2_CODE_2021") %>%
  mutate(irsd_quartile = ntile(irsd_score, 4),
         irsd_quartile = factor(irsd_quartile,
                                labels = c("Q1 Most disadvantaged", "Q2", 
                                           "Q3", "Q4 Least disadvantaged")))

cov_sa2 %>%
  ggplot(aes(x = factor(irsd_quartile), y = avg_stops)) +
  geom_boxplot() +
  labs(title = "Public Transport Service Coverage by Socioeconomic Quartile",
       x = "IRSD Quartile", 
       y = "Average Stops") +
  theme_minimal()

write_sf(seifa_vic, "data/q2_seifa_361.shp")
write_sf(seifa_clean, "data/q2_seifa_354.shp")

summary(aov(avg_stops ~ irsd_quartile, data = cov_sa2))
summary(aov(avg_trips_per_hour ~ irsd_quartile, data = freq_sa2))
