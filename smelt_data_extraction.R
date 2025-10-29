# smelt_data_extraction.R
# Script for extracting data for smelt assessments- e.g. from files, SacPAS, online sources
# Last updated 10/15/2025

library(here)
library(readr)
library(dplyr)
library(lubridate)
library(tidyr)
library(pdftools)
library(tidyverse)
library(stringr)
library(rvest)
library(janitor)
library(deltamapr)
library(ggspatial)
library(sf)
library(jsonlite) #weather
library(glue) #weather
library(purrr) #weather
source("smelt_functions.R")

# Tables from SacPAS ---------------------------------

# Reading from SacPAS
url <- "https://www.cbr.washington.edu/sacramento/workgroups/delta_smelt.html"
page <- read_html(url)
tables <- html_table(page, fill = TRUE)

## Environmental Table ----------------------------
env_table_raw <- tables[[2]][-1,]
env_table <- env_table_raw %>% clean_names()

# last date with data
# Note this does not necessarily line up with the last date each column had data
# Could extract this in a different way if we want. 
env_last_date <- ymd(tail(env_table$date, 1))

# First Flush
# Get the most recent value in the column (already 3-day averaged)
FPT_flow_avg <- as.numeric(tail(env_table$river_discharge_flow_3_day_freeport_cfs_cdec_fpt,1))
FPT_turb_avg <- as.numeric(tail(env_table$turbidity_3_day_freeport_fnu_cdec_fpt, 1))

# Adult Entrainment (Turbidity Bridge)
# Get the most recent three values in the column
OBI_turb <- as.numeric(tail(env_table$water_turbidity_1_day_old_river_at_bacon_island_usgs_fnu_cdec_obi,3))
OSJ_turb <- as.numeric(tail(env_table$water_turbidity_1_day_old_river_at_franks_tract_fnu_cdec_osj, 3))
HOL_turb <- as.numeric(tail(env_table$water_turbidity_1_day_sjr_holland_cut_fnu_cdec_hol, 3))

# Start of Larval Entrainment
# Get the most recent value in the column
# Also add SJJ when available
RVB_temp <- as.numeric(tail(env_table$water_temperature_3_day_sr_at_rio_vista_br_c_cdec_rvb,1))

# End of OMR Season
# Get the most recent three values in the column
CLC_temp <- as.numeric(tail(env_table$water_temperature_clifton_court_c_cdec_clc,3))

## Hydro Table ------------------------------
hydro_table_raw <- tables[[1]][-1,]
hydro_table <- hydro_table_raw %>% clean_names()

# last date with data
hydro_last_date <- ymd(tail(hydro_table$date, 1))

# QWEST
qwest <- as.numeric(tail(hydro_table$qwest_cfs_dwr,3))
qwest7 <- as.numeric(tail(hydro_table$qwest_7_day_cfs_dwr,1))
x2 <- as.numeric(tail(hydro_table$x2_position_km_dwr ,3))

## Weather (Lilly) --------------------------------
# include temp, precipitation, wind and summarize
make_location_summary <- function(lat, lon, name = NULL) {
  url <- glue("https://forecast.weather.gov/MapClick.php?lat={lat}&lon={lon}&FcstType=json")
  dat <- fromJSON(url)
  
  temp_label <- if ("tempLabel" %in% names(dat$data)) dat$data$tempLabel else rep(NA, length(dat$data$temperature))
  
  forecast_df <- tibble(
    period = dat$time$startPeriodName,
    temp_label = temp_label,
    temperature = as.numeric(dat$data$temperature),
    weather = dat$data$weather,
    text = dat$data$text
  )
  
  precip <- forecast_df %>%
    filter(str_detect(str_to_lower(weather), "rain|snow|shower|precip")) %>%
    mutate(summary = glue("🌧 {period}: {text}")) %>%
    pull(summary)
  
  wind <- forecast_df %>%
    mutate(wind_speed = as.numeric(str_extract(text, "(?<=\\bwind\\s)(\\d{1,2})"))) %>%
    filter(!is.na(wind_speed) & wind_speed > 15) %>%
    mutate(summary = glue("💨 {period}: {text}")) %>%
    pull(summary)
  
  extreme <- forecast_df %>%
    filter((temp_label == "High" & temperature >= 90) |
             (temp_label == "Low" & temperature <= 32)) %>%
    mutate(summary = glue("🌡 {period}: {text}")) %>%
    pull(summary)
  
  bullets <- c()
  if (length(precip) > 0) bullets <- c(bullets, precip[1:min(2, length(precip))])
  if (length(wind) > 0) bullets <- c(bullets, wind[1])
  if (length(extreme) > 0) bullets <- c(bullets, extreme[1])
  
  
  # Add weekly temperature range if quiet forecast
  if (length(bullets) == 0) {
    temp_min <- min(forecast_df$temperature, na.rm = TRUE)
    temp_max <- max(forecast_df$temperature, na.rm = TRUE)
    bullets <- glue("No precipitation, high winds, or extreme temps expected. Temperature range is {temp_min}–{temp_max}°F.")
  }
  
  header <- if (!is.null(name)) glue("**{name}**:") else ""
  bullet_lines <- paste("  -", bullets, collapse = "\n") #"-", 
  paste(header, "\n", bullet_lines)
}


# Summaries for Stockton and Antioch (can add additional locations as well)
weather_stockton <- make_location_summary(37.9537, -121.2905, "Stockton, CA")
weather_antioch <- make_location_summary(38.0169, -121.8138, "Antioch, CA")


## Smelt catch -------------------------
# define where data are stored
data_raw <- here("data_raw/smelt")

# station crosswalk 
# CDFW stations with lat/lon and region
station_region <- read_csv(here("data_clean/station_region_crosswalk.csv")) %>%
  select(Station, Latitude, Longitude, Region) %>%
  mutate(Station = as.character(Station)) %>%
  clean_names()

# Salvage lat/lon
sta_salvage <- data.frame(station = c("CVP", "SWP"),
                          latitude = c(37.815176,37.82524),
                          longitude = c(-121.560709, -121.59523),
                          region = "South")

# EDSM data - directly add in file; remove old file
edsm_data_raw <- read_excel_by_pattern("EDSM", data_raw, FALSE)
edsm_data <- edsm_data_raw %>%
  clean_names() %>%
  mutate(source = "edsm") %>%
  filter(organism_code == "DSM") %>%
  select(source, date = sample_date, region = region_code, stratum, 
         latitude = latitude_start, longitude = longitude_start, mark_code,
         fork_length, catch=sum_of_catch_count) 

# Salvage data - reading from SacPAS which is connected to the Salvage database
# We don't currently have mark_code for these - comes in the pdf - does have an adipose clip col?
salvage_data_raw <- read_csv("https://www.cbr.washington.edu/sacramento/data/php/rpt/juv_loss_detail.php?sc=1&outputFormat=csv&year=2025&species=26%3Aall&dnaOnly=no&age=no")  %>%
  filter(!is.na(Species)) %>%
  clean_names()
salvage_data <- salvage_data_raw %>% 
  mutate(sample_time = ymd_hms(sample_time),
         date = date(sample_time),
         length = as.numeric(length)) %>%
  mutate(source = "salvage") %>%
  mutate(salvage = if_else(!is.na(sample_fraction), nfish/sample_fraction, nfish)) %>%
  select(source, station = facility, date, study_type, catch = nfish, salvage, 
         fork_length = length, 
         omri = x14_day_omri) %>%
  left_join(sta_salvage)

# manually update for other DS data 
other_ds_data <- read_csv(here("data_raw/smelt/smelt_catch_test.csv")) %>%
  mutate(date = mdy(date))

# Bay Study: directly add new files in. Will combine files together.
sfbs_data_raw <- read_sfbs_files(data_raw)
sfbs_data <- sfbs_data_raw %>%
  clean_names() %>%
  mutate(station = as.character(station)) %>%
  left_join(station_region, by = "station")%>%
  mutate(source = "baystudy", 
         catch = 1) %>%
  select(source, station, date, catch, fork_length = length, latitude, longitude, region)

# SLS and 20mm issues
# - not individual fish, but instead grouped with mean, min, max
# - stars and symbols next to station numbers
# - not standardized on column naming
# - header and metadata at top, sides

# SLS: directly add file in - will read most recently modified file
sls_data_raw <- read_excel_by_pattern("SLS", data_raw, TRUE)
colnames(sls_data_raw) <- as.character(sls_data_raw[1, ])  # Set first row as column names
sls_data <- sls_data_raw[-1, ]  # Remove the first row
sls_data$Date <- as.Date(as.numeric(sls_data$Date), origin = "1899-12-30")
sls_data <- sls_data[, !is.na(names(sls_data)) & names(sls_data) != ""]
  
# right now sls data include min, mean, max length... not sure if we want to display all of them
sls_data <- sls_data %>% filter(!is.na(Date)) %>%
  clean_names() %>%
  rename(station = sls_station) %>%
  left_join(station_region, by = "station")%>%
  mutate(source = "sls",
         smelt_catch = as.numeric(smelt_catch),
         mean_length = as.numeric(mean_length))%>%
  select(source, station, date, catch = smelt_catch, species, fork_length = mean_length, latitude, longitude, region)

sls_ds <- sls_data %>% filter(species == "Delta Smelt")
sls_lfs <- sls_data %>% filter(species == "Longfin Smelt")

# 20mm: directly add file in - will read most recently modified file
twmm_data_raw <- read_excel_by_pattern("20-mm", data_raw, TRUE)
colnames(twmm_data_raw) <- as.character(twmm_data_raw[1, ])  # Set first row as column names
twmm_data <- twmm_data_raw[-1, ] # remove first row
twmm_data$Date <- as.Date(as.numeric(twmm_data$Date), origin = "1899-12-30")
twmm_data <- twmm_data[, !is.na(names(twmm_data)) & names(twmm_data) != ""]
twmm_data <- twmm_data %>% 
  filter(!is.na(Date),
         !is.na(Survey))%>%
  clean_names() %>%
  left_join(station_region, by = "station") %>%
  mutate(source = "twenty_mm",
         total_catch = as.numeric(total_catch),
         avg_length = as.numeric(avg_length)) %>%
  select(source, station, survey, date, catch = total_catch, species, fork_length = avg_length, latitude, longitude, region)

twmm_ds <- twmm_data %>% filter(species == "Delta Smelt") 
twmm_lfs <- twmm_data %>% filter(species == "Longfin Smelt")

# abundance estimates (manually update spreadsheet with each week's table for sheet 1)
abun <- read_excel(here("data_raw/smelt/abundance_estimates.xlsx"), sheet = 1)
abun_date <- read_excel(here("data_raw/smelt/abundance_estimates.xlsx"), sheet = 2)
abundance <- left_join(abun, abun_date) %>% 
  clean_names()%>%
  mutate(abundance_index = if_else(abundance_index == "0*", "0", abundance_index) ) %>%
  mutate(across(c(abundance_index, lower_bound, upper_bound), as.numeric))

## Make datasets ---------------------------------

# this one has lat/lon (for map)
# could filter by date for life stage here
ds_latlon <- bind_rows(
  edsm_data %>% select(source, date, catch, latitude, longitude, region),
  twmm_ds %>% select(source, date, catch, latitude, longitude, region),
  salvage_data %>% select(source, date, catch, latitude, longitude, region)) %>%
  filter(!is.na(catch),
         !is.na(latitude)) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326, remove = FALSE) %>%
  st_transform(crs = st_crs(WW_Delta)) %>%
  group_by(source, date, latitude, longitude, region) %>%
  summarize(total_catch = sum(catch)) %>%
  ungroup()

# this one has forklength/lifestage (for summary across wy by lifestage)
ds_detail <- bind_rows(
  edsm_data %>% select(source, date, catch, mark_code, fork_length, latitude, longitude, region),
  twmm_ds %>% select(source, date, catch, fork_length, latitude, longitude, region),
  salvage_data %>% select(source, date, catch, fork_length, latitude, longitude, region)) %>%
  filter(!is.na(catch),
         !is.na(latitude)) %>%
  mutate(life_stage = ifelse(fork_length>64, "Adult", ifelse(fork_length>25, "Juvenile", "Larva"))) %>%
  arrange(date)

