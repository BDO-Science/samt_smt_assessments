# smelt_data_extraction.R
# Script for extracting data for smelt assessments- e.g. from files, SacPAS, online sources
# Last updated 12/14/2025- lm adding A5 data

# Libraries 
library(here)
library(readr)
library(dplyr)
library(lubridate)
library(tidyr)
library(tidyverse)
library(stringr)
library(rvest)
library(janitor)
library(deltamapr)
library(ggspatial)
library(readxl)
library(sf)
library(jsonlite) #weather
library(glue) #weather
library(purrr) #weather
library(pdftools) # JPF
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

# End of turbidity bridge
# Get the most recent value in the column
# Also add SJJ when available
RVB_temp <- as.numeric(tail(env_table$water_temperature_3_day_sr_at_rio_vista_br_c_cdec_rvb,1))

# End of OMR Season
# Get the most recent three values in the column
CLC_temp <- as.numeric(tail(env_table$water_temperature_clifton_court_c_cdec_clc,3))

## Hydro Table ------------------------------
### Add JPF later on when added to SacPAS, for now pulling from DWR hydro rpt

hydro_table_raw <- tables[[1]][-1,]
hydro_table <- hydro_table_raw %>% clean_names()

# last date with data
hydro_last_date <- ymd(tail(hydro_table$date, 1))

# QWEST
qwest <- as.numeric(tail(hydro_table$qwest_cfs_dwr,3))
qwest7 <- as.numeric(tail(hydro_table$qwest_7_day_cfs_dwr,1))
x2 <- as.numeric(tail(hydro_table$x2_position_km_dwr ,3))

# Plots from SacPAS ---------------------
### Consider generating these on our own for more flexibility. For now, pulling from SacPAS.

### First Flush
download.file(
  "https://www.cbr.washington.edu/sacramento/workgroups/include_gen/deltasmelt_fpt.png",
  "smelt_figures/freeport_flow.png",
  mode = "wb"
)

### Turbidity
download.file(
  "https://www.cbr.washington.edu/sacramento/workgroups/include_gen/deltasmelt_turb1.png",
  "smelt_figures/freeport_turbidity.png",
  mode = "wb"
)

### Clifton Court
download.file(
  "https://www.cbr.washington.edu/sacramento/workgroups/include_gen/deltasmelt_clc.png",
  "smelt_figures/clifton_court_temp.png",
  mode = "wb"
)

# Weather (Lilly) --------------------------------
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


# Smelt catch -------------------------
# define where data are stored
data_raw <- here("data_raw/smelt")

## Coordinates, station crosswalk ------------------------
# CDFW stations with lat/lon and region, added stratum
station_region <- read_csv(here("data_clean/station_region_crosswalk.csv")) %>%
  select(Station, Latitude, Longitude, Region) %>%
  mutate(Station = as.character(Station)) %>%
  clean_names()

station_stratum <- read_csv(here("data_clean/station_stratum_crosswalk.csv")) %>%
  select(Station, Stratum) %>%
  mutate(Station = as.character(Station)) %>%
  clean_names()

# Salvage lat/lon
sta_salvage <- data.frame(station = c("CVP", "SWP"),
                          latitude = c(37.815176,37.82524),
                          longitude = c(-121.560709, -121.59523),
                          region = "South")

## EDSM data -----------------
# directly add in file; remove old file 
# edsm_data_raw <- read_excel_by_pattern("EDSM", data_raw, FALSE)
# edsm_data <- edsm_data_raw %>%
#   clean_names() %>%
#   mutate(source = "edsm") %>%
#   select(source, date = sample_date, region = region_code, stratum, 
#          latitude = latitude_start, longitude = longitude_start, mark_code,
#          fork_length, catch=sum_of_catch_count, organism_code)
# edsm_ds <- edsm_data %>% filter(organism_code == "DSM") %>% select(-organism_code)
# edsm_lfs <- edsm_data %>% filter(organism_code == "LFS")%>% select(-organism_code)

#test code edsm
# read in full csv from SacPAS
edsm_data_raw <- read_csv("https://www.cbr.washington.edu/sacramento/data/generated/WY2026_smeltcatch_edsm.csv")
edsm_data <- edsm_data_raw %>%
  clean_names() %>%
  select(-source) %>% 
  select(source= program, date, region, stratum, latitude=latitude_start, longitude=longitude_start, life_stage, 
          mark_code, fork_length, catch= nfish, species)
edsm_ds <- edsm_data %>% filter(species == "Delta Smelt") %>% select(-species)
edsm_lfs <- edsm_data %>% filter(species == "Longfin Smelt") %>% select(-species)

#test code for chipps island trawl catch
chipps_data_raw <- read_csv("https://www.cbr.washington.edu/sacramento/data/generated/WY2026_smeltcatch_chipps.csv")
chipps_data <- chipps_data_raw %>%
  clean_names() %>%
  select(-source) %>% 
  mutate(region = "N/A") %>%
  mutate(stratum = "Chipps Island") %>%
  select(source=program, date, region, stratum, latitude, longitude, life_stage, mark_code, fork_length,
         catch= nfish, species)
chipps_ds <- chipps_data %>% filter(species == "Delta Smelt") %>% select(-species)
chipps_lfs <- chipps_data %>% filter(species == "Longfin Smelt") %>% select(-species)

## Salvage data --------------------
# reading from SacPAS which is connected to the Salvage database 
# Will need to change the read to 2026 once salvage starts. 
# Get the new link here (probably just change 2025 to 2026 below): https://www.cbr.washington.edu/sacramento/data/query_loss_detail.html
salvage_ds_data_raw <- read_csv("https://www.cbr.washington.edu/sacramento/data/php/rpt/juv_loss_detail.php?sc=1&outputFormat=csv&year=2025&species=26%3Aall&dnaOnly=no&age=no")  %>%
  filter(!is.na(Species)) %>%
  clean_names()
salvage_ds_data <- salvage_ds_data_raw %>%
  mutate(sample_time = ymd_hms(sample_time),
         date = date(sample_time),
         length = as.numeric(length)) %>%
  mutate(source = "salvage") %>%
  mutate(salvage = if_else(!is.na(sample_fraction), nfish/sample_fraction, nfish)) %>%
  select(source, station = facility, date, study_type, catch = nfish, salvage,
         fork_length = length,
         omri = x14_day_omri) %>%
  left_join(sta_salvage)

salvage_lfs_data_raw <- read_csv("https://www.cbr.washington.edu/sacramento/data/php/rpt/juv_loss_detail.php?sc=1&outputFormat=csv&year=2025&species=25%3Aall&dnaOnly=no&age=no")  %>%
  filter(!is.na(Species)) %>%
  clean_names()
salvage_lfs_data <- salvage_lfs_data_raw %>%
  mutate(sample_time = ymd_hms(sample_time),
         date = date(sample_time),
         length = as.numeric(length)) %>%
  mutate(source = "salvage") %>%
  mutate(salvage = if_else(!is.na(sample_fraction), nfish/sample_fraction, nfish)) %>%
  select(source, station = facility, date, study_type, catch = nfish, salvage,
         fork_length = length,
         omri = x14_day_omri) %>%
  left_join(sta_salvage)

## Other data ----------------------
# manually update for DJFMP beach seines
beachsn <- read_csv(here("data_raw/smelt/Beach_seines_2026.csv")) %>%
  clean_names() %>% 
  mutate(date = mdy(date),
         date = date(ymd(date))) %>% 
  #filter(species == "DSM") %>% 
  mutate(source = "DJFMP") %>% 
  #mutate(region= "North") %>%  #fix this, obviously not true for all stations
  select(region, station, source, date, latitude, longitude, species, mark, catch,
         fork_length= fl, life_stage= stage, stratum)

beachsn_ds <- beachsn %>% filter(species == "DSM") %>% mutate(species= "Delta Smelt") %>% select(-species)
beachsn_lfs <- beachsn %>% filter(species == "LFS") %>% mutate(species= "Longfin Smelt")%>% select(-species)

# manually update for other DS data (random Broodstock, FRP)
other_ds_data <- read_csv(here("data_raw/smelt/smelt_catch_test.csv")) %>%
  mutate(date = mdy(date))

## Bay Study -----------------
# directly add new files in. Code will combine files together.
# Longfin Smelt
sfbs_data_raw <- read_sfbs_files(data_raw)
sfbs_data <- sfbs_data_raw %>%
  clean_names() %>%
  mutate(station = as.character(station)) %>%
  mutate(date = ymd_hms(date),
         date = date(date)) %>%
  #mutate(catch = coalesce(frequency, plus_count)) %>% 
  # mutate(frequency = frequency %>% # some catch # are in plus count. Use frequency unless NA, then use plus count
  #     str_remove_all("[^0-9.-]") %>%
  #     as.numeric(),
  #   plus_count = plus_count %>%
  #     str_remove_all("[^0-9.-]") %>%
  #     as.numeric(),
  #   catch = coalesce(frequency, plus_count)) %>% 
  mutate(frequency = frequency %>%
           str_remove_all("[^0-9.-]") %>%
           as.numeric()) %>% 
  filter(!is.na(frequency)) %>%
  mutate(length = length %>%
           str_remove_all("[^0-9.-]") %>%
           as.numeric()) %>% 
  filter(!is.na(length)) %>%
  left_join(station_region, by = "station")%>%
  left_join(station_stratum, by = "station")%>%
  mutate(source = "baystudy") %>%
  select(source, station, date, catch=frequency, fork_length = length, latitude, longitude, region, stratum)

# SLS and 20mm notes
# - not individual fish, but instead grouped with mean, min, max
# - stars and symbols next to station numbers
# - not standardized on column naming
# - header and metadata at top, sides

## SLS ---------------------------------
# directly add file in - will read most recently modified file
# BEFORE reading in, make sure you remove any symbols in the station col (added to read_excel_by_pattern fcn)
#updated code for this:
sls_data_raw <- read_excel_by_pattern("SLS", data_raw, TRUE) %>% 
  mutate(`SLS Station` = readr::parse_number(`SLS Station`))
# sls_data_raw <- read_excel_by_pattern("SLS", data_raw, TRUE) %>% 
#   mutate(`SLS Station` = parse_number(`SLS Station`))
#colnames(sls_data_raw) <- as.character(sls_data_raw[1, ])  # Set first row as column names
#sls_data <- sls_data_raw[-1, ]  # Remove the first row
sls_data <- sls_data_raw %>% 
  filter(!is.na(Species)) # removes lines that are notes (from CDFW)
#sls_data <- sls_data_raw[-nrow(sls_data_raw), ]   # Remove last row (notes from CDFW)
#sls_data$Date <- as.Date(as.numeric(sls_data$Date), origin = "1899-12-30") #didn't work
sls_data$Date <- as.Date(sls_data$Date) #change to date format
sls_data <- sls_data[, !is.na(names(sls_data)) & names(sls_data) != ""] #not sure what this does, but Cat had it here
  
# right now sls data include min, mean, max length... not sure if we want to display all of them
sls_data <- sls_data %>% filter(!is.na(Date)) %>%
  clean_names() %>%
  rename(station = sls_station) %>% 
  mutate(station = as.character(station)) %>%
  left_join(station_region, by = "station")%>%
  left_join(station_stratum, by = "station")%>%
  mutate(source = "sls",
         life_stage = "Larva",
         smelt_catch = as.numeric(smelt_catch),
         mean_length = as.numeric(mean_length))%>%
  select(source, station, date, catch = smelt_catch, species, fork_length = mean_length, life_stage, latitude, longitude, region, stratum)

sls_ds <- sls_data %>% filter(species == "Delta Smelt")
sls_lfs <- sls_data %>% filter(species == "Longfin Smelt")

## 20mm ---------
# directly add file in - will read most recently modified file
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

## EDSM abundance estimates --------------------------
# (manually update spreadsheet with each week's table for sheet 1)
abun <- read_excel(here("data_raw/smelt/abundance_estimates.xlsx"), sheet = 1)
abun_date <- read_excel(here("data_raw/smelt/abundance_estimates.xlsx"), sheet = 2)
abundance <- left_join(abun, abun_date) %>% 
  clean_names()%>%
  mutate(abundance_index = if_else(abundance_index == "0*", "0", abundance_index) ) %>%
  mutate(across(c(abundance_index, lower_bound, upper_bound), as.numeric))

## Smelt release table -----------------------------
smelt_release_table <- tables[[3]] %>% clean_names()

## Combine datasets ---------------------------------

### DS ------------------------
# this one has lat/lon (for map)
# could filter by date for life stage here
ds_latlon <- bind_rows(
  edsm_ds %>% select(source, date, catch, latitude, longitude, region, life_stage),
  beachsn_ds %>% select(source, date, catch, latitude, longitude, region, life_stage),
  sls_ds %>% select(source, date, catch, latitude, longitude, region, life_stage)) %>% 
  #twmm_ds %>% select(source, date, catch, latitude, longitude, region, life_state), 
  #salvage_ds_data %>% select(source, date, catch, latitude, longitude, region)) %>%
  filter(!is.na(catch),
         !is.na(latitude)) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326, remove = FALSE) %>%
  st_transform(crs = st_crs(WW_Delta)) %>%
  group_by(source, date, latitude, longitude, region) %>%
  summarize(total_catch = sum(catch)) %>%
  ungroup()

# this one has forklength/lifestage (for summary across wy by lifestage)
# uncomment salvage once salvage is updated
ds_detail <- bind_rows(
  edsm_ds %>% select(source, date, catch, mark_code, fork_length, latitude, longitude, region, stratum),
  beachsn_ds %>% select(source, date, catch, fork_length, latitude, longitude, region, stratum),
  twmm_ds %>% select(source, date, catch, fork_length, latitude, longitude, region),
  sls_ds %>% select(source, date, catch, fork_length, latitude, longitude, region, stratum),
  chipps_ds %>% select(source, date, catch, fork_length, latitude, longitude, region),
  salvage_ds_data %>% select(source, date, catch, fork_length, latitude, longitude, region)
) %>%
  filter(!is.na(catch), !is.na(latitude)) %>%
  mutate(life_stage = ifelse(fork_length>58,"Adult",ifelse(fork_length>=20,"Juvenile","Larva"))) %>%
  arrange(date)



### LFS ------------------
# this one has lat/lon (for map)
# could filter by date for life stage here
lfs_latlon <- bind_rows(
  edsm_lfs %>% select(source, date, catch, latitude, longitude, region),
  twmm_lfs %>% select(source, date, catch, latitude, longitude, region),
  beachsn_lfs %>% select(source, date, catch, latitude, longitude, region),
  sls_lfs %>% select(source, date, catch, latitude, longitude, region),
  chipps_lfs %>% select(source, date, catch, latitude, longitude, region),
  sfbs_data %>% select(source, date, catch, latitude, longitude, region), 
  salvage_lfs_data %>% select(source, date, catch, latitude, longitude, region)) %>%
  filter(!is.na(catch),
         !is.na(latitude)) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326, remove = FALSE) %>%
  st_transform(crs = st_crs(WW_Delta)) %>%
  group_by(source, date, latitude, longitude, region) %>%
  summarize(total_catch = sum(catch)) %>%
  ungroup()

# this one has forklength/lifestage (for summary across wy by lifestage)
lfs_detail <- bind_rows(
  edsm_lfs %>% select(source, date, catch, mark_code, fork_length, latitude, longitude, region, stratum),
  twmm_lfs %>% select(source, date, catch, fork_length, latitude, longitude, region),
  sls_lfs %>% select(source, date, catch, fork_length, latitude, longitude, region, stratum),
  beachsn_lfs %>% select(source, date, catch, fork_length, latitude, longitude, region, stratum),
  chipps_lfs %>% select(source, date, catch, fork_length, latitude, longitude, region, stratum),
  sfbs_data %>% select(source, date, catch, fork_length, latitude, longitude, region, stratum),
  salvage_lfs_data %>% select(source, date, catch, fork_length, latitude, longitude, region)) %>%
  filter(!is.na(catch),
         !is.na(latitude)) %>%
  mutate(life_stage = ifelse(fork_length>84, "Adult", ifelse(fork_length>=20, "Juvenile", "Larva"))) %>% 
  arrange(date)

## Jersey Point Flow ---------------------------------

# ---- download the PDF Delta Hydrology Conditions (DWR)----
url1 <- "https://water.ca.gov/-/media/DWR-Website/Web-Pages/Programs/State-Water-Project/Operations-And-Maintenance/Files/Operations-Control-Office/Delta-Status-And-Operations/Delta-Hydrologic-Conditions-Daily-Summary.pdf"
tmp1 <- tempfile(fileext = ".pdf")
download.file(url1, tmp1, mode="wb")

# ---- extract raw text ---- 
txt <- pdf_text(tmp1)

# ---- parse page 1 table ----
page1 <- txt[1] %>% str_split("\n") %>% unlist()

# find lines with data (dates etc.)
data_lines1 <- page1[str_detect(page1, "\\d{1,2}/\\d{1,2}")]

# split each row by whitespace
hydro1 <- data_lines1 %>%
  str_squish() %>%
  str_replace_all("[^[:print:]]", "") %>%
  .[str_detect(., "^\\d{1,2}/\\d{1,2}/\\d{2,4}")] %>%
  str_split_fixed(" ", n = 11) %>%
  as.data.frame(stringsAsFactors = FALSE)

# add column names
colnames(hydro1) <- c(
  "Date","SR_at_Freeport_SRWTP","Yolo_Rumsey_FRE_FWB", "E_side_streams",
  "SJR_a_Vernalis", "Stockton_rain_in","CCF_cfs",
  "Tracy_cfs","CCWD_cfs","Barker_Slough_cfs",
  "Byron_Bethany_cfs"
)

numeric.col1 <- c("SR_at_Freeport_SRWTP","Yolo_Rumsey_FRE_FWB", "E_side_streams",
                  "SJR_a_Vernalis", "Stockton_rain_in","CCF_cfs",
                  "Tracy_cfs","CCWD_cfs","Barker_Slough_cfs",
                  "Byron_Bethany_cfs")
hydro1 <- hydro1 %>% 
  dplyr::filter(str_detect(Date, "\\d{1,2}/\\d{1,2}")) %>% 
  mutate(across(all_of(numeric.col1),
                ~ str_remove_all(.x, "[^0-9.-]") %>% as.numeric())) %>% 
  mutate(Date = as.Date(Date, format = "%m/%d/%Y"))



# ---- parse page 2 table ----
page2 <- txt[2] %>% str_split("\n") %>% unlist()
data_lines2 <- page2[str_detect(page2, "\\d{1,2}/\\d{1,2}")]
hydro2 <- data_lines2 %>%
  str_squish() %>%
  str_replace_all("[^[:print:]]", "") %>%
  .[str_detect(., "^\\d{1,2}/\\d{1,2}/\\d{2,4}")] %>%
  str_split_fixed(" ", n = 10) %>%
  as.data.frame(stringsAsFactors = FALSE)

colnames(hydro2) <- c(
  "Date","Banks_PP_cfs","Delta_GCD_cfs","Rio_Vista_Flow_cfs",
  "QWEST_cfs", "JPF_cfs", "NDOI_cfs","EI_3day","EI_14day","Delta_Status"
)

numeric.col2 <- c("Banks_PP_cfs","Delta_GCD_cfs","Rio_Vista_Flow_cfs",
                  "QWEST_cfs","JPF_cfs", "NDOI_cfs","EI_3day","EI_14day","Delta_Status")

hydro2 <- hydro2 |>
  dplyr::filter(str_detect(Date, "\\d{1,2}/\\d{1,2}")) %>% 
  mutate(across(all_of(numeric.col2),
                ~ str_remove_all(.x, "[^0-9.-]") %>% as.numeric())) %>% 
  mutate(Date = as.Date(Date, format = "%m/%d/%Y"))


# Define dates: (today and previous 14 days)
hydro1_14d <- hydro1 %>%
  arrange(desc(Date)) %>%  # newest date first
  slice(1:14) %>%           # take the last 14 rows
  arrange(Date)

hydro2_14d <- hydro2 %>%
  arrange(desc(Date)) %>%
  slice(1:14) %>%
  arrange(Date)

#join tables
hydro_14d <- hydro1_14d %>%
  left_join(hydro2_14d, by = "Date")

#select cols of interest
hydro_14d <- hydro_14d %>%
  select(Date, SJR_a_Vernalis, E_side_streams, SR_at_Freeport_SRWTP, Stockton_rain_in,
         Delta_GCD_cfs, JPF_cfs, Banks_PP_cfs, CCF_cfs, Tracy_cfs)


JPF_1d_lastdate <- hydro_14d %>% tail(1) %>% pull(Date)
JPF_1d <- hydro_14d %>% tail(1) %>% pull(JPF_cfs)



# Extract variables # this was code used before JPF was added to DWR hydrology report

# # Define dates: (today and previous 7 days)
# hydro1_7d <- hydro1 %>%
#   arrange(desc(Date)) %>%  # newest date first
#   slice(1:7) %>%           # take the last 7 rows
#   arrange(Date) 
# 
# hydro2_7d <- hydro2 %>%
#   arrange(desc(Date)) %>%
#   slice(1:7) %>%
#   arrange(Date)
# 
# #join tables
# hydro_7d <- hydro1_7d %>%
#   left_join(hydro2_7d, by = "Date")
# 
# #select cols of interest
# hydro_7d <- hydro_7d %>%
#   select(Date, SJR_a_Vernalis, E_side_streams, SR_at_Freeport_SRWTP, Stockton_rain_in, 
#          Delta_GCD_cfs, JPF_cfs, Banks_PP_cfs, CCF_cfs, Tracy_cfs)

# #make calculations     
# hydro_7d <- hydro_7d %>% 
#   mutate(
#     QXGEO = 0.133 * SR_at_Freeport_SRWTP + 829,
#     Delta_precip = Stockton_rain_in / 12/5 * 682230 * 0.5041666604 * 0.65, # 65% of in Delta precip
#     Delta_div = Delta_GCD_cfs * 0.65, #65% of in Delta diversions
#     pumps = Banks_PP_cfs + Tracy_cfs #
#   )
# 
# #calc JPF for past 7-days
# hydro_7d <- hydro_7d %>% 
#   mutate(JPF = 
#       SJR_a_Vernalis +
#       E_side_streams +
#       QXGEO +
#       Delta_precip -
#       Delta_div -
#       pumps)
# 
# JPF_7d <- mean(hydro_7d$JPF)
# 
# JPF_last_date <- ymd(tail(hydro_7d$Date, 1))