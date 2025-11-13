# smelt_vars.R 
# User inputs for Assessment - this is the doc you would modify
# Aim to make any needed updates here for manual entries
# Last updated 11/13/2025

# Packages ---------------------
library(dplyr)
library(lubridate)
library(readr)
source("smelt_data_extraction.R")

## Instructions
# 1. Upload new data files that are not online already (SLS, 20mm, Bay Study, EDSM, release table, abundance estimates)
# 2. Edit which actions are relevant 
# 3. Edit any narrative text
# 4. Edit secchi depth if relevant

# Actions ---------------------
first_flush_status = "relevant"
adult_ent_status = "relevant"
larval_ent_status = "not relevant"
lfs_adult_ent_status = "relevant"
lfs_larval_ent_status = "not relevant"
end_of_season = "not relevant"

# Narrative ----------------------
# - Text about what actions have recently started, ended, or predicted to start
# - Summary of relevant hydro/trigger conditions (triggered or may trigger)
# - Summary of salvage and expectations of salvage

narrative_text <- 
"- OMR Season has not yet begun for Delta Smelt but First Flush could occur starting Dec 1
- Freeport flows and turbidity do not indicate First Flush 
- No Delta Smelt salvage has been observed this water year"

# Delta Smelt ---------------------------

## Delta Smelt current status -----------------
past_days <- 14 # choose how far back to go

ds_wy = ds_latlon %>%
  filter(date > ymd("2025-10-01"))
ds_recent <- ds_detail %>%
  filter(date > today()-past_days) 

# pull out lifestages present based off data
ds_lifestages <- paste(unique(ds_recent$life_stage), collapse = ", ")

# edit recent with simple info
ds_recent_display <- ds_recent %>% 
  group_by(source, date, region, stratum, life_stage) %>%
  summarize(sum = sum(catch)) %>% 
  ungroup() %>%
  select(Survey = source, Date=date, Region = region, Stratum = stratum, `Life Stage` = life_stage, Catch = sum)

## Abundance ------------------------------
# pull abundance estimate
abundance_current <- abundance %>%
  filter(stratum == "All Strata",
         abundance_index > 0)

abundance = last(abundance_current$abundance_index)
abundance_lcl <- last(abundance_current$lower_bound)
abundance_ucl <- last(abundance_current$upper_bound)
abundance_date <- last(abundance_current$dates)

## Counts ----------------------
ds_adults_count <- ds_recent %>% filter(life_stage == "Adult") %>% pull(catch) %>% sum()
ds_juveniles_count <- ds_recent %>% filter(life_stage == "Juvenile") %>% pull(catch) %>% sum()
ds_larvae_count <- ds_recent %>% filter(life_stage == "Larva") %>% pull(catch) %>% sum()
ds_last_catch_date <- ds_recent %>% arrange(date) %>% tail(1) %>% pull(date) 
ds_last_catch_location <- ds_recent %>% arrange(date) %>% tail(1) %>% pull(region) 
ds_last_catch_count <- ds_recent %>% arrange(date) %>% tail(1) %>% pull(catch)

# Once 2026 starts updating go back to this code
# ds_salvage_count <- ds_recent %>% filter(source == "salvage") %>% pull(catch)%>% sum()
# ds_cumsalvage <- salvage_ds_data %>% pull(salvage) %>% sum()
ds_salvage_count <- 0
ds_cumsalvage <- 0

## Releases ----------------------- 
releases <- read_csv(here("data_raw/smelt/smelt_release_table_2026.csv")) 
last_release <- releases %>%
  clean_names() %>%
  mutate(release_date = mdy(release_date)) %>%
  filter(release_date < today())%>% 
  mutate(approx_number_fish = parse_number(approx_number_fish))
total_released <- last_release %>% pull(approx_number_fish) %>% sum()
last_release_date <- last_release %>% tail(1) %>% pull(release_date)
first_last_release_date <- last_release %>% tail(2) %>% head(1) %>% pull(release_date)
last_release_location <- last_release %>% tail(1) %>% pull(release_site)
last_release_count <- last_release %>% tail(2) %>% pull(approx_number_fish) %>% sum()

## Secchi depth (currently by email)-------------------------
sd_secchi_depth <- 0.76
sd_secchi_date <- ymd("2026-05-01")

# Longfin Smelt -----------------------

# After December 1, if cumulative water year salvage of age 1+ Longfin smelt exceeds 
# 5% of the fall mid water trawl index plus one fish.
# adult_lfs_salvage_threshold 

## Longfin Smelt current status -----------------
past_days <- 14 # choose how far back to go

lfs_wy = lfs_latlon %>%
  filter(date > ymd("2025-10-01"))
lfs_recent <- lfs_detail %>%
  filter(date > today()-past_days) 

# pull out lifestages present based off data
lfs_lifestages <- paste(unique(lfs_recent$life_stage), collapse = ", ")

# edit recent with simple info
lfs_recent_display <- lfs_recent %>% 
  group_by(source, date, region, stratum, life_stage) %>%
  summarize(sum = sum(catch)) %>% 
  ungroup() %>%
  select(Survey = source, Date=date, Region = region, Stratum = stratum, `Life Stage` = life_stage, Catch = sum)

## Counts ----------------------
lfs_adults_count <- lfs_recent %>% filter(life_stage == "Adult") %>% pull(catch) %>% sum()
lfs_juveniles_count <- lfs_recent %>% filter(life_stage == "Juvenile") %>% pull(catch) %>% sum()
lfs_larvae_count <- lfs_recent %>% filter(life_stage == "Larva") %>% pull(catch) %>% sum()
# marked <- lfs_detail %>% filter(life_stage == "Adult", mark_code != "None") %>% pull(catch) %>% sum()
# unmarked <- lfs_detail %>% filter(life_stage == "Adult", mark_code == "None") %>% pull(catch) %>% sum()
lfs_last_catch_date <- lfs_recent %>% arrange(date) %>% tail(1) %>% pull(date) 
lfs_last_catch_location <- lfs_recent %>% arrange(date) %>% tail(1) %>% pull(region) 
lfs_last_catch_count <- lfs_recent %>% arrange(date) %>% tail(1) %>% pull(catch)

# Once 2026 starts updating go back to this code
# lfs_salvage_count <- lfs_recent %>% filter(source == "salvage") %>% pull(catch)%>% sum()
# lfs_cumsalvage <- salvage_lfs_data %>% pull(salvage) %>% sum()
lfs_salvage_count <- 0
lfs_cumsalvage <- 0