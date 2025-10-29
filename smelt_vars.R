# User inputs for Assessment
# Aim to make any needed updates here for manual entries
# Last updated 10/23/2025

# Packages ---------------------
library(dplyr)
library(lubridate)
source("smelt_data_extraction.R")

## Instructions
# 1. Upload new data files that are not online already (SLS, 20mm, Bay Study, EDSM, release table)
# 2. Edit which actions are relevant 
# 3. Edit any narrative text
# 4. Edit secchi depth if relevant

# Delta Smelt ---------------------------

## Actions ---------------------
first_flush_status = "relevant"
adult_ent_status = "relevant"
larval_ent_status = "relevant"
end_of_season = "not relevant"

## Narrative ----------------------
# - Text about what actions have recently started, ended, or predicted to start
# - Summary of relevant hydro/trigger conditions (triggered or may trigger)
# - Summary of salvage and expectations of salvage

narrative_text <- 
"- OMR Season has not yet begun for Delta Smelt but First Flush could occur starting Dec 1
- Freeport flows do not indicate First Flush. A storm is occurring this week. 
- No Delta Smelt salvage has been observed this water year"

## Delta Smelt current status -----------------
past_days <- 60 # choose how far back to go

df_wy = ds_latlon %>%
  filter(date > ymd("2024-10-01"))
df_recent <- ds_detail %>%
  filter(date > today()-past_days) 

# pull out lifestages present based off data
lifestages <- paste(unique(df_recent$life_stage), collapse = ", ")

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
adults_count <- ds_detail %>% filter(life_stage == "Adult") %>% pull(catch) %>% sum()
# marked <- ds_detail %>% filter(life_stage == "Adult", mark_code != "None") %>% pull(catch) %>% sum()
# unmarked <- ds_detail %>% filter(life_stage == "Adult", mark_code == "None") %>% pull(catch) %>% sum()
last_catch_date <- ds_detail %>% arrange(date) %>% tail(1) %>% pull(date) 
last_catch_location <- ds_detail %>% arrange(date) %>% tail(1) %>% pull(region) 
last_catch_count <- ds_detail %>% arrange(date) %>% tail(1) %>% pull(catch)
ds_salvage_count <- ds_detail %>% filter(source == "salvage") %>% pull(catch)%>% sum()
ds_cumsalvage <- salvage_data %>% pull(salvage) %>% sum()

## Releases ----------------------- 
releases <- read_csv(here("data_raw/smelt/smelt_release_table_2026.csv")) 
last_release <- releases %>%
  clean_names() %>%
  mutate(release_date = mdy(release_date)) %>%
  filter(release_date < "2025-11-30")%>% # this date needs to change to "today()" once releases start.
  mutate(approx_number_fish = parse_number(approx_number_fish))
total_released <- last_release %>% pull(approx_number_fish) %>% sum()
last_release_date <- last_release %>% tail(1) %>% pull(release_date)
last_release_location <- last_release %>% tail(1) %>% pull(release_site)
last_release_count <- last_release %>% tail(1) %>% pull(approx_number_fish)

## Secchi depth (currently by email)-------------------------
sd_secchi_depth <- 0.76
sd_secchi_date <- ymd("2026-05-01")

# Longfin Smelt -----------------------
# After December 1, if cumulative water year salvage of age 1+ Longfin smelt exceeds 
# 5% of the fall mid water trawl index plus one fish.
# adult_lfs_salvage_threshold 
