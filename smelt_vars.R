# User inputs for Assessment
# Aim to make any needed updates here for manual entries
# Last updated 10/15/2025

## Packages ---------------------
library(dplyr)
library(lubridate)

## Instructions
# 1. Upload new data files that are not online already
# 2. Edit which actions are relevant 
# 3. Edit any narrative text

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

## Delta Smelt current status
past_days <- 60 # choose how far back to go

df_wy = ds_latlon %>%
  filter(date > ymd("2024-10-01"))
df_recent <- ds_detail %>%
  filter(date > today()-past_days) 

# pull out lifestages present based off data
lifestages <- paste(unique(df_recent$life_stage), collapse = ", ")

# pull abundance estimate
abundance_current <- abundance %>%
  filter(stratum == "All Strata",
         abundance_index > 0)

abundance = last(abundance_current$abundance_index)
abundance_lcl <- last(abundance_current$lower_bound)
abundance_ucl <- last(abundance_current$upper_bound)
abundance_date <- last(abundance_current$dates)

# Need to figure out date filtering for these
adults_count <- ds_detail %>% filter(life_stage == "Adult") %>% select(catch)%>% sum(.)
marked <- ds_detail%>%filter(life_stage == "Adult", mark_code != "None") %>% select(catch)%>% sum(.)
unmarked <- ds_detail%>%filter(life_stage == "Adult", mark_code == "None") %>% select(catch)%>% sum(.)
last_catch_date <- max(ds_detail$date)
last_catch_location <- max(ds_detail$region)
last_catch_count <- tail(ds_detail$catch, 1)
ds_salvage_count <- ds_detail%>%filter(source == "salvage") %>% select(catch)%>% sum(.)
ds_cumsalvage <- salvage_data %>% select(salvage) %>% sum(.)

# read in release table to get: 
total_released <- 100000
last_release_date <- lubridate::ymd("2025-11-15")
last_release_location <- "Lookout Slough"
last_release_count <- 20000

# larval entrainment (currently by email)
sd_secchi_depth <- 0.76
sd_secchi_date <- ymd("2026-05-01")


