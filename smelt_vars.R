# smelt_vars.R 
# User inputs for Assessment - this is the doc you would modify
# Aim to make any needed updates here for manual entries
# Last updated 11/26/2025

# Packages ---------------------
library(dplyr)
library(lubridate)
library(readr)
library(janitor)
source("smelt_data_extraction.R")

## Instructions
# 1. Upload new data files that are not online already (SLS, 20mm, Bay Study, EDSM, abundance estimates - add the newest estimates (sheet 1) and dates (sheet 2))
# 2. Edit which actions are relevant 
# 3. Edit narrative text
# 4. Edit evaluation questions and references
# 5. Edit turbidity and secchi depth if relevant
# 6. Make edits to salvage when salvage starts (see smelt_data_extraction.R Salvage section and Salvage sections below)
# 7. Need to render pdf separately from the html to get both to be updated (option in render in quarto doc)

# Actions ---------------------
first_flush_status = "not relevant"
first_flush_lfs_status = "not relevant"
adult_ent_status = "relevant"
larval_ent_status = "relevant"
lfs_adult_ent_status = "relevant"
lfs_larval_ent_status = "relevant"
end_of_season = "not relevant"

# Narrative ----------------------
# - Text about what actions have recently started, ended, or predicted to start
# - Summary of relevant hydro/trigger conditions (triggered or may trigger)
# - Summary of salvage and expectations of salvage

narrative_text <- 
"- Entrainment management is currently active.
- Delta smelt were recently detected at Suisun Marsh, Chipps Island, and the Sacramento Deep Water Ship Channel. 
- No Delta smelt or longfin smelt salvage has been observed this water year.
- Turbidity in the central/south Delta is low.
"

# Evaluation question responses ---------------------

# Delta smelt

# 1. After the start of entrainment management, is JPF < 0, is daily average turbidity ≥ 12 FNU 
#in the OMR corridor (stations OBI, HOL, and OSJ), and has the average water temperature 
#at Jersey Point or Rio Vista not exceeded 53.6\(^{\circ}\)F (12\(^{\circ}\)C) for 3 consecutive days?
ds_eval_1 <- "No, these conditions will not be met this week."

# 2. Has the average water temperature at Jersey Point or Rio Vista not exceeded 
#53.6\(^{\circ}\)F (12\(^{\circ}\)C) for 3 consecutive days and/or has this action already been taken during WY 2026?
ds_eval_2 <- "Temperature at Rio Vista or Jersey Point has not exceeded the threshold. The Delta smelt adult 
entrainment management action has not yet been taken in WY 2026."

# 3. What is the evidence for the onset of Delta smelt spawning?
ds_eval_3 <- "Upstream migration for Delta smelt occurs between September and December and in response 
to “first flush” conditions (Sommer et al. 2011, Grimaldo et al. 2009). Migration typically ranges one 
to four weeks after flow and turbidity increases, based on salvage data (Sommer et al. 2011). 
Historically, detections of ripe Delta smelt began in January and peaked in February and March and 
the majority of Delta Smelt spawning occurs within a temperature range of 9-18˚C (Damon et al. 2016). 
Based on [historical monitoring data](https://github.com/Delta-Stewardship-Council/deltafish) from the past few years, 
first detection of larvae in the Central and South Delta has typically occurred by mid to late March. 
Because first flush conditions were met on December 23, 2025, spawning may begin occurring during the 
current assessment week, consistent with the typical one- to four-week response window following increased 
flow and turbidity. Recent captures at Chipps Island and the Sacramento Deep Water Ship Channel are consistent 
with an upstream spawning migration into tidal freshwater habitats."

# 4. After the onset of spawning, have the following conditions occurred: JPF < 0, 
#daily average turbidity is ≥12FNU in the South Delta, and PTM modeling indicates 
#OMRI no more negative than -3500 cfs for at least 7 days would avoid ≥5% 
#entrainment of the Delta smelt population at facilities after 30 days?
ds_eval_4 <- "Although spawning may occur during the current assessment period, JPF is above 0 cfs; 
therefore, the conditions required to trigger larval and juvenile Delta smelt entrainment management are not met."

# Longfin smelt

# 1. If JPF < 0, what is the trajectory of annual loss of adult longfin smelt 
#and is it likely to exceed 5% of the adult population estimate? 
#Is South Delta entrainment expected to decrease due to a reduction in export pumping?
lfs_eval_1 <- "JPF is > 0 cfs and no adult longfin smelt have been detected in salvage."

# 2. For larval and juvenile longfin smelt, if JPF < 0 cfs, do particle tracking 
#models show a moderate to high difference in particle fates across different 
#OMRI scenarios? Does Zone of Influence modeling show moderate to high changes 
#in hydrodynamic footprint across different OMRI scenarios? Are these effects 
#anticipated to cause a population decline?
lfs_eval_2 <-  "JPF is not less than 0 cfs and is not predicted to go below 0 cfs this week. 
The Zone of Influence modeling indicates moderate differences in hydrodynamic 
footprint across OMRI scenarios, with no change between current and forecasted conditions."

# DONT NEED TO CHANGE UNLESS YOU WANT TO
# 3. Is there additional information or other analyses that should be considered in this evaluation?
lfs_eval_3 <- "Additional information may be discussed if needed at the DAT call."

# REFERENCES- check to make sure are the same based on answers to questions
# refs <- "Damon, L. J., S. B. Slater, R. D. Baxter, and R. W. Fujimura. 2016. 
# Fecundity and reproductive potential of wild female Delta smelt in the upper San 
# Francisco Estuary, California. California Fish and Game 102(4):188–210. 
# 
# Grimaldo, L. F., T. Sommer, N. Van Ark, G. Jones, E. Holland, P. B. Moyle, 
# B. Herbold & P. Smith (2009) Factors Affecting Fish Entrainment into Massive Water 
# Diversions in a Tidal Freshwater Estuary: Can Fish Losses be Managed? North 
# American Journal of Fisheries Management, 29:5, 1253-1270, DOI: 10.1577/M08-062.1  
# 
# Sommer, T., F. Mejia, M. Nobriga, and L. Grimaldo. 2011. The Spawning Migration 
# of Delta Smelt in the Upper San Francisco Estuary. San Francisco Estuary and Watershed Science 9(2)."

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

## EDIT: South Delta conditions (turbidity and secchi depth-currently by email)-------------------------
sd_secchi_depth <- 0.76
sd_secchi_date <- ymd("2026-01-13")
sd_turb <- 6.4
sd_turb_date <- ymd("2025-01-26")

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
#filter(date > ymd("2025-10-01"))
ds_larvae_count <- ds_recent %>% filter(life_stage == "Larva") %>% pull(catch) %>% sum()
ds_last_catch_date <- ds_recent %>% arrange(date) %>% tail(1) %>% pull(date) 
ds_last_catch_location <- ds_recent %>% arrange(date) %>% tail(1) %>% pull(region) 
ds_last_catch_count <- ds_recent %>% arrange(date) %>% tail(1) %>% pull(catch)

#wy counts
ds_adults_count_wy <- ds_detail %>% filter(date > ymd("2025-10-01")) %>% 
  filter(life_stage == "Adult") %>% pull(catch) %>% sum()
ds_juveniles_count_wy <- ds_detail %>% filter(date > ymd("2025-10-01")) %>% 
  filter(life_stage == "Juvenile") %>% pull(catch) %>% sum()
ds_larvae_count_wy <- ds_detail %>% filter(date > ymd("2025-10-01")) %>% 
  filter(life_stage == "Larva") %>% pull(catch) %>% sum()

## Salvage ----------------------
# Once 2026 starts updating go back to this code
# ds_salvage_count <- ds_recent %>% filter(source == "salvage") %>% pull(catch)%>% sum()
# ds_cumsalvage <- salvage_ds_data %>% pull(salvage) %>% sum()
ds_salvage_count <- 0
ds_cumsalvage <- 0

## Releases ----------------------- 
# releases <- read_csv(here("data_raw/smelt/smelt_release_table_2026.csv")) 
# last_release <- smelt_release_table %>%
#   mutate(release_date = ymd(date)) %>%
#   filter(release_date < today())
# total_released <- last_release %>% pull(total_released) %>% sum()
# last_release_date <- last_release %>% tail(1) %>% pull(release_date)
# first_last_release_date <- last_release %>% tail(2) %>% head(1) %>% pull(release_date)
# last_release_location <- last_release %>% tail(1) %>% pull(location)
# last_release_count <- last_release %>% tail(2) %>% pull(total_released) %>% sum()

#prepping new code for release data
releases <- read_tsv("https://www.cbr.washington.edu/sacramento/data/generated/WY2026_smeltreleases.txt") %>% 
  filter(!is.na(Location)) %>% 
  arrange(Date) %>% 
  clean_names()
last_release <- releases %>%
  mutate(date = as.Date(date)) %>% 
  mutate(release_date= date) %>% 
    #release_date = ymd(date)) %>%
  filter(release_date < today())
total_released <- last_release %>% pull(total_released) %>% sum()
last_release_date <- last_release %>% tail(1) %>% pull(release_date)
first_last_release_date <- last_release %>% tail(2) %>% head(1) %>% pull(release_date)
last_release_location <- last_release %>% tail(1) %>% pull(location)
last_release_count <- last_release %>% tail(1) %>% pull(total_released) %>% sum()



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

#wy counts
lfs_adults_count_wy <- lfs_detail %>% filter(date > ymd("2025-10-01")) %>% 
  filter(life_stage == "Adult") %>% pull(catch) %>% sum()
lfs_juveniles_count_wy <- lfs_detail %>% filter(date > ymd("2025-10-01")) %>% 
  filter(life_stage == "Juvenile") %>% pull(catch) %>% sum()
lfs_larvae_count_wy <- lfs_detail %>% filter(date > ymd("2025-10-01")) %>% 
  filter(life_stage == "Larva") %>% pull(catch) %>% sum()

## Salvage ----------------------------
# Once 2026 starts updating go back to this code
# lfs_salvage_count <- lfs_recent %>% filter(source == "salvage") %>% pull(catch)%>% sum()
# lfs_cumsalvage <- salvage_lfs_data %>% pull(salvage) %>% sum()
lfs_salvage_count <- 0
lfs_cumsalvage <- 0