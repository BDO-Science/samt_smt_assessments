library(tidyverse)
library(janitor)
library(here)

project <- here()
year <- year(Sys.Date())
today <- Sys.Date()
all_maps <- readRDS(here(project, 'input_data/ZOI_maps.rds'))

sac <- read_csv(paste0('https://www.cbr.washington.edu/sacramento/data/php/rpt/river_daily.php?sc=1&outputFormat=csv&hafilter=All&year='
                       ,year,'&proj=FPT&span=no&startdate=1%2F1&enddate=12%2F31')) %>%
  clean_names() %>%
  filter(!is.na(river_discharge_flow_cfs)) %>%
  slice_tail() %>%
  pull(river_discharge_flow_cfs)

sjr <- read_csv(paste0('https://www.cbr.washington.edu/sacramento/data/php/rpt/river_daily.php?sc=1&outputFormat=csv&hafilter=All&year=',year,
                       '&proj=VNS&span=no&startdate=1%2F1&enddate=12%2F31')) %>%
  clean_names() %>%
  filter(!is.na(river_discharge_flow_cfs)) %>%
  slice_tail() %>%
  pull(river_discharge_flow_cfs)

flow_thresholds_all <- expand.grid(hydrology = c('lo', 'med','hi'), 
                               river = c('Sacramento River', 'San Joaquin River')) %>%
  arrange(hydrology) %>%
  mutate(min = c(0,0,13416,1984,24726,4097),
         max = c(13415,1983,24725,4096,87222,61005)) %>%
  mutate(actuals = if_else(river == 'Sacramento River', sac, sjr)) %>%
  mutate(filter = if_else(actuals >= min & actuals <= max, 1, 0)) 

flow_thresholds <- flow_thresholds_all %>%
  filter(filter == 1)

hydrology <- paste0(flow_thresholds[1,1], flow_thresholds[2,1])

####channel length

channel <- read_csv(here(project, 'input_data/zoi_channel_length.csv')) %>%
  clean_names() %>%
  dplyr::select(1,2,length = 3) %>%
  filter(!is.na(length)) %>%
  mutate(length = round(length * 0.00030),1)

channel_all <- channel %>%
  group_by(inflow_group) %>%
  slice(c(1, n())) %>%
  mutate(level = rep(c('low', 'high'))) %>%
  ungroup() %>%
  pivot_wider(names_from = 'level', values_from = c('omr_bin', 'length')) 

channel_all <- channel_all %>%
  mutate(length_change = length_high - length_low,
         stand_change = (length_change-min(channel_all$length_change))/(max(channel_all$length_change) 
                                                                        - min(channel_all$length_change)),
         stand_low = (length_low-min(channel_all$length_low))/(max(channel_all$length_low) 
                                                                     - min(channel_all$length_low)),
         stand_high = (length_high-min(channel_all$length_high))/(max(channel_all$length_high) 
                                                                     - min(channel_all$length_high)),
         prop_change = round(prop.table(length_change)*100,1)
         )

channel_filter <- channel_all %>%
  filter(inflow_group == hydrology)
