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
  filter(date == today-1) %>%
  pull(river_discharge_flow_cfs)

sjr <- read_csv(paste0('https://www.cbr.washington.edu/sacramento/data/php/rpt/river_daily.php?sc=1&outputFormat=csv&hafilter=All&year=',year,
                       '&proj=VNS&span=no&startdate=1%2F1&enddate=12%2F31')) %>%
  clean_names() %>%
  filter(date == today-1) %>%
  pull(river_discharge_flow_cfs)

flow_thresholds <- expand.grid(hydrology = c('lo', 'med','hi'), 
                               river = c('Sacramento River', 'San Joaquin River')) %>%
  arrange(hydrology) %>%
  mutate(min = c(5117,890,13416,1984,24726,4097),
         max = c(13415,1983,24725,4096,87222,61005)) %>%
  mutate(actuals = if_else(river == 'Sacramento River', sac, sjr)) %>%
  mutate(filter = if_else(actuals >= min & actuals <= max, 1, 0)) %>%
  filter(filter == 1)

hydrology <- paste0(flow_thresholds[1,1], flow_thresholds[2,1])

map <- all_maps[[hydrology]]
map
