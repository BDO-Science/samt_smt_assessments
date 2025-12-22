library(tidyverse)
library(janitor)
library(here)
library(rvest)

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
  mutate(filter_actuals = if_else(actuals >= min & actuals <= max, 1, 0)) 

flow_thresholds <- flow_thresholds_all %>%
  filter(filter_actuals == 1)

####pull in forecasted flows
hydro_url <- 'https://www.cnrfc.noaa.gov/deterministicHourlyProductCSV.php'

# Read the HTML page
page <- read_html(hydro_url)

# Extract all links
links <- page %>%
  html_nodes("a") %>%
  html_attr("href")

# Filter for .zip files
zip_files <- links[str_detect(links, "\\.zip$")]
url_root <- 'https://www.cnrfc.noaa.gov'
sj_zip <- paste0(url_root,(zip_files[grepl('SanJoaquin', zip_files) & !grepl('_N_', zip_files)]))
sac_zip <- paste0(url_root,(zip_files[grepl('LowerSacramento', zip_files)]))

#download zip files
download.file(sj_zip, here(project,paste0('input_data/forecasts/zip/san_joaquin_forecast.zip')))
download.file(sac_zip, here(project,paste0('input_data/forecasts/zip/sacramento_forecast.zip')))
#unzip files
zip_files <- list.files(path = here(project, 'input_data/forecasts/zip'), full.names = TRUE)
lapply(zip_files, function(file) {
  unzip(file, exdir = here(project, 'input_data/forecasts'))
})

#read in individual files
forecast_files <- list.files(path = here(project, 'input_data/forecasts'), full.names = TRUE)
sac_forecast <- read_csv(max(forecast_files[grepl('Sacramento', forecast_files)])) %>%
  select(1, AMER = 2, VERN = 4) %>%
  mutate(date = as.Date(GMT)) %>% 
  filter(!is.na(date)) %>%
  mutate(across(2:3, as.numeric)) %>%
  group_by(date) %>%
  summarize(AMER = mean(AMER, na.rm =TRUE),
            VERN = mean(VERN, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(VERN1D = lag(VERN)) %>%
  filter(date >= Sys.Date() & date <= Sys.Date() + 6) %>%
  mutate(FRPT = AMER + VERN1D) %>%
  select(-AMER, -VERN, - VERN1D)


sj_forecast <- read_csv(max(forecast_files[grepl('Joaquin', forecast_files)])) %>%
  select(1, VNS = 47) %>%
  mutate(date = as.Date(GMT)) %>% 
  filter(!is.na(date)) %>%
  mutate(across(2, as.numeric)) %>%
  group_by(date) %>%
  summarize(VNS = mean(VNS, na.rm =TRUE)) %>%
  ungroup() %>%
  filter(date >= Sys.Date() & date <= Sys.Date() + 6)

sac_forecast_mean <- round(mean(sac_forecast$FRPT) * 1000,0)
sj_forecast_mean <- round(mean(sj_forecast$VNS) * 1000, 0)

flow_thresholds_forecast <- flow_thresholds_all %>%
  mutate(forecasts = if_else(river == 'Sacramento River', sac_forecast_mean, sj_forecast_mean)) %>%
  mutate(filter_forecast = if_else(forecasts >= min & forecasts <= max, 1, 0)) %>%
  filter(filter_forecast == 1)
hydrology_current <- paste0(flow_thresholds[1,1], flow_thresholds[2,1])
hydrology_forecast <- paste0(flow_thresholds_forecast[1,1], flow_thresholds_forecast[2,1])

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

channel_filter_current <- channel_all %>%
  filter(inflow_group == hydrology_current)

channel_filter_forecast <- channel_all %>%
  filter(inflow_group == hydrology_forecast)

difference <- if_else(channel_filter_current$length_change < channel_filter_forecast$length_change,
                      'increase', 'decrease')
print(difference)
