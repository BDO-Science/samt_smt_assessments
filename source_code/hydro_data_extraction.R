library(lubridate)
library(dplyr)
library(readr)
library(here)
library(janitor)
library(tidyverse)

# Define start and end date
start <- "2025-10-01"
end <- today()

pumping_clean <- read_csv('https://www.cbr.washington.edu/sacramento/data/php/rpt/mg.php?map=1&mgconfig=river&tempUnit=F&avgyear=0&consolidate=1&grid=1&y1min=&y1max=&y2min=&y2max=&size=large&outputFormat=csv&data[]=PumpingDischarge&loc[]=HRO&loc[]=TRP&year[]=2025') %>%
  clean_names() %>%
  mutate(year = year(Sys.Date())) %>%
  mutate(date = ymd(paste0(year,'-',mm_dd))) %>%
  filter(!is.na(date)) %>%
  select(date, HRO = 2, TRP = 3) %>%
  pivot_longer(names_to = 'station', values_to = 'value', 2:3) %>%
  mutate(parameter = 'exports',
         facility = if_else(station == 'HRO', 'SWP', 'CVP')) %>%
  select(1,2,4,3,5) %>%
  filter(date >= start)
# Read in exports data - this is from CDEC. Is there another cleaner source?
# stations_exp <- c("TRP", "HRO")
# pumping <- lapply(stations_exp, function(x) {cdec_query(station = x ,sensor_num = 70, dur_code = "D", start_date = start,end_date = end)})
# 
# pumping_df <- bind_rows(pumping)
# pumping_clean <- pumping_df %>%
#   mutate(parameter = "exports", 
#          date = ymd(datetime)) %>%
#   select(date,station = location_id,parameter,value = parameter_value) %>%
#   filter(!is.na(value),
#          !is.na(station)) %>%
#   mutate(facility = if_else(station == "TRP", "CVP", "SWP"))

# Read in OMRI from SacPAS
url_omr <- "https://www.cbr.washington.edu/sacramento/data/php/rpt/mg.php?sc=1&mgconfig=river&outputFormat=csvSingle&hafilter=Delta&year%5B%5D=2025&loc%5B%5D=DTO&data%5B%5D=OMRIndex&tempUnit=F&startdate=1%2F1&enddate=12%2F31&avgyear=0&consolidate=1&grid=1&y1min=&y1max=&y2min=&y2max=&size=large"
omr <- read_csv(url_omr)
omr_clean <- omr %>%
  filter(!is.na(parameter)) %>%
  mutate(date = ymd(paste0(year, "-", `mm-dd`))) %>%
  filter(date < end, date >= start)


# Read in Freeport and Vernalis Flow

flow_clean <- read_csv('https://www.cbr.washington.edu/sacramento/data/php/rpt/mg.php?map=1&mgconfig=river&tempUnit=F&avgyear=0&consolidate=1&grid=1&y1min=&y1max=&y2min=&y2max=&size=large&outputFormat=csv&data[]=Flow&loc[]=FPT&loc[]=11303500&year[]=2025') %>%
  clean_names() %>%
  mutate(year = year(Sys.Date())) %>%
  mutate(date = ymd(paste0(year,'-',mm_dd))) %>%
  filter(!is.na(date)) %>%
  select(date, VNS = 2, FPT = 3) %>%
  pivot_longer(names_to = 'station', values_to = 'value', 2:3) %>%
  mutate(parameter = 'flow') %>%
  select(1,2,4,3) %>%
  filter(date >= start)
#stations_flow <- c("FPT", "VNS")
# flow <- lapply(stations_flow, function(x) {cdec_query(station = x ,sensor_num = 20, dur_code = "H", start_date = start,end_date = end)})
# 
# flow_df <- bind_rows(flow)
# flow_clean <- flow_df %>%
#   mutate(parameter = "flow", 
#          date = date(datetime)) %>%
#   select(date,station = location_id,parameter,value = parameter_value) %>%
#   filter(!is.na(value),
#          !is.na(station)) %>%
#   # calculate daily mean
#   group_by(date, station, parameter) %>%
#   summarize(value = mean(value)) %>%
#   ungroup()

# Read in triggers
# The data_triggers file needs to be updated manually
triggers <- read_csv("data_raw/data_triggers.csv")
triggers_clean <- triggers %>%
  mutate(date = mdy(date_triggered))

