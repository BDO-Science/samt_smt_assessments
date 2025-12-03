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

cvp = pumping_clean %>%
  filter(facility == 'CVP') %>%
  slice_tail() %>%
  pull(value) %>%
  prettyNum(big.mark = ",")
swp = pumping_clean %>%
  filter(facility == 'SWP') %>%
  slice_tail() %>%
  pull(value) %>%
  prettyNum(big.mark = ",")
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
url_omr5D <- "https://www.cbr.washington.edu/sacramento/data/php/rpt/mg.php?sc=1&mgconfig=river&outputFormat=csvSingle&hafilter=All&year%5B%5D=2025&loc%5B%5D=KWK&data%5B%5D=OMRIndex5Day&tempUnit=F&startdate=1%2F1&enddate=12%2F31&avgyear=0&consolidate=1&grid=1&y1min=&y1max=&y2min=&y2max=&size=large"
url_omr14D <- "https://www.cbr.washington.edu/sacramento/data/php/rpt/mg.php?sc=1&mgconfig=river&outputFormat=csvSingle&hafilter=All&year%5B%5D=2025&loc%5B%5D=KWK&data%5B%5D=OMRIndex14Day&tempUnit=F&startdate=1%2F1&enddate=12%2F31&avgyear=0&consolidate=1&grid=1&y1min=&y1max=&y2min=&y2max=&size=large"
omr <- read_csv(url_omr) %>%
  mutate(measure = 'OMR')
omr5D <- read_csv(url_omr5D) %>%
  mutate(measure = "OMR5D")
omr14D <- read_csv(url_omr14D) %>%
  mutate(measure = "OMR14D")
omr_clean <- bind_rows(omr, omr5D, omr14D) %>%
  filter(!is.na(parameter)) %>%
  mutate(date = ymd(paste0(year, "-", `mm-dd`))) %>%
  filter(date < end, date >= start) %>%
  mutate(measure = factor(measure, levels = c('OMR', 'OMR5D', 'OMR14D'),
                          labels = c('OMR', 'OMR 5 day index', 'OMR 14 day index')))
omr_text <- omr_clean %>%
  filter(measure == 'OMR') %>%
  slice_tail() %>%
  pull(value) %>%
  prettyNum(big.mark = ",")
omr5D_text <- omr_clean %>%
  filter(measure == 'OMR 5 day index') %>%
  slice_tail() %>%
  pull(value) %>%
  prettyNum(big.mark = ",")
omr14D_text <- omr_clean %>%
  filter(measure == 'OMR 14 day index') %>%
  slice_tail() %>%
  pull(value) %>%
  prettyNum(big.mark = ",")

# Read in Freeport and Vernalis Flow

flow_clean <- read_csv('https://www.cbr.washington.edu/sacramento/data/php/rpt/mg.php?map=1&mgconfig=river&tempUnit=F&avgyear=0&consolidate=1&grid=1&y1min=&y1max=&y2min=&y2max=&size=large&outputFormat=csv&data[]=Flow&loc[]=FPT&loc[]=VNS&year[]=2025') %>%
  clean_names() %>%
  mutate(year = year(Sys.Date())) %>%
  mutate(date = ymd(paste0(year,'-',mm_dd))) %>%
  filter(!is.na(date)) %>%
  select(date, VNS = 2, FPT = 3) %>%
  pivot_longer(names_to = 'station', values_to = 'value', 2:3) %>%
  mutate(parameter = 'flow') %>%
  select(1,2,4,3) %>%
  filter(date >= start)

fpt = flow_clean %>%
  filter(station == 'FPT') %>%
  slice_max(order_by = date) %>%
  pull(value) %>%
  round(0) %>%
  prettyNum(big.mark = ",")
vns = flow_clean %>%
  filter(station == 'VNS') %>%
  slice_max(order_by = date) %>%
  pull(value) %>%
  round(0) %>%
  prettyNum(big.mark = ",")
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

