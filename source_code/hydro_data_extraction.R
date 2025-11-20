library(lubridate)
library(dplyr)
library(readr)
library(here)
library(CDECRetrieve)

# Define start and end date
start <- "2025-10-01"
end <- today()


# Read in exports data - this is from CDEC. Is there another cleaner source?
stations_exp <- c("TRP", "HRO")
pumping <- lapply(stations_exp, function(x) {cdec_query(station = x ,sensor_num = 70, dur_code = "D", start_date = start,end_date = end)})

pumping_df <- bind_rows(pumping)
pumping_clean <- pumping_df %>%
  mutate(parameter = "exports", 
         date = ymd(datetime)) %>%
  select(date,station = location_id,parameter,value = parameter_value) %>%
  filter(!is.na(value),
         !is.na(station)) %>%
  mutate(facility = if_else(station == "TRP", "CVP", "SWP"))

# Read in OMRI from SacPAS
url_omr <- "https://www.cbr.washington.edu/sacramento/data/php/rpt/mg.php?sc=1&mgconfig=river&outputFormat=csvSingle&hafilter=Delta&year%5B%5D=2025&loc%5B%5D=DTO&data%5B%5D=OMRIndex&tempUnit=F&startdate=1%2F1&enddate=12%2F31&avgyear=0&consolidate=1&grid=1&y1min=&y1max=&y2min=&y2max=&size=large"
omr <- read_csv(url_omr)
omr_clean <- omr %>%
  filter(!is.na(parameter)) %>%
  mutate(date = ymd(paste0(year, "-", `mm-dd`))) %>%
  filter(date < end, date >= start)


# Read in Freeport and Vernalis Flow
stations_flow <- c("FPT", "VNS")
flow <- lapply(stations_flow, function(x) {cdec_query(station = x ,sensor_num = 20, dur_code = "H", start_date = start,end_date = end)})

flow_df <- bind_rows(flow)
flow_clean <- flow_df %>%
  mutate(parameter = "flow", 
         date = date(datetime)) %>%
  select(date,station = location_id,parameter,value = parameter_value) %>%
  filter(!is.na(value),
         !is.na(station)) %>%
  # calculate daily mean
  group_by(date, station, parameter) %>%
  summarize(value = mean(value)) %>%
  ungroup()

# Read in triggers
# The data_triggers file needs to be updated manually
triggers <- read_csv("data_raw/data_triggers.csv")
triggers_clean <- triggers %>%
  mutate(date = mdy(date_triggered))

