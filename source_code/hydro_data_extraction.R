library(lubridate)
library(dplyr)
library(readr)
library(here)
library(janitor)
library(tidyverse)
library(rvest)

# Define start and end date
start <- "2025-10-01"
end <- today()
wy <- 2026
py <- wy-1

pumping_clean <- bind_rows(read_csv(paste0('https://www.cbr.washington.edu/sacramento/data/php/rpt/mg.php?map=1&mgconfig=river&tempUnit=F&avgyear=0&consolidate=1&grid=1&y1min=&y1max=&y2min=&y2max=&size=large&outputFormat=csvSingle&data[]=PumpingDischarge&loc[]=TRP&loc[]=HRO&year[]=',wy)),
                           read_csv(paste0('https://www.cbr.washington.edu/sacramento/data/php/rpt/mg.php?map=1&mgconfig=river&tempUnit=F&avgyear=0&consolidate=1&grid=1&y1min=&y1max=&y2min=&y2max=&size=large&outputFormat=csvSingle&data[]=PumpingDischarge&loc[]=TRP&loc[]=HRO&year[]=',py))) %>%
  clean_names() %>%
  mutate(date = ymd(paste0(year,'-',mm_dd))) %>%
  filter(!is.na(date)) %>%
  select(date, station = 3, 7) %>%
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
url_omr <- paste0("https://www.cbr.washington.edu/sacramento/data/php/rpt/mg.php?sc=1&mgconfig=river&outputFormat=csvSingle&hafilter=Delta&year%5B%5D=",wy,"&loc%5B%5D=DTO&data%5B%5D=OMRIndex&tempUnit=F&startdate=1%2F1&enddate=12%2F31&avgyear=0&consolidate=1&grid=1&y1min=&y1max=&y2min=&y2max=&size=large")
url_omr5D <- paste0("https://www.cbr.washington.edu/sacramento/data/php/rpt/mg.php?sc=1&mgconfig=river&outputFormat=csvSingle&hafilter=All&year%5B%5D=",wy,"&loc%5B%5D=KWK&data%5B%5D=OMRIndex5Day&tempUnit=F&startdate=1%2F1&enddate=12%2F31&avgyear=0&consolidate=1&grid=1&y1min=&y1max=&y2min=&y2max=&size=large")
url_omr14D <- paste0("https://www.cbr.washington.edu/sacramento/data/php/rpt/mg.php?sc=1&mgconfig=river&outputFormat=csvSingle&hafilter=All&year%5B%5D=",wy,"&loc%5B%5D=KWK&data%5B%5D=OMRIndex14Day&tempUnit=F&startdate=1%2F1&enddate=12%2F31&avgyear=0&consolidate=1&grid=1&y1min=&y1max=&y2min=&y2max=&size=large")
url_omr_prev <- paste0("https://www.cbr.washington.edu/sacramento/data/php/rpt/mg.php?sc=1&mgconfig=river&outputFormat=csvSingle&hafilter=Delta&year%5B%5D=",py,"&loc%5B%5D=DTO&data%5B%5D=OMRIndex&tempUnit=F&startdate=1%2F1&enddate=12%2F31&avgyear=0&consolidate=1&grid=1&y1min=&y1max=&y2min=&y2max=&size=large")
url_omr5D_prev <- paste0("https://www.cbr.washington.edu/sacramento/data/php/rpt/mg.php?sc=1&mgconfig=river&outputFormat=csvSingle&hafilter=All&year%5B%5D=",py,"&loc%5B%5D=KWK&data%5B%5D=OMRIndex5Day&tempUnit=F&startdate=1%2F1&enddate=12%2F31&avgyear=0&consolidate=1&grid=1&y1min=&y1max=&y2min=&y2max=&size=large")
url_omr14D_prev <- paste0("https://www.cbr.washington.edu/sacramento/data/php/rpt/mg.php?sc=1&mgconfig=river&outputFormat=csvSingle&hafilter=All&year%5B%5D=",py,"&loc%5B%5D=KWK&data%5B%5D=OMRIndex14Day&tempUnit=F&startdate=1%2F1&enddate=12%2F31&avgyear=0&consolidate=1&grid=1&y1min=&y1max=&y2min=&y2max=&size=large") #url_omr14D <-
omr <- bind_rows(read_csv(url_omr),
                 read_csv(url_omr_prev)) %>%
  mutate(measure = 'OMR')
omr5D <- bind_rows(read_csv(url_omr5D),
                   read_csv(url_omr5D_prev)) %>%
  mutate(measure = "OMR5D")
omr14D <- bind_rows(read_csv(url_omr14D),
                    read_csv(url_omr14D_prev)) %>%
  mutate(measure = "OMR14D")
omr_clean <- bind_rows(omr, omr5D, omr14D) %>%
  filter(!is.na(parameter)) %>%
  mutate(date = ymd(paste0(year, "-", `mm-dd`))) %>%
  filter(date < end, date >= start) %>%
  mutate(measure = factor(measure, levels = c('OMR', 'OMR5D', 'OMR14D'),
                          labels = c('OMR', 'OMR 5 day index', 'OMR 14 day index'))) %>%
  arrange(date)
omr_text <- omr_clean %>%
  filter(measure == 'OMR',
         !is.na(value)) %>%
  slice_tail() %>%
  pull(value) %>%
  prettyNum(big.mark = ",")
omr5D_text <- omr_clean %>%
  filter(measure == 'OMR 5 day index',
         !is.na(value)) %>%
  slice_tail() %>%
  pull(value) %>%
  prettyNum(big.mark = ",")
omr14D_text <- omr_clean %>%
  filter(measure == 'OMR 14 day index',
         !is.na(value)) %>%
  slice_tail() %>%
  pull(value) %>%
  prettyNum(big.mark = ",")

# Read in Freeport and Vernalis Flow

flow_clean <- bind_rows(read_csv(paste0('https://www.cbr.washington.edu/sacramento/data/php/rpt/mg.php?map=1&mgconfig=river&tempUnit=F&avgyear=0&consolidate=1&grid=1&y1min=&y1max=&y2min=&y2max=&size=large&outputFormat=csvSingle&data[]=Flow&loc[]=FPT&loc[]=VNS&year[]=',wy)),
                        read_csv(paste0('https://www.cbr.washington.edu/sacramento/data/php/rpt/mg.php?map=1&mgconfig=river&tempUnit=F&avgyear=0&consolidate=1&grid=1&y1min=&y1max=&y2min=&y2max=&size=large&outputFormat=csvSingle&data[]=Flow&loc[]=FPT&loc[]=VNS&year[]=',py))) %>%
  clean_names() %>%
  mutate(date = ymd(paste0(year,'-',mm_dd))) %>%
  filter(!is.na(date)) %>%
  select(date, station = 3, flow = 7) %>%
  filter(date >= start)

fpt = flow_clean %>%
  filter(station == 'FPT') %>%
  slice_max(order_by = date) %>%
  pull(flow) %>%
  round(0) %>%
  prettyNum(big.mark = ",")
vns = flow_clean %>%
  filter(station == 'VNS') %>%
  slice_max(order_by = date) %>%
  pull(flow) %>%
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
  mutate(date = mdy(date_triggered),
         date_implemented= mdy(date_implemented),
         implementation_end= mdy(implementation_end),
         type= as.factor(type),
         type= ordered(type, levels= c("trigger", "threshold")))

triggers_shading <- triggers_clean %>% 
  filter(!is.na(implementation_end))

# Read in JPF historical data
jpf_all <- read_csv("https://www.cbr.washington.edu/sacramento/data/generated/WY2026_JPF.csv") %>% 
  clean_names() %>%
  mutate(date = ymd(date))

# Read in current JPF from SacPAS

url <- "https://www.cbr.washington.edu/sacramento/workgroups/delta_smelt.html"
page <- read_html(url)
tables <- html_table(page, fill = TRUE)

## Pull Environmental Table ----------------------------
hydro_table_raw <- tables[[1]][-1,]
hydro_table <- hydro_table_raw %>% clean_names()

# JPF
# Get the most recent value in the column
#jpf_daily <- as.numeric(tail(hydro_table$jpf_cfs_dwr,1))

jpf_daily <- hydro_table %>% 
  filter(!is.na(jpf_cfs_dwr)) %>%
  slice_tail() %>%
  pull(jpf_cfs_dwr) %>%
  prettyNum(big.mark = ",")
