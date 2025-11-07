library(lubridate)
library(ggplot2)
library(dplyr)
library(sharpshootR)
library(here)
library(CDECRetrieve)
library(readr)

# One panel: Vernalis and Freeport flow
# One panel: Clifton Court Inflow and Jones PP - DWR spreadsheet? 
# One panel: Daily OMR (SacPAS) + triggers

# Get data -------------------------------
# Define start and end dates - these will remain the same throughout
start <- "2025-09-01"
end <- today()

## Exports data (perhaps DWR has more up to date values though?) -------------
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

## OMR -------------------
url_omr <- "https://www.cbr.washington.edu/sacramento/data/php/rpt/mg.php?sc=1&mgconfig=river&outputFormat=csvSingle&hafilter=Delta&year%5B%5D=2025&loc%5B%5D=DTO&data%5B%5D=OMRIndex&tempUnit=F&startdate=1%2F1&enddate=12%2F31&avgyear=0&consolidate=1&grid=1&y1min=&y1max=&y2min=&y2max=&size=large"
omr <- read_csv(url_omr)
omr_clean <- omr %>%
  filter(!is.na(parameter)) %>%
  mutate(date = ymd(paste0(year, "-", `mm-dd`))) %>%
  filter(date < end, date >= start)

## Freeport and Vernalis -------------
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

## Triggers ---------------------
triggers <- read_csv("data_raw/data_triggers.csv")
triggers_clean <- triggers %>%
  mutate(date = mdy(date_triggered))

# Make plot ----------------------
## Individual plots ---------------------------

export_plot <- pumping_clean %>%
  ggplot(aes(x = date)) +
  labs(y = 'Exports (cfs)') +
  geom_col(aes(y = value, fill = facility), color = "black") +
  scale_fill_manual(values = c('#0072B2', 'gray80')) +
  scale_x_date(date_breaks = '2 weeks', date_labels = '%b %d') +
  theme_bw() +
  theme(legend.position = 'bottom',
        axis.title.x= element_blank())
export_plot

# Pull annotation start value for flow plot
ann_start_FPT <- flow_clean %>% filter(station == "FPT", date == date(start)) %>% pull(value)
ann_start_VNS <- flow_clean %>% filter(station == "VNS", date == date(start)) %>% pull(value)

flow_plot <- flow_clean %>%
  ggplot(aes(x = date)) +
  annotate("label", x = date(start)+2, y = ann_start_FPT-3000, label = "FPT", color = "steelblue3")+
  annotate("label", x = date(start)+2, y = ann_start_VNS+3000, label = "VNS", color = "gray20")+
  labs(y = 'Flow (cfs)') +
  geom_line(aes(y = value, color = station)) +
  scale_color_manual(values = c("steelblue3", "gray15")) +
  scale_x_date(date_breaks = '2 weeks', date_labels = '%b %d') +
  theme_bw() +
  theme(legend.position = 'none',
        axis.title.x = element_blank())
flow_plot

omr_plot <- 
  ggplot() +
  geom_line(data = omr_clean, aes(x = date, y = value)) +
  geom_vline(data = triggers_clean, aes(xintercept = date, color = regulation))+
  geom_text(data = triggers_clean, aes(x = date-2, y = -3500, label = plot_label))+
  labs(y = 'Daily OMRI (cfs)') +
  scale_x_date(date_breaks = '2 weeks', date_labels = '%b %d') +
  scale_color_manual(values = c("magenta4", "blue3"))+
  theme_bw() +
  theme(legend.position = "none",
        axis.title.x = element_blank())
omr_plot

## Combine plots -------------------
library(patchwork)
omr_plot + flow_plot +export_plot +  plot_layout(nrow = 3)
