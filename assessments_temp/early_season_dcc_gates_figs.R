# Figures for early season assessments- DCC gates, Action 5
# created by Lilly McCormick (lmccormick@usbr.gov)
# Developed 10/5/2026



# setup
library(CDECRetrieve)
library(here)
library(dplyr)
library(lubridate)
library(tidyr)
library(zoo)
library(ggplot2)
library(viridis)

start_date <- "2026-09-01"
end_date <- today()-1

# Pull EC data from CDEC in uS/cm (equivalent to umhos/cm)

jer_raw <- cdec_query("JER", sensor = "100", "D", start = start_date, end = end_date) %>% 
  mutate(date = as.Date(datetime))%>% 
  mutate(station = "Jersey Point")

bet_raw <- cdec_query("BET", sensor = "100", "D", start = start_date, end = end_date) %>% 
  mutate(date = as.Date(datetime)) %>% 
  mutate(station = "Bethel Island")

hol_raw <- cdec_query("HOL", sensor = "100", "D", start = start_date, end = end_date) %>% 
  mutate(date = as.Date(datetime))%>% 
  mutate(station = "Holland Cut")

ob1_raw <- cdec_query("OB1", sensor = "100", "E", start = start_date, end = end_date) %>% 
  mutate(date = as.Date(datetime)) %>% 
  group_by(date, .drop = FALSE) %>% 
  summarize(parameter_value = mean(parameter_value, na.rm = TRUE)) %>% 
  mutate(agency_cd= "CDEC",
         location_id= "OB1",
         parameter_cd= 100, datetime= NA) %>% 
  select(agency_cd, location_id, datetime, parameter_cd, parameter_value, date)%>% 
  mutate(station = "Bacon Island")

# calc 14-day rolling average

jer_14d <- jer_raw %>% 
  mutate(roll_avg = rollmean(parameter_value, k= 14, fill = NA, align = "right")) %>% 
  drop_na(roll_avg) #%>% 
#  mutate(station = "Jersey Point")

bet_14d <- bet_raw %>% 
  mutate(roll_avg = rollmean(parameter_value, k= 14, fill = NA, align = "right")) %>% 
  drop_na(roll_avg)# %>% 
#  mutate(station = "Bethel Island")

hol_14d <- hol_raw %>% 
  mutate(roll_avg = rollmean(parameter_value, k= 14, fill = NA, align = "right")) %>% 
  drop_na(roll_avg) #%>% 
#  mutate(station = "Holland Cut")

ob1_14d <- ob1_raw %>% 
  mutate(roll_avg = rollmean(parameter_value, k= 14, fill = NA, align = "right")) %>% 
  drop_na(roll_avg) #%>% 
#  mutate(station = "Bacon Island")


# combine data

all_14d <- rbind.data.frame(jer_14d, bet_14d, hol_14d, ob1_14d) %>% 
  mutate(value = roll_avg,
         avg_type = "14-day")

all_14d$station <- as.factor(all_14d$station)
all_14d$station <- ordered(all_14d$station, levels = c("Jersey Point", "Bethel Island", "Holland Cut", "Bacon Island"))

min_date <- min(all_14d$date)

all_daily <- rbind.data.frame(jer_raw, ob1_raw, bet_raw, hol_raw) %>% 
  filter(date >= min_date) %>% 
  mutate(roll_avg= NA) %>% 
  mutate(value = parameter_value,
         avg_type = "Daily")

all_ec <- rbind.data.frame(all_14d, all_daily)



# H-line table

lims <- data.frame(station= c("Jersey Point", "Bethel Island", "Holland Cut", "Bacon Island"), level= c(1800, 1000, 800, 700))

lims$station <- as.factor(lims$station)
lims$station <- ordered(lims$station, levels = c("Jersey Point", "Bethel Island", "Holland Cut", "Bacon Island"))




# graph it - just 14-day means

ec_plot <- ggplot(all_14d, aes(x= date, y= roll_avg))+
  geom_line(color= "steelblue")+
  ylab("14-day mean EC (µmhos/cm)")+
  xlab("Date")+
  geom_hline(
    data = lims,
    aes(yintercept = level),
    color = "black",
    linetype = "dashed")+
  facet_wrap(~station)+
  theme_bw()+
  theme(axis.text = element_text(size = 10),
        axis.text.x = element_text(angle = 60, vjust = 0.5),
        legend.position = "top",
        legend.box = "vertical",
        legend.title = element_blank(),
        legend.text = element_text(size = 9))

ggsave(ec_plot, file = 'outputs/ec_14_day.png', height = 6, width = 7.5)
  


# graph it - 14-day mean and daily

ec_all_plot <- ggplot(all_ec, aes(x= date, y= value, color = avg_type))+
  geom_line(size= 0.75)+
  ylab("EC mean (µmhos/cm)")+
  xlab("Date")+
  #viridis::scale_color_viridis(option = "viridis", discrete = TRUE) +
  scale_color_discrete(palette =c( "#40498EFF", "#38AAACFF"), name= "Mean type")+
  geom_hline(
    data = lims,
    aes(yintercept = level),
    color = "black",
    linetype = "dashed")+
  facet_wrap(~station)+
  theme_bw()+
  theme(axis.text = element_text(size = 10),
        axis.text.x = element_text(angle = 60, vjust = 0.5),
        legend.position = "top",
        legend.box = "vertical",
        #legend.title = element_text(),
        legend.text = element_text(size = 9))

ggsave(ec_all_plot, file = 'outputs/ec_all.png', height = 6, width = 7.5)

             
