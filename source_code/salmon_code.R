library(tidyverse)
library(rvest)
library(janitor)
library(busdater)

wy <- 2026
url <- 'https://filelib.wildlife.ca.gov/Public/salvage/Salmon%20Monitoring%20Team%20and%20Sturgeon/' #site with salvage files
season_start <- ymd(paste0(wy-1,'-10-01'))
season_end <- ymd(paste0(wy,'-06-30'))
jpe <- NA
livingston_jpe <- NA
battle_jpe <- NA
##########################################
#pull in latest salmon and steelhead files
##########################################

# ###CDFW method
# #isolating csv files on CDFW ftp site
# url_root <- 'https://filelib.wildlife.ca.gov'
# page <- read_html(url) #read url in
# links <- page %>% html_nodes("a") %>% html_attr("href") # Extract all links from the webpage
# 
# salmon_links <- grep(paste0("Salmon_",wy), links, value = TRUE, ignore.case = TRUE)
# salmon_file <- max(grep("\\.csv$", salmon_links, value = TRUE, ignore.case = TRUE))
# 
# steelhead_links <- grep(paste0('Steelhead_Salvage_Summary_', wy), links, value = TRUE, ignore.case = TRUE)
# steelhead_file <- max(grep("\\.csv$", steelhead_links, value = TRUE, ignore.case = TRUE))
# 
# #reading csv files
# salmon <- tryCatch(
#   read_csv(paste0(url_root, salmon_file)),
#   error = function(e) NULL
# )
# steelhead <- tryCatch(read_csv(paste0(url_root, steelhead_file)),
#                       error = function(e) NULL
#                       )
###SacPAS urls
# Read once and store
# salmon_raw <- read_csv(paste0('https://www.cbr.washington.edu/sacramento/data/php/rpt/juv_loss_detail.php?sc=1&outputFormat=csv&year=',
#                               wy, '&species=1%3Aall&dnaOnly=no&age=no')) %>%
#   clean_names()
# 
# # Check and assign
# salmon <- if(ncol(salmon_raw) == 1) {
#   NULL
# } else {
#   salmon_raw
# }
# 
# steelhead_raw <- read_csv(paste0('https://www.cbr.washington.edu/sacramento/data/php/rpt/juv_loss_detail.php?sc=1&outputFormat=csv&year='
#                                  ,wy,'&species=2%3Aall&dnaOnly=no&age=no')) %>%
#   clean_names()
# steelhead <- if(ncol(steelhead_raw) == 1) {
#   NULL
# } else {
#   steelhead_raw
# }

###SacPAS table
loss_summary_url <- paste0('https://www.cbr.washington.edu/sacramento/workgroups/include_gen/WY',wy,
                           '/LossUnclip_WY_summary.html')
loss_summary <- read_html(loss_summary_url) %>% 
  html_nodes("table") %>%
  html_table(fill = T) 
loss_summary_table <- loss_summary[[1]] %>%
  clean_names()
###########################################
#summarize data for report and graphs
###########################################
dates <- data.frame(Date = seq(season_start, season_end, by = '1 days'))

#natural winter-run
wr_loss <- loss_summary_table %>%
  select(dna_winter_run_chinook) %>%
  slice(3) %>%
  pull()
wr_perc <- loss_summary_table %>%
  select(dna_winter_run_chinook) %>%
  slice(4) %>%
  pull()
wr_7d <- loss_summary_table %>%
  select(dna_winter_run_chinook) %>%
  slice(1) %>%
  pull()
#hatchery winter-run
# wr_hatch_loss <- if(is.null(salmon)) {
#   print(0)
# } else {
#   salmon %>% filter(cwt_race == 'Winter') %>%
#     summarize(loss = sum(loss, na.rm = TRUE)) %>%
#     pull()
# }


#hatchery steelhead
wr_hatch <- read_csv('https://www.cbr.washington.edu/sacramento/workgroups/include_gen/WY2026/cwt_winter_releases.csv') %>%
  clean_names() %>%
  mutate(wYear = get_fy(as.Date(release_start), opt_fy_start = '10-01')) %>%
  filter(wYear == wy) %>%
  mutate(loss = ifelse(is.na(loss), 0, loss))

liv_loss <- wr_hatch %>%
  filter(grepl('livingston', hatchery, ignore.case = TRUE)) %>%
  summarize(loss = sum(loss)) %>%
  pull()

batt_loss <- wr_hatch %>%
  filter(grepl('coleman', hatchery, ignore.case = TRUE)) %>%
  summarize(loss = sum(loss)) %>%
  pull()

liv_perc <- if(is.na(liv_loss/(livingston_jpe * 0.0017))) {
  print('0%')
} else {
  paste0(round((liv_loss/(livingston_jpe*0.0017))*100,2),'%')
}

batt_perc <- if(is.na(batt_loss/(battle_jpe * 0.0017))) {
  print('0%')
} else {
  paste0(round((batt_loss/(battle_jpe*0.0017))*100,2),'%')
}

#natural steelhead
sh_loss <- loss_summary_table %>%
  select(natural_steelhead) %>%
  slice(3) %>%
  pull()
sh_perc <- loss_summary_table %>%
  select(natural_steelhead) %>%
  slice(4) %>%
  pull()
sh_7d <- loss_summary_table %>%
  select(natural_steelhead) %>%
  slice(1) %>%
  pull()

###########################################
#pull in migration timing table
###########################################

timing_url <- paste0('https://www.cbr.washington.edu/sacramento/workgroups/include_gen/WY',wy,'/samt_hrt.html')
timing <- read_html(timing_url) %>% 
  html_nodes("table") %>%
  html_table(fill = T) 
timing_table <- timing[[1]] %>%
  mutate(across(2:7, ~gsub("^(-?\\d+\\.?\\d*%).*", "\\1", .))) %>%
  select(1,2,3,4,'Sac Trawl (Sherwood)' = 5, 'Chipps Island Trawl' = 6, 7)

wr_natural_timing <- timing_table %>%
  slice(1,4)

delta_entry_wr <- wr_natural_timing[1,4] %>%
  pull()
delta_exit_wr <- wr_natural_timing[1,6] %>%
  pull()
salvage_wr <- wr_natural_timing[2,7] %>%
  pull()

sh_natural_timing <- timing_table %>%
  filter(grepl('steelhead', Species, ignore.case = TRUE)) %>%
  mutate(Species = 'Steelhead, Unclipped') %>%
  pivot_longer(names_to = 'measure',
               values_to = 'value', -1) %>%
  group_by(Species, measure) %>%
  summarize(value = max(value)) %>%
  pivot_wider(names_from = 'measure',
              values_from = 'value') %>%
  select(4,7,3,5,2,6) %>%
  mutate_all(na_if,"")

delta_entry_sh <- sh_natural_timing[1,4] %>%
  pull()
delta_exit_sh <- sh_natural_timing[1,6] %>%
  pull()
salvage_sh <- sh_natural_timing[1,7] %>%
  pull()


#######################################
#STARs table
#######################################

stars_url <- paste0('https://www.cbr.washington.edu/sacramento/workgroups/include_gen/WY',wy,'/samt_stars.html')

stars <- read_html(stars_url) %>% 
  html_nodes("table") %>%
  html_table(fill = T) 
stars_table <- stars[[1]] %>%
  filter(Stock == 'Winter Chinook')
stars_date <- min(stars_table$Date)
overall_survival <- paste0(round(stars_table[2,5],2)," (",
                           round(stars_table[2,6],2),"-",
                           round(stars_table[2,7],2),")")
id_survival <- paste0(round(stars_table[17,5],2)," (",
                      round(stars_table[17,6],2),"-",
                      round(stars_table[17,7],2),")")
id_routing <- paste0(round(stars_table[16,5],2)," (",
                     round(stars_table[16,6],2),"-",
                     round(stars_table[16,7],2),")")
