library(tidyverse)
library(rvest)
library(janitor)
library(busdater)
library(here)

project <- here()
wy <- 2026
url <- 'https://filelib.wildlife.ca.gov/Public/salvage/Salmon%20Monitoring%20Team%20and%20Sturgeon/' #site with salvage files
season_start <- ymd(paste0(wy-1,'-10-01'))
season_end <- ymd(paste0(wy,'-06-30'))
jpe <- NA
livingston_jpe <- NA
battle_jpe <- NA
wr_loss_threshold <- 0.01
wr_hatch_loss_threshold <- 0.01
sh_hatch_loss_threshold <- 0.01
sr_surrogate_threshold <- 0.01

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
  mutate(dna_winter_run_chinook = as.character(dna_winter_run_chinook)) %>%
  replace(is.na(.), "0.00%") %>%
  pull()
wr_7d <- loss_summary_table %>%
  select(dna_winter_run_chinook) %>%
  slice(1) %>%
  pull()
#hatchery winter-run

wr_hatch <- read_csv('https://www.cbr.washington.edu/sacramento/workgroups/include_gen/WY2026/cwt_winter_releases.csv') %>%
  clean_names() %>%
  mutate(wYear = get_fy(as.Date(release_start), opt_fy_start = '10-01')) %>%
  filter(wYear == wy) %>%
  mutate(loss = ifelse(is.na(loss), 0, loss))

liv_loss <- wr_hatch %>%
  filter(grepl('livingston', hatchery, ignore.case = TRUE)) %>%
  summarize(loss = sum(loss)) %>%
  pull()

# batt_loss <- wr_hatch %>%
#   filter(grepl('coleman', hatchery, ignore.case = TRUE)) %>%
#   summarize(loss = sum(loss)) %>%
#   pull()

liv_perc <- if(is.na(liv_loss/(livingston_jpe * wr_hatch_loss_threshold))) {
  print('0%')
} else {
  paste0(round((liv_loss/(livingston_jpe*wr_hatch_loss_threshold))*100,2),'%')
}

# batt_perc <- if(is.na(batt_loss/(battle_jpe * 0.0017))) {
#   print('0%')
# } else {
#   paste0(round((batt_loss/(battle_jpe*0.0017))*100,2),'%')
# }

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

#hatchery steelhead
###########################################
#pull in juvenile sampling table
###########################################

juv_url <- paste0('https://www.cbr.washington.edu/sacramento/workgroups/include_gen/WY',wy,'/samt_juvfish.html')
juv <- read_html(juv_url) %>% 
  html_nodes("table") %>%
  html_table(fill = T, header = TRUE) 

juv_table <- juv[[1]] %>%
  slice(-1)

samples <- colnames(juv_table[c(-1:-3,-6)])  


sample_list <- lapply(samples, function(sample){
  date <- juv_table %>% 
    select(2, all_of(sample)) %>%
    filter(`Data Item` %in% c('Min Sample Date', 'Max Sample Date')) %>%
    mutate(date = ymd(.data[[sample]])) %>%  # Reference the column by the name in sample
    group_by(`Data Item`) %>%  # Fixed parenthesis
    summarize(min_date = min(date, na.rm = TRUE),
              max_date = max(date, na.rm = TRUE))
  
  max_date <- date[1, 3]
  min_date <- date[2, 2]
  
  fish <- juv_table %>% 
    select(2, all_of(sample)) %>%
    filter(grepl('chinook|steelhead', `Data Item`, ignore.case = TRUE)) %>%
    mutate(catch = as.numeric(.data[[sample]])) %>%  # Reference the column by the name in sample
    group_by(`Data Item`) %>%  # Fixed parenthesis
    summarize(catch = sum(catch, na.rm = TRUE)) %>%
    replace(is.na(.), 0) %>%
    t()
  
  colnames(fish) <- fish[1, ]
  fish <- fish[-1, , drop = FALSE]
  fish <- as.data.frame(fish)
  
  all <- bind_cols(min_date, max_date, fish) %>%
    mutate(Location = sample) %>%
    select(10, 'Date Start' = 1, 'Date End' = 2, 9, 6, 8, 4, 5, 7, 3)
  return(all)
})

all_sampling <- bind_rows(sample_list) %>%
  mutate(`Date Start` = if_else(is.infinite(`Date Start`), NA, `Date Start`),
         `Date End` = if_else(is.infinite(`Date End`), NA, `Date End`),
         mutate(across(4:10, as.numeric)))

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

####pulling most recent STARs data
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

####summarizing old data for percentiles
stars_files <- list.files(here(project, "input_data/historic_stars"), 
                          full.names = TRUE, pattern = '.csv')

stars_list <- lapply(stars_files, read_csv)

stars_data <- bind_rows(stars_list) %>%
  select(1,2,17,32) %>%
  mutate(month = month(Date)) %>%
  filter(month == month(Sys.Date())) %>%
  clean_names()

ecdf_surv <- ecdf(stars_data$survival_overall_est)
perc_surv <- round(ecdf_surv(stars_table[2,5])*100,0)

ecdf_id_surv <- ecdf(stars_data$survival_interior_delta_est)
perc_id_surv <- round(ecdf_id_surv(stars_table[17,5])*100,0)

ecdf_id_route <- ecdf(stars_data$routing_probability_interior_delta_est)
perc_id_route <- round(ecdf_id_route(stars_table[16,5])*100,0)
#######################################
#hatchery steelhead jpe
#######################################

####pulling most up to date water year type
####based on 75% exceedance

url <- 'https://cdec.water.ca.gov/reportapp/javareports?name=WSI'

page <- read_html(url)

# The data is likely in a <pre> tag - extract it
pre_text <- page %>%
  html_element("pre") %>%
  html_text()

lines <- str_split(pre_text, "\n")[[1]]

sac_valley_start <- which(str_detect(lines, "SACRAMENTO VALLEY WATER"))[1]
sac_valley_lines <- lines[sac_valley_start:(sac_valley_start + 9)]
sac_valley_data <- sac_valley_lines[str_detect(sac_valley_lines, "Dec|Jan|Feb|Mar|Apr|May")]
sac_table <- read.table(text = sac_valley_data,
                        fill = TRUE,
                        col.names = c('Mon', 'Day', 'Year', '99_perc', '90_perc', 
                                      '75_perc', '50_perc', '25_perc', '10_perc')) %>%
  clean_names() %>%
  mutate(date = mdy(paste0(mon,day,year))) %>%
  select(10,6) %>%
  mutate(type = case_when(x75_perc >= 9.2 ~ 'W',
                          x75_perc > 7.8 ~ 'AN',
                          x75_perc > 6.5 ~ 'BN',
                          x75_perc > 5.4 ~ 'D',
                          x75_perc <= 5.4 ~ 'C'))
sac_wy_type <- sac_table %>%
  slice_tail(n = 1) %>%
  pull(type)

sj_valley_start <- which(str_detect(lines, "SAN JOAQUIN VALLEY WATER"))[1]
sj_valley_lines <- lines[sj_valley_start:(sj_valley_start + 9)]
sj_valley_data <- sj_valley_lines[str_detect(sj_valley_lines, "Dec|Jan|Feb|Mar|Apr|May")]
sj_table <- read.table(text = sj_valley_data,
                       fill = TRUE,
                       col.names = c('Mon', 'Day', 'Year', '99_perc', '90_perc', 
                                     '75_perc', '50_perc', '25_perc', '10_perc')) %>%
  clean_names() %>%
  mutate(date = mdy(paste0(mon,day,year))) %>%
  select(10,6) %>%
  mutate(type = case_when(x75_perc >= 3.8 ~ 'W',
                          x75_perc > 3.1 ~ 'AN',
                          x75_perc > 2.5 ~ 'BN',
                          x75_perc > 2.1 ~ 'D',
                          x75_perc <= 2.1 ~ 'C'))
sj_wy_type <- sj_table %>%
  slice_tail(n = 1) %>%
  pull(type)
