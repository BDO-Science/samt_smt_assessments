library(tidyverse)
library(rvest)
library(janitor)
library(busdater)
library(here)
library(lubridate)

project <- here()
wy <- 2026
by <- wy-1
url <- 'https://filelib.wildlife.ca.gov/Public/salvage/Salmon%20Monitoring%20Team%20and%20Sturgeon/' #site with salvage files
season_start <- ymd(paste0(wy-1,'-10-01'))
season_end <- ymd(paste0(wy,'-06-30'))
jpe <- 1057452
livingston_jpe <- 130096
# regulatory thresholds
wr_loss_threshold <- 0.01
wr_hatch_loss_threshold <- 0.01
sh_hatch_loss_threshold <- 0.01
sr_surrogate_threshold <- 0.01

#########################################################
#pull in latest salmon and steelhead files from CDFW ftp
#########################################################

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

#########################################################
#pull in latest salmon and steelhead loss from SacPAS
#########################################################

loss_summary_url <- paste0('https://www.cbr.washington.edu/sacramento/workgroups/include_gen/WY',wy,'/Loss_WY_summary.html')
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

wr_hatch <- read_csv('https://www.cbr.washington.edu/sacramento/workgroups/include_gen/WY2026/cwt_winter_releases.csv') %>%
  clean_names() %>%
  mutate(wYear = get_fy(as.Date(release_start), opt_fy_start = '10-01')) %>%
  filter(wYear == wy) %>%
  mutate(loss = ifelse(is.na(loss), 0, loss))
# 
# liv_loss <- wr_hatch %>%
#   filter(grepl('livingston', hatchery, ignore.case = TRUE)) %>%
#   summarize(loss = sum(loss)) %>%
#   pull()

# batt_loss <- wr_hatch %>%
#   filter(grepl('coleman', hatchery, ignore.case = TRUE)) %>%
#   summarize(loss = sum(loss)) %>%
#   pull()

#hatchery winter-run
#just Livingston fish in Action 5
wr_hatch_loss <- loss_summary_table %>%
  select(lsnfh_hatchery_cwt_winter_run_chinook) %>%
  slice(3) %>%
  pull()
wr_hatch_perc <- loss_summary_table %>%
  select(lsnfh_hatchery_cwt_winter_run_chinook) %>%
  slice(4) %>%
  mutate(lsnfh_hatchery_cwt_winter_run_chinook = as.character(lsnfh_hatchery_cwt_winter_run_chinook)) %>%
  replace(is.na(.), "0.00%") %>%
  pull()
wr_hatch_7d <- loss_summary_table %>%
  select(lsnfh_hatchery_cwt_winter_run_chinook) %>%
  slice(1) %>%
  pull()

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

###########################################
#pull in migration timing table
###########################################

timing_url <- paste0('https://www.cbr.washington.edu/sacramento/workgroups/include_gen/WY',wy,'/samt_hrt.html')
timing <- read_html(timing_url) %>% 
  html_nodes("table") %>%
  html_table(fill = T) 
timing_table <- timing[[1]] %>%
  mutate(across(2:7, ~gsub("^(-?\\d+\\.?\\d*).*", "\\1", .))) %>%
  mutate(across(2:7, ~round(as.numeric(.), 0))) %>%
  mutate(across(2:7, ~if_else(is.na(.), NA_character_, paste0(., '%')))) %>%
  select(1,2,3,4,'Sac Trawl (Sherwood)' = 5, 'Chipps Island Trawl' = 6, 7)

wr_natural_timing <- timing_table %>%
  slice(1,4)

delta_entry_wr <- wr_natural_timing[1,4] %>%
  pull()
delta_exit_wr <- wr_natural_timing[1,6] %>%
  pull()
salvage_wr <- wr_natural_timing[2,7] %>%
  pull()

sr_natural_timing <- timing_table %>%
  slice(2)

delta_entry_sr <- sr_natural_timing[1,4] %>%
  pull()
delta_exit_sr <- sr_natural_timing[1,6] %>%
  pull()
salvage_sr <- sr_natural_timing[1,7] %>%
  pull()

sh_natural_timing <- timing_table %>%
  filter(grepl('steelhead', Species, ignore.case = TRUE)) %>%
  mutate(Species = 'Steelhead, Unclipped') %>%
  pivot_longer(names_to = 'measure',
               values_to = 'value', -1) %>%
  group_by(Species, measure) %>%
  summarize(value = max(value, na.rm = TRUE)) %>%
  pivot_wider(names_from = 'measure',
              values_from = 'value') %>%
  select(4,7,3,5,2,6)

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
####this is for summarizing data in text
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
perc_surv <- scales::ordinal(round(ecdf_surv(stars_table[2,5])*100,0))

ecdf_id_surv <- ecdf(stars_data$survival_interior_delta_est)
perc_id_surv <- scales::ordinal(round(ecdf_id_surv(stars_table[17,5])*100,0))

ecdf_id_route <- ecdf(stars_data$routing_probability_interior_delta_est)
perc_id_route <- scales::ordinal(round(ecdf_id_route(stars_table[16,5])*100,0))
#######################################
#hatchery steelhead jpe
#######################################

####pulling most up to date water year type
####based on 75% exceedance

url <- 'https://cdec.water.ca.gov/reportapp/javareports?name=WSI'

page <- read_html(url)

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
                          x75_perc <= 5.4 ~ 'C'),
         month = month(date),
         basin = 'sac')

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
                          x75_perc <= 2.1 ~ 'C'),
         month = month(date),
         basin = 'sj')
wy_types_all <- bind_rows(sac_table, sj_table)

###Steelhead JPE data calculation
surv <- read_csv(here(project, 'input_data/sh_hatchery_survival.csv'))

releases <- read_csv(here(project, 'input_data/wy_2026_sh_releases.csv')) %>%
  mutate(date = mdy(date),
         basin = if_else(hatchery == 'MKFH', 'sj', 'sac'),
         month = month(date)) %>%
  left_join(wy_types_all, by = c('month', 'basin')) %>%
  mutate(type = if_else(is.na(type), lead(type, 1), type)) %>%
  mutate(type = 'BN') %>%
  left_join(surv, by = c('hatchery', 'type' = 'wy_type')) %>%
  mutate(jpe = round(stocked * survival,0))

release_table <- releases %>%
  select(1:4, 10:11)

new_column_names <- c('Hatchery', 'Date of Release', 'Mean Fork Length (mm)', 'Number Released', 'Estimated Survival', 'Juvenile Production Estimate')

release_table_print <- release_table %>% 
  mutate(hatchery = factor(hatchery, levels = c('CNFH', 'NMFH', 'FRFH', 'MKFH'),
                           labels = c('Coleman', 'Nimbus', 'Feather River', 'Mokelumne River')),
         survival = paste0(round(survival * 100, 0),'%'),
         stocked = prettyNum(stocked, big.mark = ","),
         jpe = prettyNum(jpe, big.mark = ","))

colnames(release_table_print) <- new_column_names

###numbers for text
sh_stocked <- sum(release_table$stocked) %>% prettyNum(big.mark = ",")
sh_jpe <- sum(release_table$jpe) %>% prettyNum(big.mark = ",")
sh_survival <- paste0(round(mean(release_table$survival)*100,0),'%')
sh_clipped_threshold <- round(sum(release_table$jpe) * 0.01,0)
n_releases <- nrow(release_table)

sh_clipped_loss_total <- loss_summary_table %>%
  select(hatchery_steelhead) %>%
  slice(3) %>%
  pull()
sh_7d <- loss_summary_table %>%
  select(hatchery_steelhead) %>%
  slice(1) %>%
  pull()
sh_clipped_perc_threshold <- paste0(round((sh_clipped_loss_total/(sum(release_table$jpe) * .01))*100,2),'%')


###########################################
#pull in juvenile sampling data
###########################################

####pull in table from SaMT page
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

####pull in annual monitoring data
url_species_data = data.frame(species = rep(c('CHN', 'CHN', 'RBT'),5), run = rep(c('Winter', 'Spring', 'NA'),5)) %>%
  arrange(species, run)
url_sample_data <- data.frame(
  site_name = c('Chipps Island', 'Sac Seine', 'Sac Trawl', 'Knights Landing', 'Tisdale'),
  site_code = c('ASB018%3A1', 'Asacbeach%3A1', 'ASR055%3A1','AKNL%3A0', 'ATIS%3A0'), 
  sample = c('trawl', 'seine', 'trawl', 'trap', 'trap')
) %>%
  slice(rep(1:n(), 3))
url_data <- bind_cols(url_sample_data, url_species_data)

sample_list <- apply(url_data, 1, function(row){
  read_csv(paste0('https://www.cbr.washington.edu/sacramento/data/php/rpt/sampling_graph.php?sc=1&outputFormat=csv&year=',by,'&species=',row['species'],'%3A',row['run'],'&loc=',row['sample'],'%3',row['site_code'],'&typeData=raw')) %>%
    select(1, catch = 2) %>%
    mutate(site = row['site_name'],
           species = row['species'],
           run = row['run'])
})

all_sample <- bind_rows(sample_list) %>%
  mutate(Date = ymd(Date)) %>%
  filter(!is.na(Date)) %>%
  mutate(region = case_when(site %in% c('Knights Landing', 'Tisdale') ~ 'Delta Entry',
                            site %in% c('Sac Seine', 'Sac Trawl') ~ 'Delta',
                            site == 'Chipps Island' ~ 'Delta Exit')) %>%
  group_by(region, species, run) %>%
  summarize(catch = sum(catch, na.rm = TRUE),
            start_date = min(Date))
###########################################
#pull in RBDD stuff
###########################################
wr_rbdd <- read_csv(paste0('https://www.cbr.washington.edu/sacramento/data/php/rpt/redbluff_by.php?sc=1&outputFormat=csv&esttype=daily&year%5B%5D=',
                           by,'&species%5B%5D=Chinook%3AWinter&stage%5B%5D=Total')) %>%
  clean_names() %>%
  mutate(date = as.Date(date))

sr_rbdd <- read_csv(paste0('https://www.cbr.washington.edu/sacramento/data/php/rpt/redbluff_by.php?sc=1&outputFormat=csv&esttype=daily&year%5B%5D=',
                           by,'&species%5B%5D=Chinook%3ASpring&stage%5B%5D=Total')) %>%
  clean_names() %>%
  mutate(date = as.Date(date))

wr_passage <- round(sum(wr_rbdd$passage_estimate, na.rm = TRUE)/1000000,2)
wr_rbdd_date <- max(wr_rbdd$date, na.rm = TRUE)
sr_passage <- round(sum(sr_rbdd$passage_estimate, na.rm = TRUE)/1000000,2)
sr_rbdd_date <- max(sr_rbdd$date, na.rm = TRUE)

###########################################
#spring-run hatchery surrogates
###########################################
sr_url <- paste0('https://www.cbr.washington.edu/sacramento/workgroups/include_gen/WY',wy,'/cwt_spring_surrogates.html')

sr_surrogate <- read_html(sr_url) %>% 
  html_nodes("table") %>%
  html_table(fill = T, header = TRUE) 

sr_surrogate_table <- sr_surrogate[[1]] %>%
  select(1:5,7,10,11) %>%
  mutate(`# of CWT Fish Released` = as.numeric(gsub(",", "", `# of CWT Fish Released`))) %>%
  mutate('Loss Threshold (1% of CWT Released)' = `# of CWT Fish Released` * sr_surrogate_threshold) %>%
  select(1:6,9,7,8)

sr_surrogate_table_clean <- sr_surrogate_table %>%
  mutate('Loss (% of threshold)' = paste0(`Confirmed Loss`,' (', `% Loss of CWT Number Released`,')')) %>%
  select(-8,-9)

yearling <- sr_surrogate_table %>%
  filter(Type == 'Yearling') %>%
  summarize(sum(`# of CWT Fish Released`)) %>%
  pull() %>%
  prettyNum(big.mark = ",")

yoy <- sr_surrogate_table %>%
  filter(Type == 'Young-of-year') %>%
  summarize(sum(`# of CWT Fish Released`)) %>%
  pull() %>%
  prettyNum(big.mark = ",")

###########################################
#EXECUTIVE SUMMARY LOGIC
#############################################
# Helper function to safely get numeric values without crashing
safe_parse <- function(var_name) {
  if (exists(var_name, where = .GlobalEnv) && !is.na(get(var_name))) {
    val <- get(var_name)
    if(is.numeric(val)) return(val)
    return(parse_number(as.character(val)))
  }
  return(0)
}

# --- 1. Action 5 / Entrainment Management Status ---
current_date <- Sys.Date()
is_season_date_range <- month(current_date) %in% c(1, 2, 3, 4, 5, 6)
pct_wr_in_delta <- safe_parse("delta_entry_wr")
pct_sh_in_delta <- safe_parse("delta_entry_sh")

# --- 2. Loss Status (From Loss Summary Table) ---
get_loss_val <- function(pattern) {
  if(!exists("loss_summary_table")) return(0)
  loss_summary_table %>%
    filter(grepl("Cumulative Loss Total", data_item, ignore.case = TRUE)) %>%
    select(matches(pattern)) %>%
    pull() %>%
    as.character() %>%
    parse_number() %>%
    sum(na.rm = TRUE)
}

loss_dna_wr    <- get_loss_val("dna_winter_run_chinook")
loss_lad_wr    <- get_loss_val("lad_winter_run_chinook")
loss_hatch_wr  <- get_loss_val("lsnfh_hatchery_cwt_winter_run_chinook")
loss_nat_sh    <- get_loss_val("natural_steelhead")
loss_hatch_sh  <- get_loss_val("hatchery_steelhead")

total_loss <- sum(loss_dna_wr, loss_lad_wr, loss_hatch_wr, loss_nat_sh, loss_hatch_sh, na.rm = TRUE)

# --- 3. Presence Logic Helper Function (UPDATED) ---
get_presence_status <- function(species_name, entry_pct, exit_pct, catch_keyword) {
  real_catch <- 0
  if(exists("all_sampling")) {
    real_catch <- all_sampling %>% 
      filter(Location %in% c('Sac Trawl (Sherwood)', 'Sac Seine')) %>% 
      select(contains(catch_keyword)) %>% 
      as.matrix() %>% sum(na.rm = TRUE)
  }
  
  case_when(
    # 1. Emigration Complete (>95% Exited)
    exit_pct >= 95 ~ paste0(species_name, " presence in the Delta is **low** (emigration nearly complete)."),
    
    # 2. Peak Presence (25% - 75% Entry)
    entry_pct >= 25 & entry_pct <= 75 ~ paste0(species_name, " presence in the Delta is **high** (historical peak)."),
    
    # 3. Decreasing Presence (>75% Entry, but still in system)
    entry_pct > 75 & exit_pct < 95 ~ paste0(species_name, " presence in the Delta is **decreasing** (winding down)."),
    
    # 4. Increasing Presence (5% - 25% Entry)
    entry_pct >= 5 & entry_pct < 25 ~ paste0(species_name, " presence in the Delta is **increasing**."),
    
    # 5. Early Detection (Low Hist < 5%, but Real Fish Caught)
    entry_pct < 5 & real_catch > 0 ~ paste0(species_name, " presence in the Delta is **low**, but fish have been **detected** (", real_catch, " captured in Delta monitoring)."),
    
    # 6. Low/Inactive
    TRUE ~ paste0(species_name, " presence in the Delta is **low** (immigration has not peaked).")
  )
}

