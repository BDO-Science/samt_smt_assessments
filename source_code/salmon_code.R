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

# OPERATIONAL LOSS THRESHOLDS (trigger management actions)
wr_loss_threshold <- 0.01        # 1% of JPE for natural winter-run
wr_hatch_loss_threshold <- 0.01  # 1% of JPE for hatchery winter-run
sh_hatch_loss_threshold <- 0.01  # 1% of JPE for hatchery steelhead
sr_surrogate_threshold <- 0.01   # 1% of JPE for spring-run surrogates

# INCIDENTAL TAKE LIMITS (BiOp Table 184) - for reporting/compliance
# These are the maximum anticipated annual amount and extent of take
itl_wr_natural_single <- 0.0056      # 0.56% of JPE single year
itl_wr_natural_3yr <- 0.0036         # 0.36% of JPE 3-year rolling
itl_wr_hatch_single <- 0.01          # 1.0% of JPE single year
itl_wr_hatch_3yr <- 0.008            # 0.8% of JPE 3-year rolling
itl_sr_surrogate <- 0.005            # 0.5% of each surrogate release group
itl_sh_natural_single <- 5294        # 5,294 juveniles single year
itl_sh_natural_3yr <- 2319           # 2,319 juveniles 3-year rolling

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

# Delta entry based on Sac Trawl (Sherwood) - column 5
# Delta exit based on Chipps Island Trawl - column 6
delta_entry_wr <- wr_natural_timing[1,5] %>%
  pull()
delta_exit_wr <- wr_natural_timing[1,6] %>%
  pull()
salvage_wr <- wr_natural_timing[2,7] %>%
  pull()

sr_natural_timing <- timing_table %>%
  slice(2)

# Delta entry based on Sac Trawl (Sherwood) - column 5
# Delta exit based on Chipps Island Trawl - column 6
delta_entry_sr <- sr_natural_timing[1,5] %>%
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
  summarize(value = max(value, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = 'measure',
              values_from = 'value')

# Delta entry based on Sac Trawl (Sherwood)
# Delta exit based on Chipps Island Trawl
delta_entry_sh <- sh_natural_timing %>%
  pull(`Sac Trawl (Sherwood)`)
delta_exit_sh <- sh_natural_timing %>%
  pull(`Chipps Island Trawl`)
salvage_sh <- if("Salvage" %in% names(sh_natural_timing)) {
  sh_natural_timing %>% pull(Salvage)
} else {
  sh_natural_timing %>% pull(7)
}


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

# Try to fetch from SacPAS HTML table first
sh_jpe_url <- paste0('https://www.cbr.washington.edu/sacramento/workgroups/include_gen/WY',wy,'/hatch_stlhd_jpe.html')

steelhead_jpe_data <- tryCatch({
  message("Attempting to fetch steelhead JPE from SacPAS...")
  sh_page <- read_html(sh_jpe_url)
  sh_tables <- sh_page %>% html_table(fill = TRUE)
  
  if (length(sh_tables) > 0) {
    message("Found ", length(sh_tables), " table(s) on steelhead JPE page")
    
    # Get the main table and clean it
    sh_table <- sh_tables[[1]] %>%
      clean_names()
    
    message("Steelhead JPE table has ", nrow(sh_table), " rows and columns: ",
            paste(names(sh_table), collapse = ", "))
    
    # Process the table - column names from SacPAS:
    # start_date, end_date, hatchery, mark_type, marked, total_released, release_type,
    # wsi_basin, wsi_prob_exceedance_percent, wsi_rel_month_forecast, 
    # wsi_water_year_type, wy_type_applied_survival_estimate, 
    # juvenile_production_estimate, surv_est_range_source
    sh_processed <- sh_table %>%
      filter(!is.na(hatchery), hatchery != "", hatchery != "Hatchery") %>%
      mutate(
        hatchery = as.character(hatchery),
        date = as.character(start_date),
        stocked = as.numeric(gsub(",", "", as.character(total_released))),
        survival = as.numeric(wy_type_applied_survival_estimate),
        jpe = as.numeric(gsub(",", "", as.character(juvenile_production_estimate)))
      ) %>%
      select(hatchery, date, stocked, survival, jpe) %>%
      filter(!is.na(stocked), stocked > 0)
    
    message("Successfully processed ", nrow(sh_processed), " steelhead releases from SacPAS")
    
    # Return the data
    list(
      data = sh_processed,
      source = "SacPAS HTML"
    )
  } else {
    message("No tables found on steelhead JPE page, falling back to CSV")
    NULL
  }
}, error = function(e) {
  message("Failed to fetch steelhead JPE from SacPAS: ", e$message)
  message("Falling back to manual CSV files")
  NULL
})

# If SacPAS fetch succeeded, use that data; otherwise fall back to CSV
if (!is.null(steelhead_jpe_data)) {
  releases <- steelhead_jpe_data$data
  message("Using steelhead JPE data from ", steelhead_jpe_data$source)
} else {
  # Fall back to original CSV-based approach
  message("Using manual CSV files for steelhead JPE calculation")
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
}

release_table <- releases %>%
  select(hatchery, date, stocked, survival, jpe)

new_column_names <- c('Hatchery', 'Date of Release', 'Number Released', 'Estimated Survival', 'Juvenile Production Estimate')

release_table_print <- release_table %>% 
  mutate(
    hatchery = case_when(
      grepl("CNFH|Coleman", hatchery, ignore.case = TRUE) ~ "Coleman",
      grepl("NMFH|Nimbus", hatchery, ignore.case = TRUE) ~ "Nimbus",
      grepl("FRFH|Feather", hatchery, ignore.case = TRUE) ~ "Feather River",
      grepl("MKFH|Mokelumne", hatchery, ignore.case = TRUE) ~ "Mokelumne River",
      TRUE ~ hatchery
    ),
    survival = paste0(round(survival * 100, 0),'%'),
    stocked = prettyNum(stocked, big.mark = ","),
    jpe = prettyNum(jpe, big.mark = ",")
  )

colnames(release_table_print) <- new_column_names

###numbers for text
sh_stocked <- sum(releases$stocked, na.rm = TRUE) %>% prettyNum(big.mark = ",")
sh_jpe <- sum(releases$jpe, na.rm = TRUE) %>% prettyNum(big.mark = ",")
sh_survival <- paste0(round(mean(releases$survival, na.rm = TRUE)*100,0),'%')
sh_clipped_threshold <- round(sum(releases$jpe, na.rm = TRUE) * 0.01,0)
n_releases <- nrow(releases)

sh_clipped_loss_total <- loss_summary_table %>%
  select(hatchery_steelhead) %>%
  slice(3) %>%
  pull()

# Convert to numeric safely
sh_clipped_loss_total <- as.numeric(gsub(",", "", as.character(sh_clipped_loss_total)))
if (is.na(sh_clipped_loss_total)) sh_clipped_loss_total <- 0

sh_7d <- loss_summary_table %>%
  select(hatchery_steelhead) %>%
  slice(1) %>%
  pull()

# Calculate percentage of threshold
sh_jpe_total <- sum(releases$jpe, na.rm = TRUE)
if (sh_jpe_total > 0 && sh_clipped_loss_total >= 0) {
  sh_clipped_perc_threshold <- paste0(round((sh_clipped_loss_total / (sh_jpe_total * 0.01)) * 100, 2), '%')
} else {
  sh_clipped_perc_threshold <- "0.00%"
}


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

###########################################
#pull in annual monitoring data
###########################################
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
  tryCatch({
    read_csv(paste0('https://www.cbr.washington.edu/sacramento/data/php/rpt/sampling_graph.php?sc=1&outputFormat=csv&year=',by,'&species=',row['species'],'%3A',row['run'],'&loc=',row['sample'],'%3',row['site_code'],'&typeData=raw'), 
             show_col_types = FALSE) %>%
      select(1, catch = 2) %>%
      mutate(site = row['site_name'],
             species = row['species'],
             run = row['run'])
  }, error = function(e) {
    message("Failed to fetch monitoring data for ", row['site_name'], " - ", row['run'], ": ", e$message)
    # FIX: Return empty data frame with ALL columns set to length 0
    data.frame(
      Date = character(),
      catch = numeric(),
      site = character(),     # Changed from row['site_name'] to character()
      species = character(),  # Changed from row['species'] to character()
      run = character(),      # Changed from row['run'] to character()
      stringsAsFactors = FALSE
    )
  })
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
# spring-run surrogate releases (PRODUCTION ONLY per regulations)
###########################################

# Per regulations:
# - Yearling surrogates = Coleman Late-Fall PRODUCTION releases
# - Young-of-year surrogates = Feather River Spring-Run PRODUCTION releases
# Pull from SacPAS CWT tables (not experimental releases)

# 2. Fetch Spring-Run Surrogates from SacPAS Spring Surrogate Page
# Per regulations: ALL Coleman Late-Fall releases (both production and experimental)
sr_surrogate_url <- paste0('https://www.cbr.washington.edu/sacramento/workgroups/include_gen/WY',wy,'/cwt_spring_surrogates.html')

# Initialize with empty data frame
lfr_data_clean <- data.frame(
  `Tag Code`=character(), 
  `Hatchery`=character(), 
  `Stock`=character(), 
  `Release Date`=character(), 
  `Type`=character(), 
  `# of CWT Fish Released`=numeric(), 
  `Confirmed Loss`=numeric(), 
  check.names=FALSE
)

# Try to fetch surrogate data
lfr_data_clean <- tryCatch({
  sr_page <- read_html(sr_surrogate_url)
  tables <- sr_page %>% html_table(fill = TRUE)
  
  message("Found ", length(tables), " tables on spring surrogate page")
  
  if (length(tables) == 0) {
    message("WARNING: No tables found on spring surrogate page")
    return(lfr_data_clean)
  }
  
  # Get the main table
  sr_table <- tables[[1]] %>%
    clean_names()
  
  message("Spring surrogate table has ", nrow(sr_table), " rows and columns: ", 
          paste(names(sr_table), collapse = ", "))
  
  # Take ALL releases (both Production and Experimental)
  sr_all <- sr_table %>%
    filter(!is.na(number_of_cwt_fish_released), number_of_cwt_fish_released != "")
  
  message("Found ", nrow(sr_all), " total releases (production + experimental)")
  
  if (nrow(sr_all) == 0) {
    message("WARNING: No releases found")
    return(lfr_data_clean)
  }
  
  # Process all releases
  sr_processed <- sr_all %>%
    mutate(
      release_date = release_start,
      tag_codes_str = tag_codes,
      num_released = as.numeric(gsub(",", "", as.character(number_of_cwt_fish_released))),
      loss = as.numeric(gsub(",", "", as.character(confirmed_loss))),
      hatch = hatchery,
      rel_type = release_type
    ) %>%
    select(hatch, release_date, rel_type, tag_codes_str, num_released, loss) %>%
    filter(!is.na(num_released), num_released > 0)
  
  # Split multiple CWT tags and divide releases/loss proportionally
  sr_final <- sr_processed %>%
    rowwise() %>%
    mutate(
      n_tags = if_else(is.na(tag_codes_str) | tag_codes_str == "", 
                       0L, 
                       as.integer(length(strsplit(gsub(" ", "", tag_codes_str), ",")[[1]])))
    ) %>%
    ungroup() %>%
    filter(n_tags > 0) %>%
    separate_rows(tag_codes_str, sep = ",\\s*") %>%
    group_by(hatch, release_date, rel_type, num_released, loss, n_tags) %>%
    mutate(
      num_per_tag = num_released / n_tags,
      loss_per_tag = loss / n_tags
    ) %>%
    ungroup() %>%
    mutate(
      `Tag Code` = tag_codes_str,
      `Hatchery` = hatch,
      `Stock` = "Late-Fall",
      `Release Date` = release_date,
      `Type` = if_else(grepl("Experimental", rel_type, ignore.case = TRUE), "Experimental", "Production"),
      `# of CWT Fish Released` = num_per_tag,
      `Confirmed Loss` = loss_per_tag
    ) %>%
    select(`Tag Code`, `Hatchery`, `Stock`, `Release Date`, `Type`, 
           `# of CWT Fish Released`, `Confirmed Loss`) %>%
    distinct()
  
  message("Successfully processed ", nrow(sr_final), " CWT groups (production + experimental)")
  
  sr_final
  
}, error = function(e) {
  message("ERROR fetching surrogate data: ", e$message)
  message("Traceback: ", paste(capture.output(traceback()), collapse = "\n"))
  lfr_data_clean
})
# 3. Process Spring-Run Surrogate Releases (PRODUCTION ONLY)
# All data comes from SacPAS CWT tables - includes both Coleman Late-Fall and Feather River Spring-Run PRODUCTION

sr_all_releases <- lfr_data_clean %>%
  # Remove any malformed rows
  filter(
    !is.na(`# of CWT Fish Released`),
    `# of CWT Fish Released` > 0,
    !is.na(`Tag Code`),
    `Tag Code` != ""
  ) %>%
  # Set NA loss values to 0 for calculations
  mutate(`Confirmed Loss` = if_else(is.na(`Confirmed Loss`), 0, `Confirmed Loss`))

# 4. Calculate JPE (Juvenile Production Estimate) for Spring-Run Surrogates
# JPE = Number Released × Survival Rate
# Per regulations: "The JPE shall be determined by the historical average survival"

# Survival lookup table - ONLY values from sr_hatchery_survival.csv
surv_lookup <- tibble(
  Hatchery     = c(rep("Feather River Hatchery", 6), rep("Coleman NFH", 4)),
  run          = c(rep("Spring", 6), rep("Late-Fall", 4)),
  WY           = c(2025, 2024, 2023, 2021, 2020, 2019, 2026, 2021, 2020, 2019),
  survival_est = c(40.5, 30.8, 40.6, 49.4, 26.8, 28.6, 11.5, 14.3, 60.4, 23.0)
)

# Calculate historical average survival rates
# Use ALL years including current year (2026) per scenario 1
cnfh_survival <- surv_lookup %>%
  filter(Hatchery == "Coleman NFH") %>%
  summarise(mean_survival = mean(survival_est, na.rm = TRUE)) %>%
  pull(mean_survival)

# Feather River - use historical average (all years available)
frfh_survival <- surv_lookup %>%
  filter(Hatchery == "Feather River Hatchery") %>%
  summarise(mean_survival = mean(survival_est, na.rm = TRUE)) %>%
  pull(mean_survival)

# Convert to proportions (from percentages)
cnfh_survival_prop <- cnfh_survival / 100
frfh_survival_prop <- frfh_survival / 100

message("Using survival rates: Coleman Late-Fall = ", round(cnfh_survival, 1), 
        "% (all years 2019-2021, 2026 from sr_hatchery_survival.csv), Feather River Spring = ", 
        round(frfh_survival, 1), "% (all years from sr_hatchery_survival.csv)")

# Apply survival rates to surrogate releases
sr_all_releases_jpe <- sr_all_releases %>%
  mutate(
    survival = case_when(
      grepl("Coleman", Hatchery, ignore.case = TRUE) ~ cnfh_survival_prop,
      grepl("Feather", Hatchery, ignore.case = TRUE) ~ frfh_survival_prop,
      TRUE ~ 0.35  # Default fallback
    ),
    jpe = round(`# of CWT Fish Released` * survival, 0)
  )

# 5. Calculate Combined Metrics using JPE
total_sr_released <- sum(sr_all_releases$`# of CWT Fish Released`, na.rm = TRUE)
total_sr_jpe <- sum(sr_all_releases_jpe$jpe, na.rm = TRUE)
sr_threshold_val <- total_sr_jpe * 0.01  # 1% of JPE (operational threshold)
sr_loss_total <- sum(sr_all_releases$`Confirmed Loss`, na.rm = TRUE)

# Calculate ITL comparison (0.5% per BiOp Table 184)
sr_itl_val <- total_sr_jpe * 0.005  # 0.5% ITL per BiOp Table 184

sr_loss_perc <- if(sr_threshold_val > 0) {
  paste0(sprintf("%.2f", (sr_loss_total / sr_threshold_val) * 100), "%")
} else {
  "0.00%"
}

# 6. Summary Variables by Source for Text
# Feather River (spring-run production if any)
fr_releases <- sr_all_releases_jpe %>%
  filter(grepl("Feather|FRFH", Hatchery, ignore.case = TRUE))
fr_yearling_count <- fr_releases %>% filter(Type == 'Yearling') %>% nrow()
fr_yoy_count <- fr_releases %>% filter(Type == 'Young-of-year') %>% nrow()
fr_yearling_total <- fr_releases %>% filter(Type == 'Yearling') %>% 
  summarize(total = sum(`# of CWT Fish Released`, na.rm = TRUE)) %>% pull()
fr_yoy_total <- fr_releases %>% filter(Type == 'Young-of-year') %>% 
  summarize(total = sum(`# of CWT Fish Released`, na.rm = TRUE)) %>% pull()
fr_jpe_total <- sum(fr_releases$jpe, na.rm = TRUE)

# Coleman (late-fall production)
coleman_releases <- sr_all_releases_jpe %>%
  filter(grepl("Coleman", Hatchery, ignore.case = TRUE))
coleman_total <- sum(coleman_releases$`# of CWT Fish Released`, na.rm = TRUE)
coleman_loss <- sum(coleman_releases$`Confirmed Loss`, na.rm = TRUE)
coleman_n_groups <- nrow(coleman_releases)
coleman_jpe_total <- sum(coleman_releases$jpe, na.rm = TRUE)

# Formatted values for display
total_sr_released_fmt <- prettyNum(total_sr_released, big.mark = ",")
total_sr_jpe_fmt <- prettyNum(total_sr_jpe, big.mark = ",")
sr_threshold_fmt <- prettyNum(round(sr_threshold_val, 0), big.mark = ",")
sr_loss_total_fmt <- prettyNum(sr_loss_total, big.mark = ",")
fr_yearling_fmt <- prettyNum(fr_yearling_total, big.mark = ",")
fr_yoy_fmt <- prettyNum(fr_yoy_total, big.mark = ",")
coleman_total_fmt <- prettyNum(coleman_total, big.mark = ",")
coleman_loss_fmt <- prettyNum(coleman_loss, big.mark = ",")
coleman_jpe_fmt <- prettyNum(coleman_jpe_total, big.mark = ",")

# 7. Calculate ITLs by experimental release group (0.5% of each group per BiOp Table 184)
# ITL applies to experimental releases: groups released on 2025-11-17, 2025-12-22, 2026-01-08
sr_experimental_itl <- if(nrow(sr_all_releases_jpe) > 0) {
  sr_all_releases_jpe %>%
    filter(Type == "Experimental") %>%
    group_by(`Release Date`) %>%
    summarize(
      fish_released = sum(`# of CWT Fish Released`, na.rm = TRUE),
      confirmed_loss = sum(`Confirmed Loss`, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      itl = round(fish_released * 0.005, 0),  # 0.5% ITL
      itl_perc = round((confirmed_loss / itl) * 100, 2)
    ) %>%
    arrange(`Release Date`)
} else {
  data.frame(`Release Date` = character(), fish_released = numeric(), 
             confirmed_loss = numeric(), itl = numeric(), itl_perc = numeric())
}

# Create ITL summary text for experimental groups
sr_itl_text <- if(nrow(sr_experimental_itl) > 0) {
  itl_lines <- sr_experimental_itl %>%
    mutate(text = paste0("Release Group ", row_number(), " (", `Release Date`, "): ",
                         round(confirmed_loss, 1), " loss of ", prettyNum(itl, big.mark = ","), 
                         " ITL (", itl_perc, "%)")) %>%
    pull(text)
  paste(itl_lines, collapse = "; ")
} else {
  "No experimental release groups available."
}

# 8. Clean Table for Report Display
# Group by release date and hatchery since individual CWT codes don't matter for assessment
sr_surrogate_table_clean <- if(nrow(sr_all_releases_jpe) > 0) {
  sr_all_releases_jpe %>%
    # Final validation - remove any remaining bad rows
    filter(
      !is.na(`Tag Code`),
      `Tag Code` != "",
      !is.na(`# of CWT Fish Released`),
      `# of CWT Fish Released` > 0
    ) %>%
    # Group by release event (date + hatchery + type)
    group_by(`Hatchery`, `Release Date`, `Stock`, `Type`) %>%
    summarize(
      fish_released_raw = sum(`# of CWT Fish Released`, na.rm = TRUE),
      `JPE` = sum(jpe, na.rm = TRUE),
      `Confirmed Loss` = sum(`Confirmed Loss`, na.rm = TRUE),
      `CWT Codes` = paste(sort(unique(`Tag Code`)), collapse = ", "),
      .groups = "drop"
    ) %>%
    # Add ITL column (0.5% for experimental groups)
    mutate(
      `ITL (0.5%)` = if_else(Type == "Experimental", round(fish_released_raw * 0.005, 0), NA_real_),
      `# of CWT Fish Released` = prettyNum(round(fish_released_raw, 0), big.mark = ","),
      `JPE` = prettyNum(round(JPE, 0), big.mark = ","),
      `Confirmed Loss` = round(`Confirmed Loss`, 1)
    ) %>%
    # Select and order columns
    select(`Hatchery`, `Release Date`, `Type`, `# of CWT Fish Released`, `JPE`, `ITL (0.5%)`, `Confirmed Loss`, `CWT Codes`) %>%
    # Sort by release date
    arrange(`Release Date`)
} else {
  # Return empty data frame with correct structure
  data.frame(
    `Hatchery` = character(),
    `Release Date` = character(),
    `Type` = character(),
    `# of CWT Fish Released` = character(),
    `JPE` = character(),
    `ITL (0.5%)` = numeric(),
    `Confirmed Loss` = numeric(),
    `CWT Codes` = character(),
    check.names = FALSE
  )
}

# Legacy variable names for compatibility
sr_surrogate_threshold_val <- sr_threshold_val
sr_surrogate_loss_total <- sr_loss_total
sr_surrogate_perc <- sr_loss_perc
yearling <- fr_yearling_fmt
yoy <- fr_yoy_fmt
n_surrogate_yearling <- fr_yearling_count
n_surrogate_yoy <- fr_yoy_count

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

# --- 3. Presence Logic Helper Function ---
# Delta entry: based on historical cumulative catch at Sac Trawl (Sherwood Harbor)
# Delta exit: based on historical cumulative catch at Chipps Island Trawl
# For LAD winter-run and steelhead
get_presence_status <- function(species_name, entry_pct, exit_pct, catch_keyword) {
  
  # Presence status is determined by historical cumulative catch percentages:
  # - entry_pct = cumulative % at Sac Trawl (Sherwood) - delta entry
  # - exit_pct = cumulative % at Chipps Island Trawl - delta exit
  case_when(
    # Decreasing: >=50% of fish historically have passed Chipps Island Trawl (delta exit)
    exit_pct >= 50 ~ paste0(species_name, " presence in the Delta is decreasing based on historical Chipps Island Trawl monitoring."),
    
    # High/Peak: significant proportion entering (>=25% at Sac Trawl) but <50% exiting at Chipps
    entry_pct >= 25 & exit_pct < 50 ~ paste0(species_name, " presence in the Delta is high based on historical monitoring."),
    
    # Increasing: fish entering at Sac Trawl (5-25%) but few exiting at Chipps yet (<10%)
    entry_pct >= 5 & entry_pct < 25 & exit_pct < 10 ~ paste0(species_name, " presence in the Delta is increasing based on historical monitoring."),
    
    # Low: minimal entry and exit based on historical timing
    TRUE ~ paste0(species_name, " presence in the Delta is low based on historical monitoring.")
  )
}

# salmon_code_rounding_fix.R
# Source this AFTER salmon_code.R to round all fish counts to whole numbers

# Round all loss values to whole numbers
if(exists("loss_dna_wr")) loss_dna_wr <- round(as.numeric(loss_dna_wr), 0)
if(exists("loss_lad_wr")) loss_lad_wr <- round(as.numeric(loss_lad_wr), 0)
if(exists("loss_hatch_wr")) loss_hatch_wr <- round(as.numeric(loss_hatch_wr), 0)
if(exists("loss_nat_sh")) loss_nat_sh <- round(as.numeric(loss_nat_sh), 0)
if(exists("loss_hatch_sh")) loss_hatch_sh <- round(as.numeric(loss_hatch_sh), 0)
if(exists("total_loss")) total_loss <- round(as.numeric(total_loss), 0)

# Round cumulative loss values shown in document
if(exists("wr_loss")) wr_loss <- round(as.numeric(wr_loss), 0)
if(exists("wr_hatch_loss")) wr_hatch_loss <- round(as.numeric(wr_hatch_loss), 0)
if(exists("sh_loss")) sh_loss <- round(as.numeric(sh_loss), 0)

# Round 7-day loss values
if(exists("wr_7d")) wr_7d <- round(as.numeric(wr_7d), 0)
if(exists("wr_hatch_7d")) wr_hatch_7d <- round(as.numeric(wr_hatch_7d), 0)
if(exists("sh_7d")) sh_7d <- round(as.numeric(sh_7d), 0)

# Round hatchery loss totals
if(exists("sh_clipped_loss_total")) sh_clipped_loss_total <- round(as.numeric(sh_clipped_loss_total), 0)

# Round spring-run values (check if they exist first)
if(exists("total_sr_released")) {
  total_sr_released <- round(total_sr_released, 0)
  total_sr_released_fmt <- prettyNum(total_sr_released, big.mark = ",")
}

if(exists("sr_loss_total")) {
  sr_loss_total <- round(sr_loss_total, 0)
  sr_loss_total_fmt <- prettyNum(sr_loss_total, big.mark = ",")
}

if(exists("coleman_total")) {
  coleman_total <- round(coleman_total, 0)
  coleman_total_fmt <- prettyNum(coleman_total, big.mark = ",")
}

if(exists("coleman_loss")) {
  coleman_loss <- round(coleman_loss, 0)
  coleman_loss_fmt <- prettyNum(coleman_loss, big.mark = ",")
}

if(exists("total_sr_jpe")) {
  total_sr_jpe <- round(total_sr_jpe, 0)
  total_sr_jpe_fmt <- prettyNum(total_sr_jpe, big.mark = ",")
}

if(exists("coleman_jpe")) {
  coleman_jpe <- round(coleman_jpe, 0)
  coleman_jpe_fmt <- prettyNum(coleman_jpe, big.mark = ",")
}

# Round JPE values
if(exists("jpe")) jpe <- round(jpe, 0)
if(exists("livingston_jpe")) livingston_jpe <- round(livingston_jpe, 0)

# Round threshold values
if(exists("sr_threshold_val")) {
  sr_threshold_val <- round(sr_threshold_val, 0)
  sr_threshold_fmt <- prettyNum(sr_threshold_val, big.mark = ",")
}

# Round passage estimates (keep at 2 decimals for millions)
# wr_passage and sr_passage are already in millions with 2 decimals - leave as is

print("All fish counts rounded to whole numbers")