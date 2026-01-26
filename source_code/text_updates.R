library(here)
library(tidyverse)

project <- here()
source(here(project, 'source_code/salmon_code.R'), echo = FALSE)

##########################
####EXECUTIVE SUMMARY#####
##########################

# Triggered if in date range OR if >5% of target species have entered
entrainment_status <- if(is_season_date_range | (pct_wr_in_delta > 5 | pct_sh_in_delta > 5)) {
  "Entrainment management season is **active**."
} else {
  "Entrainment management season is **not active** at this time."
}

#status of the salmonid loss

# Calculate LAD percent (using Natural Winter-run Threshold)
# Matches logic from salmon_code.R: jpe * wr_loss_threshold
lad_wr_threshold_val <- if(exists("jpe") & exists("wr_loss_threshold")) jpe * wr_loss_threshold else NA
lad_wr_pct <- if(!is.na(lad_wr_threshold_val) & lad_wr_threshold_val > 0) {
  paste0(sprintf("%.2f", (loss_lad_wr / lad_wr_threshold_val) * 100), "%")
} else {
  "NA%"
}

#status of the salmonid loss
salvage_status <- if(total_loss > 0) {
  paste0("Season Loss: **", round(as.numeric(loss_dna_wr), 0), "** (", wr_perc, " of threshold) DNA Winter-run, **", 
         round(as.numeric(loss_hatch_wr), 0), "** (", wr_hatch_perc, " of threshold) Hatchery Winter-run, **", 
         round(as.numeric(loss_nat_sh), 0), "** (", sh_perc, " of threshold) Natural Steelhead, **", 
         round(as.numeric(loss_hatch_sh), 0), "** (", sh_clipped_perc_threshold, " of threshold) Hatchery Steelhead, and **",
         round(as.numeric(sr_loss_total), 0), "** (", sr_loss_perc, " of threshold) Spring-run Surrogates.")
} else {
  "No salmonid loss has been recorded this season."
}

# Generate Delta status for each run
wr_presence_status <- get_presence_status("Winter-run", safe_parse("delta_entry_wr"), safe_parse("delta_exit_wr"), "Winter")
sh_presence_status <- get_presence_status("Steelhead", safe_parse("delta_entry_sh"), safe_parse("delta_exit_sh"), "Steelhead")

#####################################################
##setting language for early season migration action
#####################################################

early_season_text <- if(Sys.Date() < as.Date(paste0(wy-1,"-11-01")) | Sys.Date() > as.Date(paste0(wy,"-12-30"))) {
  print("Early Season Migration is not active at this time")
} else {
  print("Early season migration thresholds have not been triggered.")
}

#####################################################
##setting language for annual loss thresholds
#####################################################

wr_jpe <- if(is.na(jpe)) {
  print('The Juvenile Production Estimate for winter-run has not been established for the current water year.')
} else {
  # Added prettyNum here
  print(paste0('The Juvenile Production Estimate for winter-run is ', 
               prettyNum(jpe, big.mark = ","), 
               ' for the current water year.'))
}

wr_hatch_jpe <- if(is.na(livingston_jpe)) {
  print('The Juvenile Production Estimate for Livingstone Stone hatchery winter-run has not been established for the current water year.')
} else if(nrow(wr_hatch) == 0) {
  # JPE exists but no releases yet - based on current hatchery production
  print(paste0('The Juvenile Production Estimate for hatchery winter-run is ', 
               prettyNum(livingston_jpe, big.mark = ","), 
               ' based on current Livingston Stone production estimates. **Note: Physical releases have not yet occurred in WY ', wy, '.**'))
} else {
  # Added prettyNum here
  print(paste0('The Juvenile Production Estimate for hatchery winter-run is ', 
               prettyNum(livingston_jpe, big.mark = ","), 
               ' for Livingston Stone releases.'))
}

wr_threshold <- if(is.na(jpe)) {
  print('Thresholds are included from the previous water year.')
} else {
  print(paste0('The annual Loss threshold for natural winter-run is 1% of the jpe or ', 
               prettyNum(round(jpe*wr_loss_threshold, 0), big.mark = ","), 
               ' fish.'))
}

wr_hatchery_releases <- if(nrow(wr_hatch) == 0) {
  print(paste0('To date, no winter-run Livingstone hatchery releases have occurred in WY ',wy))
} else {
  print(paste0('A total of xx fish were released from Livingston Stone National Fish Hatchery on xx'))
}

wr_hatchery_loss <- if(nrow(wr_hatch) == 0) {
  print('To date, no loss has occurred as no hatchery winter-run have been released.')
} else {
  print(paste0('As of ',format(Sys.Date(), '%B %d'), ' cumulative loss of Livingston Stone hatchery fish is ', 
               wr_hatch_loss, ' or ', wr_hatch_perc, 
               ' of the annual loss threshold. Cumulative loss in the past 7 days has been ', wr_hatch_7d, '.'))
}


#####################################################
##setting language for hatchery steelhead
#####################################################


#####################################################
##setting language for weekly loss thresholds
#####################################################
# weekly_loss_text <- if(Sys.Date() < as.Date(paste0(wy,"-1-01"))) {
#   print("Weekly distributed loss thresholds are not active at this time")
# } else {
#   print(paste0('As of ',Sys.Date(),'7-day rolling some of salmon and steelhead has occurred.'))
# }

#####################################################
##setting language for STARs estimate
#####################################################
stars_text <- print(paste0("As of ",format(as.Date(stars_date), '%B %d'), 
                           ', overall through delta STARS estimated survival probability (with 80% credible intervals) is ',overall_survival, 
                           ' placing it in the ', perc_surv, ' percentile of historical STARS survival estimates for the month of ', 
                           month(Sys.Date(), label = TRUE, abbr = FALSE), ' (WYs 2018-2025)',
                           '. STARS estimated routing and survival probabilities (with 80% credible intervals) into the interior delta are ',
                           id_routing,' and ', id_survival, ', respectively, corresponding to the ', perc_id_route, ' and ', perc_id_surv, ' percentiles of historical ',
                           month(Sys.Date(), label = TRUE, abbr = FALSE), ' estimates (WYs 2018-2025).'))


#####################################################
##setting language for delta monitoring location
#####################################################

#for conditional formatting
entry_sampling <- all_sampling %>%
  filter(grepl('RST', Location)) %>%
  filter(!is.na(`Date Start`) & !is.na(`Date End`))
delta_sampling <- all_sampling %>%
  filter(grepl('Sherwood|Beach', Location))
exit_sampling <- all_sampling %>%
  filter(grepl('Chipps', Location))

##entry catch
wr_entry_catch <- if(nrow(entry_sampling) == 0){
  print('No catch has been reported at Delta Entry RSTs (Tisdale, Knights Landing, Lower Sacramento River) in the past two weeks.')
} else {
  print(paste0("Total catch of LAD winter run at RSTs at Delta Entry (Tisdale, Knights Landing, Lower Sacramento River) between ",
               format(min(all_sampling[1:3, 2, drop = TRUE], na.rm = TRUE), '%b %d'), ' and ',
               format(max(all_sampling[1:3, 3, drop = TRUE], na.rm = TRUE), '%b %d'),' is ',sum(all_sampling[1:3,4]) ,' individuals.'))
}

sr_entry_catch <- if(nrow(entry_sampling) == 0){
  print('No catch has been reported at Delta Entry RSTs (Tisdale, Knights Landing, Lower Sacramento River) in the past two weeks.')
} else {
  print(paste0("Total catch of LAD winter run at RSTs at Delta Entry (Tisdale, Knights Landing, Lower Sacramento River) between ",
               format(min(all_sampling[1:3, 2, drop = TRUE], na.rm = TRUE), '%b %d'), ' and ',
               format(max(all_sampling[1:3, 3, drop = TRUE], na.rm = TRUE), '%b %d'),' is ',sum(all_sampling[1:3,5]) ,' individuals.'))
}

sh_entry_catch <- if(nrow(entry_sampling) == 0){
  print('No catch has been reported at Delta Entry RSTs (Tisdale, Knights Landing, Lower Sacramento River) in the past two weeks.')
} else {
  print(paste0("Total catch of LAD winter run at RSTs at Delta Entry (Tisdale, Knights Landing, Lower Sacramento River) between ",
               format(min(all_sampling[1:3, 2, drop = TRUE], na.rm = TRUE), '%b %d'), ' and ',
               format(max(all_sampling[1:3, 3, drop = TRUE], na.rm = TRUE), '%b %d'),' is ',sum(all_sampling[1:3,6]) ,' individuals.'))
}


##delta catch
wr_delta_catch <- if(nrow(delta_sampling) == 0){
  print('No catch has been reported at delta monitoring loactions (Sacramento Tralws and Beach Seines) in the past two weeks.')
} else {
  print(paste0("Total catch at Sacramento Trawl and Beach Seines in the delta between ",
               format(min(all_sampling[4:5, 2, drop = TRUE], na.rm = TRUE), '%b %d'), ' and ',
               format(max(all_sampling[4:5, 3, drop = TRUE], na.rm = TRUE), '%b %d'), ' is ', sum(all_sampling[4:5,4]), ' individuals.'))
}

sr_delta_catch <- if(nrow(delta_sampling) == 0){
  print('No catch has been reported at delta monitoring loactions (Sacramento Tralws and Beach Seines) in the past two weeks.')
} else {
  print(paste0("Total catch at Sacramento Trawl and Beach Seines in the delta between ",
               format(min(all_sampling[4:5, 2, drop = TRUE], na.rm = TRUE), '%b %d'), ' and ',
               format(max(all_sampling[4:5, 3, drop = TRUE], na.rm = TRUE), '%b %d'), ' is ', sum(all_sampling[4:5,6]), ' individuals.'))
}

sh_delta_catch <- if(nrow(delta_sampling) == 0){
  print('No catch has been reported at delta monitoring loactions (Sacramento Tralws and Beach Seines) in the past two weeks.')
} else {
  print(paste0("Total catch at Sacramento Trawl and Beach Seines in the delta between ",
               format(min(all_sampling[4:5, 2, drop = TRUE], na.rm = TRUE), '%b %d'), ' and ',
               format(max(all_sampling[4:5, 3, drop = TRUE], na.rm = TRUE), '%b %d'), ' is ', sum(all_sampling[4:5,6]), ' individuals.'))
}

##delta exit catch
wr_exit_catch <- if(nrow(exit_sampling) == 0){
  print('No catch has been reported at Delta Exit at Chipps Island Trawls in the past two weeks.')
} else {
  print(paste0('Total catch at Delta Exit at Chipps Island between ',
               format(min(all_sampling[6, 2, drop = TRUE], na.rm = TRUE), '%b %d'), ' and ',
               format(max(all_sampling[6, 3, drop = TRUE], na.rm = TRUE), '%b %d'), ' is ', sum(all_sampling[6,4]), ' individuals.'))
}
sr_exit_catch <- if(nrow(exit_sampling) == 0){
  print('No catch has been reported at Delta Exit at Chipps Island Trawls in the past two weeks.')
} else {
  print(paste0('Total catch at Delta Exit at Chipps Island between ',
               format(min(all_sampling[6, 2, drop = TRUE], na.rm = TRUE), '%b %d'), ' and ',
               format(max(all_sampling[6, 3, drop = TRUE], na.rm = TRUE), '%b %d'), ' is ', sum(all_sampling[6,5]), ' individuals.'))
}
sh_exit_catch <- if(nrow(exit_sampling) == 0){
  print('No catch has been reported at Delta Exit at Chipps Island Trawls in the past two weeks.')
} else {
  print(paste0('Total catch at Delta Exit at Chipps Island between ',
               format(min(all_sampling[6, 2, drop = TRUE], na.rm = TRUE), '%b %d'), ' and ',
               format(max(all_sampling[6, 3, drop = TRUE], na.rm = TRUE), '%b %d'), ' is ', sum(all_sampling[6,6]), ' individuals.'))
}

#########################################################
#### AUTOMATED RISK ASSESSMENT LOGIC (For Evaluation Section)
#########################################################

# 1. Define Threshold Variables needed for Risk Assessment
# These come from salmon_code.R, but we define them here to be safe
wr_threshold_val <- if(exists("jpe") & exists("wr_loss_threshold")) jpe * wr_loss_threshold else NA

if (!exists("sh_clipped_threshold")) {
  if (exists("sh_hatch_loss_threshold") & exists("total_sh_released")) { 
    sh_clipped_threshold <- round(total_sh_released * sh_hatch_loss_threshold, 0)
  } else {
    sh_clipped_threshold <- NA 
  }
}

# Format sh_clipped_threshold with commas for display
sh_clipped_threshold_fmt <- if(!is.na(sh_clipped_threshold)) {
  prettyNum(sh_clipped_threshold, big.mark = ",")
} else {
  "NA"
}

# 2. Risk Assessment Helper Function
get_risk_assessment <- function(species_name, cum_loss, recent_loss, threshold) {
  
  if(is.na(threshold) | threshold == 0) return("Threshold not established.")
  
  pct_used <- (cum_loss / threshold) * 100
  if(is.na(recent_loss)) recent_loss <- 0
  
  if (cum_loss >= threshold) {
    return(paste0("**CRITICAL:** The annual loss threshold for ", species_name, " has been **exceeded**."))
  } else if (pct_used > 75) {
    return(paste0("**ELEVATED RISK:** Cumulative loss is at ", round(pct_used, 1), "% of the limit. Continued salvage at recent rates may trigger the threshold."))
  } else if (recent_loss > (threshold * 0.10)) {
    return(paste0("**INCREASING RISK:** While cumulative loss is currently low (", round(pct_used, 1), "%), recent salvage indicates a sharp upward trend."))
  } else {
    return(paste0("**LOW RISK:** Cumulative loss is currently ", round(pct_used, 1), "% of the threshold. Current trajectory suggests the threshold is unlikely to be exceeded in the upcoming week."))
  }
}

# 3. Generate Evaluation Texts
# Natural Winter-run
current_wr_7d <- if(exists("wr_7d")) wr_7d else 0
risk_q1 <- get_risk_assessment("Natural Winter-run", loss_dna_wr, current_wr_7d, wr_threshold_val)

# Spring-run Surrogates
sr_thresh_safe <- if(exists("sr_threshold_val")) sr_threshold_val else 0
sr_loss_safe   <- if(exists("sr_loss_total")) sr_loss_total else 0
sr_recent_safe <- 0  # No 7-day tracking for spring-run surrogates
risk_q2 <- get_risk_assessment("Spring-run Surrogates", sr_loss_safe, sr_recent_safe, sr_thresh_safe)

# Hatchery Steelhead
sh_loss_safe   <- if(exists("sh_clipped_loss_total")) sh_clipped_loss_total else 0 
sh_recent_safe <- if(exists("sh_7d")) sh_7d else 0
risk_q3 <- get_risk_assessment("Hatchery Steelhead", sh_loss_safe, sh_recent_safe, sh_clipped_threshold)


# wr_delta_catch <- print(paste0("Total catch of LAD winter run at RSTs at Delta Entry (Tisdale, Knights Landing, Lower Sacramento River) between ",
#                         format(min(all_sampling[1:3, 2, drop = TRUE], na.rm = TRUE), '%b %d'), ' and ',
#                         format(max(all_sampling[1:3, 3, drop = TRUE], na.rm = TRUE), '%b %d'),' is ',sum(all_sampling[1:3,4]) ,' individuals.  Total catch at Sacramento Trawl and Beach Seines in the delta between ',
#                         format(min(all_sampling[4:5, 2, drop = TRUE], na.rm = TRUE), '%b %d'), ' and ',
#                         format(max(all_sampling[4:5, 3, drop = TRUE], na.rm = TRUE), '%b %d'), ' is ', sum(all_sampling[4:5,4]), ' individuals. Total catch at Delta Exit at Chipps Island between ',
#                         format(min(all_sampling[6, 2, drop = TRUE], na.rm = TRUE), '%b %d'), ' and ',
#                         format(max(all_sampling[6, 3, drop = TRUE], na.rm = TRUE), '%b %d'), ' is ', sum(all_sampling[6,4]), ' individuals.'))
# 
# sr_delta_catch <- print(paste0("Total catch of LAD spring run at RSTs at Delta Entry (Tisdale, Knights Landing, Lower Sacramento River) between ",
#                         format(min(all_sampling[1:3, 2, drop = TRUE], na.rm = TRUE), '%b %d'), ' and ',
#                         format(max(all_sampling[1:3, 3, drop = TRUE], na.rm = TRUE), '%b %d'),' is ',sum(all_sampling[1:3,5]) ,' individuals.  Total catch at Sacramento Trawl and Beach Seines in the delta between ',
#                         format(min(all_sampling[4:5, 2, drop = TRUE], na.rm = TRUE), '%b %d'), ' and ',
#                         format(max(all_sampling[4:5, 3, drop = TRUE], na.rm = TRUE), '%b %d'), ' is ', sum(all_sampling[4:5,5]), ' individuals. Total catch at Delta Exit at Chipps Island between ',
#                         format(min(all_sampling[6, 2, drop = TRUE], na.rm = TRUE), '%b %d'), ' and ',
#                         format(max(all_sampling[6, 3, drop = TRUE], na.rm = TRUE), '%b %d'), ' is ', sum(all_sampling[6,5]), ' individuals.'))
# 
# sh_delta_catch <- print(paste0("Total catch of unclipped steelhead at RSTs at Delta Entry (Tisdale, Knights Landing, Lower Sacramento River) between ",
#                         format(min(all_sampling[1:3, 2, drop = TRUE], na.rm = TRUE), '%b %d'), ' and ',
#                         format(max(all_sampling[1:3, 3, drop = TRUE], na.rm = TRUE), '%b %d'),' is ',sum(all_sampling[1:3,6]) ,' individuals.  Total catch at Sacramento Trawl and Beach Seines in the delta between ',
#                         format(min(all_sampling[4:5, 2, drop = TRUE], na.rm = TRUE), '%b %d'), ' and ',
#                         format(max(all_sampling[4:5, 3, drop = TRUE], na.rm = TRUE), '%b %d'), ' is ', sum(all_sampling[4:5,6]), ' individuals. Total catch at Delta Exit at Chipps Island between ',
#                         format(min(all_sampling[6, 2, drop = TRUE], na.rm = TRUE), '%b %d'), ' and ',
#                         format(max(all_sampling[6, 3, drop = TRUE], na.rm = TRUE), '%b %d'), ' is ', sum(all_sampling[6,6]), ' individuals.'))

#####################################################
##setting language for RBDD passage estimates
#####################################################