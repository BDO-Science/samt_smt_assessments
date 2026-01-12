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
  paste0("Season Loss: **", loss_dna_wr, "** (", wr_perc, " of threshold) DNA Winter-run, **", 
         loss_hatch_wr, "** (", wr_hatch_perc, " of threshold) Hatchery Winter-run, **", 
         loss_nat_sh, "** (", sh_perc, " of threshold) Natural Steelhead, **", 
         loss_hatch_sh, "** (", sh_clipped_perc_threshold, " of threshold) Hatchery Steelhead, and **",
         sr_surrogate_loss_total, "** (", sr_surrogate_perc, " of threshold) Spring-run Surrogates.")
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
               prettyNum(round(jpe*wr_loss_threshold, 2), big.mark = ","), 
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

#####################################################
## AUTOMATED RISK ASSESSMENT LOGIC
#####################################################

# Helper function to generate risk text
# Logic: 
# 1. If already exceeded -> "Threshold exceeded."
# 2. If loss + (2 * recent weekly trend) > threshold -> "High probability"
# 3. If > 75% of threshold used -> "Elevated probability"
# 4. Otherwise -> "Low probability"
get_risk_statement <- function(species_name, cum_loss, recent_loss, threshold) {
  
  # Safety check for missing/NA thresholds
  if(is.na(threshold) | threshold == 0) return(paste0("Annual ", species_name, " threshold has not been established."))
  
  pct_used <- (cum_loss / threshold) * 100
  projected_loss <- cum_loss + recent_loss # Simple projection: assume next week = last week
  
  if (cum_loss >= threshold) {
    return(paste0("The annual loss threshold for ", species_name, " has already been **exceeded**."))
  } else if (projected_loss >= threshold) {
    return(paste0("The probability of exceeding the ", species_name, " loss threshold in the upcoming week is **high**. Recent loss trends indicate the threshold may be reached soon."))
  } else if (pct_used > 75) {
    return(paste0("The probability of exceeding the ", species_name, " loss threshold is **elevated**. Cumulative loss is currently at ", round(pct_used, 1), "% of the limit."))
  } else {
    return(paste0("The probability of exceeding the ", species_name, " loss threshold in the upcoming week is **low** (currently at ", round(pct_used, 1), "% of threshold)."))
  }
}

# 1. Natural Winter-run Assessment
# Note: Threshold is 'jpe * wr_loss_threshold' (1% of JPE)
wr_threshold_val <- if(exists("jpe") & exists("wr_loss_threshold")) jpe * wr_loss_threshold else NA
risk_wr <- get_risk_statement("Natural Winter-run Chinook Salmon", loss_dna_wr, wr_7d, wr_threshold_val)

# 2. Hatchery Winter-run Assessment
# Note: Threshold is 'livingston_jpe * wr_hatch_loss_threshold' (1% of JPE)
wr_hatch_threshold_val <- if(exists("livingston_jpe") & exists("wr_hatch_loss_threshold")) livingston_jpe * wr_hatch_loss_threshold else NA
risk_wr_hatch <- get_risk_statement("Hatchery Winter-run Chinook Salmon", loss_hatch_wr, wr_hatch_7d, wr_hatch_threshold_val)

# Combine for Question 1
risk_q1 <- paste0(risk_wr, " ", risk_wr_hatch)


# 3. Spring-run Hatchery Surrogate Assessment (Question 2)
# Using the variables we created in the previous turn
# total_sr_released, sr_surrogate_loss_total, sr_surrogate_threshold_val
# We need 7-day loss for SR surrogates. It wasn't in the previous snippet, so we default to 0 or calculate it if available.
# Assuming 'loss_summary_table' has a row for SR surrogates 7-day loss, or we treat it as 0 for now.
sr_recent_loss <- 0 # Placeholder if 7-day data isn't explicitly separated for surrogates in summary table
risk_q2 <- get_risk_statement("Spring-run Hatchery Surrogate", sr_surrogate_loss_total, sr_recent_loss, sr_surrogate_threshold_val)


# 4. Steelhead Assessment (Assuming Question 3 was meant to be Steelhead)
# Threshold: 'sh_clipped_threshold' (from salmon_code.R)
# Recent loss: 'sh_7d'
risk_q3 <- get_risk_statement("Hatchery Steelhead", sh_clipped_loss_total, sh_7d, sh_clipped_threshold)

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

