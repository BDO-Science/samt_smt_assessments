library(here)
library(tidyverse)

project <- here()
source(here(project, 'source_code/salmon_code.R'), echo = FALSE)

##########################
####EXECUTIVE SUMMARY#####
##########################

# Triggered if in date range OR if >5% of target species have entered
entrainment_status <- if(is_season_date_range | (pct_wr_in_delta > 5 | pct_sh_in_delta > 5)) {
  "Entrainment management season is active."
} else {
  "Entrainment management season is not active at this time."
}

# Status of the salmonid loss relative to annual loss thresholds
salvage_status <- if(total_loss > 0) {
  paste0("Annual Loss: ", round(as.numeric(loss_dna_wr), 0), " (", wr_perc, " of annual loss threshold) natural winter-run, ", 
         round(as.numeric(loss_hatch_wr), 0), " (", wr_hatch_perc, " of annual loss threshold) hatchery winter-run, ", 
         round(as.numeric(loss_nat_sh), 0), " natural steelhead, ",  # Changed 2 to 0
         round(as.numeric(loss_hatch_sh), 0), " (", sh_clipped_perc_threshold, " of annual loss threshold) hatchery steelhead, and ", # Changed 2 to 0
         round(as.numeric(sr_loss_total), 0), " (", sr_loss_perc, " of annual loss threshold) spring-run surrogates.") # Changed 2 to 0
} else {
  "No salmonid loss has been recorded this season."
}

# Calculate ITL percentages
# Single-year ITLs: WR DNA 5,922; WR Hatchery Sac River 1,301; Steelhead 5,294
# Spring-run surrogate yearlings: 0.5% of each experimental release group
itl_wr_dna_val <- round(jpe * itl_wr_natural_single, 0)  # 5,922
itl_wr_hatch_sac_val <- round(livingston_jpe * itl_wr_hatch_single, 0)  # 1,301

wr_dna_itl_perc <- if(itl_wr_dna_val > 0) {
  paste0(sprintf("%.2f", (as.numeric(loss_dna_wr) / itl_wr_dna_val) * 100), "%")
} else { "0.00%" }

wr_hatch_itl_perc <- if(itl_wr_hatch_sac_val > 0) {
  paste0(sprintf("%.2f", (as.numeric(loss_hatch_wr) / itl_wr_hatch_sac_val) * 100), "%")
} else { "0.00%" }

sh_nat_itl_perc <- if(itl_sh_natural_single > 0) {
  paste0(sprintf("%.2f", (as.numeric(loss_nat_sh) / itl_sh_natural_single) * 100), "%")
} else { "0.00%" }

# Create spring-run surrogate yearling ITL summary for executive summary
# These are the experimental late-fall releases with 0.5% ITL per release group
sr_yearling_itl_summary <- if(exists("sr_experimental_itl") && nrow(sr_experimental_itl) > 0) {
  itl_lines <- sr_experimental_itl %>%
    mutate(text = paste0("Group ", row_number(), ": ", 
                         round(confirmed_loss, 0), " (", itl_perc, "% of ",  # Changed 1 to 0
                         prettyNum(itl, big.mark = ","), " ITL)")) %>%
    pull(text)
  paste0("Spring-run surrogate yearlings (0.5% ITL per experimental release group): ", 
         paste(itl_lines, collapse = "; "), ".")
} else {
  "Spring-run surrogate yearlings: No experimental releases to date."
}

# ITL status as separate bullet
itl_status <- paste0("Single-year Incidental Take Limit (ITL) Status: ", 
                     round(as.numeric(loss_dna_wr), 0), " (", wr_dna_itl_perc, " of ", 
                     prettyNum(itl_wr_dna_val, big.mark = ","), " ITL) natural winter-run; ",
                     round(as.numeric(loss_hatch_wr), 0), " (", wr_hatch_itl_perc, " of ",
                     prettyNum(itl_wr_hatch_sac_val, big.mark = ","), " ITL) hatchery winter-run; ",
                     round(as.numeric(loss_nat_sh), 0), " (", sh_nat_itl_perc, " of ", # Changed 2 to 0
                     prettyNum(itl_sh_natural_single, big.mark = ","), " ITL) natural steelhead.")

# Generate Delta status for each run
# Based on historical cumulative catch at Chipps Island Trawl (delta exit)
wr_presence_status <- get_presence_status("LAD winter-run", safe_parse("delta_entry_wr"), safe_parse("delta_exit_wr"), "Winter")
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
               ' based on current Livingston Stone production estimates. The annual loss threshold is 1% of the JPE (',
               prettyNum(round(livingston_jpe * wr_hatch_loss_threshold, 0), big.mark = ","),
               ' fish), which is the same as the single-year ITL (BiOp Table 184). Note: Physical releases have not yet occurred in WY ', wy, '.'))
} else {
  # Added prettyNum here
  print(paste0('The Juvenile Production Estimate for hatchery winter-run is ', 
               prettyNum(livingston_jpe, big.mark = ","), 
               ' for Livingston Stone releases. The annual loss threshold is 1% of the JPE (',
               prettyNum(round(livingston_jpe * wr_hatch_loss_threshold, 0), big.mark = ","),
               ' fish), which is the same as the single-year ITL (BiOp Table 184).'))
}

wr_threshold <- if(is.na(jpe)) {
  print('Thresholds are included from the previous water year.')
} else {
  print(paste0('The annual loss threshold for natural winter-run is 1% of the JPE or ', 
               prettyNum(round(jpe*wr_loss_threshold, 0), big.mark = ","), 
               ' fish. The single-year incidental take limit (ITL) is 0.56% of the JPE (',
               prettyNum(round(jpe*itl_wr_natural_single, 0), big.mark = ","),
               ' fish) or 0.36% on a 3-year rolling average (BiOp Table 184).'))
}

wr_hatchery_releases <- if(nrow(wr_hatch) == 0) {
  print(paste0('To date, no winter-run Livingston Stone hatchery releases have occurred in WY ',wy,'.'))
} else {
  print(paste0('A total of xx fish were released from Livingston Stone National Fish Hatchery on xx.'))
}

wr_hatchery_loss <- if(nrow(wr_hatch) == 0) {
  print('To date, no loss has occurred as no hatchery winter-run have been released.')
} else {
  print(paste0('As of ',format(Sys.Date(), '%B %d'), ', cumulative loss of Livingston Stone hatchery fish is ', 
               wr_hatch_loss, ' or ', wr_hatch_perc, 
               ' of the annual loss threshold (which equals the single-year ITL). Cumulative loss in the past 7 days has been ', wr_hatch_7d, '.'))
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
  print(paste0("Total catch of LAD winter-run at RSTs at Delta Entry (Tisdale, Knights Landing, Lower Sacramento River) between ",
               format(min(all_sampling[1:3, 2, drop = TRUE], na.rm = TRUE), '%b %d'), ' and ',
               format(max(all_sampling[1:3, 3, drop = TRUE], na.rm = TRUE), '%b %d'),' is ',sum(all_sampling[1:3,4]) ,' individuals.'))
}

sr_entry_catch <- if(nrow(entry_sampling) == 0){
  print('No catch has been reported at Delta Entry RSTs (Tisdale, Knights Landing, Lower Sacramento River) in the past two weeks.')
} else {
  print(paste0("Total catch of LAD spring-run at RSTs at Delta Entry (Tisdale, Knights Landing, Lower Sacramento River) between ",
               format(min(all_sampling[1:3, 2, drop = TRUE], na.rm = TRUE), '%b %d'), ' and ',
               format(max(all_sampling[1:3, 3, drop = TRUE], na.rm = TRUE), '%b %d'),' is ',sum(all_sampling[1:3,5]) ,' individuals.'))
}

sh_entry_catch <- if(nrow(entry_sampling) == 0){
  print('No catch has been reported at Delta Entry RSTs (Tisdale, Knights Landing, Lower Sacramento River) in the past two weeks.')
} else {
  print(paste0("Total catch of unclipped steelhead at RSTs at Delta Entry (Tisdale, Knights Landing, Lower Sacramento River) between ",
               format(min(all_sampling[1:3, 2, drop = TRUE], na.rm = TRUE), '%b %d'), ' and ',
               format(max(all_sampling[1:3, 3, drop = TRUE], na.rm = TRUE), '%b %d'),' is ',sum(all_sampling[1:3,6]) ,' individuals.'))
}


##delta catch
wr_delta_catch <- if(nrow(delta_sampling) == 0){
  print('No catch has been reported at delta monitoring locations (Sacramento Trawls and Beach Seines) in the past two weeks.')
} else {
  print(paste0("Total catch at Sacramento Trawl and Beach Seines in the delta between ",
               format(min(all_sampling[4:5, 2, drop = TRUE], na.rm = TRUE), '%b %d'), ' and ',
               format(max(all_sampling[4:5, 3, drop = TRUE], na.rm = TRUE), '%b %d'), ' is ', sum(all_sampling[4:5,4]), ' individuals.'))
}

sr_delta_catch <- if(nrow(delta_sampling) == 0){
  print('No catch has been reported at delta monitoring locations (Sacramento Trawls and Beach Seines) in the past two weeks.')
} else {
  print(paste0("Total catch at Sacramento Trawl and Beach Seines in the delta between ",
               format(min(all_sampling[4:5, 2, drop = TRUE], na.rm = TRUE), '%b %d'), ' and ',
               format(max(all_sampling[4:5, 3, drop = TRUE], na.rm = TRUE), '%b %d'), ' is ', sum(all_sampling[4:5,5]), ' individuals.'))
}

sh_delta_catch <- if(nrow(delta_sampling) == 0){
  print('No catch has been reported at delta monitoring locations (Sacramento Trawls and Beach Seines) in the past two weeks.')
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

# 2. Risk Assessment Helper Function - evaluates risk based on current loss and 7-day trajectory
get_risk_level_with_projection <- function(cum_loss, recent_7d_loss, threshold, species_label, limit_label = "threshold", display_pct = NULL) {
  if(is.na(threshold) | threshold == 0) return(paste0(species_label, ": ", tools::toTitleCase(limit_label), " not established."))
  
  # Use pre-computed display percentage if provided (ensures consistency with summary/body text)
  # Otherwise calculate locally as fallback
  if(!is.null(display_pct)) {
    pct_used <- as.numeric(gsub("[^0-9.]", "", display_pct))
  } else {
    pct_used <- round((cum_loss / threshold) * 100, 2)
  }
  
  projected_loss <- cum_loss + recent_7d_loss
  pct_fmt <- sprintf("%.2f", pct_used)
  
  if (cum_loss >= threshold) {
    return(paste0("CRITICAL: ", species_label, " cumulative loss has exceeded the ", limit_label, "."))
  } else if (projected_loss >= threshold) {
    return(paste0("ELEVATED RISK: ", species_label, " cumulative loss is currently ", pct_fmt, 
                  "% of the ", limit_label, ". If recent 7-day loss (", round(recent_7d_loss, 0),
                  ") continues, the ", limit_label, " may be exceeded in the upcoming week."))
  } else if (pct_used > 75) {
    return(paste0("ELEVATED RISK: ", species_label, " cumulative loss is at ", pct_fmt, 
                  "% of the ", limit_label, "."))
  } else if (recent_7d_loss > (threshold * 0.10)) {
    return(paste0("INCREASING RISK: ", species_label, " cumulative loss is currently ", pct_fmt, 
                  "% of the ", limit_label, ", but recent 7-day loss (", round(recent_7d_loss, 0),
                  ") indicates a sharp upward trend."))
  } else {
    return(paste0("LOW RISK: ", species_label, " cumulative loss is currently ", pct_fmt, 
                  "% of the ", limit_label, "."))
  }
}

# 3. Generate Evaluation Texts
# Use the same loss/threshold values already computed for the summary and body text

# Question 1: Natural AND Hatchery Winter-run
wr_nat_loss <- as.numeric(loss_dna_wr)
wr_nat_thresh <- round(jpe * wr_loss_threshold, 0)
wr_nat_7d <- as.numeric(wr_7d)
if(is.na(wr_nat_7d)) wr_nat_7d <- 0

wr_hatch_loss_val <- as.numeric(loss_hatch_wr)
wr_hatch_thresh <- round(livingston_jpe * wr_hatch_loss_threshold, 0)
wr_hatch_7d_val <- as.numeric(wr_hatch_7d)
if(is.na(wr_hatch_7d_val)) wr_hatch_7d_val <- 0

risk_q1_nat <- get_risk_level_with_projection(wr_nat_loss, wr_nat_7d, wr_nat_thresh, "Natural winter-run", display_pct = wr_perc)
risk_q1_hatch <- get_risk_level_with_projection(wr_hatch_loss_val, wr_hatch_7d_val, wr_hatch_thresh, "Hatchery winter-run", display_pct = wr_hatch_perc)
risk_q1 <- paste0(risk_q1_nat, " ", risk_q1_hatch)

# Question 2: Spring-run Surrogates
sr_thresh_safe <- if(exists("sr_threshold_val") && !is.na(sr_threshold_val)) sr_threshold_val else 0
sr_loss_safe   <- if(exists("sr_loss_total") && !is.na(sr_loss_total)) as.numeric(sr_loss_total) else 0
sr_7d_safe <- 0  # No 7-day tracking for spring-run surrogates

risk_q2 <- get_risk_level_with_projection(sr_loss_safe, sr_7d_safe, sr_thresh_safe, "Spring-run surrogates", display_pct = sr_loss_perc)

# Question 3: Natural AND Hatchery Steelhead
sh_nat_loss_safe <- as.numeric(loss_nat_sh)
sh_nat_7d_val <- as.numeric(sh_7d)
if(is.na(sh_nat_7d_val)) sh_nat_7d_val <- 0

sh_hatch_loss_safe <- as.numeric(sh_clipped_loss_total)
if(is.na(sh_hatch_loss_safe)) sh_hatch_loss_safe <- 0
sh_hatch_thresh_safe <- if(exists("sh_clipped_threshold") && !is.na(sh_clipped_threshold)) sh_clipped_threshold else 0
sh_hatch_7d_val <- as.numeric(sh_7d)
if(is.na(sh_hatch_7d_val)) sh_hatch_7d_val <- 0

# Natural steelhead uses ITL (5,294) since there is no loss threshold
risk_q3_nat <- get_risk_level_with_projection(sh_nat_loss_safe, sh_nat_7d_val, itl_sh_natural_single, "Natural steelhead", limit_label = "single-year ITL", display_pct = sh_perc)
risk_q3_hatch <- get_risk_level_with_projection(sh_hatch_loss_safe, sh_hatch_7d_val, sh_hatch_thresh_safe, "Hatchery steelhead", display_pct = sh_clipped_perc_threshold)
risk_q3 <- paste0(risk_q3_nat, " ", risk_q3_hatch)


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