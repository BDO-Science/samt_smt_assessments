library(here)
library(tidyverse)

project <- here()
source(here(project, 'source_code/salmon_code.R'), echo = FALSE)
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
wr_annual_text <- print(paste0('As of ',format(Sys.Date()-1, '%B %d'),' cumulative loss of genetically confirmed winter-run is ', wr_loss,
                               ' or ',wr_perc,' of the annual loss threshold. Cumulative loss in the past 7 days has been ',
                               wr_7d, '.'))
sh_annual_text <- print(paste0('As of ',format(Sys.Date()-1, '%B %d'),' cumulative loss of unclipped steelhead is ', sh_loss,
                               ' or ',sh_perc,' of the annual loss threshold. Cumulative loss in the past 7 days has been ',
                               sh_7d, '.'))


wr_jpe <- if(is.na(jpe)) {
  print('The Juvenile Production Estimate for winter-run has not been established for the current water year.')
} else {
  print(paste0('The Juvenile Production Estimate for winter-run is ',jpe,' for the current water year'))
}

wr_hatch_jpe <- if(is.na(livingston_jpe)) {
  print('The Juvenile Production Estimate for Livingstone Stone hatchery winter-run has not been established for the current water year.')
} else {
  print(paste0('The Juvenile Production Estimate for hatchery winter-run is '
         ,livingston_jpe,' for Livingston Stone releases'))
}

wr_threshold <- if(is.na(jpe)) {
  print('The Juvenile Production Estimate has not been established for the current water year so thresholds are absent or are included from the previous water year.')
} else {
  print(paste0('The annual Loss threshold for natural winter-run is 0.5% of the jpe or ', round(jpe*wr_loss_threshold,2),'fish.'))
}

wr_hatch_threshold <- if(is.na(livingston_jpe)) {
  print('The Juvenile Production Estimate has not been established for the current water year so thresholds are absent or are included from the previous water year.')
} else {
  print(paste0('The annual Loss threshold for Livingston Stone hatchery winter-run releases is 1% of the jpe or ', 
               round(livingston_jpe*wr_hatch_loss_threshold,2), 'fish.'))
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
                 liv_loss, ' or ', liv_perc, ' of the annual loss threshold.'))
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
                           ' placing it in the ', perc_surv, 'th percentile of historical STARS survival estimates for the month of ', 
                           month(Sys.Date(), label = TRUE, abbr = FALSE), ' (WYs 2018-2025)',
                           '. STARS estimated routing and survival probabilities (with 80% credible intervals) into the interior delta are ',
                           id_routing,' and ', id_survival, ', respectively, corresponding to the ', perc_id_route, 'th and ', perc_id_surv, 'th percentiles of historical ',
                           month(Sys.Date(), label = TRUE, abbr = FALSE), ' estimates (WYs 2018-2025).'))


#####################################################
##setting language for delta monitoring location
#####################################################

wr_delta_catch <- print(paste0("Total catch of LAD winter run at RSTs at Delta Entry (Tisdale, Knights Landing, Lower Sacramento River between ",
                        format(min(all_sampling[1:3, 2, drop = TRUE], na.rm = TRUE), '%b %d'), ' and ',
                        format(max(all_sampling[1:3, 3, drop = TRUE], na.rm = TRUE), '%b %d'),' is ',sum(all_sampling[1:3,4]) ,' individuals.  Total catch at Sacramento Trawl and Beach Seines in the delta between ',
                        format(min(all_sampling[4:5, 2, drop = TRUE], na.rm = TRUE), '%b %d'), ' and ',
                        format(max(all_sampling[4:5, 3, drop = TRUE], na.rm = TRUE), '%b %d'), ' is ', sum(all_sampling[4:5,4]), ' individuals. Total catch at Delta Exit at Chipps Island between ',
                        format(min(all_sampling[6, 2, drop = TRUE], na.rm = TRUE), '%b %d'), ' and ',
                        format(max(all_sampling[6, 3, drop = TRUE], na.rm = TRUE), '%b %d'), ' is ', sum(all_sampling[6,4]), ' individuals.'))

sr_delta_catch <- print(paste0("Total catch of LAD spring run at RSTs at Delta Entry (Tisdale, Knights Landing, Lower Sacramento River between ",
                        format(min(all_sampling[1:3, 2, drop = TRUE], na.rm = TRUE), '%b %d'), ' and ',
                        format(max(all_sampling[1:3, 3, drop = TRUE], na.rm = TRUE), '%b %d'),' is ',sum(all_sampling[1:3,5]) ,' individuals.  Total catch at Sacramento Trawl and Beach Seines in the delta between ',
                        format(min(all_sampling[4:5, 2, drop = TRUE], na.rm = TRUE), '%b %d'), ' and ',
                        format(max(all_sampling[4:5, 3, drop = TRUE], na.rm = TRUE), '%b %d'), ' is ', sum(all_sampling[4:5,5]), ' individuals. Total catch at Delta Exit at Chipps Island between ',
                        format(min(all_sampling[6, 2, drop = TRUE], na.rm = TRUE), '%b %d'), ' and ',
                        format(max(all_sampling[6, 3, drop = TRUE], na.rm = TRUE), '%b %d'), ' is ', sum(all_sampling[6,5]), ' individuals.'))

sh_delta_catch <- print(paste0("Total catch of unclipped steelhead at RSTs at Delta Entry (Tisdale, Knights Landing, Lower Sacramento River between ",
                        format(min(all_sampling[1:3, 2, drop = TRUE], na.rm = TRUE), '%b %d'), ' and ',
                        format(max(all_sampling[1:3, 3, drop = TRUE], na.rm = TRUE), '%b %d'),' is ',sum(all_sampling[1:3,6]) ,' individuals.  Total catch at Sacramento Trawl and Beach Seines in the delta between ',
                        format(min(all_sampling[4:5, 2, drop = TRUE], na.rm = TRUE), '%b %d'), ' and ',
                        format(max(all_sampling[4:5, 3, drop = TRUE], na.rm = TRUE), '%b %d'), ' is ', sum(all_sampling[4:5,6]), ' individuals. Total catch at Delta Exit at Chipps Island between ',
                        format(min(all_sampling[6, 2, drop = TRUE], na.rm = TRUE), '%b %d'), ' and ',
                        format(max(all_sampling[6, 3, drop = TRUE], na.rm = TRUE), '%b %d'), ' is ', sum(all_sampling[6,6]), ' individuals.'))

#####################################################
##setting language for RBDD passage estimates
#####################################################

