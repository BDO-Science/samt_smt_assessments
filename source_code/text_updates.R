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
  print('The Juvenile Production Estimate for Livingstone Stone and Battle Creek hatchery winter-run has not been established for the current water year.')
} else {
  print(paste0('The Juvenile Production Estimate for hatchery winter-run is '
         ,livingston_jpe,' for Livingston Stone releases and '
         ,battle_jpe,' for Battle Creek releases in the current water year'))
}

wr_threshold <- if(is.na(jpe)) {
  print('The Juvenile Production Estimate has not been established for the current water year so thresholds are absent or are included from the previous water year.')
} else {
  print(paste0('The annual Loss threshold for natural winter-run is 0.5% of the jpe or ', round(jpe*.005,2),'fish.'))
}

wr_hatch_threshold <- if(is.na(livingston_jpe)) {
  print('The Juvenile Production Estimate has not been established for the current water year so thresholds are absent or are included from the previous water year.')
} else {
  print(paste0('The annual Loss threshold for Livingston Stone and Battle Creek hatchery winter-run releases is 0.17% of the jpe for each release or ', 
               round(livingston_jpe*.0017,2),' and ', round(battle_jpe*0.0017,2), ' fish for Livingston and Battle Creek respectively.'))
}
  
wr_hatchery_releases <- if(nrow(wr_hatch) == 0) {
   print(paste0('To date, no winter-run hatchery releases have occurred in WY ',wy))
  } else {
   if(nrow(wr_hatch) == 1) {
     print(paste0('A total of xx fish were released from Livingston Stone National Fish Hatchery on xx'))
   } else{
     print(paste0('A total of xx and xx fish were released from Livingston Stone National Fish Hatchery and Battle Creek on xx and xx respectively.'))
   }
  }

wr_hatchery_loss <- if(nrow(wr_hatch) == 0) {
  print('To date, no loss has occurred as no hatchery winter-run have been released.')
} else {
  if(nrow(wr_hatch) == 1) {
    print(paste0('As of ',format(Sys.Date(), '%B %d'), ' cumulative loss of Livingston Stone hatchery fish is ', 
               liv_loss, ' or ', liv_perc, ' of the annual loss threshold. No fish have been released from Battle Creek Hatchery.'))
  } else {
    print(paste0('As of ',format(Sys.Date(), '%B %d'), ' cumulative loss of Livingston Stone hatchery fish is ', 
                 liv_loss, ' or ', liv_perc, ' of the annual loss threshold. Cumulative loss of Battle Creek hatchery fish is ',
                 batt_loss, ' or ', batt_perc, ' of the annual loss threshold'))
  }
}
#####################################################
##setting language for weekly loss thresholds
#####################################################
weekly_loss_text <- if(Sys.Date() < as.Date(paste0(wy,"-1-01"))) {
  print("Weekly distributed loss thresholds are not active at this time")
} else {
  print(paste0('As of ',Sys.Date(),'7-day rolling some of salmon and steelhead has occurred.'))
}

#####################################################
##setting language for STARs estimate
#####################################################
stars_text <- print(paste0("As of ",format(as.Date(stars_date), '%B %d'), 
                           ' overall through delta STARs estimated survival probability (with 80% credible intervals) is ',overall_survival,
                           '. STARs estimated rounting and survival probabilities (with 80% credible intervals) into the interior delta are',
                           id_routing,' and ', id_survival, ' respectively.'))