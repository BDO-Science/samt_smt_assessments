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
wr_annual_text <- if(is.null(salmon) & is.null(steelhead)) {
  print(paste0('No loss of natural winter-run salmon or steelhead has occurred in WY ',wy))
} else {
  print('Loss has occurred')
}

#####################################################
##setting language for weekly loss thresholds
#####################################################
weekly_loss_text <- if(Sys.Date() < as.Date(paste0(wy,"-1-01"))) {
  print("Weekly distributed loss thresholds are not active at this time")
} else {
  print(paste0('As of ',Sys.Date(),'7-day rolling some of salmon and steelhead has occurred.'))
}