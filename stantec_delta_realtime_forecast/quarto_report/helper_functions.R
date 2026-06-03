get_latest_results_folder <- function(results_root = "..") {
  
  result_folders <- list.dirs(
    path = results_root,
    recursive = FALSE,
    full.names = TRUE
  )
  
  result_folders <- result_folders[
    grepl("_results$", basename(result_folders))
  ]
  
  folder_dates <- as.Date(
    gsub("_results", "", basename(result_folders)),
    format = "%Y%m%d"
  )
  
  result_folders[which.max(folder_dates)]
}