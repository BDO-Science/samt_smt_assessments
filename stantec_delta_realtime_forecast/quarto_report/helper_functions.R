############################################################
# Delta Real-Time Forecast Summary Helper Functions
#
# Purpose:
# Functions used to generate the weekly Quarto report.
############################################################


############################################################
# 1. Function: get_latest_results_folder
#
# Description:
# Searches the results directory and returns the most
# recent results folder matching the pattern:
#
# YYYYMMDD_results
#
# Example:
# 20260526_results
#
# Inputs:
# results_root - parent directory containing results folders
#
# Returns:
# Full path to latest results folder
############################################################

get_latest_results_folder <- function(results_root = "..") {
  result_folders <- list.dirs(
    path = results_root,
    recursive = FALSE,
    full.names = TRUE
  )
  
  result_folders <- result_folders[
    grepl("_results$", basename(result_folders))
  ]
  
  if (length(result_folders) == 0) {
    stop("No results folders found.")
  }
  
  folder_dates <- as.Date(
    gsub("_results", "", basename(result_folders)),
    format = "%Y%m%d"
  )
  
  result_folders[which.max(folder_dates)]
}


############################################################
# Function: get_selected_results_folder
#
# Description:
# Reads the user-selected forecast date from
# report_config.yml and returns the corresponding
# results folder.
#
# If no date is specified, the latest available
# results folder is used.
#
# Inputs:
# results_root - parent directory containing results folders
# config_file - YAML configuration file
#
# Returns:
# Full path to selected results folder
############################################################

get_selected_results_folder <- function(results_root = "..",
                                        config_file = "report_config.yml") {
  if (!requireNamespace("yaml", quietly = TRUE)) {
    stop("Please install the yaml package: install.packages('yaml')")
  }
  
  config <- yaml::read_yaml(config_file)
  
  results_date <- config$results_date
  
  if (is.null(results_date) || results_date == "") {
    return(get_latest_results_folder(results_root))
  }
  
  selected_folder <- file.path(
    results_root,
    paste0(results_date, "_results")
  )
  
  if (!dir.exists(selected_folder)) {
    stop(
      paste0(
        "Selected results folder does not exist: ",
        selected_folder
      )
    )
  }
  
  selected_folder
}


############################################################
# Function: format_cfs
#
# Description:
# Formats flow values with commas and no decimals.
#
# Example:
# 11850 becomes 11,850
############################################################

format_cfs <- function(x) {
  x_num <- suppressWarnings(
    as.numeric(gsub(",", "", as.character(x)))
  )
  
  format(
    round(x_num, 0),
    big.mark = ",",
    scientific = FALSE,
    trim = TRUE
  )
}

############################################################
# Function: create_zoi_table
#
# Description:
# Reads zoi_bins.csv from the selected results folder
# and generates Table 1:
#
# Weekly Averaged Forecasted Flow Data and Flow Bins
#
# Inputs:
# results_folder - path to selected results folder
#
# Source File:
# zoi_bins.csv
#
# Returns:
# Formatted HTML table for Quarto report
############################################################
create_zoi_table <- function(results_folder) {
  
  zoi_bins <- readr::read_csv(
    file.path(results_folder, "zoi_bins.csv"),
    show_col_types = FALSE
  )
  
  zoi_bins <- zoi_bins |>
    dplyr::mutate(
      across(where(is.numeric), format_cfs)
    )
  
  zoi_bins |>
    knitr::kable(
      format = "html",
      col.names = c(
        "Forecast Week",
        "Sacramento River at Freeport (cfs)",
        "Sac Flow Bin",
        "San Joaquin River at Vernalis (cfs)",
        "SJR Flow Bin",
        "Delta Inflow Bin"
      ),
      align = "c"
    ) |>
    kableExtra::kable_styling(
      full_width = FALSE
    ) |>
    kableExtra::column_spec(1, width = "8cm")
}



############################################################
# Function: create_average_exports_table
#
# Description:
# Reads average_exports_by_week.csv from the selected results
# folder and generates Table 2:
#
# Weekly Averaged CVP and SWP Exports by OMR Bin
#
# Source File:
# average_exports_by_week.csv
#
# Inputs:
# results_folder - path to selected results folder
#
# Returns:
# Formatted HTML table for inclusion in the Quarto report
############################################################

create_average_exports_table <- function(results_folder) {
  
  exports <- readr::read_csv(
    file.path(results_folder, "average_exports_by_week.csv"),
    show_col_types = FALSE
  )
  
  # Replace values with "-" for OMR = -6500
  exports <- exports |>
    dplyr::mutate(
      `CVP Exports (cfs)` =
        ifelse(`OMR Bins` == -6500, "-", format_cfs(`CVP Exports (cfs)`)),
      
      `SWP Exports (cfs)` =
        ifelse(`OMR Bins` == -6500, "-", format_cfs(`SWP Exports (cfs)`)),
      
      `Total Exports (cfs)` =
        ifelse(`OMR Bins` == -6500, "-", format_cfs(`Total Exports (cfs)`)),
      
      `CVP Exports (%)` =
        ifelse(`OMR Bins` == -6500, "-", `CVP Exports (%)`),
      
      `SWP Exports (%)` =
        ifelse(`OMR Bins` == -6500, "-", `SWP Exports (%)`)
    )
  
  # Format OMR Bin values
  exports$`OMR Bins` <- format_cfs(exports$`OMR Bins`)
  
  # Create Week labels
  exports$Week <- dplyr::case_when(
    exports$Week == "Week 1" ~ paste0(
      "<b>Week 1:</b><br>",
      fmt_date(week1_start),
      " -<br>",
      fmt_date(week1_end)
    ),
    
    exports$Week == "Week 2" ~ paste0(
      "<b>Week 2:</b><br>",
      fmt_date(week2_start),
      " -<br>",
      fmt_date(week2_end)
    ),
    
    exports$Week == "Week 3" ~ paste0(
      "<b>Week 3:</b><br>",
      fmt_date(week3_start),
      " -<br>",
      fmt_date(week3_end)
    ),
    
    TRUE ~ exports$Week
  )
  
  exports |>
    knitr::kable(
      format = "html",
      escape = FALSE,
      col.names = c(
        "Forecast Week",
        "OMR Bin<sup>3</sup><br>(cfs)",
        "CVP Exports<sup>1</sup><br>(cfs)",
        "SWP Exports<sup>2</sup><br>(cfs)",
        "Total Exports<br>(cfs)",
        "CVP Exports<br>(% of total)",
        "SWP Exports<br>(% of total)"
      ),
      align = "c"
    ) |>
    kableExtra::kable_styling(
      full_width = FALSE
    ) |>
    kableExtra::collapse_rows(
      columns = 1,
      valign = "middle"
    )
}


############################################################
# Function: insert_flow_export_figure
#
# Description:
# Inserts flow_export.png from the selected
# results folder.
############################################################

insert_flow_export_figure <- function(results_folder) {
  
  source_fig <- file.path(results_folder, "flow_export.png")
  
  if (!file.exists(source_fig)) {
    stop(paste("Figure not found:", source_fig))
  }
  
  dir.create("report_figures", showWarnings = FALSE)
  
  output_fig <- file.path("report_figures", "flow_export.png")
  
  file.copy(source_fig, output_fig, overwrite = TRUE)
  
  knitr::include_graphics(output_fig)
}


############################################################
# Function: insert_zoi_figure
#
# Description:
# Inserts a Zone of Influence figure from the selected
# results folder.
#
# Inputs:
# results_folder - selected results folder
# week_number - 1, 2, or 3
#
# Source Files:
# ZOI_0.75Contour_week1.png
# ZOI_0.75Contour_week2.png
# ZOI_0.75Contour_week3.png
############################################################

insert_zoi_figure <- function(results_folder, week_number) {
  
  source_fig <- file.path(
    results_folder,
    paste0("ZOI_0.75Contour_week", week_number, ".png")
  )
  
  if (!file.exists(source_fig)) {
    stop(paste("Figure not found:", source_fig))
  }
  
  dir.create("report_figures", showWarnings = FALSE)
  
  output_fig <- file.path(
    "report_figures",
    paste0("ZOI_0.75Contour_week", week_number, ".png")
  )
  
  file.copy(source_fig, output_fig, overwrite = TRUE)
  
  knitr::include_graphics(output_fig)
}



############################################################
# Function: create_channel_length_table
#
# Description:
# Reads ChannelLength_Data.xlsx from the selected
# results folder and generates Table 3:
#
# Proportion of DSM2 Channel Length with
# Hydrologic Alteration from Pumping
#
# The function:
# - Reads weekly channel length statistics
# - Formats channel lengths in miles
# - Converts hydrologic alteration fractions
# to percentages
# - Replaces values for OMR Bin = -6250
# with "-" to match report conventions
# - Merges Week 1, Week 2, and Week 3
# labels into single cells
#
# Source File:
# ChannelLength_Data.xlsx
#
# Inputs:
# results_folder - path to selected results folder
#
# Returns:
# Formatted HTML table for inclusion in the
# Quarto report
############################################################
create_channel_length_table <- function(results_folder) {
  
  channel <- readxl::read_excel(
    file.path(results_folder, "ChannelLength_Data.xlsx"),
    sheet = "Sheet2",
    range = "A5:H16",
    col_names = FALSE,
    col_types = c("text", "text", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric")
  )
  
  names(channel) <- c(
    "Week",
    "OMR Bin",
    "Low HA Miles",
    "Low HA Percent",
    "Medium HA Miles",
    "Medium HA Percent",
    "High HA Miles",
    "High HA Percent"
  )
  
  channel$`OMR Bin` <- format_cfs(channel$`OMR Bin`)
  channel <- channel |>
    tidyr::fill(Week, .direction = "down") |>
    dplyr::mutate(
      omit_row = `OMR Bin` %in% c("-6,250", "-6,500", "-6250", "-6500"),
      
      dplyr::across(
        c(
          `Low HA Miles`,
          `Low HA Percent`,
          `Medium HA Miles`,
          `Medium HA Percent`,
          `High HA Miles`,
          `High HA Percent`
        ),
        ~ ifelse(omit_row, "-", .)
      ),
      
      dplyr::across(
        c(`Low HA Miles`, `Medium HA Miles`, `High HA Miles`),
        ~ ifelse(. == "-", "-", sprintf("%.2f", as.numeric(.)))
      ),
      
      dplyr::across(
        c(`Low HA Percent`, `Medium HA Percent`, `High HA Percent`),
        ~ ifelse(. == "-", "-", paste0(round(as.numeric(.) * 100, 1), "%"))
      )
    ) |>
    dplyr::select(-omit_row)
  
  channel |>
    knitr::kable(
      format = "html",
      escape = FALSE,
      col.names = c(
        "Weekly Model Run",
        "OMR Bin<br>(cfs)",
        "Sum Channel Length with Low HA<br>(miles)",
        "Channel Length with Low HA<br>(%)",
        "Sum Channel Length with Medium HA<br>(miles)",
        "Channel Length with Medium HA<br>(%)",
        "Sum Channel Length with High HA<br>(miles)",
        "Channel Length with High HA<br>(%)"
      ),
      align = "c"
    ) |>
    kableExtra::kable_styling(full_width = FALSE) |>
    kableExtra::collapse_rows(columns = 1, valign = "middle")
}




############################################################
# Function: insert_proportional_channel_length_figure
#
# Description:
# Inserts proportional channel length figures from the
# selected results folder.
#
# Source Files:
# ZOI_Proportional_ChannelLength_week1.png
# ZOI_Proportional_ChannelLength_week2.png
# ZOI_Proportional_ChannelLength_week3.png
############################################################

insert_proportional_channel_length_figure <- function(results_folder, week_number) {
  
  source_fig <- file.path(
    results_folder,
    paste0("ZOI_Proportional_ChannelLength_week", week_number, ".png")
  )
  
  if (!file.exists(source_fig)) {
    stop(paste("Figure not found:", source_fig))
  }
  
  dir.create("report_figures", showWarnings = FALSE)
  
  output_fig <- file.path(
    "report_figures",
    paste0("ZOI_Proportional_ChannelLength_week", week_number, ".png")
  )
  
  file.copy(source_fig, output_fig, overwrite = TRUE)
  
  knitr::include_graphics(output_fig)
}
