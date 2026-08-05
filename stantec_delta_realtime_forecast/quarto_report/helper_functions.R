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
  
  exports <- exports |>
    dplyr::mutate(
      `CVP Exports (cfs)` = format_cfs(`CVP Exports (cfs)`),
      `SWP Exports (cfs)` = format_cfs(`SWP Exports (cfs)`),
      `Total Exports (cfs)` = format_cfs(`Total Exports (cfs)`)
    )
  
  exports$`OMR Bins` <- format_cfs(exports$`OMR Bins`)
  
  exports$Week <- dplyr::case_when(
    exports$Week == "Week 1" ~ paste0("<b>Week 1:</b><br>", fmt_date(week1_start), " -<br>", fmt_date(week1_end)),
    exports$Week == "Week 2" ~ paste0("<b>Week 2:</b><br>", fmt_date(week2_start), " -<br>", fmt_date(week2_end)),
    exports$Week == "Week 3" ~ paste0("<b>Week 3:</b><br>", fmt_date(week3_start), " -<br>", fmt_date(week3_end)),
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
    kableExtra::kable_styling(full_width = FALSE) |>
    kableExtra::collapse_rows(columns = 1, valign = "middle")
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
      dplyr::across(
        c(`Low HA Miles`, `Medium HA Miles`, `High HA Miles`),
        ~ sprintf("%.2f", as.numeric(.))
      ),
      dplyr::across(
        c(`Low HA Percent`, `Medium HA Percent`, `High HA Percent`),
        ~ paste0(round(as.numeric(.) * 100, 1), "%")
      )
    )
  
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



############################################################
# Function: insert_static_figure
#
# Description:
# Inserts a static figure that does not change between
# weekly forecasts.
#
# Static figures are stored in the Quarto report
# directory and are not associated with a specific
# results folder.
#
# Example Use:
# PTM injection and flux location maps
# Conceptual diagrams
# Reference figures
#
# Inputs:
# file_name - name of the image file located in the
# Quarto report directory
#
# Returns:
# Figure object for inclusion in the Quarto report
############################################################

insert_static_figure <- function(file_name) {
  
  if (!file.exists(file_name)) {
    stop(paste("Static figure not found:", file_name))
  }
  
  knitr::include_graphics(file_name)
}





############################################################
# Function: create_ptm_fate_table
#
# Description:
# Reads a PTM particle fate CSV file and creates a
# formatted table with weekly grouping and OMR bins.
#
# Source Files:
# NP_465(Chipps).csv
# NP_350(Cache Slough).csv
# NP_469(Jersey Point).csv
# NP_99(Old River).csv
#
# Inputs:
# results_folder - selected results folder
# file_name - CSV file name
#
# Returns:
# Formatted HTML table for Quarto report
############################################################

create_ptm_fate_table <- function(results_folder, file_name) {
  
  ptm <- readr::read_csv(
    file.path(results_folder, file_name),
    skip = 3,
    col_names = FALSE,
    col_types = readr::cols(.default = "c"),
    show_col_types = FALSE
  )
  
  ptm <- ptm[, 1:7]
  
  names(ptm) <- c(
    "OMR Bin",
    "Past Chipps",
    "Upstream of Decker",
    "Unresolved in Central Delta",
    "Unresolved in OMR corridor",
    "CVP Entrainment",
    "SWP Entrainment"
  )
  
  ptm <- ptm[!is.na(ptm$`OMR Bin`) & ptm$`OMR Bin` != "", ]
  
  ptm$Week <- rep(
    c(
      paste0("<b>Week 1:</b><br>", fmt_date(week1_start), " -<br>", fmt_date(week1_end)),
      paste0("<b>Week 2:</b><br>", fmt_date(week2_start), " -<br>", fmt_date(week2_end)),
      paste0("<b>Week 3:</b><br>", fmt_date(week3_start), " -<br>", fmt_date(week3_end))
    ),
    each = 4
  )
  
  ptm$`OMR Bin` <- format_cfs(ptm$`OMR Bin`)
  
  ptm <- ptm[, c(
    "Week",
    "OMR Bin",
    "Past Chipps",
    "Upstream of Decker",
    "Unresolved in Central Delta",
    "Unresolved in OMR corridor",
    "CVP Entrainment",
    "SWP Entrainment"
  )]
  
  knitr::kable(
    ptm,
    format = "html",
    escape = FALSE,
    col.names = c(
      "Forecast Week",
      "OMR Flow Bin",
      "Past Chipps",
      "Upstream of Decker",
      "Unresolved in Central Delta",
      "Unresolved in OMR corridor",
      "CVP Entrainment",
      "SWP Entrainment"
    ),
    align = "c"
  ) |>
    kableExtra::kable_styling(full_width = FALSE) |>
    kableExtra::collapse_rows(columns = 1, valign = "middle")
}

############################################################
# Function: insert_ptm_figure
#
# Description:
# Inserts a single PTM result figure from the selected
# results folder into the Quarto report.
#
# The function copies the figure into the local
# report_figures folder so the rendered HTML can find
# and display the image correctly.
#
# Inputs:
# results_folder - path to selected results folder
# figure_name - name of the PNG figure file
#
# Returns:
# Figure object for inclusion in the Quarto report
############################################################

insert_ptm_figure <- function(results_folder, figure_name) {
  
  source_fig <- file.path(results_folder, figure_name)
  
  if (!file.exists(source_fig)) {
    stop(paste("Figure not found:", source_fig))
  }
  
  dir.create("report_figures", showWarnings = FALSE)
  
  output_fig <- file.path("report_figures", figure_name)
  
  file.copy(source_fig, output_fig, overwrite = TRUE)
  
  knitr::include_graphics(output_fig)
}


############################################################
# Function: create_eco_ptm_table
#
# Description:
# Reads survival_combined.csv from the selected results
# folder and generates ECO-PTM tables.
#
# The function can generate:
#
# 1. Route ratio table
# - Uses route ratio columns from survival_combined.csv
# - Displays values as decimals rounded to two digits
# - Example: 0.09
#
# 2. Route-specific survival table
# - Uses survival columns from survival_combined.csv
# - Displays values as percentages rounded to whole numbers
# - Example: 40%
#
# For both tables, the -6,500 OMR Flow Bin row is added
# manually and values are shown as "-".
#
# Inputs:
# results_folder - path to selected results folder
# csv_file - name of ECO-PTM CSV file
# table_type - "ratio" or "survival"
#
# Source File:
# survival_combined.csv
#
# Returns:
# Formatted HTML table for inclusion in the Quarto report
############################################################

create_eco_ptm_table <- function(results_folder,
                                 csv_file = "survival_combined.csv",
                                 table_type = c("ratio", "survival")) {
  
  table_type <- match.arg(table_type)
  
  df <- readr::read_csv(
    file.path(results_folder, csv_file),
    show_col_types = FALSE
  )
  
  df <- df |>
    dplyr::filter(Model_Run %in% c("A", "B", "C", "D"))
  
  omr_bins <- c("-6,500", "-5,000", "-3,500", "-2,000")
  
  fmt_ratio <- function(x) {
    sprintf("%.2f", as.numeric(x))
  }
  
  fmt_percent <- function(x) {
    paste0(round(as.numeric(x) * 100), "%")
  }
  
  if (table_type == "ratio") {
    
    out <- data.frame(
      `OMR Flow Bin (cfs)` = omr_bins,
      `Sutter Slough Route` = fmt_ratio(df$SUT_RATIO),
      `Steamboat Slough Route` = fmt_ratio(df$STM_RATIO),
      `Sacramento River (SS) Route` = fmt_ratio(df$SACR_SS_RATIO),
      `Sacramento River (GEO) Route` = fmt_ratio(df$SACR_GEO_RATIO),
      `Georgiana Slough Route` = fmt_ratio(df$GEO_RATIO),
      check.names = FALSE
    )
    
  } else {
    
    out <- data.frame(
      `OMR Flow Bin (cfs)` = omr_bins,
      `Sutter Slough Route` = fmt_percent(df$SUT_SUV),
      `Steamboat Slough Route` = fmt_percent(df$STM_SUV),
      `Sacramento River Route` = fmt_percent(df$SAC_SUV),
      `Georgiana Slough Route` = fmt_percent(df$GEO_SUV),
      `All Routes Combined` = fmt_percent(df$Combined_suv),
      check.names = FALSE
    )
  }
  
  out |>
    knitr::kable(
      format = "html",
      escape = FALSE,
      align = "c"
    ) |>
    kableExtra::kable_styling(full_width = FALSE)
}


############################################################
# Function: create_static_csv_table
#
# Description:
# Reads a static CSV file stored in the Quarto report
# folder and creates a formatted HTML table.
#
# Static tables are used for content that does not change
# between weekly reports.
#
# Inputs:
# file_name - name of static CSV file
#
# Returns:
# Formatted HTML table for Quarto report
############################################################

create_static_csv_table <- function(file_name) {
  
  table_data <- readr::read_csv(
    file_name,
    show_col_types = FALSE
  )
  
  table_data |>
    knitr::kable(
      format = "html",
      escape = FALSE,
      align = "c"
    ) |>
    kableExtra::kable_styling(
      full_width = FALSE
    )
}



############################################################
# Function: create_lfs_entrainment_table
#
# Description:
# Reads one weekly LFS PP entrainment CSV file and creates
# the LFS entrainment estimate table for Section A.6.
#
# The function also reads average_exports_by_week.csv to add
# OMR Flow Bin and Combined Exports columns.
#
# Source Files:
# LFS_PP_Week_1_Entrainment.csv
# LFS_PP_Week_2_Entrainment.csv
# LFS_PP_Week_3_Entrainment.csv
# average_exports_by_week.csv
#
# Inputs:
# results_folder - selected results folder
# week_number - 1, 2, or 3
#
# Returns:
# Formatted HTML table for Quarto report
############################################################

create_lfs_entrainment_table <- function(results_folder, week_number) {
  
  lfs_file <- file.path(
    results_folder,
    paste0("LFS_PP_Week_", week_number, "_Entrainment.csv")
  )
  
  lfs <- readr::read_csv(
    lfs_file,
    col_types = readr::cols(.default = "c"),
    show_col_types = FALSE
  )
  
  lfs <- lfs[, 1:10]
  
  names(lfs) <- c(
    "Metric",
    "West",
    "Suisun",
    "Sacramento/North Delta",
    "Lower San Joaquin",
    "Lower Sacramento",
    "South Delta",
    "East",
    "Delta-wide Total (#)",
    "Delta-wide Total (%)"
  )
  
  exports <- readr::read_csv(
    file.path(results_folder, "average_exports_by_week.csv"),
    show_col_types = FALSE
  )
  
  week_label <- paste0("Week ", week_number)
  
  exports_week <- exports |>
    dplyr::filter(Week == week_label) |>
    dplyr::mutate(
      `OMR Bins` = format_cfs(`OMR Bins`),
      `Total Exports (cfs)` = format_cfs(`Total Exports (cfs)`)
    )
  
  omr_bins <- exports_week$`OMR Bins`
  total_exports <- exports_week$`Total Exports (cfs)`
  
  output <- data.frame(
    `OMR<br>(cfs)` = c("&nbsp;", omr_bins, omr_bins),
    `Combined<br>Exports<br>(cfs)` = c("&nbsp;", total_exports, total_exports),
    Metric = lfs$Metric,
    West = lfs$West,
    Suisun = lfs$Suisun,
    `Sacramento/<br>North Delta` = lfs$`Sacramento/North Delta`,
    `Lower<br>San Joaquin` = lfs$`Lower San Joaquin`,
    `Lower<br>Sacramento` = lfs$`Lower Sacramento`,
    `South<br>Delta` = lfs$`South Delta`,
    East = lfs$East,
    `Delta-wide<br>Total (#)` = lfs$`Delta-wide Total (#)`,
    `Delta-wide<br>Total (%)` = lfs$`Delta-wide Total (%)`,
    check.names = FALSE
  )
  
  output |>
    knitr::kable(
      format = "html",
      escape = FALSE,
      align = "c"
    ) |>
    kableExtra::add_header_above(
      c(
        " " = 3,
        "Region" = 7,
        " " = 2
      ),
      bold = TRUE
    ) |>
    kableExtra::kable_styling(full_width = FALSE) |>
    kableExtra::collapse_rows(columns = 1:2, valign = "middle")
}