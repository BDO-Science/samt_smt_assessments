############################################################
# Delta Real-Time Forecast Summary Helper Functions
############################################################

############################################################
# Function: get_latest_results_folder
#
# Description:
# Finds the most recent folder matching YYYYMMDD_results.
############################################################

get_latest_results_folder <- function(results_root = "..") {
  
  result_folders <- list.dirs(
    path = results_root,
    recursive = FALSE,
    full.names = TRUE
  )
  
  result_folders <- result_folders[
    grepl("^\\d{8}_results$", basename(result_folders))
  ]
  
  if (length(result_folders) == 0) {
    stop(
      paste(
        "No results folders matching YYYYMMDD_results were found in:",
        normalizePath(results_root, winslash = "/", mustWork = FALSE)
      )
    )
  }
  
  folder_dates <- as.Date(
    sub("_results$", "", basename(result_folders)),
    format = "%Y%m%d"
  )
  
  valid <- !is.na(folder_dates)
  
  if (!any(valid)) {
    stop("Results folders were found, but none contained a valid date.")
  }
  
  result_folders[valid][which.max(folder_dates[valid])]
}


############################################################
# Function: get_selected_results_folder
#
# Description:
# Reads results_date from report_config.yml and returns
# the selected results folder. If results_date is blank,
# the latest results folder is selected.
############################################################

get_selected_results_folder <- function(
    results_root = "..",
    config_file = "report_config.yml"
) {
  
  if (!requireNamespace("yaml", quietly = TRUE)) {
    stop(
      "The yaml package is required. Run install.packages('yaml')."
    )
  }
  
  if (!file.exists(config_file)) {
    stop(
      paste("Configuration file not found:", config_file)
    )
  }
  
  config <- yaml::read_yaml(config_file)
  results_date <- config$results_date
  
  if (is.null(results_date) ||
      is.na(results_date) ||
      trimws(as.character(results_date)) == "") {
    
    return(get_latest_results_folder(results_root))
  }
  
  results_date <- trimws(as.character(results_date))
  
  if (!grepl("^\\d{8}$", results_date)) {
    stop(
      paste(
        "results_date must use YYYYMMDD format. Current value:",
        results_date
      )
    )
  }
  
  selected_folder <- file.path(
    results_root,
    paste0(results_date, "_results")
  )
  
  if (!dir.exists(selected_folder)) {
    stop(
      paste(
        "Selected results folder does not exist:",
        normalizePath(
          selected_folder,
          winslash = "/",
          mustWork = FALSE
        )
      )
    )
  }
  
  selected_folder
}



############################################################
# Function: format_cfs
#
# Description:
# Formats flow values with a thousands separator and no
# decimal places.
#
# Existing nonnumeric values such as "-" are retained.
#
# Example:
# 11850 becomes "11,850"
############################################################

format_cfs <- function(x) {
  
  x_character <- trimws(as.character(x))
  
  x_numeric <- suppressWarnings(
    as.numeric(
      gsub(",", "", x_character)
    )
  )
  
  formatted <- ifelse(
    is.na(x_numeric),
    x_character,
    format(
      round(x_numeric, 0),
      big.mark = ",",
      scientific = FALSE,
      trim = TRUE
    )
  )
  
  formatted
}


############################################################
# Function: format_percent_auto
#
# Description:
# Formats percentage values while accommodating:
#
# - Decimal fractions such as 0.83
# - Whole percentages such as 83
# - Existing percentage text such as "83%"
# - Nonnumeric values such as "-"
#
# Examples:
# 0.83 becomes "83%"
# 83 becomes "83%"
# "83%" remains "83%"
############################################################

format_percent_auto <- function(x) {
  
  x_character <- trimws(as.character(x))
  
  already_percent <- grepl(
    "%$",
    x_character
  )
  
  x_numeric <- suppressWarnings(
    as.numeric(
      gsub(
        "%|,",
        "",
        x_character
      )
    )
  )
  
  result <- x_character
  
  valid_numeric <- !is.na(x_numeric)
  
  decimal_fraction <- (
    valid_numeric &
      !already_percent &
      abs(x_numeric) <= 1
  )
  
  whole_percent <- (
    valid_numeric &
      !already_percent &
      abs(x_numeric) > 1
  )
  
  result[decimal_fraction] <- paste0(
    round(
      x_numeric[decimal_fraction] * 100,
      1
    ),
    "%"
  )
  
  result[whole_percent] <- paste0(
    round(
      x_numeric[whole_percent],
      1
    ),
    "%"
  )
  
  result
}


############################################################
# Function: make_scrollable_table
#
# Description:
# Wraps a generated HTML table in a horizontally scrollable
# container and ensures the table HTML is rendered rather
# than printed as text.
############################################################

make_scrollable_table <- function(table_html) {
  
  htmltools::div(
    class = "table-scroll",
    htmltools::HTML(
      as.character(table_html)
    )
  )
}

############################################################
# Function: create_zoi_table
#
# Description:
# Reads zoi_bins.csv from the selected results folder and
# creates the weekly averaged forecast flow and flow-bin
# table.
#
# The function:
# - Confirms that the source file exists
# - Reads all columns as character values
# - Uses the first six columns in the source file
# - Formats flow values with thousands separators
# - Places the output table in a horizontally scrollable
# container
#
# Source File:
# zoi_bins.csv
#
# Inputs:
# results_folder - selected results folder
#
# Returns:
# Formatted, horizontally scrollable HTML table
############################################################

create_zoi_table <- function(results_folder) {
  
  file_path <- file.path(
    results_folder,
    "zoi_bins.csv"
  )
  
  if (!file.exists(file_path)) {
    
    stop(
      paste(
        "Required file not found:",
        normalizePath(
          file_path,
          winslash = "/",
          mustWork = FALSE
        )
      )
    )
  }
  
  zoi_bins <- readr::read_csv(
    file_path,
    show_col_types = FALSE,
    col_types = readr::cols(
      .default = readr::col_character()
    )
  )
  
  if (ncol(zoi_bins) < 6) {
    
    stop(
      paste0(
        "zoi_bins.csv contains ",
        ncol(zoi_bins),
        " columns, but at least 6 are required. ",
        "Detected columns: ",
        paste(
          names(zoi_bins),
          collapse = ", "
        )
      )
    )
  }
  
  zoi_bins <- zoi_bins[, 1:6]
  
  names(zoi_bins) <- c(
    "Forecast Week",
    "Sacramento River at Freeport",
    "Sac Flow Bin",
    "San Joaquin River at Vernalis",
    "SJR Flow Bin",
    "Delta Inflow Bin"
  )
  
  zoi_bins <- zoi_bins |>
    dplyr::mutate(
      `Sacramento River at Freeport` =
        format_cfs(
          `Sacramento River at Freeport`
        ),
      
      `San Joaquin River at Vernalis` =
        format_cfs(
          `San Joaquin River at Vernalis`
        )
    )
  
  table_html <- zoi_bins |>
    knitr::kable(
      format = "html",
      escape = FALSE,
      align = "c",
      col.names = c(
        "Forecast Week",
        "Sacramento River at Freeport<br>(cfs)",
        "Sac Flow Bin",
        "San Joaquin River at Vernalis<br>(cfs)",
        "SJR Flow Bin",
        "Delta Inflow Bin"
      )
    ) |>
    kableExtra::kable_styling(
      full_width = FALSE,
      position = "center",
      bootstrap_options = c(
        "striped",
        "hover",
        "condensed"
      )
    )
  
  make_scrollable_table(table_html)
}


############################################################
# Function: create_average_exports_table
#
# Description:
# Reads average_exports_by_week.csv from the selected
# results folder and creates the weekly averaged CVP and
# SWP exports table.
#
# The function:
# - Confirms that the source file exists
# - Reads all columns as character values
# - Uses the first seven columns in the source file
# - Formats OMR and export values with thousands separators
# - Formats percentage values
# - Creates dynamic Week 1, Week 2, and Week 3 date labels
# - Merges the repeated weekly labels
# - Places the table in a horizontally scrollable container
#
# Source File:
# average_exports_by_week.csv
#
# Inputs:
# results_folder - selected results folder
#
# Returns:
# Formatted, horizontally scrollable HTML table
############################################################

create_average_exports_table <- function(results_folder) {
  
  file_path <- file.path(
    results_folder,
    "average_exports_by_week.csv"
  )
  
  if (!file.exists(file_path)) {
    
    stop(
      paste(
        "Required file not found:",
        normalizePath(
          file_path,
          winslash = "/",
          mustWork = FALSE
        )
      )
    )
  }
  
  exports <- readr::read_csv(
    file_path,
    show_col_types = FALSE,
    col_types = readr::cols(
      .default = readr::col_character()
    )
  )
  
  if (ncol(exports) < 7) {
    
    stop(
      paste0(
        "average_exports_by_week.csv contains ",
        ncol(exports),
        " columns, but at least 7 are required. ",
        "Detected columns: ",
        paste(
          names(exports),
          collapse = ", "
        )
      )
    )
  }
  
  exports <- exports[, 1:7]
  
  names(exports) <- c(
    "Week",
    "OMR Bin",
    "CVP Exports",
    "SWP Exports",
    "Total Exports",
    "CVP Percent",
    "SWP Percent"
  )
  
  exports <- exports |>
    dplyr::mutate(
      `OMR Bin` =
        format_cfs(`OMR Bin`),
      
      `CVP Exports` =
        format_cfs(`CVP Exports`),
      
      `SWP Exports` =
        format_cfs(`SWP Exports`),
      
      `Total Exports` =
        format_cfs(`Total Exports`),
      
      `CVP Percent` =
        format_percent_auto(`CVP Percent`),
      
      `SWP Percent` =
        format_percent_auto(`SWP Percent`)
    )
  
  exports$Week <- dplyr::case_when(
    
    trimws(exports$Week) == "Week 1" ~ paste0(
      "<b>Week 1:</b><br>",
      fmt_date(week1_start),
      " –<br>",
      fmt_date(week1_end)
    ),
    
    trimws(exports$Week) == "Week 2" ~ paste0(
      "<b>Week 2:</b><br>",
      fmt_date(week2_start),
      " –<br>",
      fmt_date(week2_end)
    ),
    
    trimws(exports$Week) == "Week 3" ~ paste0(
      "<b>Week 3:</b><br>",
      fmt_date(week3_start),
      " –<br>",
      fmt_date(week3_end)
    ),
    
    TRUE ~ exports$Week
  )
  
  table_html <- exports |>
    knitr::kable(
      format = "html",
      escape = FALSE,
      align = "c",
      col.names = c(
        "Forecast Week",
        "OMR Bin<br>(cfs)",
        "CVP Exports<br>(cfs)",
        "SWP Exports<br>(cfs)",
        "Total Exports<br>(cfs)",
        "CVP Exports<br>(% of total)",
        "SWP Exports<br>(% of total)"
      )
    ) |>
    kableExtra::kable_styling(
      full_width = FALSE,
      position = "center",
      bootstrap_options = c(
        "striped",
        "hover",
        "condensed"
      )
    ) |>
    kableExtra::collapse_rows(
      columns = 1,
      valign = "middle"
    )
  
  make_scrollable_table(table_html)
}


############################################################
# Function: insert_flow_export_figure
#
# Description:
# Inserts flow_export.png from the selected results folder.
#
# The image is read directly from the weekly results folder.
# When embed-resources: true is included in the QMD YAML,
# the image is embedded in the final HTML file.
#
# Source File:
# flow_export.png
#
# Inputs:
# results_folder - selected results folder
#
# Returns:
# Figure object for inclusion in the Quarto report
############################################################

insert_flow_export_figure <- function(results_folder) {
  
  figure_path <- file.path(
    results_folder,
    "flow_export.png"
  )
  
  if (!file.exists(figure_path)) {
    
    stop(
      paste(
        "Required figure not found:",
        normalizePath(
          figure_path,
          winslash = "/",
          mustWork = FALSE
        )
      )
    )
  }
  
  normalized_figure_path <- normalizePath(
    figure_path,
    winslash = "/",
    mustWork = TRUE
  )
  
  knitr::include_graphics(
    normalized_figure_path
  )
}


############################################################
# Function: insert_weekly_text
#
# Description:
# Reads a Markdown-formatted weekly text file from the
# selected results folder and inserts it into the report.
############################################################

insert_weekly_text <- function(results_folder, file_name) {
  
  text_path <- file.path(
    results_folder,
    file_name
  )
  
  if (!file.exists(text_path)) {
    stop(
      paste(
        "Required weekly text file not found:",
        normalizePath(
          text_path,
          winslash = "/",
          mustWork = FALSE
        )
      )
    )
  }
  
  text_lines <- readLines(
    text_path,
    warn = FALSE,
    encoding = "UTF-8"
  )
  
  cat(
    paste(
      text_lines,
      collapse = "\n\n"
    )
  )
}

############################################################
# Function: make_scrollable_table
#
# Description:
# Places a generated HTML table inside a horizontally
# scrollable container. The HTML is rendered as a table
# rather than printed as plain text.
############################################################

make_scrollable_table <- function(table_html) {
  
  htmltools::div(
    class = "table-scroll",
    htmltools::HTML(
      as.character(table_html)
    )
  )
}

############################################################
# Function: insert_interactive_figure
#
# Description:
# Embeds a self-contained interactive Plotly HTML figure
# inside the Quarto report.
#
# The function modifies the source HTML so that:
# - The figure fits the report width
# - No horizontal scrollbar appears
# - Plotly recalculates its width after loading
# - Standard PTM figures are not cut off on the right
# - Proportional channel-length subplot titles are wrapped
# onto two lines to prevent overlap
# - Quarto generates an automatic figure number and caption
#
# Inputs:
# results_folder - selected weekly results folder
# file_name - exact HTML file name
# figure_id - unique figure ID beginning with "fig-"
# figure_title - caption shown under the figure
# height - iframe height in pixels
# figure_type - "standard" for PTM/ZOI figures or
# "proportional" for proportional-channel
# length figures
############################################################

############################################################
# Function: add_leaflet_reset_control
#
# Description:
# Adds a Reset Map button to embedded Leaflet HTML maps.
#
# Clicking the button reloads only the embedded map HTML,
# returning the Leaflet map to its original center, zoom,
# and layer configuration.
#
# The function only modifies HTML files that contain
# Leaflet content. Plotly and other HTML figures are left
# unchanged.
############################################################

add_leaflet_reset_control <- function(html_content) {
  
  ##########################################################
  # Only modify Leaflet HTML files
  ##########################################################
  
  is_leaflet <- grepl(
    "leaflet",
    html_content,
    ignore.case = TRUE
  )
  
  if (!is_leaflet) {
    return(html_content)
  }
  
  ##########################################################
  # CSS for Reset Map button
  ##########################################################
  
  reset_css <- paste0(
    "<style>",
    
    ".leaflet-reset-map-button {",
    " position: fixed !important;",
    " top: 78px !important;",
    " left: 10px !important;",
    " z-index: 999999 !important;",
    " width: 34px !important;",
    " height: 34px !important;",
    " padding: 0 !important;",
    " margin: 0 !important;",
    " border: 2px solid rgba(0,0,0,0.2) !important;",
    " border-radius: 4px !important;",
    " background: #ffffff !important;",
    " color: #333333 !important;",
    " font-family: Arial, sans-serif !important;",
    " font-size: 20px !important;",
    " font-weight: bold !important;",
    " line-height: 30px !important;",
    " text-align: center !important;",
    " cursor: pointer !important;",
    " box-sizing: border-box !important;",
    " box-shadow: none !important;",
    "}",
    
    ".leaflet-reset-map-button:hover {",
    " background: #f4f4f4 !important;",
    "}",
    
    "</style>"
  )
  
  ##########################################################
  # Reset button
  #
  # Reloading the embedded HTML returns the map to the
  # original state stored in the source Leaflet file.
  ##########################################################
  
  reset_button <- paste0(
    "<button ",
    "class='leaflet-reset-map-button' ",
    "type='button' ",
    "title='Reset Map' ",
    "aria-label='Reset Map' ",
    "onclick='window.location.reload(); return false;'>",
    "&#8634;",
    "</button>"
  )
  
  ##########################################################
  # Insert CSS into <head>
  ##########################################################
  
  if (grepl(
    "</head>",
    html_content,
    ignore.case = TRUE
  )) {
    
    html_content <- sub(
      "</head>",
      paste0(
        reset_css,
        "</head>"
      ),
      html_content,
      ignore.case = TRUE
    )
    
  } else {
    
    html_content <- paste0(
      reset_css,
      html_content
    )
  }
  
  ##########################################################
  # Insert Reset Map button before </body>
  ##########################################################
  
  if (grepl(
    "</body>",
    html_content,
    ignore.case = TRUE
  )) {
    
    html_content <- sub(
      "</body>",
      paste0(
        reset_button,
        "</body>"
      ),
      html_content,
      ignore.case = TRUE
    )
    
  } else {
    
    html_content <- paste0(
      html_content,
      reset_button
    )
  }
  
  html_content
}





insert_interactive_figure <- function(
    results_folder,
    file_name,
    figure_id,
    figure_title,
    height = 700,
    figure_type = c("standard", "proportional")
) {
  
  figure_type <- match.arg(figure_type)
  
  html_path <- file.path(
    results_folder,
    file_name
  )
  
  if (!file.exists(html_path)) {
    stop(
      paste(
        "Interactive HTML file not found:",
        normalizePath(
          html_path,
          winslash = "/",
          mustWork = FALSE
        )
      )
    )
  }
  
  if (!grepl("^fig-", figure_id)) {
    stop(
      paste0(
        "figure_id must begin with 'fig-'. Current value: ",
        figure_id
      )
    )
  }
  
  html_content <- paste(
    readLines(
      html_path,
      warn = FALSE,
      encoding = "UTF-8"
    ),
    collapse = "\n"
  )
  
  # Add Reset Map control when this HTML contains a Leaflet map.
  html_content <- add_leaflet_reset_control(html_content)
  
  ##########################################################
  # CSS inserted into the interactive HTML
  ##########################################################
  
  responsive_css <- paste0(
    "<style>",
    "html, body {",
    " width: 100% !important;",
    " max-width: 100% !important;",
    " margin: 0 !important;",
    " padding: 0 !important;",
    " overflow-x: hidden !important;",
    " box-sizing: border-box !important;",
    "}",
    "body > div,",
    ".html-widget,",
    ".plotly,",
    ".plot-container,",
    ".svg-container,",
    ".plotly-graph-div {",
    " width: 100% !important;",
    " max-width: 100% !important;",
    " box-sizing: border-box !important;",
    "}",
    "</style>"
  )
  
  ##########################################################
  # JavaScript for standard PTM/ZOI figures
  ##########################################################
  
  standard_script <- paste0(
    "<script>",
    "(function() {",
    
    "function resizeStandardPlots() {",
    
    " var availableWidth = Math.max(",
    " document.documentElement.clientWidth - 10,",
    " 300",
    " );",
    
    " var plots = document.querySelectorAll('.plotly-graph-div');",
    
    " plots.forEach(function(plot) {",
    
    " if (!window.Plotly) return;",
    
    " try {",
    
    " window.Plotly.relayout(plot, {",
    " autosize: true,",
    " width: availableWidth,",
    " 'title.text': '',",
    " 'margin.l': 75,",
    " 'margin.r': 35,",
    " 'margin.t': 30,",
    " 'margin.b': 110",
    " });",
    
    " window.Plotly.Plots.resize(plot);",
    
    " } catch (error) {",
    " console.warn('Plotly resize warning:', error);",
    " }",
    
    " });",
    
    "}",
    
    "window.addEventListener('load', function() {",
    " setTimeout(resizeStandardPlots, 100);",
    " setTimeout(resizeStandardPlots, 500);",
    " setTimeout(resizeStandardPlots, 1200);",
    "});",
    
    "window.addEventListener('resize', resizeStandardPlots);",
    
    "})();",
    "</script>"
  )
  
  ##########################################################
  # JavaScript for proportional-channel-length figures
  ##########################################################
  
  proportional_script <- paste0(
    "<script>",
    "(function() {",
    
    "function resizeProportionalPlots() {",
    
    " var availableWidth = Math.max(",
    " document.documentElement.clientWidth - 10,",
    " 300",
    " );",
    
    " var plots = document.querySelectorAll('.plotly-graph-div');",
    
    " plots.forEach(function(plot) {",
    
    " if (!window.Plotly) return;",
    
    " try {",
    
    " var updates = {",
    " autosize: true,",
    " width: availableWidth,",
    " height: 520,",
    " 'margin.l': 80,",
    " 'margin.r': 30,",
    " 'margin.t': 145,",
    " 'margin.b': 65,",
    " 'font.size': 14,",
    " 'legend.font.size': 13",
    " };",
    
    
    " var annotations = ",
    " (plot.layout && plot.layout.annotations) || [];",
    
    " annotations.forEach(function(annotation, index) {",
    
    " var annotationText = annotation.text || '';",
    
    " var isLowHeading = ",
    " annotationText.indexOf('Low hydrologic influence') !== -1;",
    
    " var isMediumHeading = ",
    " annotationText.indexOf('Medium hydrologic influence') !== -1;",
    
    " var isHighHeading = ",
    " annotationText.indexOf('High hydrologic influence') !== -1;",
    
    " var isSubplotHeading = ",
    " isLowHeading || isMediumHeading || isHighHeading;",
    
    " annotationText = annotationText",
    " .replace(",
    " 'Low hydrologic influence',",
    " 'Low hydrologic<br>influence'",
    " )",
    " .replace(",
    " 'Medium hydrologic influence',",
    " 'Medium hydrologic<br>influence'",
    " )",
    " .replace(",
    " 'High hydrologic influence',",
    " 'High hydrologic<br>influence'",
    " );",
    
    " updates['annotations[' + index + '].text'] = ",
    " annotationText;",
    
    " if (isSubplotHeading) {",
    
    " updates['annotations[' + index + '].y'] = 0.94;",
    " updates['annotations[' + index + '].yanchor'] = 'bottom';",
    " updates['annotations[' + index + '].font.size'] = 13;",
    
    " if (isLowHeading) {",
    " updates['annotations[' + index + '].x'] = 0.14;",
    " }",
    
    " if (isMediumHeading) {",
    " updates['annotations[' + index + '].x'] = 0.50;",
    " }",
    
    " if (isHighHeading) {",
    " updates['annotations[' + index + '].x'] = 0.84;",
    " }",
    
    " }",
    
    " });",
    
    " window.Plotly.relayout(plot, updates);",
    " window.Plotly.Plots.resize(plot);",
    
    " } catch (error) {",
    " console.warn('Plotly proportional resize warning:', error);",
    " }",
    
    " });",
    
    "}",
    
    "window.addEventListener('load', function() {",
    " setTimeout(resizeProportionalPlots, 100);",
    " setTimeout(resizeProportionalPlots, 500);",
    " setTimeout(resizeProportionalPlots, 1200);",
    "});",
    
    "window.addEventListener(",
    " 'resize',",
    " resizeProportionalPlots",
    ");",
    
    "})();",
    "</script>"
  )
  
  plot_script <- if (figure_type == "proportional") {
    proportional_script
  } else {
    standard_script
  }
  
  ##########################################################
  # Add CSS to the source HTML
  ##########################################################
  
  if (grepl("</head>", html_content, ignore.case = TRUE)) {
    
    html_content <- sub(
      "</head>",
      paste0(
        responsive_css,
        "</head>"
      ),
      html_content,
      ignore.case = TRUE
    )
    
  } else {
    
    html_content <- paste0(
      responsive_css,
      html_content
    )
  }
  
  ##########################################################
  # Add JavaScript to the source HTML
  ##########################################################
  
  if (grepl("</body>", html_content, ignore.case = TRUE)) {
    
    html_content <- sub(
      "</body>",
      paste0(
        plot_script,
        "</body>"
      ),
      html_content,
      ignore.case = TRUE
    )
    
  } else {
    
    html_content <- paste0(
      html_content,
      plot_script
    )
  }
  
  ##########################################################
  # Create responsive iframe
  ##########################################################
  
  figure_html <- htmltools::tags$div(
    class = "interactive-responsive-wrapper",
    
    htmltools::tags$iframe(
      class = "interactive-responsive-iframe",
      srcdoc = htmltools::HTML(html_content),
      width = "100%",
      height = paste0(height, "px"),
      frameborder = "0",
      scrolling = "no",
      loading = "lazy",
      style = paste0(
        "display:block;",
        "width:100%;",
        "max-width:100%;",
        "height:", height, "px;",
        "border:1px solid #d9d9d9;",
        "border-radius:4px;",
        "background:#ffffff;",
        "overflow:hidden;"
      )
    )
  )
  
  ##########################################################
  # Create a Quarto-numbered figure
  ##########################################################
  
  cat(
    paste0(
      "\n::: {#",
      figure_id,
      "}\n\n",
      as.character(figure_html),
      "\n\n",
      figure_title,
      "\n\n:::\n"
    )
  )
  
  invisible(NULL)
}





############################################################
# Function: create_channel_length_table
#
# Description:
# Reads ChannelLength_Data.xlsx and creates the weekly
# channel-length table.
#
# The function automatically removes an OMR scenario row
# if all six associated result cells are empty or missing.
#
# Therefore, a scenario such as -6,500 is omitted only when
# it has no results. If results become available in another
# week, the scenario is included automatically.
#
# Source File:
# ChannelLength_Data.xlsx
#
# Expected Location:
# Sheet2, cells A5:H16
#
# Inputs:
# results_folder - selected weekly results folder
#
# Returns:
# Formatted, horizontally scrollable HTML table
############################################################

create_channel_length_table <- function(results_folder) {
  
  file_path <- file.path(
    results_folder,
    "ChannelLength_Data.xlsx"
  )
  
  if (!file.exists(file_path)) {
    stop(
      paste(
        "Required Channel Length file not found:",
        normalizePath(
          file_path,
          winslash = "/",
          mustWork = FALSE
        )
      )
    )
  }
  
  channel <- readxl::read_excel(
    path = file_path,
    sheet = "Sheet2",
    range = "A5:H16",
    col_names = FALSE,
    col_types = c(
      "text",
      "text",
      "numeric",
      "numeric",
      "numeric",
      "numeric",
      "numeric",
      "numeric"
    )
  )
  
  if (ncol(channel) != 8) {
    stop(
      paste0(
        "ChannelLength_Data.xlsx returned ",
        ncol(channel),
        " columns from Sheet2!A5:H16. ",
        "Eight columns were expected."
      )
    )
  }
  
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
  
  # Fill merged Week cells downward.
  channel <- channel |>
    tidyr::fill(
      Week,
      .direction = "down"
    )
  
  result_columns <- c(
    "Low HA Miles",
    "Low HA Percent",
    "Medium HA Miles",
    "Medium HA Percent",
    "High HA Miles",
    "High HA Percent"
  )
  
  # Keep a row only when at least one result cell contains
  # an actual value.
  has_result <- apply(
    channel[, result_columns, drop = FALSE],
    1,
    function(row_values) {
      
      cleaned_values <- trimws(
        as.character(row_values)
      )
      
      any(
        !is.na(row_values) &
          cleaned_values != ""
      )
    }
  )
  
  channel <- channel[
    has_result,
    ,
    drop = FALSE
  ]
  
  if (nrow(channel) == 0) {
    stop(
      paste(
        "No populated Channel Length result rows were found in:",
        file_path
      )
    )
  }
  
  channel <- channel |>
    dplyr::mutate(
      `OMR Bin` = format_cfs(`OMR Bin`),
      
      dplyr::across(
        c(
          `Low HA Miles`,
          `Medium HA Miles`,
          `High HA Miles`
        ),
        ~ ifelse(
          is.na(.),
          "",
          sprintf("%.2f", as.numeric(.))
        )
      ),
      
      dplyr::across(
        c(
          `Low HA Percent`,
          `Medium HA Percent`,
          `High HA Percent`
        ),
        ~ ifelse(
          is.na(.),
          "",
          paste0(
            round(as.numeric(.) * 100, 1),
            "%"
          )
        )
      )
    )
  
  table_html <- channel |>
    knitr::kable(
      format = "html",
      escape = FALSE,
      align = "c",
      col.names = c(
        "Weekly Model Run",
        "OMR Bin<br>(cfs)",
        "Sum Channel Length with Low HA<br>(miles)",
        "Channel Length with Low HA<br>(%)",
        "Sum Channel Length with Medium HA<br>(miles)",
        "Channel Length with Medium HA<br>(%)",
        "Sum Channel Length with High HA<br>(miles)",
        "Channel Length with High HA<br>(%)"
      )
    ) |>
    kableExtra::kable_styling(
      full_width = FALSE,
      position = "center",
      bootstrap_options = c(
        "striped",
        "hover",
        "condensed"
      )
    ) |>
    kableExtra::collapse_rows(
      columns = 1,
      valign = "middle"
    )
  
  make_scrollable_table(table_html)
}




############################################################
# Function: insert_static_figure
#
# Description:
# Inserts a static figure stored in the quarto_report
# directory. Static figures do not change between weekly
# results folders.
#
# Inputs:
# file_name - exact image file name
#
# Returns:
# Figure object for inclusion in the Quarto report
############################################################

insert_static_figure <- function(file_name) {
  
  if (!file.exists(file_name)) {
    stop(
      paste(
        "Static figure not found:",
        normalizePath(
          file_name,
          winslash = "/",
          mustWork = FALSE
        )
      )
    )
  }
  
  knitr::include_graphics(
    normalizePath(
      file_name,
      winslash = "/",
      mustWork = TRUE
    )
  )
}



############################################################
# Function: create_ptm_fate_table
#
# Description:
# Reads a Neutral Particle or Surface-Oriented Particle
# fate CSV file and creates a weekly PTM fate table.
#
# The function:
# - Reads the first seven PTM result columns
# - Removes completely blank rows
# - Divides the source data into Week 1, Week 2, and Week 3
# based on the original row groups
# - Removes any row containing the scenario value -902
# - Retains all other available OMR scenarios
# - Formats OMR values with commas
# - Merges repeated weekly labels
# - Makes the table horizontally scrollable
#
# This approach does not assume that there are always four
# OMR scenarios per week. The source file may contain four,
# five, or another equal number of scenarios per week.
#
# Source Files include:
# NP_465(Chipps).csv
# NP_350(Cache Slough).csv
# NP_469(Jersey Point).csv
# NP_99(Old River).csv
#
# Inputs:
# results_folder - selected weekly results folder
# file_name - exact PTM CSV file name
#
# Returns:
# Formatted, horizontally scrollable HTML table
############################################################

create_ptm_fate_table <- function(results_folder, file_name) {
  
  file_path <- file.path(
    results_folder,
    file_name
  )
  
  if (!file.exists(file_path)) {
    stop(
      paste(
        "Required PTM file not found:",
        normalizePath(
          file_path,
          winslash = "/",
          mustWork = FALSE
        )
      )
    )
  }
  
  ptm <- readr::read_csv(
    file_path,
    skip = 3,
    col_names = FALSE,
    col_types = readr::cols(
      .default = readr::col_character()
    ),
    show_col_types = FALSE
  )
  
  if (ncol(ptm) < 7) {
    stop(
      paste0(
        file_name,
        " contains ",
        ncol(ptm),
        " columns after skipping the first three rows, ",
        "but at least seven columns are required."
      )
    )
  }
  
  # Keep only the seven columns used by the report.
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
  
  # Remove rows that are completely blank.
  nonblank_row <- apply(
    ptm,
    1,
    function(row_values) {
      
      cleaned <- trimws(
        as.character(row_values)
      )
      
      any(
        !is.na(row_values) &
          cleaned != ""
      )
    }
  )
  
  ptm <- ptm[
    nonblank_row,
    ,
    drop = FALSE
  ]
  
  if (nrow(ptm) == 0) {
    stop(
      paste(
        "No populated PTM rows were found in:",
        file_name
      )
    )
  }
  
  # The file must contain the same number of original
  # scenario rows for each of the three forecast weeks.
  if (nrow(ptm) %% 3 != 0) {
    stop(
      paste0(
        file_name,
        " contains ",
        nrow(ptm),
        " populated rows. The row count must be divisible ",
        "by 3 so the rows can be assigned to Weeks 1–3."
      )
    )
  }
  
  rows_per_week <- nrow(ptm) / 3
  
  week_labels <- c(
    paste0(
      "<b>Week 1:</b><br>",
      fmt_date(week1_start),
      " –<br>",
      fmt_date(week1_end)
    ),
    paste0(
      "<b>Week 2:</b><br>",
      fmt_date(week2_start),
      " –<br>",
      fmt_date(week2_end)
    ),
    paste0(
      "<b>Week 3:</b><br>",
      fmt_date(week3_start),
      " –<br>",
      fmt_date(week3_end)
    )
  )
  
  # Assign the week before removing -902 rows so the
  # original weekly grouping remains correct.
  ptm$Week <- rep(
    week_labels,
    each = rows_per_week
  )
  
  # Normalize every cell so values such as:
  # -902
  # "-902"
  # "-902.0"
  # or "-902 cfs"
  # can be identified.
  contains_minus_902 <- apply(
    ptm[, 1:7, drop = FALSE],
    1,
    function(row_values) {
      
      cleaned <- trimws(
        as.character(row_values)
      )
      
      cleaned_no_commas <- gsub(
        ",",
        "",
        cleaned
      )
      
      any(
        grepl(
          "(^|[^0-9])-902(?:\\.0+)?([^0-9]|$)",
          cleaned_no_commas,
          perl = TRUE
        ),
        na.rm = TRUE
      )
    }
  )
  
  # Exclude every row containing -902.
  ptm <- ptm[
    !contains_minus_902,
    ,
    drop = FALSE
  ]
  
  if (nrow(ptm) == 0) {
    stop(
      paste(
        "All PTM rows were removed because they contained -902 in:",
        file_name
      )
    )
  }
  
  # Remove rows with a missing OMR value.
  ptm <- ptm |>
    dplyr::filter(
      !is.na(`OMR Bin`),
      trimws(`OMR Bin`) != ""
    )
  
  # Format the remaining OMR scenarios.
  ptm$`OMR Bin` <- format_cfs(
    ptm$`OMR Bin`
  )
  
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
  
  table_html <- ptm |>
    knitr::kable(
      format = "html",
      escape = FALSE,
      align = "c",
      col.names = c(
        "Forecast Week",
        "OMR Flow Bin<br>(cfs)",
        "Past Chipps",
        "Upstream of Decker",
        "Unresolved in Central Delta",
        "Unresolved in OMR corridor",
        "CVP Entrainment",
        "SWP Entrainment"
      )
    ) |>
    kableExtra::kable_styling(
      full_width = FALSE,
      position = "center",
      bootstrap_options = c(
        "striped",
        "hover",
        "condensed"
      )
    ) |>
    kableExtra::collapse_rows(
      columns = 1,
      valign = "middle"
    )
  
  make_scrollable_table(
    table_html
  )
}



############################################################
# Function: insert_ptm_html_figures
#
# Description:
# Finds and inserts all available PTM interactive HTML
# figures for one particle type and DSM2 node.
#
# The function supports both three-scenario and
# four-scenario result folders.
#
# Standard scenario files:
# PTM_NP_Node465_ScenarioA.html
# PTM_NP_Node465_ScenarioB.html
# PTM_NP_Node465_ScenarioC.html
#
# The optional fourth scenario may be named either:
# PTM_NP_Node465.html
# or:
# PTM_NP_Node465_ScenarioD.html
#
# When the optional fourth scenario exists, it is inserted
# before Scenario A. If both possible fourth-scenario file
# names exist, the file without a scenario suffix is used
# and Scenario D is not duplicated.
#
# Scenario captions:
# Optional fourth scenario = -6,500 cfs
# Scenario A = -5,000 cfs
# Scenario B = -3,500 cfs
# Scenario C = -2,000 cfs
#
# Inputs:
# results_folder - selected weekly results folder
# particle_code - particle code, such as "NP" or "PP"
# particle_label - caption description
# node_number - DSM2 node number
# location_name - location name displayed in the caption
# injection_date - formatted particle injection date
# height - iframe height in pixels
#
# Returns:
# All available automatically numbered interactive figures
# for the specified particle type and DSM2 node
############################################################

insert_ptm_html_figures <- function(
    results_folder,
    particle_code,
    particle_label,
    node_number,
    location_name,
    height = 700
) {
  
  forecast_start_date <- as.Date(
    sub(
      "_results$",
      "",
      basename(results_folder)
    ),
    format = "%Y%m%d"
  )
  
  if (is.na(forecast_start_date)) {
    stop(
      paste(
        "Could not determine the injection date from results folder:",
        basename(results_folder)
      )
    )
  }
  
  injection_date <- format(
    forecast_start_date,
    "%m/%d/%Y"
  )
  
  file_prefix <- paste0(
    "PTM_",
    particle_code,
    "_Node",
    node_number
  )
  
  # Possible file names for the optional fourth scenario.
  optional_candidates <- c(
    paste0(file_prefix, ".html"),
    paste0(file_prefix, "_ScenarioD.html")
  )
  
  optional_file <- optional_candidates[
    file.exists(
      file.path(
        results_folder,
        optional_candidates
      )
    )
  ]
  
  # Use only one optional file if both naming patterns exist.
  if (length(optional_file) > 0) {
    optional_file <- optional_file[1]
  }
  
  figure_specs <- list()
  
  # Add the optional -6,500 scenario first.
  if (length(optional_file) == 1) {
    
    figure_specs[[length(figure_specs) + 1]] <- list(
      file_name = optional_file,
      scenario_code = "optional",
      omr_value = "-6,500"
    )
  }
  
  # Add the standard A, B, and C scenarios.
  standard_scenarios <- list(
    list(
      scenario_code = "a",
      scenario_letter = "A",
      omr_value = "-5,000"
    ),
    list(
      scenario_code = "b",
      scenario_letter = "B",
      omr_value = "-3,500"
    ),
    list(
      scenario_code = "c",
      scenario_letter = "C",
      omr_value = "-2,000"
    )
  )
  
  for (scenario in standard_scenarios) {
    
    file_name <- paste0(
      file_prefix,
      "_Scenario",
      scenario$scenario_letter,
      ".html"
    )
    
    if (!file.exists(file.path(results_folder, file_name))) {
      next
    }
    
    figure_specs[[length(figure_specs) + 1]] <- list(
      file_name = file_name,
      scenario_code = scenario$scenario_code,
      omr_value = scenario$omr_value
    )
  }
  
  if (length(figure_specs) == 0) {
    stop(
      paste0(
        "No PTM HTML figures were found for ",
        particle_code,
        " at DSM2 Node ",
        node_number,
        ". Expected files beginning with: ",
        file_prefix
      )
    )
  }
  
  for (figure_spec in figure_specs) {
    
    figure_id <- paste0(
      "fig-",
      tolower(particle_code),
      "-",
      node_number,
      "-",
      figure_spec$scenario_code
    )
    
    figure_title <- paste0(
      "PTM Results for ",
      particle_label,
      ". OMR Scenario ",
      figure_spec$omr_value,
      " cfs. Particles Injected ",
      injection_date,
      " at DSM2 Node ",
      node_number,
      " (",
      location_name,
      ")"
    )
    
    insert_interactive_figure(
      results_folder = results_folder,
      file_name = figure_spec$file_name,
      figure_id = figure_id,
      figure_title = figure_title,
      height = height,
      figure_type = "standard"
    )
  }
  
  invisible(length(figure_specs))
}


############################################################
# Function: create_eco_ptm_table
#
# Description:
# Reads survival_combined.csv and creates either:
#
# 1. Salmon particle route-ratio table
# 2. Salmon particle route-specific survival table
#
# The function determines the available OMR scenarios from
# the Model_Run column rather than assuming a fixed number
# of source rows.
#
# Model-run mapping:
# A = -5,000 cfs
# B = -3,500 cfs
# C = -2,000 cfs
# D = -6,500 cfs, when available
#
# If Model_Run D exists, it is displayed first, followed by
# A, B, and C. Missing model runs are skipped.
#
# Route ratios are displayed with two decimal places.
# Survival values are converted to whole-number percentages.
#
# Source File:
# survival_combined.csv
#
# Inputs:
# results_folder - selected results folder
# csv_file - source CSV file name
# table_type - "ratio" or "survival"
#
# Returns:
# Formatted and horizontally scrollable HTML table
############################################################

create_eco_ptm_table <- function(
    results_folder,
    csv_file = "survival_combined.csv",
    table_type = c("ratio", "survival")
) {
  
  table_type <- match.arg(table_type)
  
  file_path <- file.path(
    results_folder,
    csv_file
  )
  
  if (!file.exists(file_path)) {
    stop(
      paste(
        "Required ECO-PTM file not found:",
        normalizePath(
          file_path,
          winslash = "/",
          mustWork = FALSE
        )
      )
    )
  }
  
  eco <- readr::read_csv(
    file_path,
    show_col_types = FALSE,
    col_types = readr::cols(
      .default = readr::col_character()
    )
  )
  
  required_common_columns <- c(
    "Model_Run",
    "SUT_RATIO",
    "STM_RATIO",
    "SACR_SS_RATIO",
    "SACR_GEO_RATIO",
    "GEO_RATIO",
    "SUT_SUV",
    "STM_SUV",
    "SAC_SUV",
    "GEO_SUV",
    "Combined_suv"
  )
  
  missing_columns <- setdiff(
    required_common_columns,
    names(eco)
  )
  
  if (length(missing_columns) > 0) {
    stop(
      paste0(
        "The following required columns are missing from ",
        csv_file,
        ": ",
        paste(missing_columns, collapse = ", ")
      )
    )
  }
  
  # Standardize Model_Run values.
  eco <- eco |>
    dplyr::mutate(
      Model_Run = toupper(
        trimws(
          as.character(Model_Run)
        )
      )
    ) |>
    dplyr::filter(
      !is.na(Model_Run),
      Model_Run != ""
    )
  
  ##########################################################
  # Model-run to OMR mapping
  ##########################################################
  
  model_run_lookup <- c(
    D = -6500,
    A = -5000,
    B = -3500,
    C = -2000
  )
  
  model_run_order <- c(
    "D",
    "A",
    "B",
    "C"
  )
  
  # Retain only recognized model runs.
  eco <- eco |>
    dplyr::filter(
      Model_Run %in% names(model_run_lookup)
    )
  
  if (nrow(eco) == 0) {
    stop(
      paste0(
        "No recognized Model_Run values were found in ",
        csv_file,
        ". Expected A, B, C, or optional D."
      )
    )
  }
  
  ##########################################################
  # Handle duplicate Model_Run rows
  ##########################################################
  
  duplicate_runs <- eco |>
    dplyr::count(Model_Run) |>
    dplyr::filter(n > 1)
  
  if (nrow(duplicate_runs) > 0) {
    stop(
      paste0(
        "More than one row was found for the following ",
        "Model_Run value(s): ",
        paste(duplicate_runs$Model_Run, collapse = ", "),
        ". Each model run must have one row in ",
        csv_file,
        "."
      )
    )
  }
  
  eco <- eco |>
    dplyr::mutate(
      model_order = match(
        Model_Run,
        model_run_order
      ),
      
      `OMR Flow Bin (cfs)` = format_cfs(
        unname(
          model_run_lookup[Model_Run]
        )
      )
    ) |>
    dplyr::arrange(model_order)
  
  ##########################################################
  # Formatting functions
  ##########################################################
  
  format_ratio <- function(x) {
    
    x_numeric <- suppressWarnings(
      as.numeric(
        gsub(",", "", as.character(x))
      )
    )
    
    ifelse(
      is.na(x_numeric),
      "-",
      sprintf("%.2f", x_numeric)
    )
  }
  
  format_survival <- function(x) {
    
    x_character <- trimws(
      as.character(x)
    )
    
    already_percent <- grepl(
      "%$",
      x_character
    )
    
    x_numeric <- suppressWarnings(
      as.numeric(
        gsub(
          "%|,",
          "",
          x_character
        )
      )
    )
    
    result <- rep(
      "-",
      length(x_character)
    )
    
    # Values already stored as percentages.
    result[
      already_percent &
        !is.na(x_numeric)
    ] <- paste0(
      round(
        x_numeric[
          already_percent &
            !is.na(x_numeric)
        ]
      ),
      "%"
    )
    
    # Decimal survival fractions, such as 0.40.
    decimal_rows <- (
      !already_percent &
        !is.na(x_numeric) &
        abs(x_numeric) <= 1
    )
    
    result[decimal_rows] <- paste0(
      round(
        x_numeric[decimal_rows] * 100
      ),
      "%"
    )
    
    # Values already stored as whole percentages, such as 40.
    whole_rows <- (
      !already_percent &
        !is.na(x_numeric) &
        abs(x_numeric) > 1
    )
    
    result[whole_rows] <- paste0(
      round(
        x_numeric[whole_rows]
      ),
      "%"
    )
    
    result
  }
  
  ##########################################################
  # Route-ratio table
  ##########################################################
  
  if (table_type == "ratio") {
    
    output <- data.frame(
      `OMR Flow Bin<br>(cfs)` =
        eco$`OMR Flow Bin (cfs)`,
      
      `Sutter Slough<br>Route` =
        format_ratio(eco$SUT_RATIO),
      
      `Steamboat Slough<br>Route` =
        format_ratio(eco$STM_RATIO),
      
      `Sacramento River<br>(SS) Route` =
        format_ratio(eco$SACR_SS_RATIO),
      
      `Sacramento River<br>(GEO) Route` =
        format_ratio(eco$SACR_GEO_RATIO),
      
      `Georgiana Slough<br>Route` =
        format_ratio(eco$GEO_RATIO),
      
      check.names = FALSE
    )
    
    ##########################################################
    # Route-specific survival table
    ##########################################################
    
  } else {
    
    output <- data.frame(
      `OMR Flow Bin<br>(cfs)` =
        eco$`OMR Flow Bin (cfs)`,
      
      `Sutter Slough<br>Route` =
        format_survival(eco$SUT_SUV),
      
      `Steamboat Slough<br>Route` =
        format_survival(eco$STM_SUV),
      
      `Sacramento River<br>Route` =
        format_survival(eco$SAC_SUV),
      
      `Georgiana Slough<br>Route` =
        format_survival(eco$GEO_SUV),
      
      `All Routes<br>Combined` =
        format_survival(eco$Combined_suv),
      
      check.names = FALSE
    )
  }
  
  table_html <- output |>
    knitr::kable(
      format = "html",
      escape = FALSE,
      align = "c"
    ) |>
    kableExtra::kable_styling(
      full_width = FALSE,
      position = "center",
      bootstrap_options = c(
        "striped",
        "hover",
        "condensed"
      )
    )
  
  make_scrollable_table(
    table_html
  )
}

############################################################
# Function: create_static_csv_table
#
# Description:
# Reads a static CSV file stored in the Quarto report
# directory and creates a formatted HTML table.
#
# Static CSV files contain information that does not change
# between weekly result folders.
#
# Inputs:
# file_name - exact static CSV file name
#
# Returns:
# Formatted, horizontally scrollable HTML table
############################################################

create_static_csv_table <- function(file_name) {
  
  if (!file.exists(file_name)) {
    stop(
      paste(
        "Static CSV file not found:",
        normalizePath(
          file_name,
          winslash = "/",
          mustWork = FALSE
        )
      )
    )
  }
  
  table_data <- readr::read_csv(
    file_name,
    show_col_types = FALSE,
    col_types = readr::cols(
      .default = readr::col_character()
    )
  )
  
  if (nrow(table_data) == 0) {
    stop(
      paste(
        "Static CSV file contains no rows:",
        file_name
      )
    )
  }
  
  table_html <- table_data |>
    knitr::kable(
      format = "html",
      escape = FALSE,
      align = "c"
    ) |>
    kableExtra::kable_styling(
      full_width = FALSE,
      position = "center",
      bootstrap_options = c(
        "striped",
        "hover",
        "condensed"
      )
    )
  
  make_scrollable_table(
    table_html
  )
}






############################################################
# Function: create_lfs_entrainment_table
#
# Description:
# Reads one weekly LFS entrainment CSV and creates the
# Longfin Smelt entrainment table.
#
# Updated input behavior:
# - Any row containing a negative numeric value is treated
# as a placeholder for an unavailable -6,500 or -6,250
# OMR scenario and is removed.
# - Remaining scenario rows are expected to be ordered from
# -5,000 through -2,000.
# - The number of available scenario rows is determined
# dynamically rather than being fixed at four.
#
# The function reads OMR bins and Combined Exports from:
# average_exports_by_week.csv
#
# Source Files:
# LFS_PP_Week_1_Entrainment.csv
# LFS_PP_Week_2_Entrainment.csv
# LFS_PP_Week_3_Entrainment.csv
# average_exports_by_week.csv
############################################################

create_lfs_entrainment_table <- function(
    results_folder,
    week_number
) {
  
  if (!week_number %in% 1:3) {
    stop("week_number must be 1, 2, or 3.")
  }
  
  lfs_file <- file.path(
    results_folder,
    paste0(
      "LFS_PP_Week_",
      week_number,
      "_Entrainment.csv"
    )
  )
  
  if (!file.exists(lfs_file)) {
    stop(
      paste(
        "Required LFS entrainment file not found:",
        normalizePath(
          lfs_file,
          winslash = "/",
          mustWork = FALSE
        )
      )
    )
  }
  
  lfs <- readr::read_csv(
    lfs_file,
    col_types = readr::cols(
      .default = readr::col_character()
    ),
    show_col_types = FALSE
  )
  
  if (ncol(lfs) < 10) {
    stop(
      paste0(
        basename(lfs_file),
        " contains ",
        ncol(lfs),
        " columns, but at least 10 columns are required."
      )
    )
  }
  
  # Keep only the columns used by the report.
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
  
  ##########################################################
  # Remove completely blank rows
  ##########################################################
  
  nonblank_row <- apply(
    lfs,
    1,
    function(row_values) {
      
      cleaned <- trimws(
        as.character(row_values)
      )
      
      any(
        !is.na(row_values) &
          cleaned != ""
      )
    }
  )
  
  lfs <- lfs[
    nonblank_row,
    ,
    drop = FALSE
  ]
  
  ##########################################################
  # Remove placeholder rows containing negative values
  ##########################################################
  
  contains_negative_value <- apply(
    lfs,
    1,
    function(row_values) {
      
      cleaned <- trimws(
        as.character(row_values)
      )
      
      # Remove commas, percent signs, and spaces before
      # attempting numeric conversion.
      numeric_values <- suppressWarnings(
        as.numeric(
          gsub(
            "[,%[:space:]]",
            "",
            cleaned
          )
        )
      )
      
      any(
        !is.na(numeric_values) &
          numeric_values < 0
      )
    }
  )
  
  lfs <- lfs[
    !contains_negative_value,
    ,
    drop = FALSE
  ]
  
  if (nrow(lfs) == 0) {
    stop(
      paste(
        "No valid LFS rows remain after removing negative",
        "placeholder rows in:",
        basename(lfs_file)
      )
    )
  }
  
  ##########################################################
  # Identify the abundance and scenario rows
  ##########################################################
  
  metric_text <- tolower(
    trimws(
      as.character(lfs$Metric)
    )
  )
  
  abundance_rows <- grepl(
    "abundance",
    metric_text
  )
  
  ptm_percent_rows <- grepl(
    "ptm.*entrained|entrained.*\\(%\\)",
    metric_text
  )
  
  entrained_number_rows <- grepl(
    "smelt.*entrained|larva.*entrained|entrained.*\\(#\\)",
    metric_text
  ) &
    !ptm_percent_rows
  
  if (sum(abundance_rows) == 0) {
    stop(
      paste(
        "Could not identify the LFS Larva Abundance row in:",
        basename(lfs_file)
      )
    )
  }
  
  if (sum(ptm_percent_rows) == 0) {
    stop(
      paste(
        "Could not identify any PTM Entrained (%) rows in:",
        basename(lfs_file)
      )
    )
  }
  
  if (sum(entrained_number_rows) == 0) {
    stop(
      paste(
        "Could not identify any Smelt Larva Entrained (#)",
        "rows in:",
        basename(lfs_file)
      )
    )
  }
  
  number_ptm_scenarios <- sum(ptm_percent_rows)
  number_count_scenarios <- sum(entrained_number_rows)
  
  if (number_ptm_scenarios != number_count_scenarios) {
    stop(
      paste0(
        basename(lfs_file),
        " contains ",
        number_ptm_scenarios,
        " valid PTM Entrained (%) rows and ",
        number_count_scenarios,
        " valid Smelt Larva Entrained (#) rows. ",
        "The two scenario groups must contain the same ",
        "number of rows."
      )
    )
  }
  
  number_scenarios <- number_ptm_scenarios
  
  ##########################################################
  # Read weekly OMR and export values
  ##########################################################
  
  exports_file <- file.path(
    results_folder,
    "average_exports_by_week.csv"
  )
  
  if (!file.exists(exports_file)) {
    stop(
      paste(
        "Required exports file not found:",
        normalizePath(
          exports_file,
          winslash = "/",
          mustWork = FALSE
        )
      )
    )
  }
  
  exports <- readr::read_csv(
    exports_file,
    show_col_types = FALSE
  )
  
  required_export_columns <- c(
    "Week",
    "OMR Bins",
    "Total Exports (cfs)"
  )
  
  missing_export_columns <- setdiff(
    required_export_columns,
    names(exports)
  )
  
  if (length(missing_export_columns) > 0) {
    stop(
      paste0(
        "Missing column(s) in average_exports_by_week.csv: ",
        paste(
          missing_export_columns,
          collapse = ", "
        )
      )
    )
  }
  
  week_label <- paste0(
    "Week ",
    week_number
  )
  
  exports_week <- exports |>
    dplyr::filter(
      Week == week_label
    ) |>
    dplyr::mutate(
      omr_numeric = suppressWarnings(
        as.numeric(
          gsub(
            ",",
            "",
            as.character(`OMR Bins`)
          )
        )
      )
    ) |>
    dplyr::filter(
      !is.na(omr_numeric)
    )
  
  if (nrow(exports_week) < number_scenarios) {
    stop(
      paste0(
        "Week ",
        week_number,
        " has ",
        nrow(exports_week),
        " export scenarios, but the LFS file contains ",
        number_scenarios,
        " valid scenarios."
      )
    )
  }
  
  # The valid LFS rows are ordered from -5,000 to -2,000.
  # Select the matching final scenarios from the exports
  # table, which excludes an unavailable leading -6,500 or
  # -6,250 placeholder scenario.
  exports_week <- exports_week |>
    dplyr::slice_tail(
      n = number_scenarios
    )
  
  omr_bins <- format_cfs(
    exports_week$`OMR Bins`
  )
  
  total_exports <- format_cfs(
    exports_week$`Total Exports (cfs)`
  )
  
  ##########################################################
  # Assign OMR and export values to source rows
  ##########################################################
  
  output <- lfs
  
  output$`OMR<br>(cfs)` <- "&nbsp;"
  output$`Combined<br>Exports<br>(cfs)` <- "&nbsp;"
  
  output$`OMR<br>(cfs)`[
    ptm_percent_rows
  ] <- omr_bins
  
  output$`Combined<br>Exports<br>(cfs)`[
    ptm_percent_rows
  ] <- total_exports
  
  output$`OMR<br>(cfs)`[
    entrained_number_rows
  ] <- omr_bins
  
  output$`Combined<br>Exports<br>(cfs)`[
    entrained_number_rows
  ] <- total_exports
  
  output <- output[, c(
    "OMR<br>(cfs)",
    "Combined<br>Exports<br>(cfs)",
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
  )]
  
  names(output) <- c(
    "OMR<br>(cfs)",
    "Combined<br>Exports<br>(cfs)",
    "Metric",
    "West",
    "Suisun",
    "Sacramento/<br>North Delta",
    "Lower<br>San Joaquin",
    "Lower<br>Sacramento",
    "South<br>Delta",
    "East",
    "Delta-wide<br>Total (#)",
    "Delta-wide<br>Total (%)"
  )
  
  ##########################################################
  # Create table
  ##########################################################
  
  table_html <- output |>
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
    kableExtra::kable_styling(
      full_width = FALSE,
      position = "center",
      bootstrap_options = c(
        "striped",
        "hover",
        "condensed"
      )
    ) |>
    kableExtra::collapse_rows(
      columns = 1:2,
      valign = "middle"
    )
  
  make_scrollable_table(
    table_html
  )
}


############################################################
# Function: insert_lfs_html_figures
#
# Description:
# Inserts all available Longfin Smelt PTM interactive HTML
# figures from the selected results folder.
#
# Standard expected files:
# LFS_Scenario_A.html
# LFS_Scenario_B.html
# LFS_Scenario_C.html
#
# Optional fourth scenario may be:
# LFS.html
# LFS_Scenario_D.html
#
# The optional fourth scenario, when available, is placed
# before Scenario A.
#
# Scenario mapping:
# Optional/D = -6,500 cfs
# A = -5,000 cfs
# B = -3,500 cfs
# C = -2,000 cfs
############################################################

insert_lfs_html_figures <- function(
    results_folder,
    height = 700
) {
  
  forecast_start_date <- as.Date(
    sub(
      "_results$",
      "",
      basename(results_folder)
    ),
    format = "%Y%m%d"
  )
  
  if (is.na(forecast_start_date)) {
    stop(
      paste(
        "Could not determine the particle injection date",
        "from results folder:",
        basename(results_folder)
      )
    )
  }
  
  injection_date <- format(
    forecast_start_date,
    "%m/%d/%Y"
  )
  
  figure_specs <- list()
  
  ##########################################################
  # Optional fourth scenario
  ##########################################################
  
  optional_candidates <- c(
    "LFS.html",
    "LFS_Scenario_D.html"
  )
  
  available_optional <- optional_candidates[
    file.exists(
      file.path(
        results_folder,
        optional_candidates
      )
    )
  ]
  
  if (length(available_optional) > 0) {
    figure_specs[[length(figure_specs) + 1]] <- list(
      file_name = available_optional[1],
      scenario_code = "optional",
      omr_value = "-6,500"
    )
  }
  
  ##########################################################
  # Standard scenarios A–C
  ##########################################################
  
  standard_scenarios <- list(
    list(
      letter = "A",
      code = "a",
      omr = "-5,000"
    ),
    list(
      letter = "B",
      code = "b",
      omr = "-3,500"
    ),
    list(
      letter = "C",
      code = "c",
      omr = "-2,000"
    )
  )
  
  for (scenario in standard_scenarios) {
    
    file_name <- paste0(
      "LFS_Scenario_",
      scenario$letter,
      ".html"
    )
    
    if (!file.exists(
      file.path(
        results_folder,
        file_name
      )
    )) {
      next
    }
    
    figure_specs[[length(figure_specs) + 1]] <- list(
      file_name = file_name,
      scenario_code = scenario$code,
      omr_value = scenario$omr
    )
  }
  
  if (length(figure_specs) == 0) {
    stop(
      paste(
        "No Longfin Smelt HTML figures were found in:",
        basename(results_folder)
      )
    )
  }
  
  ##########################################################
  # Insert the figures
  ##########################################################
  
  for (figure_spec in figure_specs) {
    
    figure_id <- paste0(
      "fig-lfs-",
      figure_spec$scenario_code
    )
    
    figure_title <- paste0(
      "Average PTM Results by Injection Region for ",
      "Surface Oriented Particles Entrained at CVP and SWP. ",
      "OMR Scenario ",
      figure_spec$omr_value,
      " cfs. Particles Injected ",
      injection_date
    )
    
    insert_interactive_figure(
      results_folder = results_folder,
      file_name = figure_spec$file_name,
      figure_id = figure_id,
      figure_title = figure_title,
      height = height,
      figure_type = "standard"
     # hide_internal_title = TRUE
    )
  }
  
  invisible(
    length(figure_specs)
  )
}

############################################################
# Function: insert_date_entrainment_html_figures
#
# Description:
# Finds and inserts the dated Longfin Smelt entrainment
# HTML maps.
#
# Expected filenames:
# YYYYMMDD_entrainment_OMR_-6500.html
# YYYYMMDD_entrainment_OMR_-5000.html
# YYYYMMDD_entrainment_OMR_-3500.html
# YYYYMMDD_entrainment_OMR_-2000.html
#
# Important display behavior:
# - The original wide map layout is preserved.
# - The entire HTML is scaled proportionally to fit the
# available report width.
# - The map, legends, title, and controls remain visible.
# - The map is not stretched vertically.
# - No iframe scrollbars are displayed.
############################################################

insert_date_entrainment_html_figures <- function(
    results_folder,
    content_width = 1400,
    content_height = 900
) {
  
  ##########################################################
  # Validate results folder
  ##########################################################
  
  if (!dir.exists(results_folder)) {
    stop(
      paste(
        "Results folder not found:",
        normalizePath(
          results_folder,
          winslash = "/",
          mustWork = FALSE
        )
      )
    )
  }
  
  ##########################################################
  # Extract selected results date
  #
  # Example:
  # 20260623_results -> 20260623
  ##########################################################
  
  results_date <- sub(
    "_results$",
    "",
    basename(results_folder)
  )
  
  if (!grepl("^[0-9]{8}$", results_date)) {
    stop(
      paste0(
        "Could not determine an eight-digit results date ",
        "from folder name: ",
        basename(results_folder)
      )
    )
  }
  
  ##########################################################
  # Find matching HTML files
  ##########################################################
  
  file_pattern <- paste0(
    "^",
    results_date,
    "_entrainment_OMR_-?[0-9]+(?:\\.[0-9]+)?\\.html$"
  )
  
  matching_files <- list.files(
    path = results_folder,
    pattern = file_pattern,
    full.names = FALSE,
    ignore.case = TRUE
  )
  
  if (length(matching_files) == 0) {
    stop(
      paste0(
        "No dated entrainment HTML files were found in ",
        basename(results_folder),
        ". Expected a file such as ",
        results_date,
        "_entrainment_OMR_-2000.html"
      )
    )
  }
  
  ##########################################################
  # Extract and order OMR values
  ##########################################################
  
  extract_omr <- function(file_name) {
    
    file_stem <- tools::file_path_sans_ext(
      basename(file_name)
    )
    
    omr_text <- sub(
      "^.*_OMR_",
      "",
      file_stem,
      ignore.case = TRUE
    )
    
    suppressWarnings(
      as.numeric(omr_text)
    )
  }
  
  omr_values <- vapply(
    matching_files,
    extract_omr,
    numeric(1)
  )
  
  if (any(is.na(omr_values))) {
    
    invalid_files <- matching_files[
      is.na(omr_values)
    ]
    
    stop(
      paste0(
        "Could not extract OMR values from: ",
        paste(
          invalid_files,
          collapse = ", "
        )
      )
    )
  }
  
  # Order:
  # -6500, -5000, -3500, -2000
  file_order <- order(omr_values)
  
  matching_files <- matching_files[file_order]
  omr_values <- omr_values[file_order]
  
  ##########################################################
  # Extract the main title from each HTML
  ##########################################################
  
  extract_map_title <- function(
    html_content,
    omr_value
  ) {
    
    searchable_content <- html_content
    
    searchable_content <- gsub(
      "\\\\/",
      "/",
      searchable_content
    )
    
    searchable_content <- gsub(
      "\\\\n|\\\\r",
      " ",
      searchable_content
    )
    
    searchable_content <- gsub(
      "\\\\\"",
      "\"",
      searchable_content
    )
    
    searchable_content <- gsub(
      "<br\\s*/?>",
      " ",
      searchable_content,
      ignore.case = TRUE
    )
    
    searchable_content <- gsub(
      "<[^>]+>",
      " ",
      searchable_content
    )
    
    searchable_content <- gsub(
      "&nbsp;",
      " ",
      searchable_content,
      fixed = TRUE
    )
    
    searchable_content <- gsub(
      "&amp;",
      "&",
      searchable_content,
      fixed = TRUE
    )
    
    searchable_content <- gsub(
      "[[:space:]]+",
      " ",
      searchable_content
    )
    
    title_pattern <- paste0(
      "Cumulative Entrainment at CVP/SWP Facilities ",
      "Over 3 Weeks After [0-9]{8} ",
      "for OMR\\s*=\\s*-?[0-9,]+\\s*",
      "LFS Abundance Based on SLS Survey\\s*",
      "[0-9]+\\s*\\([0-9/]+\\)"
    )
    
    title_match <- regexpr(
      title_pattern,
      searchable_content,
      ignore.case = TRUE,
      perl = TRUE
    )
    
    if (title_match[1] != -1) {
      
      extracted_title <- regmatches(
        searchable_content,
        title_match
      )
      
      extracted_title <- gsub(
        "[[:space:]]+",
        " ",
        extracted_title
      )
      
      return(
        trimws(extracted_title)
      )
    }
    
    paste0(
      "Cumulative Entrainment at CVP/SWP Facilities ",
      "Over 3 Weeks After ",
      results_date,
      " for OMR = ",
      format_cfs(omr_value),
      " cfs"
    )
  }
  
  ##########################################################
  # Insert each HTML map
  ##########################################################
  
  for (i in seq_along(matching_files)) {
    
    file_name <- matching_files[i]
    omr_value <- omr_values[i]
    
    html_path <- file.path(
      results_folder,
      file_name
    )
    
    html_content <- paste(
      readLines(
        html_path,
        warn = FALSE,
        encoding = "UTF-8"
      ),
      collapse = "\n"
    )
    
    # Add Reset Map control to the dated Leaflet entrainment map.
    html_content <- add_leaflet_reset_control(html_content)
    
    figure_title <- extract_map_title(
      html_content,
      omr_value
    )
    
    ########################################################
    # Create unique HTML IDs
    ########################################################
    
    omr_id <- gsub(
      "[^0-9]+",
      "",
      as.character(
        abs(omr_value)
      )
    )
    
    figure_id <- paste0(
      "fig-date-entrainment-omr-",
      omr_id
    )
    
    wrapper_id <- paste0(
      "dated-map-wrapper-",
      omr_id
    )
    
    iframe_id <- paste0(
      "dated-map-iframe-",
      omr_id
    )
    
    ########################################################
    # Create fixed-size iframe
    #
    # The iframe keeps the original map dimensions.
    # JavaScript scales the entire iframe to the width of
    # the Quarto report.
    ########################################################
    
    figure_html <- htmltools::tags$div(
      id = wrapper_id,
      class = "dated-map-scale-wrapper",
      
      htmltools::tags$iframe(
        id = iframe_id,
        class = "dated-map-scale-iframe",
        srcdoc = htmltools::HTML(html_content),
        width = as.character(content_width),
        height = as.character(content_height),
        frameborder = "0",
        scrolling = "no",
        loading = "lazy"
      )
    )
    
    ########################################################
    # Resize wrapper and scale iframe dynamically
    ########################################################
    
    resize_script <- paste0(
      "<script>",
      "(function() {",
      
      " var wrapper = document.getElementById('",
      wrapper_id,
      "');",
      
      " var iframe = document.getElementById('",
      iframe_id,
      "');",
      
      " if (!wrapper || !iframe) return;",
      
      " var originalWidth = ",
      content_width,
      ";",
      
      " var originalHeight = ",
      content_height,
      ";",
      
      " function resizeDatedMap() {",
      
      " var availableWidth = wrapper.clientWidth;",
      
      " if (!availableWidth || availableWidth <= 0) {",
      " return;",
      " }",
      
      " var scale = Math.min(",
      " availableWidth / originalWidth,",
      " 1",
      " );",
      
      " iframe.style.width = originalWidth + 'px';",
      " iframe.style.height = originalHeight + 'px';",
      " iframe.style.transformOrigin = 'top left';",
      " iframe.style.transform = 'scale(' + scale + ')';",
      
      " wrapper.style.height = ",
      " Math.ceil(originalHeight * scale) + 'px';",
      
      " }",
      
      " window.addEventListener('load', function() {",
      " setTimeout(resizeDatedMap, 100);",
      " setTimeout(resizeDatedMap, 500);",
      " setTimeout(resizeDatedMap, 1200);",
      " });",
      
      " window.addEventListener(",
      " 'resize',",
      " resizeDatedMap",
      " );",
      
      " resizeDatedMap();",
      
      "})();",
      "</script>"
    )
    
    ########################################################
    # Print automatically numbered Quarto figure
    ########################################################
    
    cat(
      paste0(
        "\n::: {#",
        figure_id,
        "}\n\n",
        as.character(figure_html),
        "\n",
        resize_script,
        "\n\n",
        figure_title,
        "\n\n:::\n"
      )
    )
  }
  
  invisible(
    length(matching_files)
  )
}

