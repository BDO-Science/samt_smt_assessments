### Useful functions to read in data

# Function to read Excel files - returns single dataframe ---------------
# Load required libraries
library(readxl)
library(dplyr)
library(purrr)
library(stringr)

# Function to read Excel files by pattern - returns single dataframe
read_excel_by_pattern <- function(pattern, folder_path = ".", combine_survey_sheets = FALSE) {
  
  # Get list of all Excel files matching the pattern you specify in the function
  all_files <- list.files(path = folder_path, 
                          pattern = paste0(".*", pattern, ".*\\.(xlsx|xls)$"), 
                          full.names = TRUE,
                          ignore.case = TRUE)
  
  # Check if any files were found
  if (length(all_files) == 0) {
    message(paste("No Excel files containing '", pattern, "' found in the specified folder."))
    return(NULL)
  }
  
  # Select file: if only 1 file, use it; if multiple, use most recent
  selected_file <- if (length(all_files) == 1) {
    message(paste("Found 1 file:", basename(all_files[1])))
    all_files[1]
  } else {
    message(paste("Found", length(all_files), "files. Reading most recently modified..."))
    
    most_recent <- all_files %>%
      tibble(file_path = .) %>%
      mutate(mtime = map_dbl(file_path, ~file.info(.)$mtime)) %>%
      slice_max(mtime, n = 1) %>%
      pull(file_path)
    
    message(paste("Most recent file:", basename(most_recent)))
    most_recent
  }
  
  # Read the selected file
  data <- tryCatch({
    # Specifically for 20mm and SLS, combine all the sheets that are named "Survey x"
    if (combine_survey_sheets) {
      # Get all sheets starting with "Survey"
      survey_sheets <- excel_sheets(selected_file) %>%
        keep(~str_starts(., "Survey"))
      
      if (length(survey_sheets) == 0) {
        message("No sheets starting with 'Survey' found. Reading first sheet only.")
        read_excel(selected_file)
      } else {
        message(paste("Found", length(survey_sheets), "survey sheets:", 
                      paste(survey_sheets, collapse = ", ")))
        
        # Read and combine all survey sheets
        survey_sheets %>%
          set_names() %>%
          map_dfr(~{
            sheet_data <- read_excel(selected_file, sheet = .)
            message(paste("Read sheet:", ., 
                          "-", nrow(sheet_data), "rows x", ncol(sheet_data), "cols"))
            sheet_data
          })
      }
    } else {
      # Read just the first sheet (default behavior)
      read_excel(selected_file)
    }
    
  }, error = function(e) {
    message(paste("Error reading file:", e$message))
    return(NULL)
  })
  
  if (!is.null(data)) {
    message(paste("Successfully read:", nrow(data), "rows x", ncol(data), "cols"))
  }
  
  return(data)
}


# Function to read SFBS Excel files - returns single dataframe -------------------
read_sfbs_files <- function(folder_path = data_raw) {
  
  # Get list of all Excel files starting with 'SFBS' in filename
  all_files <- list.files(path = folder_path, 
                          pattern = "^SFBS.*\\.(xlsx|xls)$", 
                          full.names = TRUE,
                          ignore.case = TRUE)
  
  # Check if any files were found
  if (length(all_files) == 0) {
    message("No Excel files starting with 'SFBS' found in the specified folder.")
    return(NULL)
  }
  
  # If exactly 1 file, read it directly
  if (length(all_files) == 1) {
    message(paste("Found 1 file:", basename(all_files[1])))
    
    tryCatch({
      data <- read_excel(all_files[1])
      message(paste("Successfully read:", nrow(data), "rows x", ncol(data), "cols"))
      return(data)
    }, error = function(e) {
      message(paste("Error reading file:", e$message))
      return(NULL)
    })
  }
  
  # If more than 1 file, combine them
  message(paste("Found", length(all_files), "files. Combining..."))
  
  combined_data <- list()
  
  for (i in seq_along(all_files)) {
    file <- all_files[i]
    tryCatch({
      file_name <- basename(file)
      data <- read_excel(file)
      combined_data[[i]] <- data
      message(paste("Read file", i, ":", file_name, 
                    "-", nrow(data), "rows x", ncol(data), "cols"))
    }, error = function(e) {
      message(paste("Error reading", basename(file), ":", e$message))
    })
  }
  
  # Combine all dataframes
  if (length(combined_data) > 0) {
    final_data <- bind_rows(combined_data)
    message(paste("\nCombined data:", nrow(final_data), "rows x", ncol(final_data), "cols"))
    return(final_data)
  } else {
    message("No files were successfully read.")
    return(NULL)
  }
}

