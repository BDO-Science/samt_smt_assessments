# Spring-Run Chinook Salmon Surrogate JPE and Threshold Analysis
# Water Year 2026
# 
# This script calculates the 1% loss threshold for spring-run Chinook salmon surrogates
# using multiple methods to compare approaches

library(tidyverse)
library(janitor)
library(rvest)
library(lubridate)
library(flextable)
library(officer)

# Set water year
wy <- 2026

###########################################
# FETCH DATA FROM SACPAS
###########################################

# Fetch spring-run surrogate releases
sr_surrogate_url <- paste0('https://www.cbr.washington.edu/sacramento/workgroups/include_gen/WY',wy,'/cwt_spring_surrogates.html')

sr_page <- read_html(sr_surrogate_url)
tables <- sr_page %>% html_table(fill = TRUE)

sr_table <- tables[[1]] %>%
  clean_names()

# Process all releases
sr_all <- sr_table %>%
  filter(!is.na(number_of_cwt_fish_released), number_of_cwt_fish_released != "") %>%
  mutate(
    release_date = release_start,
    tag_codes_str = tag_codes,
    num_released = as.numeric(gsub(",", "", as.character(number_of_cwt_fish_released))),
    loss = as.numeric(gsub(",", "", as.character(confirmed_loss))),
    hatch = hatchery,
    rel_type = release_type
  ) %>%
  select(hatch, release_date, rel_type, tag_codes_str, num_released, loss) %>%
  filter(!is.na(num_released), num_released > 0)

# Split tags and create clean dataset
sr_releases <- sr_all %>%
  rowwise() %>%
  mutate(
    n_tags = if_else(is.na(tag_codes_str) | tag_codes_str == "", 
                     0L, 
                     as.integer(length(strsplit(gsub(" ", "", tag_codes_str), ",")[[1]])))
  ) %>%
  ungroup() %>%
  filter(n_tags > 0) %>%
  separate_rows(tag_codes_str, sep = ",\\s*") %>%
  group_by(hatch, release_date, rel_type, num_released, loss, n_tags) %>%
  mutate(
    num_per_tag = num_released / n_tags,
    loss_per_tag = loss / n_tags
  ) %>%
  ungroup()

message("Successfully processed spring-run surrogate data")

# Summarize releases by type
release_summary <- sr_releases %>%
  group_by(rel_type) %>%
  summarize(
    n_releases = n_distinct(release_date),
    n_tags = n(),
    total_released = sum(num_per_tag),
    total_loss = sum(loss_per_tag),
    .groups = "drop"
  )

print("Release Summary:")
print(release_summary)

###########################################
# SURVIVAL RATE DATA
###########################################

# Historical Coleman Late-Fall survival estimates from sr_hatchery_survival.csv
# These are the ONLY values we have - do not add other years
survival_historical_all <- tibble(
  year = c(2026, 2021, 2020, 2019),
  survival = c(11.5, 14.3, 60.4, 23.0)
)

# Calculate two averages
survival_all_years <- mean(survival_historical_all$survival)  # All years including current (2026)
survival_historical_only <- survival_historical_all %>% 
  filter(year != 2026) %>%  # Exclude current year from historical average
  pull(survival) %>% 
  mean()  # Historical years only (2019-2021)

cat("\nHistorical mean survival - All years including 2026 (2019-2021, 2026): ", round(survival_all_years, 1), "%\n")
cat("  Years: ", paste(survival_historical_all$year, collapse=", "), "\n")
cat("  Values: ", paste0(survival_historical_all$survival, "%", collapse=", "), "\n\n")
cat("Historical mean survival - Historical years only (2019-2021): ", round(survival_historical_only, 1), "%\n")
cat("  Excludes current year 2026 (11.5%)\n\n")

###########################################
# SCENARIO CALCULATIONS
###########################################

# Prepare data subsets
production_only <- sr_releases %>% 
  filter(grepl("Production", rel_type, ignore.case = TRUE))

experimental_only <- sr_releases %>% 
  filter(grepl("Experimental", rel_type, ignore.case = TRUE))

all_releases <- sr_releases

# Function to calculate JPE and threshold
calculate_jpe_threshold <- function(data, survival_pct, scenario_name) {
  total_released <- sum(data$num_per_tag, na.rm = TRUE)
  total_loss <- sum(data$loss_per_tag, na.rm = TRUE)
  
  jpe <- total_released * (survival_pct / 100)
  threshold_1pct <- jpe * 0.01
  pct_of_threshold <- (total_loss / threshold_1pct) * 100
  
  tibble(
    scenario = scenario_name,
    releases_included = ifelse(nrow(data) == nrow(all_releases), "All (Prod + Exp)", 
                               ifelse(nrow(data) == nrow(production_only), "Production Only", "Experimental Only")),
    survival_rate = paste0(round(survival_pct, 1), "%"),
    total_released = prettyNum(round(total_released, 0), big.mark = ","),
    jpe = prettyNum(round(jpe, 0), big.mark = ","),
    threshold_1pct = prettyNum(round(threshold_1pct, 0), big.mark = ","),
    current_loss = prettyNum(round(total_loss, 1), big.mark = ","),
    pct_of_threshold = paste0(round(pct_of_threshold, 1), "%")
  )
}

# Calculate all scenarios
scenarios <- bind_rows(
  # Scenario 1: All releases with historical mean (all years including 2026)
  calculate_jpe_threshold(all_releases, survival_all_years, 
                          "1. All Releases + Historical Mean (All Years)"),
  
  # Scenario 2: All releases with historical mean (historical years only, exclude 2026)
  calculate_jpe_threshold(all_releases, survival_historical_only, 
                          "2. All Releases + Historical Mean (Historical Only)"),
  
  # Scenario 3: Production only with historical mean (all years)
  calculate_jpe_threshold(production_only, survival_all_years, 
                          "3. Production Only + Historical Mean (All Years)"),
  
  # Scenario 4: Production only with historical mean (historical only)
  calculate_jpe_threshold(production_only, survival_historical_only, 
                          "4. Production Only + Historical Mean (Historical Only)"),
  
  # Scenario 5: Experimental only with historical mean (all years)
  calculate_jpe_threshold(experimental_only, survival_all_years, 
                          "5. Experimental Only + Historical Mean (All Years)"),
  
  # Scenario 6: Experimental only with historical mean (historical only)
  calculate_jpe_threshold(experimental_only, survival_historical_only, 
                          "6. Experimental Only + Historical Mean (Historical Only)")
)

print(scenarios)

###########################################
# CREATE WORD DOCUMENT
###########################################

# Create a new Word document
doc <- read_docx()

# Add title and header - matching original format
doc <- doc %>%
  body_add_par("Spring-Run Chinook Salmon Surrogate Thresholds", style = "heading 1") %>%
  body_add_par("Annual Loss Threshold Calculation Methods", style = "heading 1") %>%
  body_add_par("", style = "Normal")

# Add Proposed Action section with full regulatory text
doc <- doc %>%
  body_add_par("Proposed Action", style = "heading 2") %>%
  body_add_par(paste0(
    "Spring-Run Chinook Salmon Juveniles: If an assessment indicates annual salvage loss is on a trajectory ",
    "to exceed 1% of the JPE entering the Delta and a reduction in export pumping would materially increase ",
    "through-Delta survival, Reclamation and DWR will reduce CVP and SWP exports to maintain an average OMRI ",
    "no more negative than - 3,500 cfs for seven days and prepare a subsequent assessment. The annual salvage ",
    "loss as a proportion of the spring-run Chinook salmon JPE shall be tracked by surrogates:"
  ), style = "Normal") %>%
  body_add_par("", style = "Normal") %>%
  body_add_par("1. Yearling: Coleman Late-Fall Chinook Production Release", style = "Normal") %>%
  body_add_par("", style = "Normal") %>%
  body_add_par("2. Young of Year: Feather River Hatchery Spring-Run Production Releases", style = "Normal") %>%
  body_add_par("", style = "Normal") %>%
  body_add_par(paste0(
    "The JPE for each release group of salmonids entering the Delta shall be determined by the historical ",
    "average survival from release sites to Delta entry, or when available, by observed survival to Delta ",
    "entry provided by real-time acoustic receiver arrays. Reclamation and DWR, through Governance, may ",
    "modify survival estimates based on year-specific conditions."
  ), style = "Normal") %>%
  body_add_par("", style = "Normal")

# Add Background section
doc <- doc %>%
  body_add_par("Background", style = "heading 2") %>%
  body_add_par(paste0(
    "The Proposed Action requires tracking spring-run Chinook salmon loss using Coleman Late-Fall ",
    "releases as yearling surrogates. The 1% loss threshold is calculated as 1% of the Juvenile Production ",
    "Estimate (JPE) entering the Delta. JPE = Number Released × Survival Rate."
  ), style = "Normal") %>%
  body_add_par("", style = "Normal")

# Add release summary
doc <- doc %>%
  body_add_par("Release Summary (WY2026)", style = "heading 2")

release_summary_ft <- release_summary %>%
  mutate(
    total_released = prettyNum(round(total_released, 0), big.mark = ","),
    total_loss = round(total_loss, 1)
  ) %>%
  rename(
    `Release Type` = rel_type,
    `# Release Events` = n_releases,
    `# CWT Groups` = n_tags,
    `Total Released` = total_released,
    `Current Loss` = total_loss
  ) %>%
  flextable() %>%
  theme_booktabs() %>%
  autofit()

doc <- doc %>%
  flextable::body_add_flextable(release_summary_ft) %>%
  body_add_par("", style = "Normal")

# Add survival rates section
doc <- doc %>%
  body_add_par("Survival Rate Estimates", style = "heading 2") %>%
  body_add_par(paste0(
    "Two survival rate estimates are compared:"
  ), style = "Normal") %>%
  body_add_par(paste0(
    "• Historical Mean - All Years (2019-2021, 2026): ", round(survival_all_years, 1), 
    "% (average of ", paste(survival_historical_all$survival, collapse = "%, "), "%)"
  ), style = "Normal") %>%
  body_add_par(paste0(
    "• Historical Mean - Historical Years Only (2019-2021): ", round(survival_historical_only, 1), 
    "% (excludes current year 2026: 11.5%)"
  ), style = "Normal") %>%
  body_add_par("", style = "Normal")

# Add scenario comparison table
doc <- doc %>%
  body_add_par("Threshold Calculation Scenarios", style = "heading 2") %>%
  body_add_par(paste0(
    "The following table compares six different methods for calculating the 1% loss threshold, ",
    "varying both which releases are included and which survival rate is used."
  ), style = "Normal") %>%
  body_add_par("", style = "Normal")

scenarios_ft <- scenarios %>%
  rename(
    `Scenario` = scenario,
    `Releases Included` = releases_included,
    `Survival Rate` = survival_rate,
    `Total Released` = total_released,
    `JPE` = jpe,
    `1% Threshold` = threshold_1pct,
    `Current Loss` = current_loss,
    `% of Threshold` = pct_of_threshold
  ) %>%
  flextable() %>%
  theme_booktabs() %>%
  fontsize(size = 9, part = "all") %>%
  width(j = 1, width = 2.5) %>%
  width(j = 2:8, width = 0.95) %>%
  align(j = 3:8, align = "right", part = "body")

doc <- doc %>%
  flextable::body_add_flextable(scenarios_ft) %>%
  body_add_par("", style = "Normal")

# Add interpretation section
doc <- doc %>%
  body_add_par("Interpretation", style = "heading 2") %>%
  body_add_par(paste0(
    "The choice of calculation method significantly impacts the threshold value:"
  ), style = "Normal") %>%
  body_add_par("", style = "Normal") %>%
  body_add_par(paste0(
    "• Using historical mean survival from all years (", round(survival_all_years, 1), 
    "%) includes the current year (2026: 11.5%) which lowers the average compared to historical years only. ",
    "This provides a more conservative estimate."
  ), style = "Normal") %>%
  body_add_par("", style = "Normal") %>%
  body_add_par(paste0(
    "• Using historical mean survival from historical years only (", round(survival_historical_only, 1), 
    "%) excludes the current year 2026 and uses only the historical baseline (2019-2021). ",
    "This matches the previous approach (32.6% from 2019-2021) and provides the regulatory baseline."
  ), style = "Normal") %>%
  body_add_par("", style = "Normal") %>%
  body_add_par(paste0(
    "• Including both production and experimental releases increases the total fish counted ",
    "toward the threshold by approximately 24% compared to production-only."
  ), style = "Normal") %>%
  body_add_par("", style = "Normal") %>%
  body_add_par("Key Values from sr_hatchery_survival.csv:", style = "Normal") %>%
  body_add_par(paste0(
    "• Historical years (2019-2021): 23.0%, 60.4%, 14.3% → Average: ", round(survival_historical_only, 1), "%"
  ), style = "Normal") %>%
  body_add_par(paste0(
    "• All years including 2026: adds 11.5% → Average: ", round(survival_all_years, 1), "%"
  ), style = "Normal") %>%
  body_add_par("", style = "Normal")

# Add recommendation section
doc <- doc %>%
  body_add_par("Recommended Approach", style = "heading 2") %>%
  body_add_par(paste0(
    "The Biological Opinion states: 'The JPE for each release group of salmonids entering the Delta shall be ",
    "determined by the historical average survival from release sites to Delta entry, or when available, by ",
    "observed survival to Delta entry provided by real-time acoustic receiver arrays.'"
  ), style = "Normal") %>%
  body_add_par("", style = "Normal") %>%
  body_add_par(paste0(
    "Based on this language and current practice, the recommended method is:"
  ), style = "Normal") %>%
  body_add_par("", style = "Normal") %>%
  body_add_par(paste0(
    "Scenario 1: All Releases + Historical Mean (All Years) = ", round(survival_all_years, 1), "%"
  ), style = "Normal") %>%
  body_add_par("", style = "Normal") %>%
  body_add_par(paste0(
    "This approach: (1) aligns with the regulatory language to use historical averages, ",
    "(2) uses all available years in sr_hatchery_survival.csv (2019-2021, 2026), ",
    "(3) includes both historical baseline and current year observations, and ",
    "(4) provides the most comprehensive survival estimate from available telemetry data."
  ), style = "Normal") %>%
  body_add_par("", style = "Normal") %>%
  body_add_par(paste0(
    "Alternative: Scenario 2 (Historical Years Only = ", round(survival_historical_only, 1), "%) ",
    "excludes the current year 2026 and uses only the historical baseline (2019-2021)."
  ), style = "Normal") %>%
  body_add_par("", style = "Normal")

# Add copy-paste section for weekly assessment
doc <- doc %>%
  body_add_par("Summary for Weekly Assessment", style = "heading 2") %>%
  body_add_par(paste0(
    "The following text can be copied into the weekly salmon assessment report:"
  ), style = "Normal") %>%
  body_add_par("", style = "Normal")

# Calculate the recommended scenario values (using ALL YEARS approach)
recommended_releases <- sum(release_summary$total_released)
recommended_jpe <- round(recommended_releases * (survival_all_years / 100), 0)
recommended_threshold <- round(recommended_jpe * 0.01, 0)
recommended_current_loss <- sum(release_summary$total_loss)
recommended_pct <- round((recommended_current_loss / recommended_threshold) * 100, 1)

# Add boxed copy-paste text
copy_paste_text <- paste0(
  "Spring-Run Chinook Salmon Surrogate Releases (WY", wy, "):\n\n",
  "A total of ", prettyNum(recommended_releases, big.mark = ","), " Coleman Late-Fall Chinook salmon ",
  "(yearling surrogates) have been released from production and experimental groups. ",
  "Using the historical average survival rate (", round(survival_all_years, 1), "% from all available years 2019-2021, 2026), ",
  "the Juvenile Production Estimate (JPE) entering the Delta is approximately ",
  prettyNum(recommended_jpe, big.mark = ","), " fish.\n\n",
  "The annual loss threshold for spring-run surrogates is 1% of the JPE, which equals ",
  prettyNum(recommended_threshold, big.mark = ","), " fish. ",
  "As of [INSERT DATE], cumulative loss is ", prettyNum(round(recommended_current_loss, 1), big.mark = ","), 
  " fish or ", recommended_pct, "% of the annual loss threshold.\n\n",
  "JPE Calculation Method: Historical average survival (", round(survival_all_years, 1), 
  "%) applied to all Coleman Late-Fall releases ",
  "(both production and experimental), consistent with Biological Opinion requirements to track spring-run ",
  "loss using Coleman Late-Fall as yearling surrogates."
)

doc <- doc %>%
  body_add_par(copy_paste_text, style = "Normal")

# Save document
output_file <- "springrun_threshold_comparison.docx"
print(doc, target = output_file)

cat("\nDocument saved to:", output_file, "\n")