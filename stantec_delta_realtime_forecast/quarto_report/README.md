# Delta Real-Time Forecast Summary

## Overview

This repository generates the weekly **Delta Real-Time Forecast Summary Report** using **Quarto** and **R**.

The report automatically reads model outputs from a selected weekly results folder and generates a formatted HTML report containing:

- Forecast Flow and Export summaries
- Zone of Influence (ZOI) analysis
- PTM analysis
- ECO-PTM analysis
- Longfin Smelt Larval Population and PTM analysis
- Interactive HTML visualizations
- Tables, figures, and supporting documentation

The workflow is designed so that routine weekly updates require **no modifications to the Quarto or R source code**. Users only need to update the weekly results folder and render the report.

---

# Repository Structure

```text
stantec_delta_realtime_forecast/
│
├── YYYYMMDD_results/
│ ├── average_exports_by_week.csv
│ ├── zoi_bins.csv
│ ├── common_assumptions.txt
│ ├── table2_notes.txt
│ ├── survival_combined.csv
│ ├── FlowExportReviewPlots.html
│ ├── ZOI_Proportional_ChannelLength_week1.html
│ ├── ZOI_Proportional_ChannelLength_week2.html
│ ├── ZOI_Proportional_ChannelLength_week3.html
│ ├── PTM_*.html
│ ├── LFS_Scenario_*.html
│ ├── *_entrainment_OMR_*.html
│ ├── *.csv
│ ├── *.png
│ └── ...
│
└── quarto_report/
├── Delta_realtime_forecast_summary.qmd
├── helper_functions.R
├── report_config.yml
├── styles.css
├── static_longfin_nodes.csv
├── static_sls_processing.csv
├── map_delta_longfinsmelt.png
├── map_delta_longfinsmelt_region.png
├── ptm_injection_flux_locations.png
└── README.md
```

---

# Main Files

## Delta_realtime_forecast_summary.qmd

The main Quarto report containing:

- Report structure
- Narrative text
- Section headings
- Figure captions
- Table captions
- Calls to helper functions

The report automatically determines the available OMR scenarios from the weekly input files. No scenario-specific code modifications are required.

---

## helper_functions.R

Contains all functions used to generate the report.

### Forecast Section

- Weekly Averaged Forecasted Flow Data
- Weekly Averaged Export Tables
- Interactive Flow Export visualization

### Zone of Influence (ZOI)

- Weekly ZOI summary tables
- Interactive proportional channel length figures

### PTM Analysis

- Particle fate summary tables
- Interactive Neutral Particle figures
- Interactive Surface-Oriented Particle figures

### ECO-PTM Analysis

- Route ratio summary tables
- Route survival summary tables
- Automatic detection of all model runs from `survival_combined.csv`

### Longfin Smelt Analysis

- Static processing tables
- Injection node tables
- Weekly entrainment estimate tables
- Interactive LFS scenario figures
- Interactive cumulative entrainment maps

### Utility Functions

- Automatic results folder selection
- Automatic OMR scenario detection
- Interactive HTML figure insertion
- Static CSV table generation
- Caption formatting
- Number formatting

---

## report_config.yml

Controls which weekly results folder is used when rendering the report.

Example:

```yaml
results_date: "20260623"
```

The report automatically uses:

```text
20260623_results
```

If left blank:

```yaml
results_date: ""
```

the report automatically selects the most recent available results folder.

---

## styles.css

Contains report styling including:

- Fonts
- Table formatting
- Figure and caption formatting
- Interactive HTML figure sizing
- Layout adjustments

---

# Weekly Update Workflow

## Step 1

Copy the new weekly outputs into a folder named:

```text
YYYYMMDD_results
```

Example:

```text
20260623_results
```

This folder should contain all required CSV, PNG, HTML, and text files produced during the weekly assessment.

---

## Step 2

Update `report_config.yml`.

Example:

```yaml
results_date: "20260623"
```

Or leave blank:

```yaml
results_date: ""
```

to automatically select the newest available results folder.

---

## Step 3

Open:

```text
Delta_realtime_forecast_summary.qmd
```

and click **Render**.

No additional code modifications are required.

---

# Dynamic Weekly Content

The following report components are automatically loaded from the selected weekly results folder.

## Common Assumptions

Loaded from:

```text
common_assumptions.txt
```

## Forecast Table Notes

Loaded from:

```text
table2_notes.txt
```

## Forecast Flow and Export

- `average_exports_by_week.csv`
- `zoi_bins.csv`
- `FlowExportReviewPlots.html`

## Zone of Influence (ZOI)

- Weekly ZOI CSV files
- Interactive proportional channel length HTML figures

## PTM Analysis

- Particle fate CSV files
- Interactive Neutral Particle HTML figures
- Interactive Surface-Oriented Particle HTML figures

## ECO-PTM Analysis

Generated from:

```text
survival_combined.csv
```

The report automatically detects all available model runs using the `Model_Run` column. No hard-coded OMR scenarios are required.

## Longfin Smelt Larval Population and PTM Analysis

Generated from:

- Weekly entrainment CSV files
- Interactive LFS scenario HTML figures
- Interactive cumulative entrainment map HTML files

Rows containing negative placeholder values (e.g., **-6500** or **-6250**) are automatically excluded from the weekly entrainment estimate tables.

---

# Static Files

The following files remain in the repository and typically do not change between weekly reports.

## Static Figures

```text
map_delta_longfinsmelt.png
map_delta_longfinsmelt_region.png
ptm_injection_flux_locations.png
```

## Static Tables

```text
static_longfin_nodes.csv
static_sls_processing.csv
```

---

# Rendering the Report

## Using RStudio

Open:

```text
Delta_realtime_forecast_summary.qmd
```

Click **Render**.

## Using the Terminal

```bash
quarto render Delta_realtime_forecast_summary.qmd
```

---

# Required R Packages

```r
install.packages(c(
"readr",
"dplyr",
"tidyr",
"readxl",
"yaml",
"knitr",
"kableExtra",
"htmltools",
"xml2",
"stringr"
))
```

---

# Output

Rendering generates:

```text
Delta_realtime_forecast_summary.html
```

---

# Notes

- All weekly report content is automatically loaded from the selected results folder.
- OMR scenarios are detected dynamically from the available input files.
- Interactive HTML figures are embedded throughout the report for Forecast Flow & Export, ZOI, PTM, Longfin Smelt scenarios, and cumulative entrainment maps.
- ECO-PTM route survival tables are generated automatically from `survival_combined.csv`.
- Weekly assumptions and Forecast Table notes are populated directly from text files within the results folder.
- Static maps and lookup tables remain in the repository.
- Routine weekly updates should only require replacing the contents of the weekly results folder, updating `report_config.yml` (if desired), and rendering the report.

