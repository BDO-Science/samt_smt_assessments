# Delta Real-Time Forecast Summary

## Overview

This repository generates the weekly **Delta Real-Time Forecast Summary Report** using **Quarto** and **R**.

The report automatically reads model output files from weekly results folders and generates a formatted report containing:

- Forecast flow and export summaries
- Zone of Influence (ZOI) analysis
- PTM analysis
- ECO-PTM analysis
- Longfin Smelt larval population analysis
- Tables, figures, and supporting documentation

---

## Repository Structure

```text
stantec_delta_realtime_forecast/
│
├── YYYYMMDD_results/
│ ├── zoi_bins.csv
│ ├── average_exports_by_week.csv
│ ├── common_assumptions.txt
│ ├── flow_export.png
│ ├── ...
│
├── quarto_report/
│ ├── Delta_realtime_forecast_summary.qmd
│ ├── helper_functions.R
│ ├── report_config.yml
│ ├── styles.css
│ ├── static_longfin_nodes.csv
│ ├── static_sls_processing.csv
│ ├── map_delta_longfinsmelt.png
│ └── map_delta_longfinsmelt_region.png
│ └── README.md
```

---

## Main Files

### Delta_realtime_forecast_summary.qmd

Main Quarto report containing:

- Report structure
- Narrative text
- Section headings
- Figure captions
- Table captions
- Calls to helper functions

### helper_functions.R

Contains all report generation functions including:

#### Forecast Tables

- Weekly Averaged Forecasted Flow Data and Flow Bins
- Weekly Averaged CVP and SWP Exports by OMR Bin

#### PTM Analysis

- Particle fate tables
- PTM result figures

#### ECO-PTM Analysis

- Route ratio tables
- Route survival tables

#### Longfin Smelt Analysis

- Larval processing tables
- Injection node tables
- Entrainment estimate tables
- Longfin Smelt figures

#### Utility Functions

- Results folder selection
- Figure insertion
- Number formatting
- Static figure and table handling

### report_config.yml

Controls which weekly results folder is used when rendering the report.

Example:

```yaml
results_date: "20260331"
```

The report will automatically use:

```text
20260331_results
```

If left blank:

```yaml
results_date: ""
```

the report automatically selects the most recent available results folder.

### styles.css

Contains report styling including:

- Fonts
- Table formatting
- Caption formatting
- Layout adjustments

---

## Weekly Update Workflow

### Step 1

Update `report_config.yml`

Example:

```yaml
results_date: "20260331"
```

### Step 2

Render the report.

Open:

```text
Delta_realtime_forecast_summary.qmd
```

and click:

```text
Render
```

---

## Common Assumptions

The Common Assumptions section is populated automatically from:

```text
common_assumptions.txt
```

located inside the selected results folder.

Example:

```text
20260331_results/
└── common_assumptions.txt
```

This allows assumptions to be updated each week without modifying the Quarto report.

---

## Static Files

The following files are stored in the repository and do not change between weekly forecasts.

### Static Figures

```text
map_delta_longfinsmelt.png
map_delta_longfinsmelt_region.png
```

### Static Tables

```text
static_longfin_nodes.csv
static_sls_processing.csv
```

---

## Rendering the Report

### Using RStudio

Open:

```text
Delta_realtime_forecast_summary.qmd
```

Click:

```text
Render
```

### Using Terminal

```bash
quarto render Delta_realtime_forecast_summary.qmd
```

---

## Required R Packages

```r
install.packages(c(
"readr",
"dplyr",
"tidyr",
"readxl",
"yaml",
"knitr",
"kableExtra"
))
```

---

## Output

Rendering generates:

```text
Delta_realtime_forecast_summary.html
```


---

## Notes

- All weekly figures are loaded automatically from the selected results folder.
- Static figures and tables are maintained within the repository.
- Weekly assumptions are controlled through `common_assumptions.txt`.
- No code modifications should be required for routine weekly updates.

---
