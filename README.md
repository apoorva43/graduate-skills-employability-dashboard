# Graduate Skills Employability Dashboard

Individual assignment re-implementation of the [group project dashboard](https://github.com/UBC-MDS/DSCI-532_2026_12_GradSkills) in **Shiny for R**.

## Try it yourself

Stable app: <https://019ce081-1bab-f2cb-0c84-52e5e2236beb.share.connect.posit.cloud/>

## About

This dashboard explores graduate employability outcomes across universities, regions, fields of study, industries and graduation years. It allows users to interactively filter and visualize:

- **Employment Rate** at 6 months and 1 year after graduation
- **Average Starting Salary** across industries and fields of study
- Trends over time by graduation year

## Features

- **1 reactive calculation** - filtered dataframe that updates all outputs simultaneously
- **5 input components** - Region, Field of Study, Degree Level, Industry (all multi-select with remove buttons) and a Graduation Year range slider
- **3 KPI cards** - Employment Rate (6 months), Employment Rate (1 year), Average Starting Salary with median and quartile breakdowns
- **2 plots** - Top Industries by Average Starting Salary (bar chart) and Yearly Starting Salary by Field of Study (line chart)

## Installation & Running Locally

### 1. Clone the repo
```bash
git clone https://github.com/apoorva43/graduate-skills-employability-dashboard
cd graduate-skills-employability-dashboard
```

### 2. Install R packages
Open R or RStudio and run:
```r
install.packages(c("shiny", "bslib", "dplyr", "ggplot2", "readr", "scales", "rsconnect"))
```

### 3. Run the app
```r
shiny::runApp("app.R")
```
Or open `app.R` in RStudio and click **Run App**.

## Repository Structure
```
├── app.R                        # Main Shiny for R application
├── data/
    └── processed/
│       └── processed_data.csv   # Processed graduate employability dataset
└── README.md
```

## Dataset

This project uses a synthetic dataset modeled after graduate employability indicators including:
- Employment rates at 6 and 12 months post-graduation
- Average starting salaries by field, industry, and region
- University, degree level, and graduation year metadata

Original data can be accessed via this [link to Kaggle](https://www.kaggle.com/datasets/zulqarnain11/global-graduate-skills-and-employability-index).

## Author

Apoorva Srivastava
