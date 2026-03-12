library(shiny)
library(bslib)
library(dplyr)
library(ggplot2)
library(readr)
library(scales)

# Data
raw_data <- read_csv("data/processed/processed_data.csv", show_col_types = FALSE)
regions <- sort(unique(na.omit(raw_data$Region)))
studies <- sort(unique(na.omit(raw_data$Field_of_Study)))
industries <- sort(unique(na.omit(raw_data$Top_Industry)))
degrees <- sort(unique(na.omit(raw_data$Degree_Level)))
year_min <- min(raw_data$Graduation_Year, na.rm = TRUE)
year_max <- min(raw_data$Graduation_Year, na.rm = TRUE)

# UI
ui <- page_sidebar(
  title = "Graduate Skills Employability Dashboard",
  sidebar = sidebar(
    width = 280,
    "Filters - to add"
  ),
  "Plot and KPIs - to add"
)

# Server
server <- function(input, output, session) {
  message("Data loaded: ", nrow(raw_data), " rows, ", ncol(raw_data), " columns")
  message("Columns: ", paste(names(raw_data), collapse = ",  "))
}

shinyApp(ui, server)