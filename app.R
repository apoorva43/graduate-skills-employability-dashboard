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
year_max <- max(raw_data$Graduation_Year, na.rm = TRUE)

# UI
ui <- page_sidebar(
  title = "Graduate Skills Employability Dashboard",
  sidebar = sidebar(
    width = 280,
    
    selectInput(
      inputId = "region",
      label = "Region",
      choices = regions,
      selected = regions,
      multiple = TRUE
    ),
    
    selectInput(
      inputId = "study",
      label = "Field of Study",
      choices = studies,
      selected = studies,
      multiple = TRUE
    ),
    
    selectInput(
      inputId = "degree",
      label = "Degree Level",
      choices = degrees,
      selected = degrees,
      multiple = TRUE
    ),
    
    selectInput(
      inputId = "industry",
      label = "Industry",
      choices = industries,
      selected = industries,
      multiple = TRUE
    ), 
    
    sliderInput(
      inputId = "grad_year",
      label = "Graduation Year",
      min = year_min,
      max = year_max,
      value = c(year_max - 4, year_max),
      step = 1,
      sep = ""
    ),
    
    actionButton("reset_btn", "Reset Filters", width = "100%")
  ),
  "Plot and KPIs - to add"
)

# Server
server <- function(input, output, session) {
  message("Data loaded: ", nrow(raw_data), " rows, ", ncol(raw_data), " columns")
  message("Columns: ", paste(names(raw_data), collapse = ",  "))
  message(year_min, year_max)
}

shinyApp(ui, server)