library(shiny)
library(bslib)
library(dplyr)
library(ggplot2)
library(readr)
library(scales)

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
  # to add
}

shinyApp(ui, server)