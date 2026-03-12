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

# KPI card helper
kpi_card <- function(title, value, median_val, q1_val, q3_val) {
  div(
    style = paste0(
      "background:#2196F3;",
      "color:white;",
      "border-radius:16px;",
      "padding:28px;",
      "text-align:center;",
      "display:flex;",
      "flex-direction:column;",
      "align-items:center;",
      "justify-content:center;",
      "min-height:200px;"
    ),
    div(
      style = "font-size:20px; font-weight:500; margin-bottom:14px;",
      title
    ),
    div(
      style = "font-size:56px; font-weight:700; line-height:1.1;",
      value
    ),
    div(
      style = "font-size:14px; opacity:0.9; line-height:1.8;",
      div(paste0("Median: ", median_val)),
      div(paste0("Bottom 25%: < ", q1_val)),
      div(paste0("Top 25%: > ", q3_val))
    )
  )
}

# UI
ui <- page_sidebar(
  title = "Graduate Skills Employability Dashboard",
  theme = bs_theme(
    bootswatch = "flatly", 
    base_font = font_google("Inter"),
    "navbar-padding-y" = "0.3rem",
    "navbar-brand-font-size" = "1rem"
  ),
  
  sidebar = sidebar(
    width = 400, # wider sidebar
    
    selectizeInput(
      inputId = "region",
      label = "Region",
      choices = regions,
      selected = regions,
      multiple = TRUE,
      options = list(plugins = list("remove_button"))
    ),
    
    selectizeInput(
      inputId = "study",
      label = "Field of Study",
      choices = studies,
      selected = studies,
      multiple = TRUE,
      options = list(plugins = list("remove_button"))
    ),
    
    selectizeInput(
      inputId = "degree",
      label = "Degree Level",
      choices = degrees,
      selected = degrees,
      multiple = TRUE,
      options = list(plugins = list("remove_button"))
    ),
    
    selectizeInput(
      inputId = "industry",
      label = "Industry",
      choices = industries,
      selected = industries,
      multiple = TRUE,
      options = list(plugins = list("remove_button"))
    ), 
    
    sliderInput(
      inputId = "grad_year",
      label = "Graduation Year",
      min = year_min,
      max = year_max,
      value = c(year_max - 4, year_max),
      step = 1,
      sep = "", 
      ticks = FALSE
    ),
    
    actionButton("reset_btn", 
                 "Reset Filters", 
                 width = "100%",
                 class = "btn-outline-primary")
  ),
  
  # KPI
  layout_columns(
    col_widths = c(4, 4, 4),
    uiOutput("kpi_emp6"),
    uiOutput("kpi_emp12"),
    uiOutput("kpi_salary")
  ), 
  
  # Plots 
  layout_columns(
    col_widths = c(6, 6),
    card(
      full_screen = TRUE,
      card_header("Top Industries by Average Starting Salary"),
      plotOutput("industries_bar", height = "400px")
    ),
    card(
      full_screen = TRUE,
      card_header("Yearly Starting Salary by Field of Study"),
      plotOutput("salary_line", height = "400px")
    )
  )
)

# Server
server <- function(input, output, session) {
  
  # Reset button
  observeEvent(input$reset_btn, {
    updateSelectInput(session, "region", selected = regions)
    updateSelectInput(session, "study", selected = studies)
    updateSelectInput(session, "degree", selected = degrees)
    updateSelectInput(session, "industry", selected = industries)
    updateSliderInput(session, "grad_year", 
                      value = c(year_max - 4, year_max))
  })
  
  # Reactive filtered data
  filtered_data <- reactive({
    raw_data |>
      filter(
        Graduation_Year >= input$grad_year[1],
        Graduation_Year <= input$grad_year[2],
        Region %in% input$region,
        Field_of_Study %in% input$study,
        Degree_Level %in% input$degree,
        Top_Industry %in% input$industry
      )
  })
  
  # KPI cards
  output$kpi_emp6 <- renderUI({
    df <- filtered_data()
    col <- df$`Employment_Rate_12_Months (%)`
    val <- if (nrow(df) == 0) "N/A" else
      paste0(round(mean(col, na.rm = TRUE), 1), "%")
    med <- if (nrow(df) == 0) "N/A" else
      paste0(round(median(col, na.rm = TRUE), 1), "%")
    q1 <- if (nrow(df) == 0) "N/A" else
      paste0(round(quantile(col, 0.25, na.rm = TRUE), 1), "%")
    q3 <- if (nrow(df) == 0) "N/A" else
      paste0(round(quantile(col, 0.75, na.rm = TRUE), 1), "%")
    kpi_card("Employment Rate (6 months)", val, med, q1, q3)
  })
  
  output$kpi_emp12 <- renderUI({
    df <- filtered_data()
    col <- df$`Employment_Rate_12_Months (%)`
    val <- if (nrow(df) == 0) "N/A" else
      paste0(round(mean(col, na.rm = TRUE), 1), "%")
    med <- if (nrow(df) == 0) "N/A" else
      paste0(round(median(col, na.rm = TRUE), 1), "%")
    q1 <- if (nrow(df) == 0) "N/A" else
      paste0(round(quantile(col, 0.25, na.rm = TRUE), 1), "%")
    q3 <- if (nrow(df) == 0) "N/A" else
      paste0(round(quantile(col, 0.75, na.rm = TRUE), 1), "%")
    kpi_card("Employment Rate (1 year)", val, med, q1, q3)
  })
  
  output$kpi_salary <- renderUI({
    df <- filtered_data()
    col <- df$Average_Starting_Salary_USD / 1000
    val <- if (nrow(df) == 0) "N/A" else
      paste0("$", round(mean(col, na.rm = TRUE), 1), "K")
    med <- if (nrow(df) == 0) "N/A" else
      paste0("$", round(median(col, na.rm = TRUE), 1), "K")
    q1 <- if (nrow(df) == 0) "N/A" else
      paste0("$", round(quantile(col, 0.25, na.rm = TRUE), 1), "K")
    q3 <- if (nrow(df) == 0) "N/A" else
      paste0("$", round(quantile(col, 0.75, na.rm = TRUE), 1), "K")
    kpi_card("Avg Starting Salary (USD)", val, med, q1, q3)
  })
  
  # Industries bar chart
  output$industries_bar <- renderPlot({
    df <- filtered_data()
    
    if (nrow(df) == 0) return(NULL)
    
    industry_summary <- df |>
      group_by(Top_Industry) |>
      summarise(avg_salary = mean(Average_Starting_Salary_USD, 
                                  na.rm = TRUE)) |>
      arrange(desc(avg_salary)) |>
      mutate(Top_Industry = factor(Top_Industry, 
                                   levels = rev(Top_Industry)))
    
    ggplot(industry_summary, 
           aes(x = avg_salary, y = Top_Industry, fill = Top_Industry)) +
      geom_col(show.legend = FALSE) +
      scale_x_continuous(labels = dollar_format()) +
      labs(x = "Average Starting Salary (USD)", y = NULL) +
      theme_minimal(base_size = 13) +
      theme(
        panel.grid.major.y = element_blank(),
        axis.text.y        = element_text(size = 11, color = "#111111"),
        axis.text.x        = element_text(color = "#111111"),
        axis.title         = element_text(color = "#111111", size = 12),
        panel.background   = element_rect(fill = "white", color = NA),
        plot.background    = element_rect(fill = "white", color = NA)
      )
  })
  
  # Salary line chart
  output$salary_line <- renderPlot({
    df <- filtered_data()
    
    if (nrow(df) == 0) return(NULL)
    
    salary_over_time <- df |>
      group_by(Graduation_Year, Field_of_Study) |>
      summarise(avg_salary = mean(Average_Starting_Salary_USD, 
                                  na.rm = TRUE),
                .groups = "drop")
    
    ggplot(salary_over_time,
           aes(x = Graduation_Year, y = avg_salary,
               color = Field_of_Study, group = Field_of_Study)) +
      geom_line(linewidth = 1) +
      geom_point(size = 2) +
      scale_x_continuous(breaks = unique(salary_over_time$Graduation_Year)) +
      scale_y_continuous(labels = dollar_format()) +
      scale_color_brewer(palette = "Set2") +
      labs(
        x     = "Graduation Year",
        y = NULL,
        color = NULL
      ) +
      theme_minimal(base_size = 13) +
      theme(
        legend.position    = "top",
        legend.text        = element_text(size = 10, color = "#111111"),
        legend.title       = element_text(color = "#111111"),
        panel.grid.minor   = element_blank(),
        axis.text          = element_text(angle = 45, hjust = 1, color = "#111111"),
        axis.title         = element_text(color = "#111111", size = 12),
        panel.background   = element_rect(fill = "white", color = NA),
        plot.background    = element_rect(fill = "white", color = NA)
      )
  })
}

shinyApp(ui, server)