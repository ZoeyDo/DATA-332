Because our data is too big, we upload all of the data files to this drive: https://drive.google.com/drive/folders/1tA6BRJbabY_oJSWi-kBEqZZwCEJ2o9Sp



Code snippet of the cleaning code:
# Load necessary libraries
library(readr)
library(dplyr)
library(lubridate)

# Read and clean CSV files with multiple encodings
read_csv_with_encodings <- function(file_path) {
  encodings <- c("UTF-8", "latin1", "ISO-8859-1", "CP1252", "UTF-16", "UTF-32")
  for (encoding in encodings) {
    tryCatch({
      df <- read.csv(file_path, encoding = encoding, stringsAsFactors = FALSE)
      return(df)
    }, error = function(e) {})
  }
  return(NULL)
}

# Function to standardize and clean provider information
create_provider_info_tables <- function(raw_provider_info) {
  tables <- list()
  for (key in names(raw_provider_info)) {
    df <- raw_provider_info[[key]]
    year <- tail(strsplit(key, "_")[[1]], 1)
    columns_to_keep <- c('provnum', 'federal.provider.number', 'provname', 'provider.name',
                         'address', 'provider.address', 'city', 'provider.city', 'state', 'provider.state',
                         'zip', 'provider.zip.code', 'phone', 'provider.phone.number',
                         'county_ssa', 'provider.ssa.county.code', 'county_name', 'provider.county.name',
                         'ownership', 'ownership.type', 'bedcert', 'number.of.certified.beds',
                         'restot', 'average.number.of.residents.per.day', 'overall_rating', 'overall.rating',
                         'tot_penlty_cnt', 'total.number.of.penalties', 'rnhrd',
                         'reported.rn.staffing.hours.per.resident.per.day',
                         'totlichrd', 'reported.licensed.staffing.hours.per.resident.per.day',
                         'tothrd', 'reported.total.nurse.staffing.hours.per.resident.per.day',
                         'pthrd', 'reported.physical.therapist.staffing.hours.per.resident.per.day',
                         'inhosp', 'provider.resides.in.hospital', 'year')
    valid_columns <- columns_to_keep[columns_to_keep %in% colnames(df)]
    if (length(valid_columns) > 0) {
      tables[[paste0('provider_basic_', year)]] <- df[, valid_columns, drop = FALSE]
    }
  }
  return(tables)
}

# Function to fill missing provider information from available data
fill_missing_provider_info <- function(df) {
  provider_cols <- c('provnum', 'provname', 'address', 'city', 'state', 'zip',  
                     'phone', 'county_ssa', 'county_name', 'ownership', 'rural_versus_urban')
  provider_cols <- provider_cols[provider_cols %in% colnames(df)]
  provider_details <- list()

  is_missing_safe <- function(x) {
    x_clean <- iconv(as.character(x), from = "UTF-8", to = "UTF-8", sub = "")
    return(is.na(x_clean) || x_clean == "" || tolower(x_clean) == "nan")
  }

  for (i in 1:nrow(df)) {
    provnum <- df$provnum[i]
    if (!is.na(provnum) && provnum != '') {
      details <- list()
      for (col in provider_cols) {
        if (!is_missing_safe(df[i, col])) {
          details[[col]] <- df[i, col]
        }
      }
      if (!(provnum %in% names(provider_details)) || 
          length(details) > length(provider_details[[provnum]])) {
        provider_details[[provnum]] <- details
      }
    }
  }

  for (i in 1:nrow(df)) {
    provnum <- df$provnum[i]
    if (!is.na(provnum) && provnum != '' && provnum %in% names(provider_details)) {
      for (col in provider_cols) {
        if (is_missing_safe(df[i, col]) && col %in% names(provider_details[[provnum]])) {
          df[i, col] <- provider_details[[provnum]][[col]]
        }
      }
    }
  }
  return(df)
}

Code snippet of the shinyapp code:
# Load libraries
library(shiny)
library(leaflet)
library(dplyr)
library(lubridate)
library(readr)
library(ggplot2)
library(tidyr)
library(DT)
library(scales)
library(maps)
library(mapproj)
library(viridis)

# Load data
dataset <- readRDS("cleaned_nursing.rds")

# UI
ui <- navbarPage("Nursing Home Industry Research",
  tabPanel("Research Overview",
    fluidPage(
      fluidRow(
        h3("NURSING HOME INDUSTRY RESEARCH", align = "Center"),
        tags$style(HTML(".centerFigure {display: flex; flex-direction: column; align-items: center; margin-top: 20px;}")),
        tags$figure(class = "centerFigure", tags$img(src = "nursing.jpg", width = 500, alt = "Nursing Home Animation"))
      ),
      fluidRow(
        h4("Research Explanation"),
        div(style = "border: 1px solid #ddd; border-radius: 5px; padding: 15px; height: 300px; overflow-y: auto;",
            htmlOutput("research_explanation"))
      )
    )
  ),
  tabPanel("Industry Overview", fluidPage(
    # KPI Table
    fluidRow(column(12, h4("Industry KPI"), tableOutput("industry_table"))),
    # Year selector
    fluidRow(column(12, wellPanel(selectInput("selected_year", "Year:", choices = c("All", unique(as.character(dataset$year))), selected = "All")))),
    # Plots
    fluidRow(
      column(6, h4("Occupancy Rate by Ownership Type"), plotOutput("occupancy_ownership", height = "100px")),
      column(6, h4("Rating by Ownership Type"), plotOutput("rating_ownership", height = "100px"))
    ),
    fluidRow(
      column(6, h4("Number of Providers by State"), plotOutput("provider_state", height = "300px")),
      column(6, h4("Net Income by State"), plotOutput("net_income_state", height = "300px"))
    ),
    # Commentary
    fluidRow(h4("Industry Overview Explanations"),
             div(style = "border: 1px solid #ddd; border-radius: 5px; padding: 15px; height: 300px; overflow-y: auto;",
                 htmlOutput("industry_comments")))
  )),
  tabPanel("Financial Performance", fluidPage(
    fluidRow(column(12, h4("Financial KPI (2015-2021)"), tableOutput("financial_table"))),
    fluidRow(column(12, wellPanel(selectInput("selected_state", "State:", choices = c("All", sort(unique(as.character(na.omit(dataset$state))))), selected = "All")))),
    fluidRow(
      column(6, h4("Income Growth Rate"), plotOutput("income_growth", height = "300px")),
      column(6, h4("Total Income vs. Operations Income"), plotOutput("income_structure", height = "300px"))
    ),
    fluidRow(
      column(6, h4("Revenue vs. Cost"), plotOutput("revenue_vs_cost", height = "300px")),
      column(6, h4("Financial Explanations"),
             div(style = "border: 1px solid #ddd; border-radius: 5px; padding: 15px; height: 300px; overflow-y: auto;",
                 htmlOutput("financial_comments")))
    )
  )),
  tabPanel("Predicted Income", fluidPage(
    fluidRow(
      column(9, h4("Industry Net Income Forecast with Fixed Effects"), plotOutput("income_forecast_plot")),
      column(3, tableOutput("model_metrics"))
    ),
    fluidRow(h4("Forecasting Explanations"),
             div(style = "border: 1px solid #ddd; border-radius: 5px; padding: 15px; height: 300px; overflow-y: auto;",
                 htmlOutput("forecasting_comments")))
  ))
)

# Server
server <- function(input, output, session) {

  # Reactive filters
  filtered_data <- reactive({
    if (input$selected_state == "All") dataset
    else dataset %>% filter(state == input$selected_state)
  })
  
  filtered_year <- reactive({
    if (input$selected_year == "All") dataset
    else dataset %>% filter(year == as.numeric(input$selected_year))
  })

  # Outputs
  output$research_explanation <- renderUI({ ... })   # Full HTML explanation here
  output$industry_table <- renderTable({ ... })      # KPI aggregation
  output$occupancy_ownership <- renderPlot({ ... })  # Occupancy plot
  output$rating_ownership <- renderPlot({ ... })     # Rating plot
  output$industry_comments <- renderUI({ ... })      # Overview commentary
  output$provider_state <- renderPlot({ ... })       # Heatmap of providers
  output$net_income_state <- renderPlot({ ... })     # Heatmap of income
  output$financial_table <- renderTable({ ... })     # Financial KPIs
  output$income_growth <- renderPlot({ ... })        # Income growth
  output$income_structure <- renderPlot({ ... })     # Income structure
  output$revenue_vs_cost <- renderPlot({ ... })      # Revenue vs cost
  output$financial_comments <- renderUI({ ... })     # Financial comments
  output$income_forecast_plot <- renderPlot({ ... }) # Forecast
  output$model_metrics <- renderTable({ ... })       # Forecast model summary
  output$forecasting_comments <- renderUI({ ... })   # Forecast commentary
}

# Run App
shinyApp(ui = ui, server = server)
