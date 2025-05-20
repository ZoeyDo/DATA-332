# 🏥 Nursing Home Industry Research Project

This project explores trends in the U.S. nursing home industry from 2015 to 2021. It involves cleaning and consolidating large datasets and visualizing insights through an interactive Shiny dashboard.

[Interact with our Shiny App here](https://minhnguyen22.shinyapps.io/Final/)
---

## 📂 Data Access

Due to the large size of the files, all data is hosted externally:

🔗 [Access the dataset on Google Drive](https://drive.google.com/drive/folders/1tA6BRJbabY_oJSWi-kBEqZZwCEJ2o9Sp)

---

## 🧹 Data Cleaning Snippet
```r
library(readr)
library(dplyr)
library(lubridate)

rm(list = ls())

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

directory <- "/Users/giangdo/Documents/DATA332/r_projects/final_project"

file_pattern_provider <- file.path(directory, "ProviderInfo_*.csv")
provider_files <- Sys.glob(file_pattern_provider)
raw_provider_info <- list()

for (file_path in provider_files) {
  filename <- basename(file_path)
  year <- gsub("ProviderInfo_|\\.csv", "", filename)
  df <- read_csv_with_encodings(file_path)
  if (!is.null(df)) {
    df$year <- year
    raw_provider_info[[paste0("raw_pi_", year)]] <- df
  }
}

for (key in names(raw_provider_info)) {
  colnames(raw_provider_info[[key]]) <- tolower(colnames(raw_provider_info[[key]]))
}

create_provider_info_tables <- function(raw_provider_info) {
  tables <- list()
  for (key in names(raw_provider_info)) {
    df <- raw_provider_info[[key]]
    year <- sub("raw_pi_", "", key)
    columns_to_keep <- c(
      'provnum', 'federal.provider.number', 'provname', 'provider.name',
      'address', 'provider.address', 'city', 'provider.city',
      'state', 'provider.state', 'zip', 'provider.zip.code',
      'phone', 'provider.phone.number', 'county_ssa', 'provider.ssa.county.code',
      'county_name', 'provider.county.name', 'ownership', 'ownership.type',
      'bedcert', 'number.of.certified.beds', 'restot', 'average.number.of.residents.per.day',
      'overall_rating', 'overall.rating', 'tot_penlty_cnt', 'total.number.of.penalties',
      'rnhrd', 'reported.rn.staffing.hours.per.resident.per.day',
      'totlichrd', 'reported.licensed.staffing.hours.per.resident.per.day',
      'tothrd', 'reported.total.nurse.staffing.hours.per.resident.per.day',
      'pthrd', 'reported.physical.therapist.staffing.hours.per.resident.per.day',
      'inhosp', 'provider.resides.in.hospital', 'year'
    )
    valid_columns <- columns_to_keep[columns_to_keep %in% colnames(df)]
    if (length(valid_columns) > 0) {
      tables[[paste0('provider_basic_', year)]] <- df[, valid_columns, drop = FALSE]
    }
  }
  tables
}

clean_provider_info <- create_provider_info_tables(raw_provider_info)

for (key in names(clean_provider_info)) {
  if (key %in% c('provider_basic_2020', 'provider_basic_2021')) {
    rename_mapping <- c(
      'federal.provider.number' = 'provnum',
      'provider.name' = 'provname',
      'provider.address' = 'address',
      'provider.city' = 'city',
      'provider.state' = 'state',
      'provider.zip.code' = 'zip',
      'provider.phone.number' = 'phone',
      'provider.ssa.county.code' = 'county_ssa',
      'provider.county.name' = 'county_name',
      'ownership.type' = 'ownership',
      'number.of.certified.beds' = 'bedcert',
      'average.number.of.residents.per.day' = 'restot',
      'overall.rating' = 'overall_rating',
      'total.number.of.penalties' = 'tot_penlty_cnt',
      'reported.rn.staffing.hours.per.resident.per.day' = 'rnhrd',
      'reported.licensed.staffing.hours.per.resident.per.day' = 'totlichrd',
      'reported.total.nurse.staffing.hours.per.resident.per.day' = 'tothrd',
      'reported.physical.therapist.staffing.hours.per.resident.per.day' = 'pthrd',
      'provider.resides.in.hospital' = 'inhosp'
    )
    current_cols <- colnames(clean_provider_info[[key]])
    for (old_name in names(rename_mapping)) {
      if (old_name %in% current_cols) {
        current_cols[current_cols == old_name] <- rename_mapping[old_name]
      }
    }
    colnames(clean_provider_info[[key]]) <- current_cols
  }
}

union_provider_info <- do.call(rbind, clean_provider_info)
rownames(union_provider_info) <- NULL

file_pattern_cost <- file.path(directory, "*_CostReport.csv")
cost_files <- Sys.glob(file_pattern_cost)
raw_cost_report <- list()

for (file_path in cost_files) {
  filename <- basename(file_path)
  year <- gsub("_CostReport\\.csv", "", filename)
  df <- read_csv_with_encodings(file_path)
  if (!is.null(df)) {
    df$year <- year
    raw_cost_report[[paste0("raw_cost_", year)]] <- df
  }
}

for (key in names(raw_cost_report)) {
  colnames(raw_cost_report[[key]]) <- tolower(colnames(raw_cost_report[[key]]))
}

create_cost_report_tables <- function(raw_cost_report) {
  tables <- list()
  for (key in names(raw_cost_report)) {
    df <- raw_cost_report[[key]]
    year <- sub("raw_cost_", "", key)
    columns_to_keep <- c(
      'provider_ccn', 'provider.ccn', 'rural_versus_urban', 'rural.versus.urban',
      'gross_revenue', 'gross.revenue', 'net_income', 'net.income',
      'net_patient_revenue', 'net.patient.revenue', 'number_of_beds', 'number.of.beds',
      'total_income', 'total.income', 'total_salaries_adjusted', 'total.salaries..adjusted.',
      'fiscal_year_begin_date', 'fiscal_year_end_date', 'fiscal.year.begin.date', 'fiscal.year.end.date',
      'less.total.operating.expense', 'less_total_operating_expense',
      'net_income_from_patients', 'net.income.from.service.to.patients',
      'overhead_non_salary_costs', 'overhead.non.salary.costs',
      'wage_related_costs_core', 'wage.related.costs..core.',
      'less_discounts_on_patients', "less.contractual.allowance.and.discounts.on.patients..accounts",
      'snf_admissions_total', 'nf.admissions.total',
      'total.days.total', 'total_days_total',
      'total_bed_days_available', 'total.bed.days.available', 'year'
    )
    valid_columns <- columns_to_keep[columns_to_keep %in% colnames(df)]
    if (length(valid_columns) > 0) {
      tables[[paste0('cost_report_clean_', year)]] <- df[, valid_columns, drop = FALSE]
    }
  }
  tables
}

clean_cost_report <- create_cost_report_tables(raw_cost_report)

for (key in names(clean_cost_report)) {
  if (key %in% c('cost_report_clean_2020', 'cost_report_clean_2021')) {
    rename_mapping <- c(
      'provider.ccn' = 'provider_ccn',
      'rural.versus.urban' = 'rural_versus_urban',
      'gross.revenue' = 'gross_revenue',
      'net.income' = 'net_income',
      'net.patient.revenue' = 'net_patient_revenue',
      'number.of.beds' = 'number_of_beds',
      'total.income' = 'total_income',
      'total.salaries..adjusted.' = 'total_salaries_adjusted',
      'fiscal.year.begin.date' = 'fiscal_year_begin_date',
      'fiscal.year.end.date' = 'fiscal_year_end_date',
      'less.total.operating.expense' = 'less_total_operating_expense',
      'net.income.from.service.to.patients' = 'net_income_from_patients',
      'overhead.non.salary.costs' = 'overhead_non_salary_costs',
      'wage.related.costs..core.' = 'wage_related_costs_core',
      "less.contractual.allowance.and.discounts.on.patients..accounts" = 'less_discounts_on_patients',
      'nf.admissions.total' = 'snf_admissions_total',
      'total.days.total' = 'total_days_total',
      'total.bed.days.available' = 'total_bed_days_available'
    )
    current_cols <- colnames(clean_cost_report[[key]])
    for (old_name in names(rename_mapping)) {
      if (old_name %in% current_cols) {
        current_cols[current_cols == old_name] <- rename_mapping[old_name]
      }
    }
    colnames(clean_cost_report[[key]]) <- current_cols
  }
}

union_cost_report <- do.call(rbind, clean_cost_report)
rownames(union_cost_report) <- NULL

union_provider_info$provnum <- sprintf("%06s", as.character(union_provider_info$provnum))
union_cost_report$provider_ccn <- sprintf("%06s", as.character(union_cost_report$provider_ccn))

nursing_merge <- merge(
  union_provider_info,
  union_cost_report,
  by.x = c("provnum", "year"),
  by.y = c("provider_ccn", "year"),
  all.y = TRUE
)

nursing_merge <- unique(nursing_merge)

nursing_merge$fiscal_year_begin_date <- as.Date(nursing_merge$fiscal_year_begin_date)
nursing_merge$fiscal_year_end_date <- as.Date(nursing_merge$fiscal_year_end_date, format = "%m/%d/%Y")

nursing_merge$phone <- as.character(nursing_merge$phone)

nursing_merge$inhosp <- ifelse(
  nursing_merge$inhosp == "N", "NO",
  ifelse(nursing_merge$inhosp == "Y", "YES", nursing_merge$inhosp)
)

provider_info_final <- nursing_merge

save(provider_info_final, file = "provider_info_final.rda")
write.csv(provider_info_final, file = "provider_info_final.csv", row.names = FALSE)
```
---

## 🧹 Shinyapp Snippet
```r
ui <- navbarPage("Nursing Home Industry Research",
                 tabPanel("Research Overview",
                          fluidPage(
                            fluidRow(
                              h3("NURSING HOME INDUSTRY RESEARCH", align = "Center"),
                              tags$style(HTML("
                            .centerFigure {
                            display: flex;
                            flex-direction: column;
                            align-items: center;
                            margin-top: 20px;}"))
                            ),
                            tags$figure(
                              class = "centerFigure",
                              tags$img(
                                src = "nursing.jpg",
                                width = 500,
                                alt = "Nursing Home Animation"
                              )
                            )
                          ),
                          fluidRow(
                            h4("Research Explanation"),
                            div(
                              style = "border: 1px solid #ddd; border-radius: 5px; padding: 15px; height: 300px; overflow-y: auto;",
                              htmlOutput("research_explanation")
                            )
                          )
                 ),
                 tabPanel("Industry Overview",
                          fluidPage(
                            fluidRow(
                              column(12,
                                     h4("Industry KPI"),
                                     tableOutput("industry_table")
                              )
                            ),
                            fluidRow(
                              column(12,
                                     wellPanel(
                                       selectInput("selected_year", "Year:",
                                                   choices = c("All", unique(as.character(dataset$year))),
                                                   selected = "All")
                                     )
                              )
                            ),
                            fluidRow(
                              column(6,
                                     h4("Occupancy Rate by Ownership Type"),
                                     plotOutput("occupancy_ownership", height = "100px")
                              ),
                              column(6,
                                     h4("Rating by Ownership Type"),
                                     plotOutput("rating_ownership", height = "100px")
                              )
                            ),
                            fluidRow(
                              column(6,
                                     h4("Number of Providers by State"),
                                     plotOutput("provider_state", height = "300px")
                              ),
                              column(6,
                                     h4("Net Income by State"),
                                     plotOutput("net_income_state", height = "300px")
                              )
                            ),
                            fluidRow(
                              h4("Industry Overview Explanations"),
                              div(
                                style = "border: 1px solid #ddd; border-radius: 5px; padding: 15px; height: 300px; overflow-y: auto;",
                                htmlOutput("industry_comments")
                              )
                            )
                          )
                 ),
                 tabPanel("Financial Performance",
                          fluidPage(
                            # Financial KPI table at the top
                            fluidRow(
                              column(12,
                                     h4("Financial KPI (2015-2021)"),
                                     tableOutput("financial_table")
                              )
                            ),
                            fluidRow(
                              column(12,
                                     wellPanel(
                                       selectInput("selected_state", "State:",
                                                   choices = c("All", dataset %>%
                                                                 filter(!is.na(state) & state != "NA") %>%
                                                                 pull(state) %>%
                                                                 as.character() %>%
                                                                 unique() %>%
                                                                 sort()),
                                                   selected = "All")
                                     )
                              )
                            ),
                            
                            # Income growth rate and income structure side by side
                            fluidRow(
                              column(6,
                                     h4("Income Growth Rate"),
                                     plotOutput("income_growth", height = "300px")
                              ),
                              column(6,
                                     h4("Total Income vs. Operations Income"),
                                     plotOutput("income_structure", height = "300px")
                              )
                            ),
                            
                            # Revenue vs cost and analytics insights side by side
                            fluidRow(
                              column(6,
                                     h4("Revenue vs. Cost"),
                                     plotOutput("revenue_vs_cost", height = "300px")
                              ),
                              column(6,
                                     h4("Financial Explanations"),
                                     div(
                                       style = "border: 1px solid #ddd; border-radius: 5px; padding: 15px; height: 300px; overflow-y: auto;",
                                       htmlOutput("financial_comments")
                                     )
                              )
                            )
                          )
                 ),
                 tabPanel("Predicted Income",
                          fluidPage(
                            fluidRow(
                              column(9,
                                     h4("Industry Net Income Forecast with Fixed Effects"),
                                     plotOutput("income_forecast_plot")),
                              column(3,
                                     tableOutput("model_metrics"))
                            ),
                            fluidRow(
                              h4("Forecasting Explanations"),
                              div(
                                style = "border: 1px solid #ddd; border-radius: 5px; padding: 15px; height: 300px; overflow-y: auto;",
                                htmlOutput("forecasting_comments")
                              )
                            )
                          )
                 )
)

server <- function(input, output, tab) {
  
  filtered_data <- reactive({
    if(input$selected_state == "All") {
      dataset
    } else {
      dataset %>% filter(as.character(state) == as.character(input$selected_state))
    }
  })
  
  filtered_year <- reactive({

    if(input$selected_year == "All") {
      dataset  # Return all data when "All" is selected
    } else {
      dataset %>% filter(as.numeric(year) == as.numeric(input$selected_year))
    }
  })  
  
  output$research_explanation <- renderUI({
    HTML("
    <p>This research project investigates the financial performance and investment potential of the U.S. nursing home industry, using a near-complete dataset (2015–2021) from the U.S. Department of Health and Human Services.</p>
    <ul>
      <li>The dataset covers over 95% of all licensed nursing home providers, offering a robust foundation for industry-wide analysis.</li>
      <li>Our primary objective is to assess whether this sector presents a viable opportunity for investment by examining key operational and financial metrics.</li>
      <li>The scope of this project includes national and state-level trend analysis, ownership structure comparisons, and forecasting net income using a fixed effects regression model.</li>
      <li>While we identified fixed effects as a theoretically sound model for handling panel data, our implementation showed high residual variance, suggesting room for model refinement in future research. Ideas we plan to explore in subsequent iterations include testing alternative predictor variables, exploring different dependent variables (e.g., margin or ROI), and incorporating external economic indicators.</li>
   </ul>
    <p>This research was conducted by <a href = 'https://www.linkedin.com/in/minhnbnguyen/' target = '_blank'> Minh Nguyen</a> and <a href = 'https://www.linkedin.com/in/gianghdo/' target = '_blank'> Zoey Do </a>under the mentorship of <a href ='https://www.linkedin.com/in/john-brosius/' target = '_blank'> John Brosius</a>.</p>
  ")
  })

  output$industry_table <- renderTable({
    filtered_year() %>%
      mutate(
        restot = as.numeric(restot),
        tot_penlty_cnt = as.numeric(tot_penlty_cnt),
        total_days_total_annualized = as.numeric(total_days_total_annualized),
        total_bed_days_available_annualized = as.numeric(total_bed_days_available_annualized),
        overall_rating = as.numeric(overall_rating),
        
        occupancy_rate = case_when(
          is.na(total_bed_days_available_annualized) |
            total_bed_days_available_annualized == 0 ~ NA_real_,
          TRUE ~ total_days_total_annualized/total_bed_days_available_annualized * 100
        )
      ) %>%
      summarise(
        Total_Providers = comma(as.integer(n_distinct(provnum, na.rm = TRUE))),
        Residents_per_Day = comma(as.integer(sum(restot, na.rm = TRUE))),
        Total_Penalty = comma(as.integer(sum(tot_penlty_cnt, na.rm = TRUE))),
        Avg_Rating = mean(overall_rating, na.rm = TRUE),
        
        Occupancy_Rate = case_when(
          all(is.na(occupancy_rate)) ~ "N/A",
          TRUE ~ paste0(round(mean(occupancy_rate, na.rm = TRUE), 2), "%")
        )
      )
  })
  
