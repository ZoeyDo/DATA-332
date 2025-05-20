# 🏥 Nursing Home Industry Research Project

This project explores trends in the U.S. nursing home industry from 2015 to 2021. It involves cleaning and consolidating large datasets and visualizing insights through an interactive Shiny dashboard.

---

## 📂 Data Access

Due to the large size of the files, all data is hosted externally:

🔗 [Access the dataset on Google Drive](https://drive.google.com/drive/folders/1tA6BRJbabY_oJSWi-kBEqZZwCEJ2o9Sp)

---

## 🧹 Data Cleaning Snippet
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

---

## 🧹 Shinyapp Snippet
library(readr)
library(dplyr)
library(lubridate)

read_csv_with_encodings <- function(file_path) {
  encodings <- c("UTF-8", "latin1", "ISO-8859-1", "CP1252", "UTF-16", "UTF-32")
  for (enc in encodings) {
    tryCatch({
      df <- read.csv(file_path, encoding = enc, stringsAsFactors = FALSE)
      return(df)
    }, error = function(e) {})
  }
  return(NULL)
}

create_provider_info_tables <- function(raw_provider_info) {
  tables <- list()
  for (key in names(raw_provider_info)) {
    df <- raw_provider_info[[key]]
    year <- tail(strsplit(key, "_")[[1]], 1)
    cols_to_keep <- c('provnum', 'provname', 'address', 'city', 'state', 'zip', 'phone', 'ownership', 'year')
    valid_cols <- intersect(cols_to_keep, colnames(df))
    tables[[paste0('provider_basic_', year)]] <- df[, valid_cols, drop = FALSE]
  }
  return(tables)
}

fill_missing_provider_info <- function(df) {
  provider_cols <- c('provnum', 'provname', 'address', 'city', 'state', 'zip', 'phone', 'ownership')
  provider_cols <- intersect(provider_cols, colnames(df))
  provider_details <- list()

  is_missing <- function(x) {
    x <- iconv(as.character(x), from = "UTF-8", to = "UTF-8", sub = "")
    is.na(x) || x == "" || tolower(x) == "nan"
  }

  for (i in seq_len(nrow(df))) {
    provnum <- df$provnum[i]
    if (!is.na(provnum) && provnum != '') {
      details <- list()
      for (col in provider_cols) {
        if (!is_missing(df[i, col])) {
          details[[col]] <- df[i, col]
        }
      }
      if (!(provnum %in% names(provider_details)) || length(details) > length(provider_details[[provnum]])) {
        provider_details[[provnum]] <- details
      }
    }
  }

  for (i in seq_len(nrow(df))) {
    provnum <- df$provnum[i]
    if (!is.na(provnum) && provnum != '' && provnum %in% names(provider_details)) {
      for (col in provider_cols) {
        if (is_missing(df[i, col]) && col %in% names(provider_details[[provnum]])) {
          df[i, col] <- provider_details[[provnum]][[col]]
        }
      }
    }
  }
  return(df)
}

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

dataset <- readRDS("cleaned_nursing.rds")

ui <- navbarPage("Nursing Home Industry Research")

server <- function(input, output, session) {}

shinyApp(ui = ui, server = server)
