# Clean and convert data
library(readr)
library(dplyr) 
library(lubridate)

rm(list = ls())

# CLEAN PROVIDER INFO
# Function to read CSV files with different encodings
read_csv_with_encodings <- function(file_path) {
  encodings <- c("UTF-8", "latin1", "ISO-8859-1", "CP1252", "UTF-16", "UTF-32")
  
  for (encoding in encodings) {
    tryCatch({
      df <- read.csv(file_path, encoding = encoding, stringsAsFactors = FALSE)
      cat(sprintf("Successfully read the file with %s encoding\n", encoding))
      return(df)
    }, error = function(e) {
      cat(sprintf("Failed to read with %s encoding\n", encoding))
    })
  }
  
  cat("Could not read the file with any of the common encodings\n")
  return(NULL)
}

directory <- "/Users/giangdo/Documents/DATA332/r_projects/final_project"

# Get all CSV files matching the pattern
file_pattern <- file.path(directory, "ProviderInfo_*.csv")
files <- Sys.glob(file_pattern)

# Dictionary (named list) to store original DataFrames
raw_provider_info <- list()

# Process each file
for (file_path in files) {
  filename <- basename(file_path)
  year <- gsub("ProviderInfo_|.csv", "", filename)
  
  df <- read_csv_with_encodings(file_path)
  
  if (!is.null(df)) {
    df$year <- year
    raw_provider_info[[paste0("raw_pi_", year)]] <- df
  }
}

# Lowercase all column name
for (key in names(raw_provider_info)) {
  colnames(raw_provider_info[[key]]) <- tolower(colnames(raw_provider_info[[key]]))
}

# List (equivalent to dictionary) to store custom tables
clean_provider_info <- list()

# Define a function to create provider info tables
create_provider_info_tables <- function(raw_provider_info) {
  tables <- list()
  
  for (key in names(raw_provider_info)) {
    df <- raw_provider_info[[key]]
    year <- strsplit(key, "_")[[1]][length(strsplit(key, "_")[[1]])]
    
    # List the columns to keep
    columns_to_keep <- c(
      'provnum', 'federal.provider.number',
      'provname', 'provider.name',
      'address', 'provider.address',
      'city', 'provider.city',
      'state', 'provider.state',
      'zip', 'provider.zip.code',
      'phone', 'provider.phone.number',
      'county_ssa', 'provider.ssa.county.code',
      'county_name', 'provider.county.name',
      'ownership', 'ownership.type',
      'bedcert', 'number.of.certified.beds',
      'restot', 'average.number.of.residents.per.day',
      'overall_rating', 'overall.rating',
      'tot_penlty_cnt', 'total.number.of.penalties',
      'rnhrd', 'reported.rn.staffing.hours.per.resident.per.day',
      'totlichrd', 'reported.licensed.staffing.hours.per.resident.per.day',
      'tothrd', 'reported.total.nurse.staffing.hours.per.resident.per.day',
      'pthrd', 'reported.physical.therapist.staffing.hours.per.resident.per.day',
      'inhosp', 'provider.resides.in.hospital',
      'year'
    )
    
    # Only keep columns that exist in the dataframe
    valid_columns <- columns_to_keep[columns_to_keep %in% colnames(df)]
    
    # Create new table with only the columns you need
    if (length(valid_columns) > 0) {
      tables[[paste0('provider_basic_', year)]] <- df[, valid_columns, drop = FALSE]
    }
  }
  
  return(tables)
}

# Create the tables
provider_info_tables <- create_provider_info_tables(raw_provider_info)

# Add the tables to clean_provider_info
for (name in names(provider_info_tables)) {
  clean_provider_info[[name]] <- provider_info_tables[[name]]
}

# Rename 2020 and 2021 file columns to standard names
for (key in names(clean_provider_info)) {
  if (key %in% c('provider_basic_2020', 'provider_basic_2021')) {
    # Create rename mapping
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
    
    # Get current column names
    current_cols <- colnames(clean_provider_info[[key]])
    
    # Create new column names by replacing matched names with their new values
    for (old_name in names(rename_mapping)) {
      if (old_name %in% current_cols) {
        current_cols[current_cols == old_name] <- rename_mapping[old_name]
      }
    }
    
    # Set the new column names
    colnames(clean_provider_info[[key]]) <- current_cols
  }
}

# Union all files - equivalent to pandas concat
union_provider_info <- do.call(rbind, clean_provider_info)

# Reset row names
rownames(union_provider_info) <- NULL

# CLEAN COST REPORT
# Get all CSV files matching the pattern
file_pattern <- file.path(directory, "*_CostReport.csv")
files <- Sys.glob(file_pattern)

# Dictionary (named list) to store original DataFrames
raw_cost_report <- list()

# Process each file
for (file_path in files) {
  filename <- basename(file_path)
  year <- gsub("_CostReport.csv", "", filename)
  
  df <- read_csv_with_encodings(file_path)
  
  if (!is.null(df)) {
    df$year <- year
    raw_cost_report[[paste0("raw_cost_", year)]] <- df
  }
}

# Lowercase all column name
for (key in names(raw_cost_report)) {
  colnames(raw_cost_report[[key]]) <- tolower(colnames(raw_cost_report[[key]]))
}

# List (equivalent to dictionary) to store custom tables
clean_cost_report <- list()

# Define a function to create provider info tables
create_cost_report_tables <- function(raw_cost_report) {
  tables <- list()
  
  for (key in names(raw_cost_report)) {
    df <- raw_cost_report[[key]]
    year <- strsplit(key, "_")[[1]][length(strsplit(key, "_")[[1]])]
    
    # List the columns to keep
    columns_to_keep <- c(
      'provider_ccn', 'provider.ccn',
      'rural_versus_urban', 'rural.versus.urban',
      'gross_revenue', 'gross.revenue',
      'net_income', 'net.income',
      'net_patient_revenue', 'net.patient.revenue',
      'number_of_beds', 'number.of.beds',
      'total_income', 'total.income',
      'total_salaries_adjusted', 'total.salaries..adjusted.',
      'fiscal_year_begin_date', 'fiscal_year_end_date',
      'fiscal.year.begin.date','fiscal.year.end.date',
      'less.total.operating.expense','less_total_operating_expense',
      'net_income_from_patients','net.income.from.service.to.patients',
      'overhead_non_salary_costs','overhead.non.salary.costs',
      'wage_related_costs_core','wage.related.costs..core.',
      'less_discounts_on_patients',"less.contractual.allowance.and.discounts.on.patients..accounts",
      'snf_admissions_total','nf.admissions.total',
      'total.days.total','total_days_total',
      'total_bed_days_available','total.bed.days.available',
      'year'
    )
    
    # Only keep columns that exist in the dataframe
    valid_columns <- columns_to_keep[columns_to_keep %in% colnames(df)]
    
    # Create new table with only the columns you need
    if (length(valid_columns) > 0) {
      tables[[paste0('cost_report_clean_', year)]] <- df[, valid_columns, drop = FALSE]
    }
  }
  
  return(tables)
} 

# Create the tables
cost_report_tables <- create_cost_report_tables(raw_cost_report)

# Add the tables to clean_provider_info (similar to update() in Python)
for (name in names(cost_report_tables)) {
  clean_cost_report[[name]] <- cost_report_tables[[name]]
}

# Rename 2020 and 2021 file columns to standard names
for (key in names(clean_cost_report)) {
  if (key %in% c('cost_report_clean_2020', 'cost_report_clean_2021')) {
    # Create rename mapping
    rename_mapping <- c(
      'provider.ccn'='provider_ccn',
      'rural.versus.urban'='rural_versus_urban',
      'gross.revenue'='gross_revenue',
      'net.income'='net_income',
      'net.patient.revenue'='net_patient_revenue',
      'number.of.beds'='number_of_beds',
      'total.income'='total_income',
      'total.salaries..adjusted.'='total_salaries_adjusted',
      'fiscal.year.begin.date'='fiscal_year_begin_date',
      'fiscal.year.end.date'='fiscal_year_end_date',
      'less.total.operating.expense'='less_total_operating_expense',
      'net.income.from.service.to.patients'='net_income_from_patients',
      'overhead.non.salary.costs'='overhead_non_salary_costs',
      'wage.related.costs..core.'='wage_related_costs_core',
      #            'total costs': 'total_costs',
      "less.contractual.allowance.and.discounts.on.patients..accounts"='less_discounts_on_patients',
      'nf.admissions.total'='snf_admissions_total',
      'total.days.total'='total_days_total',
      'total.bed.days.available'='total_bed_days_available'
    )
    
    # Get current column names
    current_cols <- colnames(clean_cost_report[[key]])
    
    # Create new column names by replacing matched names with their new values
    for (old_name in names(rename_mapping)) {
      if (old_name %in% current_cols) {
        current_cols[current_cols == old_name] <- rename_mapping[old_name]
      }
    }
    
    # Set the new column names
    colnames(clean_cost_report[[key]]) <- current_cols
  }
}

# Union all files - equivalent to pandas concat
union_cost_report <- do.call(rbind, clean_cost_report)

# Reset row names (similar to pandas ignore_index=True)
rownames(union_cost_report) <- NULL

# Make sure the data type for provnum is char(6)
union_provider_info$provnum <- sprintf("%06s", as.character(union_provider_info$provnum))
union_cost_report$provider_ccn <- sprintf("%06s", as.character(union_cost_report$provider_ccn))

# Merge the dataframes
nursing_merge <- merge(
  union_provider_info,
  union_cost_report,
  by.x = c("provnum", "year"),
  by.y = c("provider_ccn", "year"),
  all.y = TRUE  # equivalent to right join
)

# Remove duplicate rows
nursing_merge <- unique(nursing_merge)

# Clean data type in merging file
# Date Column
nursing_merge$fiscal_year_begin_date <- as.Date(nursing_merge$fiscal_year_begin_date)
nursing_merge$fiscal_year_end_date <- as.Date(nursing_merge$fiscal_year_end_date, format="%m/%d/%Y")

# Convert phone to character
nursing_merge$phone <- as.character(nursing_merge$phone)

# Check categories - equivalent to .unique()
rvu <- unique(nursing_merge$rural_versus_urban)
cat("rvu:", rvu, "\n")

state_cat <- unique(nursing_merge$state)
cat("state:", state_cat, "\n")

ownership_cat <- unique(nursing_merge$ownership)
cat("ownership:", ownership_cat, "\n")

inhosp <- unique(nursing_merge$inhosp)
cat("inhosp:", inhosp, "\n")

# inhosp has N No Y Yes Category -> Clean this
nursing_merge$inhosp <- ifelse(nursing_merge$inhosp == "N", "NO", 
                               ifelse(nursing_merge$inhosp == "Y", "YES", 
                                      nursing_merge$inhosp))

inhosp_new <- unique(nursing_merge$inhosp)
cat("inhosp after replacement:", inhosp_new, "\n")

# There are more provider numbers in the cost report than in provider info
# My guess is there's a mismatch in the data collection process
# Therefore, I will fill in those missing value in the original provider
# info table by looking it up with the previous year info

fill_missing_provider_info <- function(df) {
  # Make a copy of the input df
  filled_nursing <- df
  
  # Convert problematic character columns to clean UTF-8
  for (col in colnames(filled_nursing)) {
    if (is.character(filled_nursing[[col]])) {
      # Replace invalid UTF-8 characters with empty strings
      filled_nursing[[col]] <- iconv(filled_nursing[[col]], from = "UTF-8", to = "UTF-8", sub = "")
    }
  }
  
  # Create reference list of provider details
  provider_details <- list()
  
  # Columns to fill
  provider_cols <- c('provnum', 'provname', 'address', 'city', 'state', 'zip', 
                     'phone', 'county_ssa', 'county_name', 
                     'ownership', 'rural_versus_urban')
  
  # Filter to only columns that exist in the dataframe
  provider_cols <- provider_cols[provider_cols %in% colnames(filled_nursing)]
  
  # Safer checking function for missing values
  is_missing_safe <- function(x) {
    if (is.na(x)) return(TRUE)
    if (!is.character(x)) {
      x <- as.character(x)
    }
    # Safely clean the string before checking
    x_clean <- iconv(x, from = "UTF-8", to = "UTF-8", sub = "")
    return(is.na(x_clean) || x_clean == "" || tolower(x_clean) == "nan")
  }
  
  # Collect all available provider details
  for (i in 1:nrow(filled_nursing)) {
    provnum <- filled_nursing$provnum[i]
    if (is.na(provnum) || provnum == '') {
      next
    }
    
    # Create list with values for provider columns
    details <- list()
    for (col in provider_cols) {
      current_value <- filled_nursing[i, col]
      if (!is_missing_safe(current_value)) {
        details[[col]] <- current_value
      }
    }
    
    # Add details to provider details list if it doesn't exist or has fewer non-null values
    if (!(provnum %in% names(provider_details)) || 
        length(details) > length(provider_details[[provnum]])) {
      provider_details[[provnum]] <- details
    }
  }
  
  # Fill in missing values
  for (i in 1:nrow(filled_nursing)) {
    provnum <- filled_nursing$provnum[i]
    if (is.na(provnum) || provnum == '') {
      next
    }
    
    if (provnum %in% names(provider_details)) {
      for (col in provider_cols) {
        current_value <- filled_nursing[i, col]
        
        if (is_missing_safe(current_value) && col %in% names(provider_details[[provnum]])) {
          filled_nursing[i, col] <- provider_details[[provnum]][[col]]
        }
      }
    }
  }
  
  return(filled_nursing)
} 

filled_nursing_merge <- fill_missing_provider_info(nursing_merge)

# Same sample, the fiscal year in each report is different -> take all value/day in fiscal year * 365
# Column to adjust
adjust_cols <- c('gross_revenue', 'net_income', 'net_patient_revenue', 
                 'total_salaries_adjusted', 'total_income', 'less_total_operating_expense',
                 'net_income_from_patients', 'snf_admissions_total',
                 'total_bed_days_available', 'total_days_total')

# Calculate fiscal period days
nursing_merge$fiscal_period_days <- as.numeric(nursing_merge$fiscal_year_end_date - 
                                                 nursing_merge$fiscal_year_begin_date)

# Adjust value to column
for (col in adjust_cols) {
  nursing_merge[paste0(col, "_annualized")] <- nursing_merge[[col]] * 365 / nursing_merge$fiscal_period_days
}

# One company can have multiple cost reports in 1 year, so we'll aggregate them
# to have only 1 company - 1 cost report - 1 year
consolidate_annual_reports <- function(df) {
  # Define columns that are regular financial metrics (non-annualized)
  financial_cols <- c(
    'gross_revenue', 'inpatient_revenue', 'net_income', 'net_patient_revenue',
    'total_income', 'total_salaries_adjusted', 'less_total_operating_expense',
    'net_income_from_patients', 'snf_admissions_total', 'total_days_total',
    'total_bed_days_available'
  )
  
  # Identify corresponding annualized columns
  annualized_cols <- paste0(financial_cols, "_annualized")
  
  # Find which columns actually exist in the dataframe
  existing_financial_cols <- intersect(financial_cols, names(df))
  existing_annualized_cols <- intersect(annualized_cols, names(df))
  
  # All other columns (except grouping columns)
  other_cols <- setdiff(
    names(df), 
    c('provnum', 'year', existing_financial_cols, existing_annualized_cols)
  )
  
  # First, sum the financial values by provider and year
  result_df <- df %>%
    group_by(provnum, year) %>%
    summarise(
      # Sum financial columns (not average)
      across(all_of(existing_financial_cols), ~sum(., na.rm = TRUE)),
      # Take first value of other columns
      across(all_of(other_cols), ~first(.)),
      .groups = 'drop'
    )
  
  # Now calculate the annualized values directly from the summed values
  # This ensures consistency between regular and annualized values
  for (col in existing_financial_cols) {
    annualized_col <- paste0(col, "_annualized")
    if (annualized_col %in% names(df)) {
      result_df[[annualized_col]] <- result_df[[col]]
    }
  }
  
  return(result_df)
}

# Apply the function to your data
annualized_nursing_data <- consolidate_annual_reports(nursing_merge)

filtered_annualize <- annualized_nursing_data %>%
  filter(!is.na(fiscal_period_days)) %>%
  filter(fiscal_period_days > 0) %>%
  distinct()

# Check for missing provider information 
sum(is.na(filtered_annualize$provnum))
# None -> Good to proceed as data has been cleaned

# Convert to RDS to save memory space (for easier shiny app deploy)
saveRDS(filtered_annualize, 'cleaned_nursing.rds', compress = TRUE)
