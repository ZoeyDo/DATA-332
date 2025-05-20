library(shiny)
library(leaflet)
library(dplyr)
library(lubridate)
library(readr)
library(ggplot2)
library(tidyr)
library(DT)
library(scales)
#install.packages(c("maps", "mapproj", "viridis"))
library(maps)
library(mapproj)
library(viridis)

# Read and clean data
dataset <- readRDS("cleaned_nursing.rds")

# Define UI
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
# Server
server <- function(input, output, tab) {
  
  #filtered_data <- reactive({
  # dataset %>%
  #  filter(as.character(state) == input$selected_state)
  #})
  
  filtered_data <- reactive({
    if(input$selected_state == "All") {
      dataset
    } else {
      dataset %>% filter(as.character(state) == as.character(input$selected_state))
    }
  })
  
  filtered_year <- reactive({
    # Check if "All" is selected or a specific year
    if(input$selected_year == "All") {
      dataset  # Return all data when "All" is selected
    } else {
      dataset %>% filter(as.numeric(year) == as.numeric(input$selected_year))
    }
  })  
  
  # Tab 1: Research Explanation
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
  # Tab 2: Industry Overview
  output$industry_table <- renderTable({
    filtered_year() %>%
      mutate(
        restot = as.numeric(restot),
        tot_penlty_cnt = as.numeric(tot_penlty_cnt),
        total_days_total_annualized = as.numeric(total_days_total_annualized),
        total_bed_days_available_annualized = as.numeric(total_bed_days_available_annualized),
        overall_rating = as.numeric(overall_rating),
        
        # Add safety check for division by zero
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
        
        # Handle NA values when calculating mean
        Occupancy_Rate = case_when(
          all(is.na(occupancy_rate)) ~ "N/A",
          TRUE ~ paste0(round(mean(occupancy_rate, na.rm = TRUE), 2), "%")
        )
      )
  })
  
  output$occupancy_ownership <- renderPlot({
    filtered_year() %>%
      # First, filter out rows with missing or zero values that would cause issues
      filter(!is.na(total_days_total_annualized),
             !is.na(total_bed_days_available_annualized),
             total_bed_days_available_annualized > 0) %>%
      mutate(
        # Ensure proper conversion to numeric
        total_days_total_annualized = as.numeric(total_days_total_annualized),
        total_bed_days_available_annualized = as.numeric(total_bed_days_available_annualized),
        
        # Calculate occupancy rate with safety check
        occupancy_rate = total_days_total_annualized / total_bed_days_available_annualized * 100,
        
        # Group ownership types
        ownership_grouped = case_when(
          grepl("Government", ownership, ignore.case = TRUE) ~ "Government",
          grepl("For Profit", ownership, ignore.case = TRUE) ~ "For Profit",
          grepl("Non Profit", ownership, ignore.case = TRUE) ~ "Non Profit",
          TRUE ~ NA_character_
        )
      ) %>%
      # Remove any NA ownership groups and any invalid occupancy rates
      filter(!is.na(ownership_grouped),
             !is.na(occupancy_rate),
             occupancy_rate >= 0,
             occupancy_rate <= 100) %>%
      # Group by ownership type
      group_by(ownership_grouped) %>%
      # Calculate average occupancy rate
      summarise(
        Occupancy_Rate = mean(occupancy_rate, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      # Create the plot
      ggplot(aes(x = ownership_grouped, y = Occupancy_Rate)) +
      geom_bar(stat = 'identity', fill = "dodgerblue") +
      coord_flip() +
      labs(
        title = NULL,
        x = NULL,
        y = "Occupancy Rate (%)"
      ) +
      theme_minimal() +
      scale_y_continuous(limits = c(0, 100), labels = function(x) paste0(round(x, 1), "%")) +
      theme(legend.position = "none")
  })
  
  output$rating_ownership <- renderPlot({
    filtered_year() %>%
      mutate(
        overall_rating = as.numeric(overall_rating),
        ownership_grouped = case_when(
          grepl("Government", ownership, ignore.case = TRUE) ~ "Government",
          grepl("For Profit", ownership, ignore.case = TRUE) ~ "For Profit",
          grepl("Non Profit", ownership, ignore.case = TRUE) ~ "Non Profit",
          TRUE ~ NA_character_
        )
      ) %>%
      group_by(ownership_grouped) %>%  # Make sure to group by the ownership column
      summarise(
        avg_rating = mean(overall_rating, na.rm = TRUE)
      ) %>%
      ggplot(aes(x = ownership_grouped, y = avg_rating)) +
      geom_bar(stat = 'identity', fill = "dodgerblue") +
      coord_flip() +
      labs(
        title = NULL,
        x = NULL,
        y = "Average Rating (1-5)"
      ) +
      theme_minimal() +
      theme(legend.position = "none") # Remove redundant legend
  })
  
  output$industry_comments <- renderUI({
    HTML("
    <p>This page provides an overview of the nursing home industry.</p>
    <ul>
      <li>We present key performance indicators such as the number of providers, number of patients, number of penalties, average rating, and average occupancy rate.</li>
      <li>Two bar charts are included to show how rating and occupancy rate vary by ownership type.</li>
      <li>This breakdown helps identify which types of nursing homes tend to receive higher ratings and maintain higher occupancy rates.</li>
      <li>Since we are evaluating the investment potential of this industry, understanding which segments perform well is important.</li>
      <li>Two heat maps are included to show geometry information of number of nursing homes and net income by state.</li>
      <li>This breakdown helps identify which state this industry is thriving. We didn't use leaflet map because we don't have specific coordination.</li>
      <li>The analysis is based on historical data, and users can select different years to explore trends over time.</li>
    </ul>
  ")
  })
  
  output$provider_state <- renderPlot({
    provider_counts <- filtered_year() %>%
      group_by(state) %>%
      summarize(
        provider_count = n_distinct(provnum),
        .groups = "drop"
      ) %>%
      filter(!is.na(state))  # Filter out any NA state values
    
    # Check if there's data to plot
    if(nrow(provider_counts) == 0) {
      return(ggplot() +
               geom_blank() +
               labs(title = "No data available for the selected year") +
               theme_minimal())
    }
    
    states_map <- map_data("state")
    
    state_lookup <- data.frame(
      state_abbr = state.abb,
      state_name = tolower(state.name),
      stringsAsFactors = FALSE
    )
    
    # Check if states are 2-letter codes safely
    if(all(!is.na(provider_counts$state)) &&
       all(nchar(as.character(provider_counts$state)) == 2)) {
      provider_counts <- provider_counts %>%
        left_join(state_lookup, by = c("state" = "state_abbr")) %>%
        rename(region = state_name)  # maps package uses "region" for state names
    } else {
      # If your data already uses full state names
      provider_counts <- provider_counts %>%
        mutate(region = tolower(as.character(state)))
    }
    
    # Only keep states that have a matching region
    provider_counts <- provider_counts %>%
      filter(!is.na(region))
    
    map_data <- states_map %>%
      left_join(provider_counts, by = "region")
    
    ggplot(map_data, aes(x = long, y = lat, group = group, fill = provider_count)) +
      geom_polygon(color = "white", size = 0.2) +
      coord_map("albers", lat0 = 39, lat1 = 45) +
      scale_fill_viridis_c(
        name = "Number of Providers",
        option = "plasma",
        direction = -1,  # Darker colors for higher values
        na.value = "grey90"
      ) +
      labs(
        title = NULL,
        x = "",
        y = ""
      ) +
      theme_minimal() +
      theme(
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank(),
        legend.position = "right"
      )
  })
  
  incomeByState <- reactive({
    filtered_year() %>%
      group_by(state) %>%
      summarize(
        total_income = sum(total_income_annualized, na.rm = TRUE),
        .groups = "drop"
      )
  })
  
  output$net_income_state <- renderPlot({
    # Get income by state
    income_by_state <- incomeByState() %>%
      filter(!is.na(state))  # Filter out any NA state values
    
    # Check if there's data to plot
    if(nrow(income_by_state) == 0) {
      return(ggplot() +
               geom_blank() +
               labs(title = "No data available for the selected year") +
               theme_minimal())
    }
    
    # Get US states map data
    states_map <- map_data("state")
    
    # Convert state abbreviations to full names (if needed)
    state_lookup <- data.frame(
      state_abbr = state.abb,
      state_name = tolower(state.name),
      stringsAsFactors = FALSE
    )
    
    # Join income data with state names - handle potential NAs or factors
    if(all(!is.na(income_by_state$state)) &&
       all(nchar(as.character(income_by_state$state)) == 2)) {
      # If your data uses state abbreviations
      income_by_state <- income_by_state %>%
        left_join(state_lookup, by = c("state" = "state_abbr")) %>%
        rename(region = state_name)
    } else {
      # If your data already uses full state names
      income_by_state <- income_by_state %>%
        mutate(region = tolower(as.character(state)))
    }
    
    # Filter out any states that don't have a matching region
    income_by_state <- income_by_state %>%
      filter(!is.na(region))
    
    # Join the map data with income data
    map_data <- states_map %>%
      left_join(income_by_state, by = "region")
    
    # Create the heatmap
    ggplot(map_data, aes(x = long, y = lat, group = group, fill = total_income)) +
      geom_polygon(color = "white", size = 0.2) +
      coord_map("albers", lat0 = 39, lat1 = 45) +
      scale_fill_viridis_c(
        name = "Total Income ($)",
        option = "viridis",
        direction = -1,
        na.value = "grey90",
        labels = scales::dollar_format(scale = 1/1000000, suffix = "M")
      ) +
      labs(
        title = NULL,
        subtitle = "Values in Millions of Dollars",
        x = "",
        y = ""
      ) +
      theme_minimal() +
      theme(
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank(),
        legend.position = "right"
      )
  })
  
  # Tab 2: Financial Overview
  output$revenue_vs_cost <- renderPlot({
    # Reshape the data from wide to long format
    long_data <- filtered_data() %>%
      select(year, gross_revenue_annualized, less_total_operating_expense_annualized) %>%
      rename(Revenue = gross_revenue_annualized,
             Expense = less_total_operating_expense_annualized) %>%
      pivot_longer(
        cols = c(Revenue, Expense),
        names_to = "Category",
        values_to = "Amount"
      )
    
    dollar_format <- function(x) {
      scales::dollar_format(scale_cut = scales::cut_short_scale())(x)
    }
    
    ggplot(long_data, aes(x = factor(year), y = Amount, fill = Category)) +
      geom_bar(position = "dodge", stat = "identity") +
      labs(
        title = NULL,
        x = "Year",
        y = "Amount",
        fill = ""
      ) +
      theme_minimal() +
      scale_y_continuous(labels = dollar_format) +
      scale_fill_manual(values = c("Revenue" = "green", "Expense" = "red"))
  })
  
  output$income_structure <- renderPlot({
    aggregated_data <- filtered_data() %>%
      group_by(year) %>%
      summarize(
        total_income_annualized = sum(total_income_annualized, na.rm = TRUE),
        net_income_from_patients_annualized = sum(net_income_from_patients_annualized, na.rm = TRUE)
      ) %>%
      ungroup()
    # Reshape the data from wide to long format
    long_data <- aggregated_data %>%
      select(year, total_income_annualized, net_income_from_patients_annualized) %>%
      rename(Total_Income = total_income_annualized,
             Operating_Income = net_income_from_patients_annualized
      ) %>%
      pivot_longer(
        cols = c("Total_Income", "Operating_Income"),
        names_to = "Income_Type",
        values_to = "Amount"
      )
    
    long_data$year <- as.numeric(as.character(long_data$year))
    long_data <- long_data %>% filter(!is.na(Amount), !is.na(year))
    
    dollar_format <- function(x) {
      scales::dollar_format(scale_cut = scales::cut_short_scale())(x)
    }
    
    # Create the dual line graph
    ggplot(long_data, aes(x = year, y = Amount, color = Income_Type, group = Income_Type)) +
      geom_line(linewidth = 1.2, na.rm = TRUE) +
      labs(
        title = NULL,
        x = "Year",
        y = "Amount ($)",
        color = ""
      ) +
      theme_minimal() +
      scale_color_manual(values = c(Total_Income = "blue", Operating_Income = "orange")) +
      scale_y_continuous(labels = dollar_format) +
      scale_x_continuous(breaks = unique(long_data$year)) # Ensure all years are shown on x-axis
  })
  
  output$income_growth <- renderPlot({
    # Store the filtered data for inspection
    filtered <- filtered_data()
    
    # Calculate average total income by year with diagnostic output
    yearly_income <- filtered %>%
      mutate(year = as.numeric(year)) %>%
      mutate(total_income_annualized = as.numeric(total_income_annualized)) %>%
      group_by(year) %>%
      summarize(
        avg_income = mean(total_income_annualized, na.rm = TRUE),
        count = n()) %>%
      arrange(year)
    
    yearly_income <- yearly_income %>%
      filter(!is.na(avg_income), !is.na(year), is.finite(avg_income))
    
    # Use a different approach - calculate growth rates manually
    years <- yearly_income$year
    incomes <- yearly_income$avg_income
    
    # Create empty dataframe for growth rates
    growth_data <- data.frame(
      year = numeric(),
      avg_income = numeric(),
      prev_year_income = numeric(),
      growth_rate = numeric(),
      count = numeric()
    )
    
    # Manually calculate growth rates
    for(i in 2:length(years)) {
      current_year <- years[i]
      current_income <- incomes[i]
      prev_income <- incomes[i-1]
      count <- yearly_income$count[i]
      
      # Calculate growth rate
      growth <- ((current_income - prev_income) / prev_income) * 100
      
      # Add to growth_data
      growth_data <- rbind(growth_data, data.frame(
        year = current_year,
        avg_income = current_income,
        prev_year_income = prev_income,
        growth_rate = growth,
        count = count
      ))
    }
    
    # Create plot with manually calculated growth data
    ggplot(growth_data, aes(x = year, y = growth_rate, group = 1)) +
      geom_line(color = "blue", linewidth = 1.2) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "gray") +
      labs(
        title = NULL,
        x = "Year",
        y = "Growth Rate (%)"
      ) +
      theme_minimal() +
      scale_x_continuous(breaks = unique(growth_data$year)) +
      scale_y_continuous(labels = function(x) paste0(round(x, 1), "%"))
  })
  
  output$financial_table <- renderTable({
    filtered_data() %>%
      mutate(
        total_income_annualized = as.numeric(total_income_annualized),
        gross_revenue_annualized = as.numeric(gross_revenue_annualized),
        net_income_from_patients_annualized = as.numeric(net_income_from_patients_annualized),
        
        # Prevent division by zero with case_when
        net_profit_margin = case_when(
          is.na(gross_revenue_annualized) | gross_revenue_annualized == 0 ~ NA_real_,
          TRUE ~ total_income_annualized/gross_revenue_annualized * 100
        ),
        
        net_operating_margin = case_when(
          is.na(gross_revenue_annualized) | gross_revenue_annualized == 0 ~ NA_real_,
          TRUE ~ net_income_from_patients_annualized / gross_revenue_annualized * 100
        )
      ) %>%
      summarise(
        Gross_Revenue = paste0("$", scales::comma(round(sum(gross_revenue_annualized, na.rm = TRUE), 0))),
        Total_Income = paste0("$", scales::comma(round(sum(total_income_annualized, na.rm = TRUE), 0))),
        
        # Handle case where all values might be NA
        Net_Profit_Margin = case_when(
          all(is.na(net_profit_margin)) ~ "N/A",
          TRUE ~ paste0(round(mean(net_profit_margin, na.rm = TRUE), 2), "%")
        ),
        
        Net_Operating_Margin = case_when(
          all(is.na(net_operating_margin)) ~ "N/A",
          TRUE ~ paste0(round(mean(net_operating_margin, na.rm = TRUE), 2), "%")  
        )
      )
  })
  
  output$financial_comments <- renderUI({
    HTML("
    <p>This page presents the financial performance of the nursing home industry by state.</p>
    <ul>
      <li>We show key performance indicators including revenue, income, profit, and operating margin.</li>
      <li>The income growth rate chart is included to illustrate the industry's growth potential.</li>
      <li>The total income vs. operating income chart highlights the income structure. Notably, although operating income dropped significantly in 2020 due to COVID-19, total income still increased because of government funding. This suggests the industry has strong income potential.</li>
      <li>The revenue vs. cost chart evaluates how efficiently providers manage their finances.</li>
      <li>Since the goal is to assess whether this industry is worth investing in, these visualizations help identify where to invest.</li>
      <li>Users can select different states to observe financial trends based on historical data.</li>
    </ul>
  ")
  })
  
  # Tab 3: Forecasting
  output$forecasting_comments <- renderUI({
    HTML("
    <p><strong>We used ChatGPT to give us a forecasting model. It suggested Fixed Effects based on the reasons below:</strong></p>
    <ul>
      <li>Controls for unobserved, time-invariant characteristics unique to each nursing home.</li>
      <li>Focuses on within-entity variation to assess the impact of time-varying factors.</li>
      <li>Reduces omitted variable bias by accounting for constant factors over time.</li>
      <li>Widely adopted in health economics research for analyzing panel data.</li>
    </ul>
    <p>Moreover, we also found a that used Fixed Effects Regression Analysis to find the correlation between quality and operating margin </p>
    <p>Weech-Maldonado, R., Pradhan, R., Dayama, N., Lord, J., & Gupta, S. (2019). Nursing Home Quality and Financial Performance: Is There a Business Case for Quality?. Inquiry : a journal of medical care organization, provision and financing, 56, 46958018825191. <a href='https://pmc.ncbi.nlm.nih.gov/articles/PMC6376502/' target='_blank'>https://pmc.ncbi.nlm.nih.gov/articles/PMC6376502/</a></p>
    <p>However, when we run this model to predict the future income based on the rural/urban, overall rating, ownership type, hospital status -> the standard residuals is quite large. Thus it's not very useful to predict income based on these variables. We can consider choosing other x variables or change the y variable. However, that's not in the scope of this project. We will work on it in later research</p>

  ")
  })
  
  
  output$income_forecast_plot <- renderPlot({
    # Prepare data for fixed effects model
    model_data <- dataset %>%
      # Ensure factors are properly coded
      mutate(
        rural_versus_urban = factor(rural_versus_urban),
        ownership = factor(ownership),
        inhosp = factor(inhosp),
        overall_rating = factor(overall_rating, ordered = TRUE),
        # Add year as numeric for forecasting
        year_num = as.numeric(as.character(year))
      ) %>%
      # Remove any rows with NA in critical variables
      filter(!is.na(rural_versus_urban),
             !is.na(ownership),
             !is.na(inhosp),
             !is.na(overall_rating),
             !is.na(total_income_annualized))
    
    # Build fixed effects model
    fe_model <- lm(total_income_annualized ~ overall_rating + rural_versus_urban +
                     ownership + inhosp + year_num, data = model_data)
    
    # Create prediction data for future years
    current_max_year <- max(model_data$year_num)
    forecast_years <- (current_max_year + 1):(current_max_year + 5)
    
    # Get unique combinations of predictor variables for prediction
    predictors_df <- model_data %>%
      select(overall_rating, rural_versus_urban, ownership, inhosp) %>%
      distinct()
    
    # Create prediction frame for each combination and future year
    prediction_data <- expand.grid(
      year_num = forecast_years,
      stringsAsFactors = FALSE
    ) %>%
      cross_join(predictors_df)
    
    # Add historical data for plotting
    historical_data <- model_data %>%
      group_by(year_num) %>%
      summarize(mean_income = mean(total_income_annualized, na.rm = TRUE),
                .groups = 'drop')
    
    # Make predictions
    prediction_data$predicted_income <- predict(fe_model, prediction_data)
    
    # Summarize predictions by year for overall industry forecast
    forecast_summary <- prediction_data %>%
      group_by(year_num) %>%
      summarize(
        mean_predicted = mean(predicted_income, na.rm = TRUE),
        lower_ci = mean_predicted - qt(0.975, df = df.residual(fe_model)) *
          sigma(fe_model)/sqrt(n()),
        upper_ci = mean_predicted + qt(0.975, df = df.residual(fe_model)) *
          sigma(fe_model)/sqrt(n()),
        .groups = 'drop'
      )
    
    # Format years for display
    historical_data$year_label <- as.character(historical_data$year_num)
    forecast_summary$year_label <- as.character(forecast_summary$year_num)
    
    # Create plot
    ggplot() +
      # Historical data points
      geom_point(data = historical_data,
                 aes(x = year_num, y = mean_income, color = "Historical"),
                 size = 3) +
      # Forecast line
      geom_line(data = forecast_summary,
                aes(x = year_num, y = mean_predicted, color = "Forecast"),
                size = 1.2) +
      # Confidence interval ribbon
      geom_ribbon(data = forecast_summary,
                  aes(x = year_num,
                      ymin = lower_ci,
                      ymax = upper_ci),
                  fill = "blue", alpha = 0.2) +
      # Add year labels
      scale_x_continuous(breaks = c(historical_data$year_num, forecast_summary$year_num),
                         labels = c(historical_data$year_label, forecast_summary$year_label)) +
      # Custom formatting for y-axis
      scale_y_continuous(labels = scales::dollar_format(scale_cut = scales::cut_short_scale())) +
      # Colors
      scale_color_manual(values = c("Historical" = "#3498DB", "Forecast" = "#E74C3C"),
                         name = "") +
      # Labels
      labs(title = NULL,
           subtitle = "Based on overall rating, rural/urban status, ownership type and hospital status",
           x = "Year",
           y = "Average Net Income") +
      theme_minimal() +
      theme(
        legend.position = "bottom",
        plot.title = element_text(face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1)
      )
  })
  
  # Model metrics table
  output$model_metrics <- renderTable({
    # Build model on filtered data
    model_data <- dataset %>%
      mutate(
        rural_versus_urban = factor(rural_versus_urban),
        ownership = factor(ownership),
        inhosp = factor(inhosp),
        overall_rating = factor(overall_rating, ordered = TRUE),
        year_num = as.numeric(as.character(year))
      ) %>%
      filter(!is.na(rural_versus_urban),
             !is.na(ownership),
             !is.na(inhosp),
             !is.na(overall_rating),
             !is.na(total_income_annualized))
    
    # Build fixed effects model
    fe_model <- lm(total_income_annualized ~ overall_rating + rural_versus_urban +
                     ownership + inhosp + year_num, data = model_data)
    
    # Get model summary
    model_summary <- summary(fe_model)
    
    # Create metrics table
    tibble(
      Metric = c("Adjusted R²",
                 "F-statistic",
                 "p-value",
                 "Residual Std Error",
                 "Degrees of Freedom",
                 "Sample Size"),
      Value = c(
        sprintf("%.4f", model_summary$adj.r.squared),
        sprintf("%.2f", model_summary$fstatistic[1]),
        sprintf("%.4e", pf(model_summary$fstatistic[1],
                           model_summary$fstatistic[2],
                           model_summary$fstatistic[3],
                           lower.tail = FALSE)),
        sprintf("%.2f", model_summary$sigma),
        sprintf("%d", model_summary$df[2]),
        sprintf("%d", nrow(model_data))
      )
    )
  })
}

# Run the App
shinyApp(ui, server)