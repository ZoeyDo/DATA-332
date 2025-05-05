library(shiny)
library(ggplot2)
library(readxl)
library(lubridate)
library(janitor)
library(DT)
library(RCurl)
library(dplyr)
library(data.table)
library(stringr)

#import data
data1 <- read.csv("/Users/giangdo/Documents/DATA332/r_projects/giang_do_shiny_individual/dataset1.csv")
data2 <- read.csv("/Users/giangdo/Documents/DATA332/r_projects/giang_do_shiny_individual/dataset2.csv")
data3 <- read.csv("/Users/giangdo/Documents/DATA332/r_projects/giang_do_shiny_individual/dataset3.csv")
data4 <- read.csv("/Users/giangdo/Documents/DATA332/r_projects/giang_do_shiny_individual/dataset4.csv")
data5 <- read.csv("/Users/giangdo/Documents/DATA332/r_projects/giang_do_shiny_individual/dataset5.csv")
data6 <- read.csv("/Users/giangdo/Documents/DATA332/r_projects/giang_do_shiny_individual/dataset6.csv")
data7 <- read.csv("/Users/giangdo/Documents/DATA332/r_projects/giang_do_shiny_individual/dataset7.csv")

#clean data2
data1_cleaned <- data1 %>%
  select(vehicle_type, init_speed, final_speed, recorder) %>%  
  mutate(
    vehicle_type = tolower(vehicle_type),  
    vehicle_type = ifelse(vehicle_type == "truck", "pickup_truck", vehicle_type),
    vehicle_style = vehicle_type,
    mph = init_speed,  
    student = recorder,  
    `if_they_slow_down_.YES..NO.` = ifelse(init_speed >= final_speed, "yes", "no") 
  ) %>%
  select(student, mph, vehicle_style, `if_they_slow_down_.YES..NO.`)

#clean data2
data2_cleaned <- data2 %>%
  select(`Collector.Name`, Date, Speed, `Time.of.the.day`, `Type.of.Car`) %>%
  rename(
    hr.min = `Time.of.the.day`,
    mph = Speed,
    date = Date,
    vehicle_style = `Type.of.Car`,
    student = `Collector.Name`
  ) %>%
  mutate(
    vehicle_style = tolower(vehicle_style),
    hr.min = format(strptime(hr.min, "%I:%M %p"), "%H:%M"),
    vehicle_style = ifelse(vehicle_style == "truck", "pickup_truck", vehicle_style)
  )

#clean data3
data3_cleaned <- data3 %>%
  select(student, date, mph, brand, hr.min, vehicle_style, if_they_slow_down_.YES..NO.)

#clean data4
data4_cleaned <- data4 %>%
  select(Initial_Speed, Body_Style, Difference) %>%  
  rename(mph = Initial_Speed, 
         vehicle_style = Body_Style,
         if_they_slow_down_.YES..NO. = Difference) %>%
  mutate(vehicle_style = tolower(vehicle_style),   
         vehicle_style = ifelse(vehicle_style == "truck", "pickup_truck", vehicle_style), 
         if_they_slow_down_.YES..NO. = ifelse(if_they_slow_down_.YES..NO. > 0, "yes", "no"))  

#clean data5
data5_cleaned <- data5 %>%
  select(Name, Date_Recorded, Initial_Read, Time_Recorded, Type_of_Car, Difference_In_Readings) %>%
  rename(
    mph = Initial_Read,
    `if_they_slow_down_.YES..NO.` = Difference_In_Readings,
    date = Date_Recorded,
    `hr.min` = Time_Recorded,
    vehicle_style = Type_of_Car,
    student = Name
  ) %>%
  mutate(
    `if_they_slow_down_.YES..NO.` = ifelse(`if_they_slow_down_.YES..NO.` > 0, "yes", "no"),
    `hr.min` = format(strptime(`hr.min`, format="%H:%M:%S"), format="%H:%M"),
    vehicle_style = case_when(
      vehicle_style == 1 ~ "emergency",
      vehicle_style == 2 ~ "hatchback",
      vehicle_style == 3 ~ "sedan",
      vehicle_style == 4 ~ "suv",
      vehicle_style == 5 ~ "van",
      vehicle_style == 6 ~ "minivan",
      vehicle_style == 7 ~ "motorcycle",
      vehicle_style == 8 ~ "coupe",
      vehicle_style == 9 ~ "truck",
      vehicle_style == 10 ~ "pickup_truck",
      TRUE ~ as.character(vehicle_style)
    )
  )

#clean data6
data6_cleaned <- data6 %>%
  mutate(
    date = format(mdy(Date), "%m/%d/%Y"),
    hr.min = format(hms(Time), "%H:%M")
  ) %>%
  rename(
    mph = `Speed..mph.`,
    student = Observer
  ) %>%
  select(student, date, mph, hr.min)

#clean data7
data7_cleaned <- data7 %>%
  mutate(
    cleaned_time = gsub("^[A-Za-z]+,\\s+", "", TimeTracked),
    parsed_date = mdy(cleaned_time),
    date = format(parsed_date, "%m/%d/%Y"),
    hr.min = NA  
  ) %>%
  rename(
    mph = MPH,
    if_they_slow_down_.YES..NO. = Slow,
    student = Student
  ) %>%
  mutate(
    if_they_slow_down_.YES..NO. = case_when(
      if_they_slow_down_.YES..NO. == "N" ~ "no",
      if_they_slow_down_.YES..NO. == "Y" ~ "yes",
      TRUE ~ if_they_slow_down_.YES..NO.
    )
  ) %>%
  select(student, date, mph, hr.min, if_they_slow_down_.YES..NO.)

#bind data
dataset_list <- list(data1_cleaned, data2_cleaned, data3_cleaned, data4_cleaned, data5_cleaned, data6_cleaned, data7_cleaned)
dataset <- bind_rows(dataset_list) %>% distinct()

#prepare data
dataset <- dataset %>%
  mutate(mph_group = cut(
    mph,
    breaks = c(0, 5, 10, 15, 20, 25, 30, 35, 40),
    labels = c("0-5", "6-10", "11-15", "16-20", "21-25", "26-30", "31-35", "36-40"),
    include.lowest = TRUE,
    right = TRUE
  ))

# count -> x variable
brand_count <- dataset %>% count(brand, name = "count")
style_count <- dataset %>% count(vehicle_style, name = "count")
speed_count <- dataset %>% count(mph_group, if_they_slow_down_.YES..NO., name = "count")

# UI set up
ui<-fluidPage( 
  
  titlePanel(title = "Explore Counting Cars"),
  h4('Nick Camacho, Zoey Do, Minh Nguyen'),
  
  fluidRow(
    column(12,
           h5("Summary Statistics"),
           tableOutput('summaryTable'),
           br()
    )
  ),
  
  fluidRow(
    column(2,
           selectInput('X', 'Choose X',choices=c("brand","vehicle_style","speed"),selected="brand")),
    column(10,plotOutput('plot_01'),br(),htmlOutput("analysis"))
  ))

# Set up server
server<-function(input,output) {
  data <- reactive({
    if(input$X == "brand") {
      return(brand_count)
    } else if(input$X == "vehicle_style"){
      return(style_count)
    } else {
      return(speed_count)
    }
  })
  # Main plot output
  output$plot_01 <- renderPlot({
    if(input$X == "speed") {
      # Special handling for speed graph with stacked bars
      ggplot(speed_count, aes(x = mph_group, y = count, fill = if_they_slow_down_.YES..NO.)) +
        geom_bar(stat = "identity", position = "stack") +
        labs(x = "MPH Group", y = "#Cars", fill = "Slowed Down?") +
        scale_fill_manual(values = c("yes" = "blue", "no" = "grey")) +
        theme_minimal() +
        theme(
          axis.title.y = element_text(size = 14),
          axis.title.x = element_text(size = 14),
          axis.text = element_text(size = 12),
          legend.title = element_text(size = 13),
          legend.text = element_text(size = 12)
        )
    } else {
      # Standard bar chart for brand or vehicle_style
      ggplot(data(), aes(x = reorder(get(input$X), -count), y = count, fill = count)) +
        geom_bar(stat = "identity") +
        scale_fill_gradient(low = "grey", high = "blue") +
        labs(x = input$X, y = "#Cars") +
        theme_minimal() +
        theme(
          axis.title.y = element_text(size = 14),
          axis.title.x = element_text(size = 14),
          axis.text = element_text(size = 12),
          legend.title = element_text(size = 13),
          legend.text = element_text(size = 12)
        )
    }
  })
  output$summaryTable <- renderTable({
    dataset %>%
      summarise(
        Min_Speed = min(mph, na.rm = TRUE),
        Max_Speed = max(mph, na.rm = TRUE),
        Mean_Speed = round(mean(mph, na.rm = TRUE), 2)
      )
  })
  output$analysis <- renderUI({
    text <- if (input$X == "speed") {
      "We decided to make a stacked bar chart and group the mph by every 5 miles for the x value and get the count of cars for the y value so that we can get a range of what mph range is most likely to slow down. The speed range that slowed down the most was 16 - 20 mph. It is important to note that the speed limit for that spot was 30 mph, and there were no cars that slowed down if they were going past the 30 mph speed limit."
    } else if (input$X == "vehicle_style") {
      "The chart shows that <b>SUVs</b> are the most common vehicle type with <b>121 cars</b>, followed by <b>sedans (67)</b> and <b>pickup trucks (24)</b>. Other styles like hatchbacks, bugs, and coupes the least, appear 8 times in total. This suggests SUVs and sedans make up the majority of vehicles observed in the area."
    } else {
      "The chart shows <b>Ford</b> is the most frequently observed brand with <b>42 cars</b>, followed by <b>Chevrolet (31)</b> and <b>Honda (21)</b>.Brands like Lexus, Pontiac, and Prius appear the least, with 3 times in total. This indicates that Ford and Chevrolet dominate the traffic in the observed area."
    }
    
    HTML(paste0("<div style='font-size:16px;'>", text, "</div>"))
  })
}  
shinyApp(ui=ui, server=server)

