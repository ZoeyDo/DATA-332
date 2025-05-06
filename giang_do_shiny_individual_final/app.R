library(shiny)
library(dplyr)
library(ggplot2)
library(lubridate)
library(stringr) 
library(rsconnect) 
library(RCurl)

#import data
dataURL1 <- getURL("https://raw.githubusercontent.com/ZoeyDo/DATA-332/main/giang_do_shiny_individual_final/dataset1.csv")
dataset1 <- read.csv(text = dataURL1)

dataURL2 <- getURL("https://raw.githubusercontent.com/ZoeyDo/DATA-332/main/giang_do_shiny_individual_final/dataset2.csv")
dataset2 <- read.csv(text = dataURL2)

dataURL3 <- getURL("https://raw.githubusercontent.com/ZoeyDo/DATA-332/main/giang_do_shiny_individual_final/dataset3.csv")
dataset3 <- read.csv(text = dataURL3)

dataURL4 <- getURL("https://raw.githubusercontent.com/ZoeyDo/DATA-332/main/giang_do_shiny_individual_final/dataset4.csv")
dataset4 <- read.csv(text = dataURL4)

dataURL5 <- getURL("https://raw.githubusercontent.com/ZoeyDo/DATA-332/main/giang_do_shiny_individual_final/dataset5.csv")
dataset5 <- read.csv(text = dataURL5)

dataURL6 <- getURL("https://raw.githubusercontent.com/ZoeyDo/DATA-332/main/giang_do_shiny_individual_final/dataset6.csv")
dataset6 <- read.csv(text = dataURL6)

dataURL7 <- getURL("https://raw.githubusercontent.com/ZoeyDo/DATA-332/main/giang_do_shiny_individual_final/dataset7.csv")
dataset7 <- read.csv(text = dataURL7)

#clean data1
dataset1_cleaned <- dataset1 %>%
  transmute(
    student = recorder,
    mph = init_speed,
    vehicle_style = ifelse(tolower(vehicle_type) == "truck", "pickup_truck", tolower(vehicle_type)),
    `if_they_slow_down_.YES..NO.` = ifelse(init_speed >= final_speed, "yes", "no")
  )

#clean data2
dataset2_cleaned <- dataset2 %>%
  transmute(
    student = `Collector.Name`,
    date = Date,
    mph = Speed,
    hr.min = format(strptime(`Time.of.the.day`, "%I:%M %p"), "%H:%M"),
    vehicle_style = ifelse(tolower(`Type.of.Car`) == "truck", "pickup_truck", tolower(`Type.of.Car`))
  )

#clean data3
dataset3_cleaned <- dataset3 %>%
  transmute(
    student,
    date,
    mph,
    brand,
    hr.min,
    vehicle_style,
    `if_they_slow_down_.YES..NO.`
  )

#clean data4
dataset4_cleaned <- dataset4 %>%
  transmute(
    mph = Initial_Speed,
    vehicle_style = ifelse(tolower(Body_Style) == "truck", "pickup_truck", tolower(Body_Style)),
    `if_they_slow_down_.YES..NO.` = ifelse(Difference > 0, "yes", "no")
  )

#clean data5
dataset5_cleaned <- dataset5 %>%
  transmute(
    student = Name,
    date = Date_Recorded,
    mph = Initial_Read,
    hr.min = format(strptime(Time_Recorded, "%H:%M:%S"), "%H:%M"),
    vehicle_style = case_when(
      Type_of_Car == 1 ~ "emergency",
      Type_of_Car == 2 ~ "hatchback",
      Type_of_Car == 3 ~ "sedan",
      Type_of_Car == 4 ~ "suv",
      Type_of_Car == 5 ~ "van",
      Type_of_Car == 6 ~ "minivan",
      Type_of_Car == 7 ~ "motorcycle",
      Type_of_Car == 8 ~ "coupe",
      Type_of_Car == 9 ~ "truck",
      Type_of_Car == 10 ~ "pickup_truck",
      TRUE ~ as.character(Type_of_Car)
    ),
    `if_they_slow_down_.YES..NO.` = ifelse(Difference_In_Readings > 0, "yes", "no")
  )

#clean data6
dataset6_cleaned <- dataset6 %>%
  transmute(
    student = Observer,
    date = format(mdy(Date), "%m/%d/%Y"),
    mph = `Speed..mph.`,
    hr.min = format(hms(Time), "%H:%M")
  )

#clean data7
dataset7_cleaned <- dataset7 %>%
  mutate(
    date = format(mdy(gsub("^[A-Za-z]+,\\s+", "", TimeTracked)), "%m/%d/%Y"),
    hr.min = NA,
    mph = MPH,
    student = Student,
    if_they_slow_down_.YES..NO. = case_when(
      Slow == "N" ~ "no",
      Slow == "Y" ~ "yes",
      TRUE ~ as.character(Slow)
    )
  ) %>%
  select(student, date, mph, hr.min, if_they_slow_down_.YES..NO.)

#bind data
dataset_list <- list(dataset1_cleaned, dataset2_cleaned, dataset3_cleaned, dataset4_cleaned, dataset5_cleaned, dataset6_cleaned, dataset7_cleaned)
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
      "Most vehicles were recorded in the 26–35 MPH range, with a noticeable number slowing down, especially in that band. Slower groups (6–20 MPH) had fewer vehicles, while higher speeds showed a decline in both total and slowing vehicles. Data gaps (NA) are also present."
    } else if (input$X == "vehicle_style") {
      "The chart shows SUVs as the most common vehicle style, followed by sedans and pickup trucks. Less frequent styles include hatchbacks, minivans, and coupes, with rare entries like bug and muscle car. Duplicate and inconsistent labels suggest a need for data cleaning."
    } else {
      "The chart shows car counts by brand (excluding missing data), with Ford and Chevrolet leading, followed by Honda, Jeep, Kia, and Toyota. Brands like GMC and Nissan are moderately common, while premium or rare brands such as BMW and Audi appear infrequently. The inclusion of prius suggests a data entry error."
    }
    
    HTML(paste0("<div style='font-size:16px;'>", text, "</div>"))
  })
}  
shinyApp(ui=ui, server=server)



