library(ggplot2)
library(dplyr)
library(tidyverse)
library(readxl)
library(tidyr)

rm(list = ls())

setwd('~/Documents/DATA332/r_projects/rscript')

#1 - Union all the trucking data together that has the unique identifier of the truck
df_truck_0001 <- read_excel('truck data 0001.xlsx', sheet = 2, skip = 3, .name_repair = 'universal')
df_truck_0369 <- read_excel('truck data 0369.xlsx', sheet = 2, skip = 3, .name_repair = 'universal')
df_truck_1226 <- read_excel('truck data 1226.xlsx', sheet = 2, skip = 3, .name_repair = 'universal')
df_truck_1442 <- read_excel('truck data 1442.xlsx', sheet = 2, skip = 3, .name_repair = 'universal')
df_truck_1478 <- read_excel('truck data 1478.xlsx', sheet = 2, skip = 3, .name_repair = 'universal')
df_truck_1539 <- read_excel('truck data 1539.xlsx', sheet = 2, skip = 3, .name_repair = 'universal')
df_truck_1769 <- read_excel('truck data 1769.xlsx', sheet = 2, skip = 3, .name_repair = 'universal')
df_pay <- read_excel('Driver Pay Sheet.xlsx', .name_repair = 'universal')

df <- rbind(df_truck_0001, df_truck_0369, df_truck_1226, df_truck_1442, df_truck_1478, df_truck_1539, df_truck_1769)

df_starting_Pivot <- df %>%
  group_by(Truck.ID) %>%
  summarize(count = n())

#2 - Join in the pay data
df <- left_join(df, df_pay, by = c('Truck.ID'))

#delete 
df <- df[, -which(names(df) == "df_driver_pay")]

#3 - Make sure you can calculate number of locations to or from location
#from locations
df[c('warehouse', 'starting_city_state')] <- str_split_fixed(df$Starting.Location, ',', 2)

df$starting_city_state <- gsub(',',"", df$starting_city_state)

df_starting_Pivot <- df %>%
  group_by(starting_city_state) %>%
  summarize(count = n(),
            mean_size_hours = mean(Hours, na.rm = TRUE),
            sd_hours = sd(Hours, na.rm = TRUE),
            total_hours = sum(Hours, na.rm = TRUE),
            total_gallons = sum(Gallons, na.rm = TRUE)
  )

#locations to
df[c('destination', 'delivery_city_state')] <- str_split_fixed(df$Delivery.Location, ',', 2)

df$delivery_city_state <- gsub(',',"", df$delivery_city_state)

df_delivery_Pivot <- df %>%
  group_by(delivery_city_state) %>%
  summarize(count = n(),
            mean_size_hours = mean(Hours, na.rm = TRUE),
            sd_hours = sd(Hours, na.rm = TRUE),
            total_hours = sum(Hours, na.rm = TRUE),
            total_gallons = sum(Gallons, na.rm = TRUE)
  )

#4 - Calculate pay with the drive and place the total pay amount inside of a bar chart. 
df$total_pay <- (sum(df$Odometer.Ending) - sum(df$Odometer.Beginning)) * df$labor_per_mil

df$full_name <- paste(df$first, df$last)

df_summary_pay <- df %>%
  group_by(full_name) %>%
  summarize(total_pay = sum(total_pay))

#5 - Make the bar chart other than black and white.
ggplot(df_summary_pay, aes(x = full_name, y = total_pay)) +  
  geom_col(fill = "lightgreen") +  
  theme_minimal() +  
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 1)) +
  labs(title = "Total Pay by Driver", x = "Driver Name", y = "Total Pay")
