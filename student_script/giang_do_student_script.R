rm(list = ls())

#1.Left Join the data together and find insights in the data.
setwd('~/Documents/DATA332/r_projects/student')

df_course <- read_excel('Course.xlsx', .name_repair = 'universal')
df_registration <- read_excel('Registration.xlsx', .name_repair = 'universal')
df_student <- read_excel('Student.xlsx', .name_repair = 'universal')

df <- left_join(df_student, df_registration, by = c('Student.ID'))
df <- left_join(df, df_course, by = c('Instance.ID'))

#2.Chart on the number of majors (TITLE) 

df_no_major <- df %>%
  group_by(Title) %>%
  summarize(count = n())

ggplot(df_no_major, aes(x = Title, y = count)) +  
  geom_col(fill = "pink") +  
  theme_minimal() +  
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 1)) +
  labs(title = "Total Majors", x = "Major", y = "Number")

#3.Chart on the birth year of the student

df$BirthYear <- year(ymd(df$Birth.Date))

df_birth_year <- df %>%
  group_by(BirthYear) %>%
  summarize(count = n())

ggplot(df, aes(x = BirthYear)) +
  geom_histogram(binwidth = 5, fill = "pink", color = "black") +  # Adjust bin width as needed
  labs(title = "Histogram of Birth Years", x = "Birth Year", y = "Frequency") +
  theme_minimal()

#4.Total cost per major, segment by payment plan
df_major_cost <- df %>%
  group_by(Title, Payment.Plan) %>%
  summarize(major_cost = sum(Cost))

ggplot(df_major_cost, aes(x = Title, y = major_cost, fill = as.factor(Payment.Plan))) +
  geom_bar(stat = "identity", position = "dodge") + 
  labs(title = "Total Cost per Major by Payment Plan", x = "Major", y = "Total Cost", fill = "Payment Plan") +
  scale_fill_manual(values = c("TRUE" = "lightpink", "FALSE" = "lightgreen"), labels = c("TRUE" = "Payment Plan", "FALSE" = "Paid in Full")) +
  theme_minimal()

#5.Total balance due by major, segment by payment plan 
df_major_balance_due <- df %>%
  group_by(Title, Payment.Plan) %>%
  summarize(major_balance_due = sum(Balance.Due))

ggplot(df_major_balance_due, aes(x = Title, y = major_balance_due, fill = as.factor(Payment.Plan))) +
  geom_bar(stat = "identity", position = "dodge") +  
  labs(title = "Total Balance Due per Major by Payment Plan", x = "Major", y = "Total Balance Due", fill = "Payment Plan") +
  scale_fill_manual(values = c("TRUE" = "yellow", "FALSE" = "lightblue"), labels = c("TRUE" = "Payment Plan", "FALSE" = "Paid in Full")) +
  theme_minimal()





