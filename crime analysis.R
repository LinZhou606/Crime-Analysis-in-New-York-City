library(dplyr)
library(lubridate)
library(skimr)
library(forecast)
library(tseries)
library(tidyr)



# Data tiding and parsing

#Load the csv files and merge them
arrest_data1 <- read.csv("/Users/wangyuan/Desktop/NYPD_Arrest1.csv")
arrest_data2 <- read.csv("/Users/wangyuan/Desktop/NYPD_Arrest2.csv")
arrest_data <- rbind(arrest_data1, arrest_data2)

# Checking the distribution of data types and missing values
str(arrest_data)
skim(arrest_data)

# Drop some unnecessary columns
arrest_data_cleaned <- arrest_data %>% select(-c(PD_CD, PD_DESC, KY_CD, LAW_CODE, JURISDICTION_CODE, ARREST_PRECINCT))

# Convert ARREST_DATE to date format and create a new list of crime months
arrest_data_cleaned$ARREST_DATE <- mdy(arrest_data_cleaned$ARREST_DATE)
arrest_data_cleaned$ARREST_MONTH <- month(arrest_data_cleaned$ARREST_DATE)

# Create a new column to store the month and year information
arrest_data_cleaned$ARREST_YM <- floor_date(arrest_data_cleaned$ARREST_DATE, "month")

# Group according to ARREST_YM and calculate the number of crimes per month
monthly_crime_counts <- arrest_data_cleaned %>%
  group_by(ARREST_YM) %>%
  summarise(MONTHLY_CRIME_COUNT = n())
arrest_data <- arrest_data_cleaned %>%
  left_join(monthly_crime_counts, by = "ARREST_YM")

# For LAW_CAT_CD 
table(arrest_data$LAW_CAT_CD)
# Missing data are categorical variables, so they are transformed to "not provided"
arrest_data <- arrest_data %>%
  mutate(LAW_CAT_CD = case_when(
    LAW_CAT_CD == "F" ~ "felony",
    LAW_CAT_CD == "M" ~ "misdemeanor",
    LAW_CAT_CD == "V" ~ "violation",
    LAW_CAT_CD == 9 | LAW_CAT_CD == "I" ~ "other",
    LAW_CAT_CD == "(null)" | LAW_CAT_CD == "" ~ "not provided",
    TRUE ~ LAW_CAT_CD
  ))
# For ARREST_BORO
arrest_data <- arrest_data %>%
  mutate(ARREST_BORO = case_when(
    ARREST_BORO == "B" ~ "Bronx",
    ARREST_BORO == "S" ~ "Staten Island",
    ARREST_BORO == "K" ~ "Brooklyn",
    ARREST_BORO == "M" ~ "Manhattan",
    ARREST_BORO == "Q" ~ "Queens",
    TRUE ~ ARREST_BORO
  ))
table(arrest_data$ARREST_BORO)

# For other variables
arrest_data <- arrest_data %>%
  mutate(PERP_SEX = case_when(
    PERP_SEX == "F" ~ "female",
    PERP_SEX == "M" ~ "male",
    TRUE ~ PERP_SEX
  ),
  PERP_RACE = tolower(PERP_RACE),
  OFNS_DESC = tolower(OFNS_DESC)
  )
cleaned_dataset <- arrest_data %>%
  arrange(ARREST_DATE)  %>%
  filter(year(ARREST_DATE) >= 2018 & year(ARREST_DATE) <= 2023)

# Export the cleaned_dataset
write.csv(cleaned_dataset, "/Users/wangyuan/Desktop/cleaned_dataset.csv", row.names = FALSE)

# Filter the records from 2018 to 2023
str(cleaned_dataset)
cleaned_dataset <- cleaned_dataset %>%
  arrange(ARREST_DATE)  %>%
  filter(year(ARREST_DATE) >= 2018 & year(ARREST_DATE) <= 2023)
write.csv(cleaned_dataset, "/Users/wangyuan/Desktop/cleaned_dataset.csv", row.names = FALSE)





# Research question 1: Is there a correlation between the total number of crimes and the seasons in New York City?

# Create a time series about number of crimes in New York

total_crime_ts <- cleaned_dataset %>%
  group_by(ARREST_YM) %>%
  summarise(Total_Crimes = n()) %>%
  arrange(ARREST_YM) %>%
  complete(ARREST_YM = seq.Date(min(ARREST_YM), max(ARREST_YM), by="month")) %>%
  replace_na(list(Total_Crimes = 0)) 

# Convert the data into time series
crime_counts_vector <- total_crime_ts$Total_Crimes  # specify the number of crimes
start_year <- year(min(total_crime_ts$ARREST_YM))   # specify the start year
start_month <- month(min(total_crime_ts$ARREST_YM)) # specify the start month

total_crime_ts <- ts(crime_counts_vector, 
                     start = c(start_year, start_month), 
                     frequency = 12) # specify that there are 12 months in one year

# Exploratory data analysis on number of crimes in New York

# Create line charts to observe the pattern
autoplot(total_crime_ts)
ggseasonplot(total_crime_ts)
total_crime_ts%>%
  stl(s.window = 'periodic')%>%
  autoplot()

# Build model and conduct Ljung-Box to test whether the time series fully captures time-dependent pattern

# AMRIMA Model: white noise
arima_model_total <- auto.arima(total_crime_ts)
arima_model_total # AIC=1257.52 AICc=1258.13 BIC=1266.57 
Box.test(arima_model_total$residuals, type = "Ljung-Box") # p-value=0.8517
checkresiduals(arima_model_total) # p-value=0.1499

# ETS AAA Model：need further analysis
ets_aaa_total = ets(total_crime_ts,model = 'AAA')
ets_aaa_total #AIC=1376.670 AICc=1388.004 BIC=1415.374
Box.test(ets_aaa_total$residuals, type = "Ljung-Box") # p-value=0.9002
checkresiduals(ets_aaa_total) # p-value=0.04675

# ETS Auto Model: need further analysis
ets_model_total <- ets(total_crime_ts)
ets_model_total #ANN AIC=1382.483 AICc=1382.836 BIC=1389.313
Box.test(ets_model_total$residuals, type = "Ljung-Box") # p-value=0.4292
checkresiduals(ets_model_total) # p-value=0.03696


library(ggplot2)

# Divide 12 months into 4 seaons
cleaned_dataset <- cleaned_dataset %>%
  mutate(Season = case_when(
    month(ARREST_DATE) %in% c(12, 1, 2) ~ "Winter",
    month(ARREST_DATE) %in% c(3, 4, 5) ~ "Spring",
    month(ARREST_DATE) %in% c(6, 7, 8) ~ "Summer",
    month(ARREST_DATE) %in% c(9, 10, 11) ~ "Autumn"
  ))

# Create line chart to observe seasonal changes in the number of crimes
ggplot(cleaned_dataset, aes(x = ARREST_DATE, y = MONTHLY_CRIME_COUNT, group = Season, color = Season)) +
  geom_line() + 
  labs(title = "Seasonal Variation in Crime Rates", x = "Date", y = "Total Crimes") +
  theme_minimal()

# Create box chart to compare the number of crimes in four seasons
ggplot(cleaned_dataset, aes(x = Season, y = MONTHLY_CRIME_COUNT, fill = Season)) +
  geom_boxplot() +
  labs(title = "Crime Count by Season", x = "Season", y = "Total Crimes") +
  theme_minimal()

# Summary the crime rates in four seasons
seasonal_summary <- cleaned_dataset %>%
  group_by(Season) %>%
  summarise(
    Average = mean(MONTHLY_CRIME_COUNT),
    Median = median(MONTHLY_CRIME_COUNT),
    IQR = IQR(MONTHLY_CRIME_COUNT)
  )
print(seasonal_summary)

cleaned_dataset_ts <- ts(cleaned_dataset$MONTHLY_CRIME_COUNT, frequency = 12, start = c(year(min(cleaned_dataset$ARREST_DATE)), month(min(cleaned_dataset$ARREST_DATE))))
# Seasonal-Trend decomposition using LOESS
stl_result <- stl(cleaned_dataset_ts, s.window = "periodic")
plot(stl_result)

# Anova Test
anova_result <- aov(MONTHLY_CRIME_COUNT ~ Season, data = cleaned_dataset)
summary(anova_result) # P-value<0.001,significant seasonality at 95% significance level





# Research question 2: Is there a correlation between the number of assault crimes and the seasons in New York City?

# Find the most frwquent crime type in New York:assaults
top_crime_type <- cleaned_dataset %>%
  group_by(OFNS_DESC) %>%
  summarise(Total = n()) %>%
  ungroup() %>%
  arrange(desc(Total)) %>%
  slice(1) %>%
  pull(OFNS_DESC)

crime_counts <- cleaned_dataset %>%
  group_by(OFNS_DESC) %>%
  summarise(Total = n()) %>%
  arrange(desc(Total))

library(ggplot2)

top_ten_crime_counts <- crime_counts %>%
  slice(1:10)  # Selecting the top 10 crime types

# Plotting
ggplot(top_ten_crime_counts, aes(x = reorder(OFNS_DESC, Total), y = Total, fill = OFNS_DESC == "assault 3 & related offenses")) +
  geom_bar(stat = "identity") +
  labs(title = "Top 10 Crime Types in NYC",
       x = "Offense Description",
       y = "Total Incidents") +
  scale_fill_manual(values = c("true" = "red", "false" = "grey"), 
                    guide = FALSE) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x labels for better readability

monthly_topcrime_counts <- cleaned_dataset %>%
  filter(OFNS_DESC == top_crime_type) %>%
  group_by(ARREST_YM) %>%
  summarise(MONTHLY_TOPCRIME_COUNT = n())
cleaned_dataset <- cleaned_dataset %>%
  left_join(monthly_topcrime_counts, by = "ARREST_YM")

top_crime_ts <- cleaned_dataset %>%
  filter(OFNS_DESC == top_crime_type) %>%
  group_by(ARREST_YM) %>%
  summarise(Top_Crime_Count = n()) %>%
  arrange(ARREST_YM) %>%
  complete(ARREST_YM = seq.Date(min(ARREST_YM), max(ARREST_YM), by="month")) %>%
  replace_na(list(Top_Crime_Count = 0)) 

top_crime_type



# Convert the data into time series
crime_counts_vector <- top_crime_ts$Top_Crime_Count  # specify the number of crimes
start_year <- year(min(top_crime_ts$ARREST_YM))   # specify the start year
start_month <- month(min(top_crime_ts$ARREST_YM)) # # specify the start month

# Create a time series about number of crimes in New York
top_crime_ts <- ts(crime_counts_vector, 
                     start = c(start_year, start_month), 
                     frequency = 12) # specify that there are 12 months in one year

# Exploratory data analysis on number of crimes in New York

# Create line charts to observe the pattern
autoplot(top_crime_ts)
ggseasonplot(top_crime_ts)
top_crime_ts%>%
  stl(s.window = 'periodic')%>%
  autoplot()

# Build model and conduct Ljung-Box to test whether the time series fully captures time-dependent pattern

# AMRIMA Model: white noises
arima_model_top_crime <- auto.arima(top_crime_ts)
arima_model_top_crime # AIC=955.99 AICc=956.35 BIC=962.78
Box.test(arima_model_top_crime$residuals, type = "Ljung-Box") # p-value = 0.8833
checkresiduals(arima_model_top_crime) # p-value = 0.1207

# ETS AAA Model：white noise
ets_aaa_top = ets(top_crime_ts,model = 'AAA')
ets_aaa_top # AIC=1070.901 AICc=1082.234 BIC=1109.604 
Box.test(ets_aaa_top$residuals, type = "Ljung-Box") # p-value = 0.9639
checkresiduals(ets_aaa_top) # p-value = 0.379

# ETS Auto Model: white noise
ets_model_top_crime <- ets(top_crime_ts)
ets_model_top_crime # AIC=1064.292 AICc=1072.863 BIC=1098.442 
Box.test(ets_model_top_crime$residuals, type = "Ljung-Box") # p-value = 0.9547
checkresiduals(ets_model_top_crime) # p-value = 0.2875

library(ggplot2)
# Divide 12 months into 4 seaons
cleaned_dataset <- cleaned_dataset %>%
  mutate(Season = case_when(
    month(ARREST_DATE) %in% c(12, 1, 2) ~ "Winter",
    month(ARREST_DATE) %in% c(3, 4, 5) ~ "Spring",
    month(ARREST_DATE) %in% c(6, 7, 8) ~ "Summer",
    month(ARREST_DATE) %in% c(9, 10, 11) ~ "Autumn"
  ))


# Create line chart to observe seasonal changes in the number of assaults
ggplot(cleaned_dataset, aes(x = ARREST_DATE, y = MONTHLY_TOPCRIME_COUNT, group = Season, color = Season)) +
  geom_line() + 
  labs(title = "Seasonal Variation in Crime Rates", x = "Date", y = "Top Crimes") +
  theme_minimal()

# Create box chart to compare the number of crimes in four seasons
ggplot(cleaned_dataset, aes(x = Season, y = MONTHLY_TOPCRIME_COUNT, fill = Season)) +
  geom_boxplot() +
  labs(title = "Crime Count by Season", x = "Season", y = "Top Crimes") +
  theme_minimal()

# Summary the crime rates in four seasons
seasonal_summary <- cleaned_dataset %>%
  group_by(Season) %>%
  summarise(
    Average = mean(MONTHLY_TOPCRIME_COUNT),
    Median = median(MONTHLY_TOPCRIME_COUNT),
    IQR = IQR(MONTHLY_TOPCRIME_COUNT)
  )
print(seasonal_summary)


cleaned_dataset_ts <- ts(cleaned_dataset$MONTHLY_TOPCRIME_COUNT, frequency = 12, start = c(year(min(cleaned_dataset$ARREST_DATE)), month(min(cleaned_dataset$ARREST_DATE))))
# Seasonal-Trend decomposition using LOESS
stl_result <- stl(cleaned_dataset_ts, s.window = "periodic")
plot(stl_result)

# Anova Test
anova_result <- aov(MONTHLY_TOPCRIME_COUNT ~ Season, data = cleaned_dataset)
summary(anova_result)


