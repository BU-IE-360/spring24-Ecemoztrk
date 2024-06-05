# Import required libraries
install.packages("GGally")
install.packages("matrixStats")
install.packages("readxl")
install.packages("lubridate")
install.packages("writexl")
library(readxl)
library(forecast)
library(data.table)
library(lubridate)
library(dplyr)
library(tidyr)
library(GGally)
library(dplyr)
library(writexl)
library(matrixStats)

tday=today("Turkey")
day_before_tday <- tday - 1
#day_before_tday <- tday - 2
prediction_day <- tday +1
start_date <- as.Date("2024-02-01")
end_date <- as.Date("2024-05-15")


file_weather = paste0("/Users/ecemozturk/Desktop/processed_weather.csv")
file_production = paste0("/Users/ecemozturk/Desktop/production-4.csv")
weather_data = fread(file_weather)
production_data = fread(file_production)

# getting full weather date and hours as a template
template_dt = unique(weather_data[,list(date,hour)])
template_dt = merge(template_dt,production_data,by=c('date','hour'),all.x=T)
#template_dt = template_dt[date<=(tday + 1)]
template_dt = template_dt[date<=(tday)]

###NA VALUES###
any_na <- anyNA(weather_data)
if (any_na) {
  cat("The dataset contains NA values.\n")
  # Display the count of NAs per column
  print(colSums(is.na(weather_data)))
} else {
  cat("The dataset does not contain any NA values.\n")
}
# Display all rows that have NA values
na_rows <- weather_data[!complete.cases(weather_data), ]
View(na_rows)

# Fill NA values with the average of the surrounding values (linear interpolation)
merged_data_filled <- weather_data %>%
  mutate(across(where(is.numeric), ~ na.approx(.x, na.rm = FALSE)))

# Fill leading NAs with the next available value, upward
merged_data_filled <- merged_data_filled %>%
  mutate(across(where(is.numeric), ~ na.locf(.x, fromLast = TRUE, na.rm = FALSE)))

weather_data<- merged_data_filled

#Coordinate aggregation by long to wide format
long_weather <- weather_data
long_weather <- melt(weather_data,id.vars=c(1:4))
hourly_region_averages = dcast(long_weather, date+hour~variable,fun.aggregate=mean)
View(hourly_region_averages)


# Merge with hourly_region_averages
template_dt_with_weather <- merge(template_dt, hourly_region_averages, by = c('date', 'hour'), all.x = TRUE)
View(template_dt_with_weather)
#Order it by date and hour
template_dt_with_weather = template_dt_with_weather[order(date,hour)]

template_dt_with_aggregate <- template_dt_with_weather

template_dt_with_aggregate$hourly_cloud_average <- rowMeans(select(template_dt_with_aggregate, starts_with("tcdc_")), na.rm = TRUE)
template_dt_with_aggregate$hourly_max_t <- rowMaxs(as.matrix(select(template_dt_with_aggregate, starts_with("tmp_"))), na.rm = TRUE)
View(template_dt_with_aggregate)

# Use select to exclude coumns starting with "tcdc_" to focus on average
template_dt_with_aggregate <- template_dt_with_aggregate %>%
  select(-starts_with("tcdc_"))
#-starts_with("tmp_"))

# Read your holiday dataset
#holiday_data <- read.csv("/Users/ecemozturk/Desktop/Short Holiday.csv")
holiday_data <- read_excel("/Users/ecemozturk/Desktop/Short Holiday12.xlsx")
# Convert date column to Date format
holiday_data$date <- as.Date(holiday_data$date, format = "%d.%m.%Y", na.rm = TRUE)

# Merge holiday data with production dataset based on the date column
all_data <- merge(template_dt_with_aggregate, holiday_data, by = "date", all.x = TRUE)


all_data = all_data[!is.na(production)]
all_data_daily <- all_data[all_data$date == day_before_tday, ]

available_data = all_data[!is.na(production) & hour >= 4 & hour <= 19,]
#to_be_forecasted = template_dt_with_weather[is.na(production)]
to_be_forecasted <- all_data[is.na(production) & hour >= 4 & hour <= 19, ]

View(all_data)



# Plot production data as a line graph
plot(all_data$date, all_data$production, type = "l", xlab = "Date", ylab = "Production", main = "Production Data")


# Convert wday and month to characters
all_data[, wday := as.character(wday(date, label = TRUE))]
all_data[, month := as.character(month(date, label = TRUE))]

#special period was defined to represent the period when the capacity reaches up to 12.
all_data$special_period <- as.numeric(all_data$date >= as.Date("2022-06-01") & all_data$date <= as.Date("2022-11-16"))

#Linear regression for cummulative production
lm_model <- lm(production ~.+hourly_max_t*month+wday  -date, data = all_data)
summary(lm_model)


available_data <- all_data[!is.na(production) & hour >= 4 & hour <= 18 & date < start_date,]
to_be_forecasted <- all_data[!is.na(production) & hour >= 4 & hour <= 18 & date >= start_date & date <= end_date, ]

library(dplyr)

# Filter data for hour 4
hour_4_data <- all_data[all_data$hour == 4, ]
hour_4_data$trend_hour_4 <- 1:nrow(hour_4_data)
hour_4_data[, lag_1_production := shift(production,1)]
hour_4_data[,lag_1_diff:=production-lag_1_production]
hour_4_data <- hour_4_data[!is.na(lag_1_production)]
# Fit linear regression model for hour 4
lm_hour_4 <- lm(production ~+lag_1_production +dlwrf_surface+tmp_surface+hourly_cloud_average+special_period+trend_hour_4+month, data = hour_4_data)
# Summarize the model
summary(lm_hour_4)
checkresiduals(lm_hour_4)

#Production data for hour 4
hour_4_data <- all_data[all_data$hour == 4, ]

# Plot production for hour 4
ggplot(hour_4_data, aes(x = date, y = production)) +
  geom_line() +
  labs(title = "Hourly Production Data for Hour 4",
       x = "Date",
       y = "Production") +
  theme_minimal()

hour_4_data <- all_data %>%
  filter(hour == 4, date >= start_date & date <= end_date)

forecast_data_hour_4 <- to_be_forecasted %>%
  filter(hour == 4)
forecast_data_hour_4$trend_hour_4 <- nrow(hour_4_data) + 1:nrow(forecast_data_hour_4)

last_production_value <- tail(hour_4_data$production, 1)
forecast_data_hour_4 <- forecast_data_hour_4 %>%
  mutate(lag_1_production = last_production_value)

predictions_hour_4 <- predict(lm_hour_4, newdata = forecast_data_hour_4)
predictions_hour_4[predictions_hour_4 < 0] <- 0

predictions_hour_4_df <- data.frame(
  date = forecast_data_hour_4$date,  
  predicted = predictions_hour_4
)

merged_data <- merge(hour_4_data, predictions_hour_4_df, by = "date")

output_data <- merged_data %>%
  select(date, actual = production, predicted)

output_data <- output_data %>%
  mutate(error = abs(predicted - actual))

wmape <- sum(output_data$error) / sum(output_data$actual) * 100

print(paste("WMAPE:", wmape, "%"))

output_dir <- "C:/Users/ecemozturk/Desktop"
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

file_path <- file.path(output_dir, "predictions_hour_4_with_actuals.xlsx")

write_xlsx(output_data, file_path)

print(output_data)

# Filter data for hour 5
hour_5_data <- all_data[all_data$hour == 5, ]
hour_5_data <- hour_5_data[,-c(2)]
hour_5_data[, lag_1_production := shift(production,1)]
hour_5_data[,lag_1_diff:=production-lag_1_production]
hour_5_data <- hour_5_data[!is.na(lag_1_production)]
hour_5_data$trend_hour_5 <- 1:nrow(hour_5_data)
# Fit linear regression model for hour 5
lm_hour_5 <- lm(production ~+lag_1_production+dlwrf_surface+is.ramadan+special_period+trend_hour_5 +month*hourly_max_t, data = hour_5_data)
# Summarize the model
summary(lm_hour_5)
checkresiduals(lm_hour_5)
plot(lm_hour_5)

#Production data for hour 5
hour_5_data <- all_data[all_data$hour == 5, ]

# Plot production for hour 4
ggplot(hour_5_data, aes(x = date, y = production)) +
  geom_line() +
  labs(title = "Hourly Production Data for Hour 5",
       x = "Date",
       y = "Production") +
  theme_minimal()

hour_5_data <- all_data %>%
  filter(hour == 5, date >= start_date & date <= end_date)

forecast_data_hour_5 <- to_be_forecasted %>%
  filter(hour == 5)
forecast_data_hour_5$trend_hour_5 <- nrow(hour_5_data) + 1:nrow(forecast_data_hour_5)

last_production_value <- tail(hour_5_data$production, 1)
forecast_data_hour_5 <- forecast_data_hour_5 %>%
  mutate(lag_1_production = last_production_value)

predictions_hour_5 <- predict(lm_hour_5, newdata = forecast_data_hour_5)
predictions_hour_5[predictions_hour_5 < 0] <- 0

predictions_hour_5_df <- data.frame(
  date = forecast_data_hour_5$date,  
  predicted = predictions_hour_5
)

merged_data <- merge(hour_5_data, predictions_hour_5_df, by = "date")

output_data <- merged_data %>%
  select(date, actual = production, predicted)

output_data <- output_data %>%
  mutate(error = abs(predicted - actual))

wmape <- sum(output_data$error) / sum(output_data$actual) * 100

print(paste("WMAPE:", wmape, "%"))

output_dir <- "C:/Users/ecemozturk/Desktop"
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

file_path <- file.path(output_dir, "predictions_hour_5_with_actuals.xlsx")

write_xlsx(output_data, file_path)

print(output_data)


# Filter data for hour 6
hour_6_data <- all_data[all_data$hour == 6, ]
hour_6_data <- hour_6_data[, -c(2)]
# Convert the data frame to a data.table
setDT(hour_6_data)
# Create a lagged variable for production with a lag of 1 period
hour_6_data[, lag_1_production := shift(production,1)]
hour_6_data[,lag_1_diff:=production-lag_1_production]
hour_6_data <- hour_6_data[!is.na(lag_1_production)]
hour_6_data$trend_hour_6 <- 1:nrow(hour_6_data)
# Fit linear regression model for hour 6
lm_hour_6 <- lm(production~+lag_1_production+uswrf_top_of_atmosphere+wday+ hourly_cloud_average+is.ramadan+special_period+is.religousday+month*hourly_max_t, data = hour_6_data) 
#lm_hour_6 <- lm(production ~ +uswrf_top_of_atmosphere + is.ramadan +is.weekend+ is.religousday +is.publicholiday + month*hourly_max_t , data = hour_6_data)
# Summarize the model
summary(lm_hour_6)
checkresiduals(lm_hour_6)
plot(lm_hour_6)


hour_6_data <- all_data[all_data$hour == 6, ]

# Plot production for hour 6
ggplot(hour_6_data, aes(x = date, y = production)) +
  geom_line() +
  labs(title = "Hourly Production Data for Hour 6",
       x = "Date",
       y = "Production") +
  theme_minimal()

hour_6_data <- all_data %>%
  filter(hour == 6, date >= start_date & date <= end_date)

forecast_data_hour_6 <- to_be_forecasted %>%
  filter(hour == 6)
forecast_data_hour_6$trend_hour_6 <- nrow(hour_6_data) + 1:nrow(forecast_data_hour_6)

last_production_value <- tail(hour_6_data$production, 1)
forecast_data_hour_6 <- forecast_data_hour_6 %>%
  mutate(lag_1_production = last_production_value)

predictions_hour_6 <- predict(lm_hour_6, newdata = forecast_data_hour_6)
predictions_hour_6[predictions_hour_6 < 0] <- 0

predictions_hour_6_df <- data.frame(
  date = forecast_data_hour_6$date,  
  predicted = predictions_hour_6
)

merged_data <- merge(hour_6_data, predictions_hour_6_df, by = "date")

output_data <- merged_data %>%
  select(date, actual = production, predicted)

output_data <- output_data %>%
  mutate(error = abs(predicted - actual))

wmape <- sum(output_data$error) / sum(output_data$actual) * 100

print(paste("WMAPE:", wmape, "%"))

output_dir <- "C:/Users/ecemozturk/Desktop"
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

file_path <- file.path(output_dir, "predictions_hour_6_with_actuals.xlsx")

write_xlsx(output_data, file_path)

print(output_data)


library(data.table)

# Filter data for hour 7 and remove the hour column
hour_7_data <- all_data[all_data$hour == 7, ]
hour_7_data <- hour_7_data[, -c(2)]

# Create a trend variable for hour 7
hour_7_data$trend_hour_7 <- 1:nrow(hour_7_data)

# Convert the data frame to a data.table
setDT(hour_7_data)

# Create a lagged variable for production with a lag of 1 period
hour_7_data[, lag_1_production := shift(production,1)]
hour_7_data[,lag_1_diff:=production-lag_1_production]
# Remove rows with NA in lagged production to ensure the model can run
hour_7_data <- hour_7_data[!is.na(lag_1_production)]

# Fit linear regression model for hour 7 including the lagged variable
lm_hour_7 <- lm(production ~+lag_1_production+trend_hour_7 + special_period + dlwrf_surface + hourly_cloud_average + month+hourly_max_t,data=hour_7_data)
summary(lm_hour_7)

# Check residuals
library(forecast)
checkresiduals(lm_hour_7)

# Plot the model
plot(lm_hour_7)


# Plot production for hour 7
ggplot(hour_7_data, aes(x = date, y = production)) +
  geom_line() +
  labs(title = "Hourly Production Data for Hour 7",
       x = "Date",
       y = "Production") +
  theme_minimal()

hour_7_data <- all_data %>%
  filter(hour == 7, date >= start_date & date <= end_date)

forecast_data_hour_7 <- to_be_forecasted %>%
  filter(hour == 7)
forecast_data_hour_7$trend_hour_7 <- nrow(hour_7_data) + 1:nrow(forecast_data_hour_7)

last_production_value <- tail(hour_7_data$production, 1)
forecast_data_hour_7 <- forecast_data_hour_7 %>%
  mutate(lag_1_production = last_production_value)

predictions_hour_7 <- predict(lm_hour_7, newdata = forecast_data_hour_7)
predictions_hour_7[predictions_hour_7 < 0] <- 0

predictions_hour_7_df <- data.frame(
  date = forecast_data_hour_7$date,  
  predicted = predictions_hour_7
)

merged_data <- merge(hour_7_data, predictions_hour_7_df, by = "date")

output_data <- merged_data %>%
  select(date, actual = production, predicted)

output_data <- output_data %>%
  mutate(error = abs(predicted - actual))

wmape <- sum(output_data$error) / sum(output_data$actual) * 100

print(paste("WMAPE:", wmape, "%"))

output_dir <- "C:/Users/ecemozturk/Desktop"
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

file_path <- file.path(output_dir, "predictions_hour_7_with_actuals.xlsx")

write_xlsx(output_data, file_path)

print(output_data)

# Filter data for hour 8
hour_8_data <- all_data[all_data$hour == 8, ]
hour_8_data <- hour_8_data[,-c(2)]
hour_8_data$trend_hour_8 <- 1:nrow(hour_8_data)
# Create a lagged variable for production with a lag of 1 period
hour_8_data[, lag_1_production := shift(production,1)]
hour_8_data[,lag_1_diff:=production-lag_1_production]
# Remove rows with NA in lagged production to ensure the model can run
hour_8_data <- hour_8_data[!is.na(lag_1_production)]
#Fit linear regression model for hour 8
lm_hour_8 <- lm(production ~ +lag_1_production+trend_hour_8+ csnow_surface+dlwrf_surface+hourly_cloud_average+is.ramadan+month*hourly_max_t, data = hour_8_data)
# Summarize the model
summary(lm_hour_8)

checkresiduals(lm_hour_8)
plot(lm_hour_8)

# Plot production for hour 8
ggplot(hour_8_data, aes(x = date, y = production)) +
  geom_line() +
  labs(title = "Hourly Production Data for Hour 8",
       x = "Date",
       y = "Production") +
  theme_minimal()

hour_8_data <- all_data %>%
  filter(hour == 8, date >= start_date & date <= end_date)

forecast_data_hour_8 <- to_be_forecasted %>%
  filter(hour == 8)
forecast_data_hour_8$trend_hour_8 <- nrow(hour_8_data) + 1:nrow(forecast_data_hour_8)

last_production_value <- tail(hour_8_data$production, 1)
forecast_data_hour_8 <- forecast_data_hour_8 %>%
  mutate(lag_1_production = last_production_value)

predictions_hour_8 <- predict(lm_hour_8, newdata = forecast_data_hour_8)
predictions_hour_8[predictions_hour_8 < 0] <- 0

predictions_hour_8_df <- data.frame(
  date = forecast_data_hour_8$date,  
  predicted = predictions_hour_8
)

merged_data <- merge(hour_8_data, predictions_hour_8_df, by = "date")

output_data <- merged_data %>%
  select(date, actual = production, predicted)

output_data <- output_data %>%
  mutate(error = abs(predicted - actual))

wmape <- sum(output_data$error) / sum(output_data$actual) * 100

print(paste("WMAPE:", wmape, "%"))

output_dir <- "C:/Users/ecemozturk/Desktop"
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

file_path <- file.path(output_dir, "predictions_hour_8_with_actuals.xlsx")

write_xlsx(output_data, file_path)

print(output_data)

# Filter data for hour 9
hour_9_data <- all_data[all_data$hour == 9, ]
hour_9_data <- hour_9_data[,-c(2)]
hour_9_data$trend_hour_9 <- 1:nrow(hour_9_data)
hour_9_data[, lag_1_production := shift(production,1)]
hour_9_data[,lag_1_diff:=production-lag_1_production]
# Remove rows with NA in lagged production to ensure the model can run
hour_9_data <- hour_9_data[!is.na(lag_1_production)]
# Fit linear regression model for hour 9
lm_hour_9 <- lm(production ~+lag_1_production+special_period+dswrf_surface+csnow_surface+dlwrf_surface+is.nationalday+uswrf_surface+hourly_cloud_average+month*hourly_max_t, data = hour_9_data)
# Summarize the model
summary(lm_hour_9)
checkresiduals(lm_hour_9)
plot(lm_hour_9)

# Plot production for hour 9
ggplot(hour_9_data, aes(x = date, y = production)) +
  geom_line() +
  labs(title = "Hourly Production Data for Hour 9",
       x = "Date",
       y = "Production") +
  theme_minimal()

hour_9_data <- all_data %>%
  filter(hour == 9, date >= start_date & date <= end_date)

forecast_data_hour_9 <- to_be_forecasted %>%
  filter(hour == 9)
forecast_data_hour_9$trend_hour_9 <- nrow(hour_9_data) + 1:nrow(forecast_data_hour_9)

last_production_value <- tail(hour_9_data$production, 1)
forecast_data_hour_9 <- forecast_data_hour_9 %>%
  mutate(lag_1_production = last_production_value)

predictions_hour_9 <- predict(lm_hour_9, newdata = forecast_data_hour_9)
predictions_hour_9[predictions_hour_9 < 0] <- 0

predictions_hour_9_df <- data.frame(
  date = forecast_data_hour_9$date,  
  predicted = predictions_hour_9
)

merged_data <- merge(hour_9_data, predictions_hour_9_df, by = "date")

output_data <- merged_data %>%
  select(date, actual = production, predicted)

output_data <- output_data %>%
  mutate(error = abs(predicted - actual))

wmape <- sum(output_data$error) / sum(output_data$actual) * 100

print(paste("WMAPE:", wmape, "%"))

output_dir <- "C:/Users/ecemozturk/Desktop"
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

file_path <- file.path(output_dir, "predictions_hour_9_with_actuals.xlsx")

write_xlsx(output_data, file_path)

print(output_data)

# Create a trend variable for hour 10
hour_10_data <- all_data[all_data$hour == 10, ]
hour_10_data <- hour_10_data[,-c(2)]
hour_10_data$trend_hour_10 <- 1:nrow(hour_10_data)
hour_10_data[, lag_1_production := shift(production,1)]
hour_10_data[,lag_1_diff:=production-lag_1_production]
# Remove rows with NA in lagged production to ensure the model can run
hour_10_data <- hour_10_data[!is.na(lag_1_production)]
# Fit linear regression model for hour 10
lm_hour_10 <- lm(production ~+lag_1_production +csnow_surface+dlwrf_surface+hourly_cloud_average+is.weekend+is.religousday+is.nationalday+month*hourly_max_t, data = hour_10_data)
summary(lm_hour_10)

checkresiduals(lm_hour_10)
plot(lm_hour_10)

pacf(hour_10_data$production)

# Plot production for hour 10
ggplot(hour_10_data, aes(x = date, y = production)) +
  geom_line() +
  labs(title = "Hourly Production Data for Hour 10",
       x = "Date",
       y = "Production") +
  theme_minimal()

hour_10_data <- all_data %>%
  filter(hour == 10, date >= start_date & date <= end_date)

forecast_data_hour_10 <- to_be_forecasted %>%
  filter(hour == 10)
forecast_data_hour_10$trend_hour_10 <- nrow(hour_10_data) + 1:nrow(forecast_data_hour_10)

last_production_value <- tail(hour_10_data$production, 1)
forecast_data_hour_10 <- forecast_data_hour_10 %>%
  mutate(lag_1_production = last_production_value)

predictions_hour_10 <- predict(lm_hour_10, newdata = forecast_data_hour_10)
predictions_hour_10[predictions_hour_10 < 0] <- 0

predictions_hour_10_df <- data.frame(
  date = forecast_data_hour_10$date,  
  predicted = predictions_hour_10
)

merged_data <- merge(hour_10_data, predictions_hour_10_df, by = "date")

output_data <- merged_data %>%
  select(date, actual = production, predicted)

output_data <- output_data %>%
  mutate(error = abs(predicted - actual))

wmape <- sum(output_data$error) / sum(output_data$actual) * 100

print(paste("WMAPE:", wmape, "%"))

output_dir <- "C:/Users/ecemozturk/Desktop"
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

file_path <- file.path(output_dir, "predictions_hour_10_with_actuals.xlsx")

write_xlsx(output_data, file_path)

print(output_data)

# Create a trend variable for hour 11
hour_11_data <- all_data[all_data$hour == 11, ]
hour_11_data <- hour_11_data[,-c(2)]
hour_11_data$trend_hour_11 <- 1:nrow(hour_11_data)

hour_11_data[, lag_19_production := shift(production,19)]
hour_11_data[,lag_19_diff:=production-lag_19_production]
# Remove rows with NA in lagged production to ensure the model can run
hour_11_data <- hour_11_data[!is.na(lag_19_production)]
# Fit linear regression model for hour 11
lm_hour_11 <- lm(production ~+lag_19_production+trend_hour_11+special_period+csnow_surface+dlwrf_surface+tmp_surface+hourly_cloud_average+is.weekend+is.religousday+is.nationalday+month*hourly_max_t, data = hour_11_data)
summary(lm_hour_11)
checkresiduals(lm_hour_11)
plot(lm_hour_11)

# Plot production for hour 11
ggplot(hour_11_data, aes(x = date, y = production)) +
  geom_line() +
  labs(title = "Hourly Production Data for Hour 11",
       x = "Date",
       y = "Production") +
  theme_minimal()

hour_11_data <- all_data %>%
  filter(hour == 11, date >= start_date & date <= end_date)

forecast_data_hour_11 <- to_be_forecasted %>%
  filter(hour == 11)
forecast_data_hour_11$trend_hour_11 <- nrow(hour_11_data) + 1:nrow(forecast_data_hour_11)

last_production_value <- tail(hour_11_data$production, 1)
forecast_data_hour_11 <- forecast_data_hour_11 %>%
  mutate(lag_19_production = last_production_value)

predictions_hour_11 <- predict(lm_hour_11, newdata = forecast_data_hour_11)
predictions_hour_11[predictions_hour_11 < 0] <- 0

predictions_hour_11_df <- data.frame(
  date = forecast_data_hour_11$date,  
  predicted = predictions_hour_11
)

merged_data <- merge(hour_11_data, predictions_hour_11_df, by = "date")

output_data <- merged_data %>%
  select(date, actual = production, predicted)

output_data <- output_data %>%
  mutate(error = abs(predicted - actual))

wmape <- sum(output_data$error) / sum(output_data$actual) * 100

print(paste("WMAPE:", wmape, "%"))

output_dir <- "C:/Users/ecemozturk/Desktop"
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

file_path <- file.path(output_dir, "predictions_hour_11_with_actuals.xlsx")

write_xlsx(output_data, file_path)

print(output_data)

# Create a trend variable for hour 12
hour_12_data <- all_data[all_data$hour == 12, ]
hour_12_data <- hour_12_data[,-c(2)]
hour_12_data[, lag_1_production := shift(production,1)]
hour_12_data[,lag_1_diff:=production-lag_1_production]
hour_12_data <- hour_12_data[!is.na(lag_1_production)]
hour_12_data$trend_hour_12 <- 1:nrow(hour_12_data)
lm_hour_12 <- lm(production ~ +lag_1_production+trend_hour_12+csnow_surface+dlwrf_surface+tmp_surface+hourly_cloud_average+is.weekend+is.religousday+is.nationalday+month*hourly_max_t , data = hour_12_data)
summary(lm_hour_12)
checkresiduals(lm_hour_12)
plot(lm_hour_12)

ggplot(hour_12_data, aes(x = date, y = production)) +
  geom_line() +
  labs(title = "Hourly Production Data for Hour 12",
       x = "Date",
       y = "Production") +
  theme_minimal()

hour_12_data <- all_data %>%
  filter(hour == 12, date >= start_date & date <= end_date)

forecast_data_hour_12 <- to_be_forecasted %>%
  filter(hour == 12)
forecast_data_hour_12$trend_hour_12 <- nrow(hour_12_data) + 1:nrow(forecast_data_hour_12)

last_production_value <- tail(hour_12_data$production, 1)
forecast_data_hour_12 <- forecast_data_hour_12 %>%
  mutate(lag_1_production = last_production_value)

predictions_hour_12 <- predict(lm_hour_12, newdata = forecast_data_hour_12)
predictions_hour_12[predictions_hour_12 < 0] <- 0

predictions_hour_12_df <- data.frame(
  date = forecast_data_hour_12$date,  
  predicted = predictions_hour_12
)

merged_data <- merge(hour_12_data, predictions_hour_12_df, by = "date")

output_data <- merged_data %>%
  select(date, actual = production, predicted)

output_data <- output_data %>%
  mutate(error = abs(predicted - actual))

wmape <- sum(output_data$error) / sum(output_data$actual) * 100

print(paste("WMAPE:", wmape, "%"))

output_dir <- "C:/Users/ecemozturk/Desktop"
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

file_path <- file.path(output_dir, "predictions_hour_12_with_actuals.xlsx")

write_xlsx(output_data, file_path)

print(output_data)

# Create a trend variable for hour 13
hour_13_data <- all_data[all_data$hour == 13, ]
hour_13_data <- hour_13_data[,-c(2)]

hour_13_data[, lag_1_production := shift(production,1)]
hour_13_data[,lag_1_diff:=production-lag_1_production]
# Remove rows with NA in lagged production to ensure the model can run
hour_13_data <- hour_13_data[!is.na(lag_1_production)]
# Remove rows with missing values in the hourly_max_t column
hour_13_data$trend_hour_13 <- 1:nrow(hour_13_data)
lm_hour_13 <- lm(production ~+lag_1_production +trend_hour_13+dlwrf_surface+tmp_surface+hourly_cloud_average+is.weekend+is.nationalday+is.publicholiday+month * hourly_max_t , data = hour_13_data)
summary(lm_hour_13)
checkresiduals(lm_hour_13)
plot(lm_hour_13)

ggplot(hour_13_data, aes(x = date, y = production)) +
  geom_line() +
  labs(title = "Hourly Production Data for Hour 13",
       x = "Date",
       y = "Production") +
  theme_minimal()

hour_13_data <- all_data %>%
  filter(hour == 13, date >= start_date & date <= end_date)

forecast_data_hour_13 <- to_be_forecasted %>%
  filter(hour == 13)
forecast_data_hour_13$trend_hour_13 <- nrow(hour_13_data) + 1:nrow(forecast_data_hour_13)

last_production_value <- tail(hour_13_data$production, 1)
forecast_data_hour_13 <- forecast_data_hour_13 %>%
  mutate(lag_1_production = last_production_value)

predictions_hour_13 <- predict(lm_hour_13, newdata = forecast_data_hour_13)
predictions_hour_13[predictions_hour_13 < 0] <- 0

predictions_hour_13_df <- data.frame(
  date = forecast_data_hour_13$date,  
  predicted = predictions_hour_13
)

merged_data <- merge(hour_13_data, predictions_hour_13_df, by = "date")

output_data <- merged_data %>%
  select(date, actual = production, predicted)

output_data <- output_data %>%
  mutate(error = abs(predicted - actual))

wmape <- sum(output_data$error) / sum(output_data$actual) * 100

print(paste("WMAPE:", wmape, "%"))

output_dir <- "C:/Users/ecemozturk/Desktop"
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

file_path <- file.path(output_dir, "predictions_hour_13_with_actuals.xlsx")

write_xlsx(output_data, file_path)

print(output_data)

hour_14_data <- all_data[all_data$hour == 14, ]
hour_14_data <- hour_14_data[,-c(2)]
hour_14_data$trend_hour_14 <- 1:nrow(hour_14_data)
hour_14_data[, lag_14_production := shift(production,1)]
hour_14_data[,lag_14_diff:=production-lag_14_production]
# Remove rows with NA in lagged production to ensure the model can run
hour_14_data <- hour_14_data[!is.na(lag_14_production)]
lm_hour_14 <- lm(production ~ +lag_14_production+ special_period+dlwrf_surface+tmp_surface+is.weekend+is.ramadan+is.religousday+is.nationalday+is.publicholiday+hourly_cloud_average+month*hourly_max_t , data = hour_14_data)
summary(lm_hour_14)
checkresiduals(lm_hour_14)
plot(lm_hour_14)

ggplot(hour_14_data, aes(x = date, y = production)) +
  geom_line() +
  labs(title = "Hourly Production Data for Hour 14",
       x = "Date",
       y = "Production") +
  theme_minimal()

hour_14_data <- all_data %>%
  filter(hour == 14, date >= start_date & date <= end_date)

forecast_data_hour_14 <- to_be_forecasted %>%
  filter(hour == 14)
forecast_data_hour_14$trend_hour_14 <- nrow(hour_14_data) + 1:nrow(forecast_data_hour_14)

last_production_value <- tail(hour_14_data$production, 1)
forecast_data_hour_14 <- forecast_data_hour_14 %>%
  mutate(lag_14_production = last_production_value)

predictions_hour_14 <- predict(lm_hour_14, newdata = forecast_data_hour_14)
predictions_hour_14[predictions_hour_14 < 0] <- 0

predictions_hour_14_df <- data.frame(
  date = forecast_data_hour_14$date,  
  predicted = predictions_hour_14
)

merged_data <- merge(hour_14_data, predictions_hour_14_df, by = "date")

output_data <- merged_data %>%
  select(date, actual = production, predicted)

output_data <- output_data %>%
  mutate(error = abs(predicted - actual))

wmape <- sum(output_data$error) / sum(output_data$actual) * 100

print(paste("WMAPE:", wmape, "%"))

output_dir <- "C:/Users/ecemozturk/Desktop"
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

file_path <- file.path(output_dir, "predictions_hour_14_with_actuals.xlsx")

write_xlsx(output_data, file_path)

print(output_data)

hour_15_data <- all_data[all_data$hour == 15, ]
hour_15_data <- hour_15_data[,-c(2)]
hour_15_data$trend_hour_15 <- 1:nrow(hour_15_data)
hour_15_data[, lag_15_production := shift(production,1)]
hour_15_data[,lag_15_diff:=production-lag_15_production]
# Remove rows with NA in lagged production to ensure the model can run
hour_15_data <- hour_15_data[!is.na(lag_15_production)]
lm_hour_15 <- lm(production ~+lag_15_production+trend_hour_15+special_period+dswrf_surface+uswrf_top_of_atmosphere+dlwrf_surface+hourly_cloud_average+tmp_surface+is.weekend+is.ramadan+is.publicholiday +month*hourly_max_t , data = hour_15_data)
summary(lm_hour_15)
checkresiduals(lm_hour_15)
plot(lm_hour_15)

ggplot(hour_15_data, aes(x = date, y = production)) +
  geom_line() +
  labs(title = "Hourly Production Data for Hour 15",
       x = "Date",
       y = "Production") +
  theme_minimal()

# Filter the actual data for hour 15 and the specific date range
hour_15_data <- all_data %>%
  filter(hour == 15, date >= start_date & date <= end_date)

# Filter the to_be_forecasted data for hour 15
forecast_data_hour_15 <- to_be_forecasted %>%
  filter(hour == 15)

# Add the trend variable for hour 15
forecast_data_hour_15$trend_hour_15 <- nrow(hour_15_data) + 1:nrow(forecast_data_hour_15)

# Add the last known production value from hour_15_data to forecast_data_hour_15 for creating the lagged variable
last_production_value <- tail(hour_15_data$production, 1)
forecast_data_hour_15 <- forecast_data_hour_15 %>%
  mutate(lag_15_production = last_production_value)

# Make predictions
predictions_hour_15 <- predict(lm_hour_15, newdata = forecast_data_hour_15)
predictions_hour_15[predictions_hour_15 < 0] <- 0

# Create a dataframe with predictions and corresponding dates
predictions_hour_15_df <- data.frame(
  date = forecast_data_hour_15$date,  # Assuming 'date' column exists in forecast_data_hour_15
  predicted = predictions_hour_15
)

# Merge with actual values
merged_data <- merge(hour_15_data, predictions_hour_15_df, by = "date")

# Select and rename the columns to keep
output_data <- merged_data %>%
  select(date, actual = production, predicted)

# Calculate WMAPE
output_data <- output_data %>%
  mutate(error = abs(predicted - actual))

wmape <- sum(output_data$error) / sum(output_data$actual) * 100

# Print WMAPE
print(paste("WMAPE:", wmape, "%"))

# Ensure the directory exists
output_dir <- "C:/Users/ecemozturk/Desktop"
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

# Specify the file path
file_path <- file.path(output_dir, "predictions_hour_15_with_actuals.xlsx")

# Write the combined data to an Excel file
write_xlsx(output_data, file_path)

# Print the combined data
print(output_data)

hour_16_data <- all_data[all_data$hour == 16, ]
hour_16_data <- hour_16_data[,-c(2)]
hour_16_data$trend_hour_16 <- 1:nrow(hour_16_data)
hour_16_data[, lag_16_production := shift(production,1)]
hour_16_data[,lag_16_diff:=production-lag_16_production]
# Remove rows with NA in lagged production to ensure the model can run
hour_16_data <- hour_16_data[!is.na(lag_16_production)]
lm_hour_16 <- lm(production ~+lag_16_production+trend_hour_16+ special_period+dswrf_surface+uswrf_top_of_atmosphere+hourly_cloud_average+is.ramadan+month*hourly_max_t, data = hour_16_data)
summary(lm_hour_16)
checkresiduals(lm_hour_16)
plot(lm_hour_16)

ggplot(hour_16_data, aes(x = date, y = production)) +
  geom_line() +
  labs(title = "Hourly Production Data for Hour 16",
       x = "Date",
       y = "Production") +
  theme_minimal()

# Filter the actual data for hour 16 and the specific date range
hour_16_data <- all_data %>%
  filter(hour == 16, date >= start_date & date <= end_date)

# Filter the to_be_forecasted data for hour 16
forecast_data_hour_16 <- to_be_forecasted %>%
  filter(hour == 16)

# Add the trend variable for hour 16
forecast_data_hour_16$trend_hour_16 <- nrow(hour_16_data) + 1:nrow(forecast_data_hour_16)

# Add the last known production value from hour_16_data to forecast_data_hour_16 for creating the lagged variable
last_production_value <- tail(hour_16_data$production, 1)
forecast_data_hour_16 <- forecast_data_hour_16 %>%
  mutate(lag_16_production = last_production_value)

# Make predictions
predictions_hour_16 <- predict(lm_hour_16, newdata = forecast_data_hour_16)
predictions_hour_16[predictions_hour_16 < 0] <- 0

# Create a dataframe with predictions and corresponding dates
predictions_hour_16_df <- data.frame(
  date = forecast_data_hour_16$date,  # Assuming 'date' column exists in forecast_data_hour_16
  predicted = predictions_hour_16
)

# Merge with actual values
merged_data <- merge(hour_16_data, predictions_hour_16_df, by = "date")

# Select and rename the columns to keep
output_data <- merged_data %>%
  select(date, actual = production, predicted)

# Calculate WMAPE
output_data <- output_data %>%
  mutate(error = abs(predicted - actual))

wmape <- sum(output_data$error) / sum(output_data$actual) * 100

# Print WMAPE
print(paste("WMAPE:", wmape, "%"))

# Ensure the directory exists
output_dir <- "C:/Users/ecemozturk/Desktop"
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

# Specify the file path
file_path <- file.path(output_dir, "predictions_hour_16_with_actuals.xlsx")

# Write the combined data to an Excel file
write_xlsx(output_data, file_path)

# Print the combined data
print(output_data)

hour_17_data <- all_data[all_data$hour == 17, ]
hour_17_data <- hour_17_data[,-c(2)]
hour_17_data$trend_hour_17 <- 1:nrow(hour_17_data)
hour_17_data$nm <- as.numeric(format(hour_17_data$date, "%m"))
# Assuming your date column is named 'date_column'
hour_17_data$is_not_winter <- as.numeric(!hour_17_data$nm %in% c(12, 1, 2))
hour_17_data <- as.data.table(hour_17_data)
#hour_17_data <- hour_17_data[production > 0]
#hour_17_data[, log_production := log(production)]
hour_17_data[, lag_17_production := shift(production,1)]
hour_17_data[,lag_17_diff:=production-lag_17_production]
# Remove rows with NA in lagged production to ensure the model can run
hour_17_data <- hour_17_data[!is.na(lag_17_production)]
hour_17_data <- hour_17_data[!is.na(lag_17_diff)]
lm_hour_17 <- lm(production ~+lag_17_production+trend_hour_17+special_period+is.ramadan+is.religousday+uswrf_surface+uswrf_top_of_atmosphere+dswrf_surface+month*hourly_max_t, data = hour_17_data)
summary(lm_hour_17)
checkresiduals(lm_hour_17)
plot(lm_hour_17)

ggplot(hour_17_data, aes(x = date, y = production)) +
  geom_line() +
  labs(title = "Hourly Production Data for Hour 17",
       x = "Date",
       y = "Production") +
  theme_minimal()

# Filter the actual data for hour 17 and the specific date range
hour_17_data <- all_data %>%
  filter(hour == 17, date >= start_date & date <= end_date)

# Filter the to_be_forecasted data for hour 17
forecast_data_hour_17 <- to_be_forecasted %>%
  filter(hour == 17)

# Add the trend variable for hour 17
forecast_data_hour_17$trend_hour_17 <- nrow(hour_17_data) + 1:nrow(forecast_data_hour_17)

# Add the last known production value from hour_17_data to forecast_data_hour_17 for creating the lagged variable
last_production_value <- tail(hour_17_data$production, 1)
forecast_data_hour_17 <- forecast_data_hour_17 %>%
  mutate(lag_17_production = last_production_value)

# Make predictions
predictions_hour_17 <- predict(lm_hour_17, newdata = forecast_data_hour_17)
predictions_hour_17[predictions_hour_17 < 0] <- 0

# Create a dataframe with predictions and corresponding dates
predictions_hour_17_df <- data.frame(
  date = forecast_data_hour_17$date,  # Assuming 'date' column exists in forecast_data_hour_17
  predicted = predictions_hour_17
)

# Merge with actual values
merged_data <- merge(hour_17_data, predictions_hour_17_df, by = "date")

# Select and rename the columns to keep
output_data <- merged_data %>%
  select(date, actual = production, predicted)

# Calculate WMAPE
output_data <- output_data %>%
  mutate(error = abs(predicted - actual))

wmape <- sum(output_data$error) / sum(output_data$actual) * 100

# Print WMAPE
print(paste("WMAPE:", wmape, "%"))

# Ensure the directory exists
output_dir <- "C:/Users/ecemozturk/Desktop"
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

# Specify the file path
file_path <- file.path(output_dir, "predictions_hour_17_with_actuals.xlsx")

# Write the combined data to an Excel file
write_xlsx(output_data, file_path)

# Print the combined data
print(output_data)


hour_18_data <- all_data[all_data$hour == 18, ]
hour_18_data <- hour_18_data[,-c(2)]
# Assuming your dataframe is named 'data' and the production column is named 'production'
#data_18_filtered <- hour_18_data[hour_18_data$production != 0, ]
hour_18_data$trend_hour_18 <- 1:nrow(hour_18_data)
hour_18_data[, lag_18_production := shift(production,1)]
hour_18_data[,lag_18_diff:=production-lag_18_production]
# Remove rows with NA in lagged production to ensure the model can run
hour_18_data <- hour_18_data[!is.na(lag_18_production)]
lm_hour_18 <- lm(production ~+lag_18_production +trend_hour_18+special_period+tmp_surface+dswrf_surface+month*hourly_max_t  , data = hour_18_data)
summary(lm_hour_18)
checkresiduals(lm_hour_18)
plot(lm_hour_18)

ggplot(hour_18_data, aes(x = date, y = production)) +
  geom_line() +
  labs(title = "Hourly Production Data for Hour 18",
       x = "Date",
       y = "Production") +
  theme_minimal()

# Filter the actual data for hour 18 and the specific date range
hour_18_data <- all_data %>%
  filter(hour == 18, date >= start_date & date <= end_date)

# Filter the to_be_forecasted data for hour 18
forecast_data_hour_18 <- to_be_forecasted %>%
  filter(hour == 18)

# Add the trend variable for hour 18
forecast_data_hour_18$trend_hour_18 <- nrow(hour_18_data) + 1:nrow(forecast_data_hour_18)

# Add the last known production value from hour_18_data to forecast_data_hour_18 for creating the lagged variable
last_production_value <- tail(hour_18_data$production, 1)
forecast_data_hour_18 <- forecast_data_hour_18 %>%
  mutate(lag_18_production = last_production_value)

# Make predictions
predictions_hour_18 <- predict(lm_hour_18, newdata = forecast_data_hour_18)
predictions_hour_18[predictions_hour_18 < 0] <- 0

# Create a dataframe with predictions and corresponding dates
predictions_hour_18_df <- data.frame(
  date = forecast_data_hour_18$date,  # Assuming 'date' column exists in forecast_data_hour_18
  predicted = predictions_hour_18
)

# Merge with actual values
merged_data <- merge(hour_18_data, predictions_hour_18_df, by = "date")

# Select and rename the columns to keep
output_data <- merged_data %>%
  select(date, actual = production, predicted)

# Calculate WMAPE
output_data <- output_data %>%
  mutate(error = abs(predicted - actual))

wmape <- sum(output_data$error) / sum(output_data$actual) * 100

# Print WMAPE
print(paste("WMAPE:", wmape, "%"))

# Print the combined data
print(output_data)
