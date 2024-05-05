library(dplyr)
library(tidyr)
library(zoo)
library(lubridate)
library(forecast)

w_data <- read.csv("/Users/ecemozturk/Desktop/weather_info.csv")
p_data <- read.csv("/Users/ecemozturk/Desktop/production.csv")

wide_data <- w_data %>%
  pivot_wider(names_from = c(lat, lon),
              values_from = c(DSWRF_surface, TCDC_low.cloud.layer, 
                              TCDC_middle.cloud.layer, TCDC_high.cloud.layer,
                              TCDC_entire.atmosphere, USWRF_top_of_atmosphere,
                              CSNOW_surface, DLWRF_surface, USWRF_surface, TMP_surface),
              names_prefix = "DSWRF_surface_")


ordered_data <- wide_data %>%
  arrange(date, hour)

# Transforming date-hour columns into datetime format
ordered_data$datetime = as.POSIXct(paste(ordered_data$date, ordered_data$hour), format="%Y-%m-%d %H")
p_data$datetime <- as.POSIXct(paste(p_data$date, p_data$hour), format="%Y-%m-%d %H")
ordered_data <- ordered_data[, c("datetime", setdiff(names(ordered_data), "datetime"))]
p_data <- p_data[, c("datetime", setdiff(names(p_data), "datetime"))]
ordered_data <- ordered_data[, !names(ordered_data) %in% c("date", "hour")]
p_data <- p_data[, !names(p_data) %in% c("date", "hour")]


w_info <- ordered_data
w_info
# Finding the duplicated rows and removing them 
duplicated_rows <- w_info[duplicated(w_info$datetime) | duplicated(w_info$datetime, fromLast = TRUE), ]
duplicated_rows2 <- p_data[duplicated(p_data$datetime) | duplicated(p_data$datetime, fromLast = TRUE), ]
# View the duplicated rows
print(duplicated_rows)
print(duplicated_rows2)
# Remove duplicated rows based on the datetime column
unique_p_data <- unique(p_data)
# Overwrite the original dataframe with the unique values:
p_data <- unique_p_data




# Merge the data
merged_data <- merge(w_info, p_data, by = "datetime", all = TRUE)
merged_data 
merged_data$production[1:10] <- 0
prod_index <- which(names(merged_data) == "production")

# Reorder the columns with "production" as the second column
merged_data <- merged_data[, c(1, prod_index, setdiff(2:ncol(merged_data), prod_index))]

tail(merged_data$production,136)
#136 last rows are all NAs which needs to be filled

tail(merged_data)

# Check for NA values
rows_with_na <- which(rowSums(is.na(merged_data)) > 0)

# Display rows with NA values
merged_data[rows_with_na, ]

# Assuming your dataframe is named 'ordered_data'
# Sort the dataframe by datetime in ascending order
merged_data <- merged_data %>% arrange(datetime)


# Identify columns to fill NA values
cols_to_fill <- setdiff(names(merged_data), c("datetime", "production"))

# Fill NA values with the next observation (24 hours later) for the first four hours
for (col in cols_to_fill) {
  merged_data[[col]][1:4] <- merged_data[[col]][25:28]
}


# Check for the remaining NA values 
rows_with_na <- which(rowSums(is.na(merged_data)) > 0)

# Display rows with NA values
merged_data[rows_with_na, ]

############ Filling the NA values with rolling average of 24 hours #############

# extracting hours that have no sun
#merged_data$hour <- as.numeric(format(merged_data$datetime, "%H"))
#pro <- merged_data[!(merged_data$hour == 0 | merged_data$hour == 1 | merged_data$hour == 2 | merged_data$hour == 3 | merged_data$hour == 4), ]
#pro <- pro[!(pro$hour == 20 | pro$hour == 21 | pro$hour == 22 | pro$hour == 23), ]

# Define the window size for the rolling average********( CHECK WINDOW SIZE )*******
window_size <- 24  # For example, using a window size of 24 hours

# Apply rolling mean to fill missing values in all columns except datetime and production
cols_to_fill <- setdiff(names(merged_data), c("datetime", "production"))
for (col in cols_to_fill) {
  # Create a temporary copy of the column
  temp_col <- merged_data[[col]]
  
  # Fill NA values in the temporary column with rolling average
  temp_col_filled <- rollapply(temp_col, width = window_size, FUN = mean, na.rm = TRUE, align = "right", fill = NA)
  
  # Replace NA values in the original column with the corresponding values from the filled column
  merged_data[[col]][is.na(merged_data[[col]])] <- temp_col_filled[is.na(merged_data[[col]])]
}

# Subset the dataframe to select rows where "production" column contains NA values ( CHECK NA PRODUCTION VALUES )
na_production_rows <- merged_data[is.na(merged_data$production), ]

# Print the rows
print(na_production_rows)
# Set NA values in the "production" column to 0
merged_data$production[is.na(merged_data$production)] <- 0


#calculating max_temp
max_columns <- grepl("TMP_surface_DSWRF_surface_", colnames(merged_data))
max_temp <- colnames(merged_data)[max_columns]
merged_data$max_t <- max(merged_data[max_temp])
##calculating temp mean

temp_columns_2 <- grepl("TMP_surface_DSWRF_surface_", colnames(merged_data))
temp_mean <- colnames(merged_data)[temp_columns_2]
merged_data$mean_temp <- rowMeans(merged_data[temp_mean])
##calculating TCDC_high.cloud.layer_DSWRF_surface_ mean
th_columns <- grepl("TCDC_high.cloud.layer_DSWRF_surface_", colnames(merged_data))
th_mean <- colnames(merged_data)[th_columns]
merged_data$th_mean <- rowMeans(merged_data[th_mean])
##calculating TCDC_middle.cloud.layer_DSWRF_surface_ mean
tm_columns <- grepl("TCDC_middle.cloud.layer_DSWRF_surface_", colnames(merged_data))
tm_mean <- colnames(merged_data)[th_columns]
merged_data$tm_mean <- rowMeans(merged_data[tm_mean])
##calculating TCDC_low.cloud.layer_DSWRF_surface_ mean
tl_columns <- grepl("TCDC_low.cloud.layer_DSWRF_surface_", colnames(merged_data))
tl_mean <- colnames(merged_data)[tl_columns]
##calculating DLWRF_surface_DSWRF_surface_ mean
DLWRF_columns <- grepl("DLWRF_surface_DSWRF_surface_", colnames(merged_data))
DLWRF_mean <- colnames(merged_data)[DLWRF_columns]
merged_data$DLWRF_mean <- rowMeans(merged_data[DLWRF_mean])
##calculating USWRF_surface_DSWRF_surface_ mean
us_columns <- grepl("USWRF_surface_DSWRF_surface_", colnames(merged_data))
us_mean <- colnames(merged_data)[us_columns]
merged_data$us_mean <- rowMeans(merged_data[us_mean])
##calculating USWRF_top_of_atmosphere_DWSRF_surface mean
u_columns <- grepl("USWRF_top_of_atmosphere_DSWRF_surface_", colnames(merged_data))
u_mean <- colnames(merged_data)[u_columns]
merged_data$u_mean <- rowMeans(merged_data[u_mean])
#calculating DSWRF_surface mean
matching_columns <- grepl("DSWRF_surface_DSWRF_surface_", colnames(merged_data))
columns_to_mean <- colnames(merged_data)[matching_columns]
merged_data$mean_DSWRF_SURFACE <- rowMeans(merged_data[columns_to_mean])
##calculating TCDC_entire.atmosphere_DSWRF_surface_ mean
tc_columns <- grepl("TCDC_entire.atmosphere_DSWRF_surface_", colnames(merged_data))
tc_mean <- colnames(merged_data)[tc_columns]
merged_data$tc_mean <- rowMeans(merged_data[tc_mean])

######calculating the OVERALL cloud layer variable 
cloud_columns <- grepl("tm_mean|tl_mean|th_mean", colnames(merged_data))
cloud_mean <- colnames(merged_data)[cloud_columns]
merged_data$cloud_mean <- rowMeans(merged_data[cloud_mean])

## removing the redundant columns
merged_data <- merged_data %>% 
  select(-matches("TMP_surface_DSWRF_surface_"))
merged_data <- merged_data %>% 
  select(-matches("TCDC_high.cloud.layer_DSWRF_surface_"))
merged_data <- merged_data %>% 
  select(-matches("TCDC_middle.cloud.layer_DSWRF_surface_"))
merged_data <- merged_data %>% 
  select(-matches("TCDC_low.cloud.layer_DSWRF_surface_"))
merged_data <- merged_data %>% 
  select(-matches("DLWRF_surface_DSWRF_surface_"))
merged_data <- merged_data %>% 
  select(-matches("USWRF_surface_DSWRF_surface_"))
merged_data <-  merged_data %>% 
  select(-matches("USWRF_top_of_atmosphere_DSWRF_surface_"))
merged_data <- merged_data %>% 
  select(-matches("DSWRF_surface_DSWRF_surface_"))
merged_data <- merged_data %>% 
  select(-matches("TCDC_entire.atmosphere_DSWRF_surface_"))
###########################

merged_data2 <- merged_data

#not necessary but you can still write it to an csv
write.csv(merged_data,"/Users/ecemozturk/Desktop/final_data.csv",row.names = FALSE)

############# as an example I'm creating time series for each hour that has sun, for now it's up to 10th hour but can be extended up to 19

# Create a new column 'hour' containing the hour part of 'datetime'
merged_data$hour <- as.numeric(format(merged_data$datetime, "%H"))


hoursix <- merged_data[merged_data$hour == 6, ]
hourseven <- merged_data[merged_data$hour == 7, ]
houreight <- merged_data[merged_data$hour == 8, ]
hournine <- merged_data[merged_data$hour == 9, ]
hourten <- merged_data[merged_data$hour == 10, ]
houreleven <- merged_data[merged_data$hour == 11, ]
hourtwelve <- merged_data[merged_data$hour == 12, ]
hour13 <- merged_data[merged_data$hour == 13, ]
hour14 <- merged_data[merged_data$hour == 14, ]
hour15 <- merged_data[merged_data$hour == 15, ]
hour16 <- merged_data[merged_data$hour == 16, ]
hour17 <- merged_data[merged_data$hour == 17, ]
hour18 <- merged_data[merged_data$hour == 18, ]

#### FORECASTING FOR HOUR SIX
hoursix$production_lag9 <- lag(hoursix$production, 9)
hourseven$production_lag9 <- lag(hourseven$production, 9)
houreight$production_lag9 <- lag(houreight$production, 9)
hournine$production_lag9 <- lag(hournine$production, 9)
hourten$production_lag9 <- lag(hourten$production, 9)
houreleven$production_lag9 <- lag(houreleven$production, 9)
hourtwelve$production_lag9 <- lag(hourtwelve$production, 9)
hour13$production_lag9 <- lag(hour13$production, 9)
hour14$production_lag9 <- lag(hour14$production, 9)
hour15$production_lag9 <- lag(hour15$production, 9)
hour16$production_lag9 <- lag(hour16$production, 9)
hour17$production_lag9 <- lag(hour17$production, 9)
hour18$production_lag9 <- lag(hour18$production, 9)


library(tseries)
adf.test(hoursix$production)
library(forecast)

train_data <- hoursix[1:800, ]
test_data <- hoursix[801:nrow(hoursix), ]
snow_variables <- grepl("CSNOW_surface_DSWRF_", colnames(hoursix))
predictors <- c("mean_temp","cloud_mean","tc_mean","DLWRF_mean","production_lag9",colnames(train_data)[snow_variables])

specific_date_predictors <- subset(test_data, datetime == as.POSIXct("2024-04-16 06:00:00"))[, predictors]


# Train a linear regression model using your training data
lm_model <- lm(production ~ . + production_lag9, data = train_data[, c("production", predictors)])

linear_regression_prediction <- predict(lm_model, newdata = specific_date_predictors)


linear_regression_prediction <- tail(linear_regression_prediction, 1)
arima_forecast <- forecast(arima_model, h = 1)
arima_prediction <- arima_forecast$mean
prediction_hour_6 <- linear_regression_prediction + arima_prediction
prediction_hour_6

#### FORECASTING FOR HOUR SEVEN
# Split data into training and testing sets
train_data <- hourseven[1:800, ]
test_data <- hourseven[801:nrow(hourseven), ]
snow_variables <- grepl("CSNOW_surface_DSWRF_", colnames(hourseven))
predictors <- c("mean_temp","cloud_mean","tc_mean","DLWRF_mean","production_lag9",colnames(train_data)[snow_variables])
# Assuming train_data and test_data are your training and testing datasets
# predictors are the variables used for modeling
specific_date_predictors <- subset(test_data, datetime == as.POSIXct("2024-04-16 07:00:00"))[, predictors]
# Make predictions using the linear regression model for the specific date and hour
# Train a linear regression model using your training data
lm_model <- lm(production ~ . + production_lag9, data = train_data[, c("production", predictors)])

# Make predictions using the linear regression model for the specific date and hour
linear_regression_prediction <- predict(lm_model, newdata = specific_date_predictors)

linear_regression_prediction <- predict(lm_model, newdata = specific_date_predictors)
linear_regression_prediction <- tail(linear_regression_prediction, 1)
arima_forecast <- forecast(arima_model, h = 1)
arima_prediction <- arima_forecast$mean
prediction_hour_7 <- linear_regression_prediction + arima_prediction
prediction_hour_7



#### FORECASTING FOR HOUR EIGHT
# Split data into training and testing sets
train_data <- houreight[1:800, ]
test_data <- houreight[801:nrow(houreight), ]
snow_variables <- grepl("CSNOW_surface_DSWRF_", colnames(houreight))
predictors <- c("mean_temp","cloud_mean","tc_mean","DLWRF_mean","production_lag9",colnames(train_data)[snow_variables])
# Assuming train_data and test_data are your training and testing datasets
# predictors are the variables used for modeling
specific_date_predictors <- subset(test_data, datetime == as.POSIXct("2024-04-16 08:00:00"))[, predictors]
# Make predictions using the linear regression model for the specific date and hour
lm_model <- lm(production ~ . + production_lag9, data = train_data[, c("production", predictors)])
linear_regression_prediction <- predict(lm_model, newdata = specific_date_predictors)
linear_regression_prediction <- tail(linear_regression_prediction, 1)
arima_forecast <- forecast(arima_model, h = 1)
arima_prediction <- arima_forecast$mean
prediction_hour_8 <- linear_regression_prediction + arima_prediction

#### FORECASTING FOR HOUR NINE
# Split data into training and testing sets
train_data <- hournine[1:800, ]
test_data <- hournine[801:nrow(houreight), ]
snow_variables <- grepl("CSNOW_surface_DSWRF_", colnames(hournine))
predictors <- c("mean_temp","cloud_mean","tc_mean","DLWRF_mean","production_lag9",colnames(train_data)[snow_variables])
# Assuming train_data and test_data are your training and testing datasets
# predictors are the variables used for modeling
specific_date_predictors <- subset(test_data, datetime == as.POSIXct("2024-04-16 09:00:00"))[, predictors]
lm_model <- lm(production ~ . + production_lag9, data = train_data[, c("production", predictors)])
# Make predictions using the linear regression model for the specific date and hour
linear_regression_prediction <- predict(lm_model, newdata = specific_date_predictors)
linear_regression_prediction <- tail(linear_regression_prediction, 1)
arima_forecast <- forecast(arima_model, h = 1)
arima_prediction <- arima_forecast$mean
prediction_hour_9 <- linear_regression_prediction + arima_prediction
baseline<- hournine$production[hournine$datetime == as.POSIXct("2024-04-14 09:00:00")]
#### FORECASTING FOR HOUR TEN
# Split data into training and testing sets
train_data <- hourten[1:800, ]
test_data <- hourten[801:nrow(houreight), ]
snow_variables <- grepl("CSNOW_surface_DSWRF_", colnames(hourten))
predictors <- c("mean_temp","cloud_mean","tc_mean","DLWRF_mean","production_lag9",colnames(train_data)[snow_variables])
# Assuming train_data and test_data are your training and testing datasets
# predictors are the variables used for modeling
specific_date_predictors <- subset(test_data, datetime == as.POSIXct("2024-04-16 10:00:00"))[, predictors]
lm_model <- lm(production ~ . + production_lag9, data = train_data[, c("production", predictors)])
# Make predictions using the linear regression model for the specific date and hour
linear_regression_prediction <- predict(lm_model, newdata = specific_date_predictors)
linear_regression_prediction <- tail(linear_regression_prediction, 1)
arima_forecast <- forecast(arima_model, h = 1)
arima_prediction <- arima_forecast$mean
prediction_hour_10 <- linear_regression_prediction + arima_prediction

#### FORECASTING FOR HOUR ELEVEN
# Split data into training and testing sets
train_data <- houreleven[1:800, ]
test_data <- houreleven[801:nrow(houreight), ]
snow_variables <- grepl("CSNOW_surface_DSWRF_", colnames(houreleven))
predictors <- c("mean_temp","cloud_mean","tc_mean","DLWRF_mean","production_lag9",colnames(train_data)[snow_variables])
# Assuming train_data and test_data are your training and testing datasets
# predictors are the variables used for modeling
specific_date_predictors <- subset(test_data, datetime == as.POSIXct("2024-04-16 11:00:00"))[, predictors]
lm_model <- lm(production ~ . + production_lag9, data = train_data[, c("production", predictors)])
# Make predictions using the linear regression model for the specific date and hour
linear_regression_prediction <- predict(lm_model, newdata = specific_date_predictors)
linear_regression_prediction <- tail(linear_regression_prediction, 1)
arima_forecast <- forecast(arima_model, h = 1)
arima_prediction <- arima_forecast$mean
prediction_hour_11 <- linear_regression_prediction + arima_prediction

#### FORECASTING FOR HOUR TWELVE
# Split data into training and testing sets
train_data <- hourtwelve[1:800, ]
test_data <- hourtwelve[801:nrow(houreight), ]
snow_variables <- grepl("CSNOW_surface_DSWRF_", colnames(hourtwelve))
predictors <- c("mean_temp","cloud_mean","tc_mean","DLWRF_mean","production_lag9",colnames(train_data)[snow_variables])
# Assuming train_data and test_data are your training and testing datasets
# predictors are the variables used for modeling
specific_date_predictors <- subset(test_data, datetime == as.POSIXct("2024-04-16 12:00:00"))[, predictors]
lm_model <- lm(production ~ . + production_lag9, data = train_data[, c("production", predictors)])
# Make predictions using the linear regression model for the specific date and hour
linear_regression_prediction <- predict(lm_model, newdata = specific_date_predictors)
linear_regression_prediction <- tail(linear_regression_prediction, 1)
arima_forecast <- forecast(arima_model, h = 1)
arima_prediction <- arima_forecast$mean
prediction_hour_12 <- linear_regression_prediction + arima_prediction
#### FORECASTING FOR HOUR THIRTEEN
# Split data into training and testing sets
train_data <- hour13[1:800, ]
test_data <- hour13[801:nrow(houreight), ]
snow_variables <- grepl("CSNOW_surface_DSWRF_", colnames(hour13))
predictors <- c("mean_temp","cloud_mean","tc_mean","DLWRF_mean","production_lag9",colnames(train_data)[snow_variables])
# Assuming train_data and test_data are your training and testing datasets
# predictors are the variables used for modeling
specific_date_predictors <- subset(test_data, datetime == as.POSIXct("2024-04-16 13:00:00"))[, predictors]
lm_model <- lm(production ~ . + production_lag9, data = train_data[, c("production", predictors)])
# Make predictions using the linear regression model for the specific date and hour
linear_regression_prediction <- predict(lm_model, newdata = specific_date_predictors)
linear_regression_prediction <- tail(linear_regression_prediction, 1)
arima_forecast <- forecast(arima_model, h = 1)
arima_prediction <- arima_forecast$mean
prediction_hour_13 <- linear_regression_prediction + arima_prediction
#### FORECASTING FOR HOUR 14
# Split data into training and testing sets
train_data <- hour14[1:800, ]
test_data <- hour14[801:nrow(houreight), ]
snow_variables <- grepl("CSNOW_surface_DSWRF_", colnames(hour14))
predictors <- c("mean_temp","cloud_mean","tc_mean","DLWRF_mean","production_lag9",colnames(train_data)[snow_variables])
# Assuming train_data and test_data are your training and testing datasets
# predictors are the variables used for modeling
specific_date_predictors <- subset(test_data, datetime == as.POSIXct("2024-04-16 14:00:00"))[, predictors]
lm_model <- lm(production ~ . + production_lag9, data = train_data[, c("production", predictors)])
# Make predictions using the linear regression model for the specific date and hour
linear_regression_prediction <- predict(lm_model, newdata = specific_date_predictors)
linear_regression_prediction <- tail(linear_regression_prediction, 1)
arima_forecast <- forecast(arima_model, h = 1)
arima_prediction <- arima_forecast$mean
prediction_hour_14 <- linear_regression_prediction + arima_prediction
#### FORECASTING FOR HOUR 15
# Split data into training and testing sets
train_data <- hour15[1:800, ]
test_data <- hour15[801:nrow(houreight), ]
snow_variables <- grepl("CSNOW_surface_DSWRF_", colnames(hour15))
predictors <- c("mean_temp","cloud_mean","tc_mean","DLWRF_mean","production_lag9",colnames(train_data)[snow_variables])
# Assuming train_data and test_data are your training and testing datasets
# predictors are the variables used for modeling
specific_date_predictors <- subset(test_data, datetime == as.POSIXct("2024-04-16 15:00:00"))[, predictors]
lm_model <- lm(production ~ . + production_lag9, data = train_data[, c("production", predictors)])
# Make predictions using the linear regression model for the specific date and hour
linear_regression_prediction <- predict(lm_model, newdata = specific_date_predictors)
linear_regression_prediction <- tail(linear_regression_prediction, 1)
arima_forecast <- forecast(arima_model, h = 1)
arima_prediction <- arima_forecast$mean
prediction_hour_15 <- linear_regression_prediction + arima_prediction
#### FORECASTING FOR HOUR 16
# Split data into training and testing sets
train_data <- hour16[1:800, ]
test_data <- hour16[801:nrow(houreight), ]
snow_variables <- grepl("CSNOW_surface_DSWRF_", colnames(hour16))
predictors <- c("mean_temp","cloud_mean","tc_mean","DLWRF_mean","production_lag9",colnames(train_data)[snow_variables])
# Assuming train_data and test_data are your training and testing datasets
# predictors are the variables used for modeling
specific_date_predictors <- subset(test_data, datetime == as.POSIXct("2024-04-16 16:00:00"))[, predictors]
lm_model <- lm(production ~ . + production_lag9, data = train_data[, c("production", predictors)])
# Make predictions using the linear regression model for the specific date and hour
linear_regression_prediction <- predict(lm_model, newdata = specific_date_predictors)
linear_regression_prediction <- tail(linear_regression_prediction, 1)
arima_forecast <- forecast(arima_model, h = 1)
arima_prediction <- arima_forecast$mean
prediction_hour_16 <- linear_regression_prediction + arima_prediction
#### FORECASTING FOR HOUR 17
# Split data into training and testing sets
train_data <- hour17[1:800, ]
test_data <- hour17[801:nrow(houreight), ]
snow_variables <- grepl("CSNOW_surface_DSWRF_", colnames(hour17))
predictors <- c("mean_temp","cloud_mean","tc_mean","DLWRF_mean","production_lag9",colnames(train_data)[snow_variables])
# Assuming train_data and test_data are your training and testing datasets
# predictors are the variables used for modeling
specific_date_predictors <- subset(test_data, datetime == as.POSIXct("2024-04-16 17:00:00"))[, predictors]
lm_model <- lm(production ~ . + production_lag9, data = train_data[, c("production", predictors)])
# Make predictions using the linear regression model for the specific date and hour
linear_regression_prediction <- predict(lm_model, newdata = specific_date_predictors)
linear_regression_prediction <- tail(linear_regression_prediction, 1)
arima_forecast <- forecast(arima_model, h = 1)
arima_prediction <- arima_forecast$mean
prediction_hour_17 <- linear_regression_prediction + arima_prediction
#### FORECASTING FOR HOUR 18
# Split data into training and testing sets
train_data <- hour18[1:800, ]
test_data <- hour18[801:nrow(houreight), ]
snow_variables <- grepl("CSNOW_surface_DSWRF_", colnames(hour13))
predictors <- c("mean_temp","cloud_mean","tc_mean","DLWRF_mean","production_lag9",colnames(train_data)[snow_variables])
# Assuming train_data and test_data are your training and testing datasets
# predictors are the variables used for modeling
specific_date_predictors <- subset(test_data, datetime == as.POSIXct("2024-04-16 18:00:00"))[, predictors]
lm_model <- lm(production ~ . + production_lag9, data = train_data[, c("production", predictors)])
# Make predictions using the linear regression model for the specific date and hour
linear_regression_prediction <- predict(lm_model, newdata = specific_date_predictors)
linear_regression_prediction <- tail(linear_regression_prediction, 1)
arima_forecast <- forecast(arima_model, h = 1)
arima_prediction <- arima_forecast$mean
prediction_hour_18 <- linear_regression_prediction + arima_prediction


# actual values 
actual_value6 <- hoursix$production[hoursix$datetime == as.POSIXct("2024-04-15 06:00:00")]
actual_value7 <- hourseven$production[hourseven$datetime == as.POSIXct("2024-04-15 07:00:00")]
actual_value8  <- houreight$production[houreight$datetime == as.POSIXct("2024-04-15 08:00:00")]
actual_value9 <- hournine$production[hournine$datetime == as.POSIXct("2024-04-14 09:00:00")]
actual_value10 <- hourten$production[hourten$datetime == as.POSIXct("2024-04-15 10:00:00")]
actual_value11 <- houreleven$production[houreleven$datetime == as.POSIXct("2024-04-15 11:00:00")]
actual_value12 <- hourtwelve$production[hourtwelve$datetime == as.POSIXct("2024-04-15 12:00:00")]
actual_value13 <- hoursix$production[hoursix$datetime == as.POSIXct("2024-04-15 13:00:00")]
actual_value14 <- hourseven$production[hourseven$datetime == as.POSIXct("2024-04-15 14:00:00")]
actual_value15  <- houreight$production[houreight$datetime == as.POSIXct("2024-04-15 15:00:00")]
actual_value16 <- hournine$production[hournine$datetime == as.POSIXct("2024-04-14 16:00:00")]
actual_value17 <- hourten$production[hourten$datetime == as.POSIXct("2024-04-15 17:00:00")]
actual_value18 <- houreleven$production[houreleven$datetime == as.POSIXct("2024-04-15 18:00:00")]
actual_values <- c(actual_value6,actual_value7,actual_value8,actual_value9,actual_value10,actual_value11,actual_value12,actual_value13,actual_value14,actual_value15,actual_value16,actual_value17,actual_value18)
actual_values

predictions <- c(prediction_hour_6,prediction_hour_7,prediction_hour_8,prediction_hour_9,prediction_hour_10,prediction_hour_11,prediction_hour_12,prediction_hour_13,prediction_hour_14,prediction_hour_15,prediction_hour_16,prediction_hour_17,prediction_hour_18)
predictions

#reporting accuracy
accu <- function(actual, forecast) {
  n <- length(actual)
  error <- actual - forecast
  mean_val <- mean(actual)
  sd_val <- sd(actual)
  CV <- sd_val / mean_val
  FBias <- sum(error) / sum(actual)
  MAPE <- sum(abs(error / actual)) / n
  RMSE <- sqrt(mean(error^2))
  MAD <- mean(abs(error))
  MADP <- sum(abs(error)) / sum(abs(actual))
  WMAPE <- MAD / mean_val
  
  result <- data.frame(n = n,
                       mean = mean_val,
                       sd = sd_val,
                       CV = CV,
                       FBias = FBias,
                       MAPE = MAPE,
                       RMSE = RMSE,
                       MAD = MAD,
                       MADP = MADP,
                       WMAPE = WMAPE)
  return(result)
}
accuracy_metrics <- accu(actual_values, predictions)

# Print the accuracy metrics
print(accuracy_metrics)



