library(tidyverse)
library(fpp3)
library(fabletools)
library(dplyr)
library(seasonal)
library(forecast)
library(lubridate) 
library(forecast) 
library(TTR) 
library(MLmetrics) 
library(tseries) 
library(TSstudio) 
library(ggplot2)
library(tidyr)
library(padr)
library(hrbrthemes)
library(zoo)

#Loading the Dataset
gas_data <- read_csv("Downloads/archive (7)/monthly_csv.csv")
View(gas_data)

#Handling Missing Values
missing_values <- colSums(is.na(gas_data))
print(missing_values)

#Checking the structure of the data frame
str(gas_data)

#Creating a new column with date values (year-month format)
gas_data$Date <- as.yearmon(gas_data$Month)

#Converting 'Date' column to standard Date class
gas_data$Date <- as.Date(as.yearmon(gas_data$Date))

#Checking the structure of the data frame after conversion
str(gas_data)

#Dropping the 'Month' column
gas_data <- select(gas_data, -Month)

#checking if there is any date that is not available in this dataset
period <- seq.Date(from=as.Date("1997-01-01"), to=as.Date("2020-08-01"), by="month")
all(period==gas_data$Date)

#Creating a time series object
gas_ts <- ts(gas_data$Price, frequency = 12, start = c(1997, 1))
gas_ts

#Plotting the time series
plot(gas_ts, main = "Gas Prices Over Time", xlab = "Year", ylab = "Price")

#Decompose the `gas_ts` using STL
gas_stl <- stl(gas_ts, s.window="periodic")
gas_stl

# Visualize the decomposed components
autoplot(gas_stl) +
  theme_minimal() +
  labs(title = "STL Decomposition of Gas Price Time Series")

# Number of observations for the test set (last 36 months)
test_obs <- 36

# Create the test set using tail()
gas_test <- tail(gas_ts, test_obs)
head(gas_test)
tail(gas_test)

# Create the training set using head()
gas_train <- head(gas_ts, -test_obs)

# Print the datasets
print("Training Set:")
print(gas_train)

print("Test Set:")
print(gas_test)

#Fitting an ETS model
ets_model <- ets(gas_train, model = "ZZZ", lambda=0)

# Print the model summary
summary(ets_model)

fc_ets <- ets_model |> forecast(h=36)
accuracy(fc_ets, gas_train)

# Fitting an ETS model
ets_model <- ets(gas_train, model = "ZZZ", lambda = 0)
fc_ets <- forecast(ets_model, h = 36, biasadj = T)

# Plotting the forecast
autoplot(fc_ets) +
  labs(title = "ETS Model Gas Consumption Forecast",
       y = "Consumption",
       x = "Time")

test_forecast <- forecast(ets_model, h = length(gas_test))

#Calculate accuracy metrics for the test set
accuracy_ETS <- accuracy(test_forecast, gas_test)
print(accuracy_ETS)

#ARIMA

# Using ndiffs function to determine the number of differences needed to make the time series stationary
ndiffs(gas_train)

#Creating an object called `gas_ts_stationary`: one time differencing of `gas_ts`
gas_stationary <- diff(gas_train, differences= 1)

#autoplot(gas_stationary)

# Observe the plot of `gas_ts_stationary`
#plot(gas_stationary, type="l", main="Stationary `gas_ts`")

#Augmented Dickey-Fuller test to check for stationarity
adf.test(gas_stationary)

gas_stationary %>% 
  tsdisplay()

# Fitting ARIMA Models manually
order_combinations <- list(c(0, 1, 5), c(5, 1, 5), c(0, 1, 9), c(9, 1, 5), c(9,1,9))

arima_models <- list()

for (i in seq_along(order_combinations)) {
  order <- order_combinations[[i]]
  model_name <- paste("model_arima", i, sep = "_")
  arima_models[[model_name]] <- Arima(gas_train, order = order, lambda = 0)
}


# Print the fitted models
arima_models

# Fitting ARIMA automatically
model_arima_auto <- auto.arima(gas_train, seasonal = F, lambda =0)
model_arima_auto


# Extract AIC values
aic_values <- sapply(arima_models, AIC)

# Convert to a tibble
aic_tibble <- tibble(Model = names(aic_values), AIC = aic_values)

# Print the AIC values
print(aic_tibble)

# Forecasting using the best model (model 1)
best_model <- arima_models[["model_arima_1"]]
forecast_result <- forecast(best_model, h = 36)

# Print the forecast result
print(forecast_result)

# Plot the forecast
autoplot(forecast_result) +
  labs(title = "ARIMA Model Forecast (0,1,5)",
       y = "Price",
       x = "Year")

#residuals diagnostics
tsdisplay(residuals(best_model), lag.max=12, main='ARIMA Best Model Residuals')

fit_arima <- Arima(gas_train, order = c(0, 1, 5), lambda = 0)

# Obtain residuals
residuals <- residuals(fit_arima)

# Ljung-Box test
ljung_box_test <- Box.test(residuals, lag = 12, type = "Ljung-Box")

# Print the test results
print(ljung_box_test)

#SARIMA
diff(gas_train, lag=36) %>% 
  diff() %>%
  adf.test()

diff(gas_train, lag=12) %>% 
  diff() %>% 
  tsdisplay(lag.max=36)


# order combinations for SARIMA models
order_combinations <- list(c(0, 1, 5), c(5, 1, 5), c(0, 1, 9), c(9, 1, 5), c(9, 1, 9))

Sarima_models <- list()

for (i in seq_along(order_combinations)) {
  order <- order_combinations[[i]]
  model_name <- paste("model_sarima", i, sep = "_")
  Sarima_models[[model_name]] <- Arima(gas_train, order = order, seasonal = list(order = c(0, 1, 1)), lambda = 0)
}

# Fit SARIMA models manually
Sarima_models <- lapply(order_combinations, function(order) {
  Arima(gas_train, order = order, seasonal = list(order = c(0, 1, 1)), lambda = 0)
})

# Print the fitted models
print(Sarima_models)

# Fit ARIMA model automatically
model_Sarima_auto <- auto.arima(gas_train, seasonal = TRUE, lambda = 0)
print(model_Sarima_auto)

# Extract AIC values
Sarima_aic_values <- sapply(Sarima_models, AIC)

# Convert AIC values to a tibble
Sarima_aic_tibble <- tibble(Model = names(Sarima_aic_values), AIC = Sarima_aic_values)
print(Sarima_aic_tibble)

# Forecasting using the best model (model 1)
Sarima_best_model <- Sarima_models[["model_sarima_1"]]
print(Sarima_best_model)
fc_Sarima <- forecast(Sarima_best_model, h = 36)

# Print the forecast result
print(fc_Sarima)

# Plot the forecast
autoplot(fc_Sarima) +
  labs(title = "SARIMA Model Forecast (0,1,5)(0,1,1)[12]",
       y = "Price",
       x = "Year")

# Display residuals diagnostics
tsdisplay(residuals(Sarima_best_model), lag.max = 24, main = 'SARIMA Best Model Residuals')
