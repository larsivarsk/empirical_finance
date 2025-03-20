library(quantmod)
library(ggplot2)
library(tseries)
library(dplyr)
library(forecast)
library(rugarch)

# Retrieve NOK/EUR exchange rate from Yahoo Finance
getSymbols("NOKEUR=X", src = "yahoo", from = "2015-01-01", to = "2025-01-01") 

# Convert data to dataframe and adjust column names
nokeur_data <- data.frame(Date = index(`NOKEUR=X`), coredata(`NOKEUR=X`))
nokeur_data <- na.omit(nokeur_data)
colnames(nokeur_data) <- c("Date", "Open", "High", "Low", "Close", "Volume", "Adjusted")

# Plot exchange rate
ggplot(nokeur_data, aes(x = Date, y = Close)) +
  geom_line(color = "darkorchid4") + 
  labs(title = "NOK/EUR Exchange Rate", x = "Date", y = "Exchange Rate") +
  theme_minimal()

# Compute log returns for stationarity
nokeur_data <- nokeur_data %>%
  mutate(Log_Returns = log(Close / lag(Close)))

# Remove NA values caused by lag
nokeur_data <- na.omit(nokeur_data)

# Plot log returns
ggplot(nokeur_data, aes(x = Date, y = Log_Returns)) +
  geom_line(color = "cyan4") +
  labs(title = "Log Returns of NOK/EUR Exchange Rate", x = "Date", y = "Log Returns") +
  theme_minimal()

# ADF test for stationarity
adf.test(nokeur_data$Log_Returns, alternative = "stationary")

# -------------------------------
# ACF and PACF for ARIMA MODEL
# -------------------------------
par(mfrow = c(1, 2))
acf(nokeur_data$Log_Returns, main = "ACF of Log Returns")
pacf(nokeur_data$Log_Returns, main = "PACF of Log Returns")

# Build ARIMA model based on ACF/PACF
arima_model <- auto.arima(nokeur_data$Close)
summary(arima_model)

# Forecast ARIMA
forecasted_values <- forecast(arima_model, h = 30)
plot(forecasted_values)

