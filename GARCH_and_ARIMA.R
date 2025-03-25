library(quantmod)
library(ggplot2)
library(tseries)
library(dplyr)
library(forecast)
library(rugarch)

# We retrieve NOK/EUR exchange rate from Yahoo Finance
getSymbols("NOKEUR=X", src = "yahoo", from = "2015-01-01", to = "2025-01-01") 

# We convert the data to dataframe and adjust column names
nokeur_data <- data.frame(Date = index(`NOKEUR=X`), coredata(`NOKEUR=X`))
nokeur_data <- na.omit(nokeur_data)
colnames(nokeur_data) <- c("Date", "Open", "High", "Low", "Close", "Volume", "Adjusted")
#nokeur_data %>% filter(Date >= "2025-02-14" & Date <= "2025-02-17")

# Plot exchange rate
ggplot(nokeur_data, aes(x = Date, y = Close)) +
  geom_line(color = "darkorchid4") + 
  labs(title = "NOK/EUR Exchange Rate", x = "Date", y = "Exchange Rate") +
  theme_minimal()

# Since most models require stationary data, we compute log returns that tend to be stationary
nokeur_data <- nokeur_data %>%
  mutate(Log_Returns = log(Close / lag(Close)))

# Remove NA values caused by lag
nokeur_data <- na.omit(nokeur_data)

# We plot the data to visualy inspect stationarity, volatility clustering,...
ggplot(nokeur_data, aes(x = Date, y = Log_Returns)) +
  geom_line(color = "cyan4") +
  labs(title = "Log Returns of NOK/EUR Exchange Rate", x = "Date", y = "Log Returns") +
  theme_minimal()

# We confirm stationarity with ADF test (H0 = Time series is non-stationary)
adf.test(nokeur_data$Log_Returns, alternative = "stationary")

# ACF and PACF of Log Returns
par(mfrow = c(1, 2))
acf(nokeur_data$Log_Returns, main = "ACF of Log Returns")
pacf(nokeur_data$Log_Returns, main = "PACF of Log Returns")

# Build ARIMA model based on ACF/PACF
arima_model <- auto.arima(nokeur_data$Log_Returns)
summary(arima_model)

# Forecast ARIMA
forecasted_values <- forecast(arima_model, h = 30)
plot(forecasted_values)


acf(nokeur_data$Log_Returns^2, main = "ACF of Squared Log Returns")
pacf(nokeur_data$Log_Returns^2, main = "PACF of Squared Log Returns")

# Ljung-Box Test for volatility clustering
lags <- 1:10
ljung_box_results <- data.frame(
  lag = lags,
  Q_statistic = sapply(lags, function(lag) Box.test(
    nokeur_data$Log_Returns^2,
    lag = lag,
    type = "Ljung-Box")$statistic),
  p_value = sapply(lags, function(lag) Box.test(
    nokeur_data$Log_Returns^2,
    lag = lag,
    type = "Ljung-Box")$p.value)
)
print(ljung_box_results)

# We define the GARCH model
garch_spec <- ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
  # "sGARCH" is the standard GARCH model, which models volatility directly.
  
  mean.model = list(armaOrder = c(0, 0), include.mean = TRUE),
  distribution.model = "norm",
  # We assume the residuals follows a normal distribution.
)

# Calculate log returns 

#log_returns <- nokeur_data$Log_Returns


# Fitting the GARCH model to the data
garch_fit <- ugarchfit(spec = garch_spec, data = nokeur_data$Log_Returns, solver = "hybrid")
garch_fit

# Forecast volatility for the next 30 days
garch_forecast <- ugarchforecast(garch_fit, n.ahead = 31)
plot(garch_forecast, which = 2)
# which = 3 plots the predicted volatility (standard deviation) over time.




