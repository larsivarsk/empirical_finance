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
#arima_model <- auto.arima(nokeur_data$Close)
arima_model <- auto.arima(nokeur_data$Log_Returns)
#arima_model <- auto.arima(nokeur_data$Log_Returns, max.p = 3, max.q = 3, stepwise = FALSE, approximation = FALSE)
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

log_returns <- nokeur_data$Log_Returns


# Fitting the GARCH model to the data
garch_fit <- ugarchfit(spec = garch_spec, data = log_returns, solver = "hybrid")
garch_fit

# Forecast volatility for the next 30 days
garch_forecast <- ugarchforecast(garch_fit, n.ahead = 30)
plot(garch_forecast, which = 3)
# which = 3 plots the predicted volatility (standard deviation) over time.




# MODEL EVALUATION PART - METRICS
# Comparing ARIMA and GARCH using AIC and BIC 

# Extract AIC and BIC for ARIMA
arima_aic <- AIC(arima_model)
arima_bic <- BIC(arima_model)

# Infocriteria(garch_fit) reports values per observation as default
# Thus finding the number of observations in order to scale AIC and BIC 
n_obs <- length(nokeur_data$Log_Returns)

garch_criteria <- infocriteria(garch_fit)
garch_aic <- as.numeric(garch_criteria[1]) * n_obs
garch_bic <- as.numeric(garch_criteria[2]) * n_obs

aic_bic_comparison <- data.frame(
  Model = c("ARIMA", "GARCH"),
  AIC = c(arima_aic, garch_aic),
  BIC = c(arima_bic, garch_bic)
  )

print(aic_bic_comparison)

# Output shows that GARCH has both the lowest AIC and BIC
# => suggesting that the GARCH model fits best in this case


# Comparing ARIMA and GARCH using ACF and PACF
# Load required library
library(ggplot2)

# Get residuals from ARIMA and GARCH models
arima_resid <- residuals(arima_model)
garch_resid <- residuals(garch_fit)

# Plot ACF and PACF for ARIMA residuals
par(mfrow = c(2, 2))  # 2x2 layout for plots

acf(arima_resid, main = "ACF of ARIMA Residuals")
pacf(arima_resid, main = "PACF of ARIMA Residuals")

# Plot ACF and PACF for GARCH residuals
acf(garch_resid, main = "ACF of GARCH Residuals")
pacf(garch_resid, main = "PACF of GARCH Residuals")

# Reset plotting layout
par(mfrow = c(1, 1))


# RMSE and MAE for model performance
library(Metrics)

# Extracting actual values
actual_values <- log_returns

# Extracting fitted values from ARIMA
arima_fitted <- fitted(arima_model)

# Extracting fitted values from GARCH
garch_fitted <- fitted(garch_fit)

# Computing RMSE
arima_rmse <- rmse(actual_values, arima_fitted)
garch_rmse <- rmse(actual_values, garch_fitted)

# Computing MAE
arima_mae <- mae(actual_values, arima_fitted)
garch_mae <- mae(actual_values, garch_fitted)

# Comparison
rmse_mae_comparison <- data.frame(
  Model = c("ARIMA", "GARCH"),
  RMSE = c(arima_rmse, garch_rmse),
  MAE = c(arima_mae, garch_mae)
)

print(rmse_mae_comparison)

# Output shows that ARIMA slightly outperforms GARCH in terms of prediction accuracy
# However, the difference is so small that it might not be practically significant
# Also, GARCH is designed to model volatility rather than returns, so RMSE/MAE might not fully capture its strength


