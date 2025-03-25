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



#----------------------------------------
# MODEL EVALUATION PART - LIV
#----------------------------------------
# Comparing ARIMA and GARCH using AIC and BIC 

# AIC() and BIC() does not divide by sample size T
# Thus finding the number of observations in order to downscale AIC and BIC 
n_obs <- length(nokeur_data$Log_Returns)

# Extract AIC and BIC for ARIMA
arima_aic <- AIC(arima_model) / n_obs
arima_bic <- BIC(arima_model) / n_obs

garch_criteria <- infocriteria(garch_fit)
garch_aic <- as.numeric(garch_criteria[1]) 
garch_bic <- as.numeric(garch_criteria[2]) 

aic_bic_comparison <- data.frame(
  Model = c("ARIMA", "GARCH"),
  AIC = c(arima_aic, garch_aic),
  BIC = c(arima_bic, garch_bic)
  )

print(aic_bic_comparison)

# Output shows that GARCH has both the lowest AIC and BIC
# => suggesting that the GARCH model fits best in this case


#----------------------------------------
# Using RMSE and MAE for model performance

# RMSE penalizes large errors more than MAE due to squaring the differences before averaging.
# It is useful when large deviations should be weighted more heavily
# MAE gives equal weight to all errors. It is more robust to outliers.

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

# ARIMA slightly outperforms GARCH in terms of prediction accuracy
# However, the difference is so small that it might not be practically significant

#Visualization of RMSE and MAE
# Load necessary library
library(ggplot2)

# Reshape data for plotting
rmse_mae_long <- tidyr::pivot_longer(rmse_mae_comparison, cols = c(RMSE, MAE), names_to = "Metric", values_to = "Value")

# Bar plot comparing RMSE and MAE
ggplot(rmse_mae_long, aes(x = Model, y = Value, fill = Metric)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "RMSE and MAE Comparison for ARIMA and GARCH", x = "Model", y = "Error Value") +
  theme_minimal() +
  scale_fill_manual(values = c("RMSE" = "steelblue", "MAE" = "darkorange"))



#-----------------------------------------
# fiGARCH and TARCH model performance
#----------------------------------------
# CODE FROM LARS IVAR: 
# CHANGING GARCH_SPEC TO GARCH_SPEC_FI AND GARCH_FIT TO GARCH_FIT_FI
# SAME WITH TARCH => GARCH_SPEC_TA AND GARCH_FIT_TA
#----------------------------------------

# fiGARCH is a model where the volatility has long memory. So if the volatility is still stationary,
# it will take longer to go back to equilibrium when hit by a shock.
library(rugarch)
# Fit fiGARCH model
garch_spec_fi <- ugarchspec(
  variance.model = list(
    model = "fiGARCH",
    garchOrder = c(1,1)
  ),
  mean.model = list(
    armaOrder = c(0,0),
    include.mean = TRUE
  ),
  distribution.model = "norm",
  # Giving some starting parameters to help convergence
  start.pars = list(omega = 0.0001, alpha1 = 0.2, beta1 = 0.5)
)
garch_fit_fi <- ugarchfit(spec = garch_spec_fi, data = nokeur_data$Log_Returns, solver = "hybrid") #crashes if I run "lbfgs"
garch_fit_fi
# We get a significant delta parameter without robust standard errors, it is insignificant with robust standard errors.


# TARCH will help capture asymmetries in the dynamics of the returns.
garch_spec_ta <- ugarchspec(
  variance.model = list(
    model = "gjrGARCH",
    garchOrder = c(1,1)
  ),
  mean.model = list(
    armaOrder = c(0,0),
    include.mean = TRUE
  ),
  distribution.model = "norm"
)
garch_fit_ta <- ugarchfit(spec = garch_spec_ta, data = nokeur_data$Log_Returns)
garch_fit_ta
# The gamma parameter is also significant without robust standard errors, but insignificant with robust standard errors.
# Since the gamma is positive and significant without robust standard errors, it indicates that negative shocks have 
# larger impact on volatility. This is also known as the leverage effect.

#----------------------------------------
# MODEL PERFORMANCE fiGARCH AND TARCH
#----------------------------------------

# Extracting AIC and BIC
aic_fi <- infocriteria(garch_fit_fi)[1]
bic_fi <- infocriteria(garch_fit_fi)[2]

aic_ta <- infocriteria(garch_fit_ta)[1]
bic_ta <- infocriteria(garch_fit_ta)[2]

# Print 
aic_bic_comparison_fi_ta <- data.frame(
  Model = c("fiGARCH", "TARCH"),
  AIC = c(aic_fi, aic_ta),
  BIC = c(bic_fi, bic_ta)
)

print(aic_bic_comparison_fi_ta)

# Output shows that TARCH has both the lowest AIC and BIC



#----------------------------------------
# Computing RMSE and MAE

library(Metrics)

# Extracting actual values
actual_values <- log_returns

# Extracting fitted values from fiGARCH and TARCH
figarch_fitted <- fitted(garch_fit_fi)
tarch_fitted <- fitted(garch_fit_ta)

# Computing RMSE
figarch_rmse <- rmse(actual_values, figarch_fitted)
tarch_rmse <- rmse(actual_values, tarch_fitted)

# Computing MAE
figarch_mae <- mae(actual_values, figarch_fitted)
tarch_mae <- mae(actual_values, tarch_fitted)

# Comparison
rmse_mae_comparison_fi_ta <- data.frame(
  Model = c("fiGARCH", "TARCH"),
  RMSE = c(figarch_rmse, tarch_rmse),
  MAE = c(figarch_mae, tarch_mae)
)

print(rmse_mae_comparison_fi_ta)

# Output is very similar to that of ARIMA and GARCH 
# => Small differences between the models, and similar sizes of errors

#Visualization
library(ggplot2)

# Reshape data for plotting
rmse_mae_long_fi_ta <- tidyr::pivot_longer(rmse_mae_comparison_fi_ta, cols = c(RMSE, MAE), names_to = "Metric", values_to = "Value")

# Bar plot comparing RMSE and MAE
ggplot(rmse_mae_long_fi_ta, aes(x = Model, y = Value, fill = Metric)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "RMSE and MAE Comparison fiGARCH and TARCH", x = "Model", y = "Error Value") +
  theme_minimal() +
  scale_fill_manual(values = c("RMSE" = "steelblue", "MAE" = "darkorange"))


#-------------------------------------------------------------------------
# MODEL PERFORMANCE: ALL FOUR MODELS - ARIMA, GARCH, fiGARCH AND TARCH
#-------------------------------------------------------------------------

library(ggplot2)
library(Metrics)
library(tidyr)
library(gridExtra)

#----------------------------------------
# AIC AND BIC FOR ALL MODELS

aic_values_all <- c(arima_aic, garch_aic, aic_fi, aic_ta)
bic_values_all <- c(arima_bic, garch_bic, bic_fi, bic_ta)

model_names_all <- c("ARIMA", "GARCH", "fiGARCH", "TARCH")

aic_bic_comparison_all <- data.frame(
  Model = model_names_all,
  AIC = aic_values_all,
  BIC = bic_values_all
)

print(aic_bic_comparison_all)

#----------------------------------------
# RMSE AND MAE FOR ALL MODELS

# Extract fitted values
fitted_values <- list(
  "ARIMA" = arima_fitted,
  "GARCH" = garch_fitted,
  "fiGARCH" = figarch_fitted,
  "TARCH" = tarch_fitted
)

# Computing RMSE and MAE
rmse_values_all <- sapply(fitted_values, function(fit) rmse(actual_values, fit))
mae_values_all <- sapply(fitted_values, function(fit) mae(actual_values, fit))

# Create dataframe
rmse_mae_comparison_all <- data.frame(
  Model = model_names_all,
  RMSE = rmse_values_all,
  MAE = mae_values_all
)

print(rmse_mae_comparison_all)

# Reshape data for plotting
rmse_mae_long_all <- pivot_longer(rmse_mae_comparison_all, cols = c(RMSE, MAE), names_to = "Metric", values_to = "Value")

# Bar plot comparing RMSE and MAE for all models
ggplot(rmse_mae_long_all, aes(x = Model, y = Value, fill = Metric)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "RMSE and MAE Comparison for All Models", x = "Model", y = "Error Value") +
  theme_minimal() +
  scale_fill_manual(values = c("RMSE" = "steelblue", "MAE" = "darkorange"))

