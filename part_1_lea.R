library(quantmod)
library(ggplot2)
library(tseries)
library(dplyr)

# We retrieve NOK/EUR exchange rate from Yahoo Finance
getSymbols("NOKEUR=X", src = "yahoo", from = "2015-01-01", to = "2025-01-01") 

# We convert the data to dataframe and adjust column names
nokeur_data <- data.frame(Date = index(`NOKEUR=X`), coredata(`NOKEUR=X`))
nokeur_data <- na.omit(nokeur_data)
colnames(nokeur_data) <- c("Date", "Open", "High", "Low", "Close", "Volume", "Adjusted")

ggplot(nokeur_data, aes(x = Date, y = Close)) +
  geom_line(color = "darkorchid4") + 
  labs(title = "NOK/EUR Exchange Rate", 
       x = "Date", y = "Exchange Rate") +
  theme_minimal()

# Since most models require stationary data, we compute log returns that tend to be stationary
nokeur_data <- nokeur_data %>%
  mutate(Log_Returns = log(Close / lag(Close)))

# Remove NA values caused by lag
nokeur_data <- na.omit(nokeur_data)

# We plot the data to visualy inspect stationarity, volatility clustering,...
ggplot(nokeur_data, aes(x = Date, y = Log_Returns)) +
  geom_line(color = "cyan4") +
  labs(title = "Log Returns of NOK/EUR Exchange Rate", 
       x = "Date", y = "Log Returns") +
  theme_minimal()

# We confirm stationarity with ADF test (H0 = Time series is non-stationary)
adf.test(nokeur_data$Log_Returns, alternative = "stationary")
# p-value is < 0.05: we reject H0 and conclude that our data is stationary

# ACF and PACF of Log Returns
par(mfrow = c(1, 2))
acf(nokeur_data$Log_Returns, main = "ACF of Log Returns")
pacf(nokeur_data$Log_Returns, main = "PACF of Log Returns")
# We see that there is NO (or very little) linear dependence meaning past returns do not help predict future returns and AR(1) is Not Useful

# ACF and PACF of squared Log Returns to check for volatility clustering and non-linear dependence
acf(nokeur_data$Log_Returns^2, main = "ACF of Squared Log Returns")
pacf(nokeur_data$Log_Returns^2, main = "PACF of Squared Log Returns")
# squared returns show significant autocorrelation -> we confirm that with the following test:
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
ljung_box_results
# All p-values are 0 ->  reject the null hypothesis. of no autocorrelation-> volatility clusters over time.
# We should use models like GARCH, TARCH, or FIGARCH





