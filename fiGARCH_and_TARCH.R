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

# Lea's work ^^
# ---------------------------------------------------------------------------

# fiGARCH is a model where the volatility has long memory. So if the volatility is still stationary,
# it will take longer to go back to equilibrium when hit by a shock.
library(rugarch)
# Fit fiGARCH model
garch_spec <- ugarchspec(
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
garch_fit <- ugarchfit(spec = garch_spec, data = nokeur_data$Log_Returns, solver = "hybrid") #crashes if I run "lbfgs"
garch_fit
# We get a significant delta parameter without robust standard errors, it is insignificant with robust standard errors.


# TARCH will help capture asymmetries in the dynamics of the returns.
garch_spec <- ugarchspec(
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
garch_fit <- ugarchfit(spec = garch_spec, data = nokeur_data$Log_Returns)
garch_fit
# The gamma parameter is also significant without robust standard errors, but insignificant with robust standard errors.
# Since the gamma is positive and significant without robust standard errors, it indicates that negative shocks have 
# larger impact on volatility. This is also known as the leverage effect.
