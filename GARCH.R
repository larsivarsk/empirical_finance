library(quantmod)
library(ggplot2)
library(tseries)
library(rugarch)

# Retrieve NOK/EUR exchange rate from Yahoo Finance
assign("nokeur_data", getSymbols("NOKEUR=X", src = "yahoo", from = "2015-01-01", to = "2025-01-01", auto.assign = FALSE))
nokeur_data <- nokeur_data[nokeur_data$Date <= as.Date("2025-01-01"), ]

# Convert to dataframe
nokeur_data <- data.frame(Date = index(`NOKEUR=X`), coredata(`NOKEUR=X`))
nokeur_data <- na.omit(nokeur_data)

# View first rows
head(nokeur_data)

# Extracting the closing prices
closing_prices <- nokeur_data$NOKEUR.X.Close
closing_prices <- na.omit(closing_prices)

ggplot(nokeur_data, aes(x = Date, y = NOKEUR.X.Close)) +
  geom_line() + 
  labs(title = "NOK/EUR Exchange Rate", x = "Date", y = "Exchange Rate")


adf.test(nokeur_data$NOKEUR.X.Close, alternative = "stationary")


# -------------------------------
# GARCH-MODELL FOR VOLATILITET
# -------------------------------
# Log-returdata (fordi GARCH jobber med retur, ikke pris)


garch_spec <- ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
  mean.model = list(armaOrder = c(0, 0), include.mean = TRUE),
  distribution.model = "norm",
)


# Definer log_returns korrekt
log_returns <- diff(log(closing_prices))
log_returns <- na.omit(log_returns)


garch_fit <- ugarchfit(spec = garch_spec, data = log_returns, solver = "nlminb")
summary(garch_fit)
garch_fit

garch_forecast <- ugarchforecast(garch_fit, n.ahead = 30)
plot(garch_forecast, which = 3)



