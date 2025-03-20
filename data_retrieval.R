library(quantmod)
library(ggplot2)
library(tseries)

# Retrieve NOK/EUR exchange rate from Yahoo Finance
getSymbols("NOKEUR=X", src = "yahoo", from = "2015-01-01", to = "2025-01-01") # sys.date should be updated?

# Convert to dataframe
nokeur_data <- data.frame(Date = index(`NOKEUR=X`), coredata(`NOKEUR=X`))
nokeur_data <- na.omit(nokeur_data)

# View first rows
head(nokeur_data)

# Extracting the closing prices
closing_prices <- nokeur_data$NOKEUR.X.Close

ggplot(nokeur_data, aes(x = Date, y = closing_prices)) +
  geom_line() + 
  labs(title = "NOK/EUR Exchange Rate", x = "Date", y = "Exchange Rate")

adf.test(closing_prices, alternative = "stationary")
