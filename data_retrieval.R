library(quantmod)
library(ggplot2)
library(tseries)

# Retrieve NOK/EUR exchange rate from Yahoo Finance
getSymbols("NOKEUR=X", src = "yahoo", from = "2015-01-01", to = Sys.Date()) # sys.date should be updated?

# Convert to dataframe
nokeur_data <- data.frame(Date = index(`NOKEUR=X`), coredata(`NOKEUR=X`))
nokeur_data <- na.omit(nokeur_data)

# View first rows
head(nokeur_data)

# Extracting the closing prices
closing_prices <- Cl(NOKEUR.X)



ggplot(nokeur_data, aes(x = Date, y = NOKEUR.X.Close)) +
  geom_line() + 
  labs(title = "NOK/EUR Exchange Rate", x = "Date", y = "Exchange Rate")


adf.test(nokeur_data$NOKEUR.X.Close, alternative = "stationary")
