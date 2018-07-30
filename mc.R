library(quantmod)
library(dplyr)
library(ggplot2)
library(PerformanceAnalytics)
library(xts)
library(TTR)

getSymbols("NVDA", from = "2017-01-01", to = "2018-07-16")
prices <- NVDA$NVDA.Adjusted
rets <- Return.calculate(prices)

# indicators
sma10 <- SMA(prices, n = 10)
sma50 <- SMA(prices, n = 50)

# rules
sig1050 <- sma10 > sma50
basic <- rets * sig1050
cbasic <- cumsum(na.omit(basic))


df <- merge.xts(prices, rets, sma10, sma50, all = TRUE)
colnames(df) <- c("price", "return", "sma10", "sma50")

ggplot() +
  geom_line(aes(x = index(df), y = df$return))

            