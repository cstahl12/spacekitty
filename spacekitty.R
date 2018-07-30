library(IKTrading)
library(quantstrat)
library(TTR)
library(PerformanceAnalytics)

initDate="2016-01-01"
from="2017-01-01"
to="2018-07-14"

options(width=70)
options("getSymbols.warning4.0"=FALSE)
currency('USD')
Sys.setenv(TZ="UTC")

source("symbols.R")
source("checkBlotterUpdate.R")

symbols <- c("NVDA", "AMD", "BA", "LMT", "NOC", "PANW",
             "FB", "GME", "GE", "NFLX")

suppressMessages(getSymbols(symbols, from=from, to=to, src="yahoo", adjust=TRUE))  
stock(symbols, currency="USD", multiplier=1)

source("strategy1.R")

# set up analytics
updatePortf(portfolio.st)
dateRange <- time(getPortfolio(portfolio.st)$summary)[-1]
updateAcct(portfolio.st,dateRange)
updateEndEq(account.st)

# trading stats
tstats <- tradeStats(Portfolios = portfolio.st)
tstats$Profit.Factor

# visualize
chart.Posn(Portfolio = portfolio.st, Symbol = "AMD")

rets <- PortfReturns(Account = account.st)
rownames(rets) <- NULL

# add buy and hold strategies

#charts.PerformanceSummary(rets$SPY.DailyEqPL, colorset = bluefocus)

tab.perf <- table.Arbitrary(rets,
                            metrics=c(
                              "Return.cumulative",
                              "Return.annualized",
                              "SharpeRatio.annualized",
                              "CalmarRatio"),
                            metricsNames=c(
                              "Cumulative Return",
                              "Annualized Return",
                              "Annualized Sharpe Ratio",
                              "Calmar Ratio"))

chart.Posn(Portfolio = portfolio.st, Symbol = "NFLX")
pts <- perTradeStats(portfolio.st, Symbol = "NFLX")

