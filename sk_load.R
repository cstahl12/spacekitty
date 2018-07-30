library(quantmod)
library(quantstrat)

pv <- list(
  source = "yahoo",
  account_equity = 20000,
  transaction_fees = 0,
  init_date = as.POSIXct(Sys.Date()),
  start_date = "1999-01-01",
  end_date = "2018-06-30",
  adjust = TRUE
)

Sys.setenv(TZ = "UTC")
currency('USD')

initEq <- pv$account_equity
Sys.setenv(TZ = "UTC")
currency('USD')

symbols <- basic_symbols()

suppressWarnings(getSymbols(symbols, src = pv$source, index.class = c("POSIXt", "POSIXct"),
           from = pv$start_date, to = pv$end_date, adjust = pv$adjust))

for(symbol in symbols) {
  stock(symbol, currency = "USD", multiplier = 1)
  x <- get(symbol)
  indexFormat(x) <- "%Y-%m-%d"
  colnames(x) <- gsub("x", symbol, colnames(x))
  assign(symbol, x)
  # Set pv$init_date to one day prior to earliest date in all of symbols
  if(pv$init_date > min(index(x))) pv$init_date = min(index(x)) - 86400
  rm(x)
}