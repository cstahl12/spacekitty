# trade sizing and initial equity settings
tradeSize <- 30000
initEq <- tradeSize*length(symbols)

strategy.st <- portfolio.st <- account.st <- "spacekitty"
rm.strat(portfolio.st)
rm.strat(strategy.st)
rm.strat(account.st)

initPortf(portfolio.st, symbols = symbols, initDate = initDate, currency = 'USD')
initAcct(account.st, portfolios = portfolio.st, initDate = initDate, currency = 'USD', initEq = initEq)
initOrders(portfolio.st, initDate = initDate)
strategy(strategy.st, store = TRUE)

# END OF BOILERPLATE #

# parameters
pctATR=.005
period=15

fSMA <- 20
fSMA_label <- paste0("sma",fSMA)
sSMA <- 50
sSMA_label <- paste0("sma",sSMA)

stoploss_pct <- 0.05

# indicators
add.indicator(strategy.st, name="lagATR", 
              arguments=list(HLC=quote(HLC(mktdata)), n=period), 
              label="atrX")

add.indicator(strategy = strategy.st,
              name = "SMA",
              arguments = list(x = quote(Cl(mktdata)), n = fSMA),
              label = fSMA_label)

add.indicator(strategy = strategy.st,
              name = "SMA",
              arguments = list(x = quote(Cl(mktdata)), n = sSMA),
              label = sSMA_label)

# signals
add.signal(strategy = strategy.st,
           name="sigCrossover",
           arguments = list(columns = c(fSMA_label, sSMA_label), relationship = "gt"),
           label = "crossFastSlow")

add.signal(strategy = strategy.st,
           name="sigCrossover",
           arguments = list(columns = c(fSMA_label, sSMA_label), relationship = "lt"),
           label = "crossSlowFast")

add.rule(strategy.st,
         name = "ruleSignal", 
         arguments=list(sigcol = "crossSlowFast",
                        sigval = TRUE,
                        orderqty = "all", 
                        ordertype = "market",
                        orderside = "long", 
                        replace = TRUE,
                        prefer = "Open"), 
         type="exit",
         label = "longExit",
         path.dep=TRUE)

add.rule(strategy.st, name="ruleSignal", 
         arguments=list(sigcol="crossFastSlow",
                        sigval=TRUE, ordertype="market", 
                        orderside="long",
                        replace=FALSE,
                        prefer="Open",
                        osFUN=osDollarATR,
                        tradeSize=tradeSize,
                        pctATR=pctATR,
                        atrMod="X"), 
         type="enter", path.dep=TRUE)

# apply strategy
t1 <- Sys.time()
out <- applyStrategy(strategy=strategy.st, portfolios=portfolio.st)
t2 <- Sys.time()
print(t2-t1)