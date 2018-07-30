library(quantmod)
library(FatTailsR)
library(fitteR)
library(ggplot2)
library(purrr)
library(dplyr)
library(tidyr)

date_from <- "2011-07-01"
date_to <- "2018-07-27"

suppressMessages(getSymbols("PANW", from=date_from, to=date_to, src="yahoo", adjust=TRUE))  
stock <- PANW$PANW.Adjusted

colnames(stock) <- c("Price")
stock$Ret <- dailyReturn(stock$Price)
start_price <- as.numeric(stock$Price[nrow(stock)])

trials <- 10000
days <- 500

mu <- mean(stock$Ret)
sigma <- sd(stock$Ret)
annual_vol <- sigma * sqrt(251)

f <- paramkienerX(stock$Ret)
ret_paths <- matrix(nrow = days, ncol = trials)

dist1 <- data.frame(dist = "Emp", rets = rkiener2(100000, m = f[1], g = f[2], a = f[3], w = f[4]))
dist2 <- data.frame(dist = "Zero", rets = rkiener2(100000, m = 0, g = f[2], a = f[3], w = f[4]))
dists <- rbind(dist1, dist2)

# visualize the fit distribution
ggplot() +
  geom_histogram(bins = 1000, data = dist1, aes(x = `rets`), colour = 'red', alpha = 0.25) +
  geom_histogram(bins = 100, data = stock, aes(x = `Ret`), colour = 'blue') +
  xlim(-.30, .3)


for(i in c(1:days)){
  ret_paths[i,] <- rkiener2(trials, m = 0, g = f[2], a = f[3], w = f[4])
  # ret_paths[i,] <- rnorm(trials, mean = 0, sd = sigma)
}

df_ret <- data.frame(ret = ret_paths)

# calc possible price paths
df_tidy <- df_ret %>%
  gather() %>%
  group_by(key) %>%
  mutate(path = (start_price * (1 + cumsum(`value`)))) %>%
  mutate(day = c(1:days))

#ggplot(data = df_tidy) +
  #geom_line(aes(x=day, y=path, colour=key))

strike <- 75
discount <- ((1 + 0.05) ^ (days/251))

call <- FALSE

df_results <- df_tidy %>%
  filter(`day` == days)

if(call){
  df_results$`cash flow` <- df_results$path - strike
}else{
  df_results$`cash flow` <- strike - df_results$path
}

df_results$`cash flow`[df_results$`cash flow` < 0] <- 0
df_results$`dcf` <- df_results$`cash flow` / discount

sum(df_results$`dcf`) / trials

