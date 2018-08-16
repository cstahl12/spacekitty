library(tidyquant)
library(FatTailsR)
library(ggplot2)
library(purrr)
library(dplyr)
library(tidyr)
library(lubridate)

date_from <- "2005-07-01"
date_to <- "2018-08-15"
date_strike <- "2019-01-18"

prices <- tq_get("WFC", get = "stock.prices", from=date_from, to=date_to)

rets <- prices %>%
  tq_transmute(select     = adjusted, 
               mutate_fun = periodReturn, 
               period     = "daily", 
               col_rename = "ret")

start_price <- as.numeric(prices$adjusted[nrow(prices)])

trials <- 100000
days <- ((as.numeric(days(ymd(date_strike) - ymd(date_to))) / 60 / 60 / 24) / 365) * 252
days <- round(days, digits = 0)

mu <- mean(rets$ret)
sigma <- sd(rets$ret)
annual_vol <- sigma * sqrt(251)

f <- regkienerLX(rets$ret, model = "K1")

ret_paths <- matrix(nrow = days, ncol = trials)

# dist1 <- data.frame(dist = "Emp", rets = rkiener4(3075, m = f$coefk4["m"],
#                                                          g = f$coefk4["g"],
#                                                          k = f$coefk4["k"],
#                                                          e = f$coefk4["e"]))
# 
# gauss <- data.frame(dist = "Gauss", rets = rnorm(3075, mean = mu, sd = sigma))
# 
# # visualize the fit distribution
# ggplot() +
#   geom_histogram(bins = 100, data = dist1, aes(x = `rets`), fill = 'red', alpha = 0.25) +
#   geom_histogram(bins = 100, data = rets, aes(x = `ret`), fill = 'green', alpha = 0.6) +
#   geom_histogram(bins = 100, data = gauss, aes(x = `rets`), fill = 'blue', alpha = 0.9) +
#   xlim(-.15,-.02) + ylim(0,10)

for(i in c(1:days)){
  # ret_paths[i,] <- rkiener1(trials,
  #                           m = 0,
  #                           g = f$coefk1["g"],
  #                           k = 5.25)
  ret_paths[i,] <- rnorm(trials, mean = 0, sd = sigma)
}

df_ret <- data.frame(ret = ret_paths)

# calc possible price paths
df_tidy <- df_ret %>%
  gather() %>%
  group_by(key) %>%
  mutate(path = start_price*((1 + cumsum(`value`)))) %>%
  mutate(day = c(1:days))

min(df_tidy$value)
max(df_tidy$value)
mean(df_tidy$value)

strike <- 35
discount <- ((1 + 0.02) ^ (days/252))

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

ret_labels <- unique(df_tidy$key)

ggplot(data = df_tidy %>% filter(`key` %in% ret_labels[1:50])) +
  geom_line(aes(x = `day`, y = `path`, colour = `key`))
  
