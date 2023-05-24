{
library(tidyverse)
library(moments)
library(TTR)
  library(lubridate)
  library(tsibble)
  library(zoo)
  library(moments)
}


strategy_performance <- function(returns, dates=NULL, period=252) {
  # if(any(is.na(returns)))
  #   stop("strategy_performance: NAs are not allowed, replace them with zeros") 
  if(!is.null(dates) & class(dates) != "Date")
    stop(paste("strategy_performance: dates should be of class Date"))
  if(class(returns) != "numeric")
    stop(paste("strategy_performance: returns should be of class numeric"))
  if(is.null(dates)) 
    dates <- seq(as.Date("1970/01/01"), by = "day", length.out = length(returns))
  df <- data.frame(Dates=dates, Returns=returns) %>% arrange(Dates)
  annual_returns <- group_by(df, year(Dates)) %>% summarise(Dates=first(year(Dates)), Returns = sum(Returns, na.rm=TRUE)) %>% pull(Returns)
  monthly_returns <- group_by(df, yearmonth(Dates)) %>% summarise(Dates=first(yearmonth(Dates)), Returns = sum(Returns, na.rm=TRUE)) %>% pull(Returns)
  mean_ann_ret <- mean(annual_returns, na.rm=TRUE) * 100
  ann_sd <- sd(df$Returns, na.rm=TRUE) * sqrt(period) * 100
  sr <- mean(df$Returns, na.rm=TRUE) / sd(df$Returns, na.rm=TRUE) * sqrt(period)
  skew <- skewness(monthly_returns, na.rm=TRUE)
  q <- quantile(df$Returns[df$Returns!=0], probs=c(0.01, 0.3, 0.7, 0.99), na.rm=TRUE)
  lower_tail <- as.numeric(q[1] / q[2] / 4.43)
  upper_tail <- as.numeric(q[4] / q[3] / 4.43)
  cum_returns = cumsum(replace(df$Returns, is.na(df$Returns), 0)) 
  peak = cummax(cum_returns)
  drawdown = peak - cum_returns
  max_drawdown <- -(exp(drawdown[which.max(drawdown)])-1)*100
  avg_drawdown <- -(exp(mean(drawdown))-1)*100
  gpr <-  sum(df$Returns, na.rm=TRUE) / sum(abs(df$Returns[df$Returns<0]), na.rm=TRUE)
  cum_annual_returns = cumsum(replace(annual_returns, is.na(annual_returns), 0)) 
  r2 <- summary(lm(1:length(cum_annual_returns) ~ 0+cum_annual_returns))$adj.r.squared
  #turnover <- round(length(rle(as.vector(na.omit(sign(returns))))$length) / period, 1)
  results <- list("Mean annual return"=mean_ann_ret, 
                  "Annualized standard deviation"=ann_sd, 
                  "Sharpe ratio"=sr, 
                  "Skew"=skew, 
                  "Lower tail"=lower_tail, 
                  "Upper tail"=upper_tail, 
                  "Max drawdown"=max_drawdown, 
                  "Average drawdown"=avg_drawdown,
                  "GPR"=gpr,
                  "R2"=r2
                  )
  return(lapply(results, round, 2))
}

portfolio_summary <- function(portfolio, dates=NULL, period=252, benchmark.dates=NULL, benchmark.returns=NULL, plot_stats=FALSE, symbol_wise=FALSE) {
  # if(any(is.na(portfolio)))
  #   stop("portfolio_summary: NAs are not allowed, replace them with zeros") 
  if(!is.null(dates) & class(dates) != "Date")
    stop(paste("portfolio_summary: dates should be of class Date"))
  if(class(portfolio) != "matrix")
    stop(paste("portfolio_summary: portfolio should be of class matrix"))
  if(is.null(dates)) 
    dates <- seq(as.Date("1970/01/01"), by = "day", length.out = length(returns))
  returns <- rowMeans(portfolio, na.rm=TRUE)
  results <- strategy_performance(returns, dates = dates, period=period)
  alpha <- NA
  beta <- NA
  if(!is.null(benchmark.dates)) {
    benchmark <- data.frame(Dates=benchmark.dates, Returns=benchmark.returns)
    df <- data.frame(Dates=dates, Returns=returns)
    z <- merge(benchmark, df, by="Dates") %>% na.omit
    z <- group_by(z, yearmonth(Dates)) %>% summarise(X=sum(Returns.x, na.rm=TRUE), Y=sum(Returns.y, na.rm=TRUE))
    fit <- (lm(Y ~ X, z))
    alpha <- as.numeric(coef(fit)[1]) * 100 * 12
    beta <- as.numeric(coef(fit)[2]) 
    results[["alpha"]] <- round(alpha, 2)
    results[["beta"]] <- round(beta, 2)
    results[["correlation"]] <- round(cor(z$X, z$Y, use = "pairwise.complete.obs"), 2)
  }
  if(plot_stats) {
    par(mfrow=c(2,1), mar=c(2,4,1,0.5))
    returns <- replace(returns, is.na(returns), 0) * 100
    cum_returns <- cumsum(returns)
    plot(sort(as.Date(dates)), cum_returns, ylab="Equity curve %")
    chunks <- group_by(data.frame(Date=as.Date(dates), ret=returns), year(Date)) %>% 
      summarise(sum=round(sum(ret),1), first=first(Date), .groups = 'drop')
    abline(v=chunks$first, lty=2, lwd=0.5)
    text(x=chunks$first+period/2, y=max(cum_returns), labels=chunks$sum, cex = 0.75)
    matplot(apply(portfolio, 2, function(x) cumsum(replace(x, is.na(x), 0))  ), type = "l", lwd=2, lty=1, ylab="Assets curves %", xaxt="n")
    abline(h=0, lwd=2)
    axis(side = 1, labels=dates, at=seq(1, length(dates)), tick = FALSE)
  }
  symbol_result <- NULL
  if(symbol_wise) {
    symbol_result <- apply(portfolio, 2, function(x) unlist(strategy_performance(x, dates = dates))) %>% t
  }

  return(list(Aggregate=results, Symbols=symbol_result))
}


merge_portfolio_list <- function(portfolio_list) {
  full_df <- Reduce(function(...) full_join(..., by="Date", all=TRUE, incomparables = NA), portfolio_list) %>% arrange(Date)
  colnames(full_df) <- c("Date", names(portfolio_list))
  #full_df[is.na(full_df)] <- 0
  return(full_df)
}

# in percentages
calculate_volatility <- function(returns, long_span=252, short_span=35,  weights=c(0.3, 0.7), period=252){
  vol_short <- sqrt(EMA(replace(returns, is.na(returns), 0)^2, short_span))
  vol_long <- runMean(vol_short, long_span)
  vol <-  (weights[1] * vol_long + weights[2] * vol_short) * sqrt(period) # one year instead of ten
  return(vol)
}
  

Futures <- read_rds("/home/marco/trading/Historical Data/Barchart/Futures.RDS")
target_vol <- 0.2

## Stategy 1
{SP500 <- backadjust_future(Futures[["ES"]], N = 5)
SP500$Return[is.na(SP500$Return)] <- 0
ggplot(SP500) + geom_line(aes(Date, Nearest)) + geom_line(aes(Date, Backadjusted), color="gray")
with(SP500, matplot2(cbind(-Nearest+ Backadjusted)))
with(SP500, plot(Date,cumsum(Return*100)))
summary_statistics(SP500$Date, SP500$Return) %>% unlist
ZN <- backadjust_future(Futures[["ZN"]], N = 5)
ZN$Return[is.na(ZN$Return)] <- 0
ggplot(ZN) + geom_line(aes(Date, Nearest)) + geom_line(aes(Date, Backadjusted), color="gray")
with(ZN, matplot2(cbind(-Nearest+ Backadjusted)))
with(ZN, plot(Date,cumsum(Return*100)))
summary_statistics(ZN$Date, ZN$Return) %>% unlist
}
## Stategy 2 (not very useful)
{ºSP500 <- backadjust_future(Futures[["ES"]], N = 5)
SP500$Return[is.na(SP500$Return)] <- 0
SP500$Nearest <- na.locf(SP500$Nearest, na.rm = FALSE )
SP500$Position = 0.20 / 0.16
SP500$Position[is.na(SP500$Position)] <- 0
SP500$Excess <- SP500$Return * SP500$Position 
SP500$Excess[is.na(SP500$Excess)] <- 0
with(SP500, plot(Date,cumsum(Excess)))
summary_statistics(SP500$Date, SP500$Excess) %>% unlist
}
## Stategy 3
{
SP500 <- backadjust_future(Futures[["ES"]], N = 2)
SP500$Return[is.na(SP500$Return)] <- 0
SP500$Nearest <- na.locf(SP500$Nearest, na.rm = FALSE )
ema <- sqrt(EMA(SP500$Return^2, 32))
SP500$Risk <-  (0.3 * runMean(ema, 252) + 0.7 * ema) * 16 # one year instead of ten
SP500$Position = 0.20 / SP500$Risk
SP500$Position[is.na(SP500$Position)] <- 0
SP500$Excess <- SP500$Return * SP500$Position 
with(SP500, plot(Date,cumsum(Excess)))
strategy_performance(SP500$Excess,SP500$Date) %>% unlist
}

## Stategy 4 (benchmark)
{
# Risk parity
{
SP500 <- backadjust_future(Futures[["ES"]], N = 5)
SP500$Return[is.na(SP500$Return)] <- 0
ema <- sqrt(EMA(SP500$Return^2, 32))
SP500$Risk <-  (0.3 * runMean(ema, 252) + 0.7 * ema) * 16 # one year instead of ten
SP500$Position = 1 * 0.20 / SP500$Risk
SP500 <- na.omit(SP500)
SP500$Excess <- SP500$Return * SP500$Position 
summary_statistics(SP500$Date, SP500$Excess) %>% unlist
ZN <- backadjust_future(Futures[["ZN"]], N = 5)
ZN$Return[is.na(ZN$Return)] <- 0
ema <- sqrt(EMA(ZN$Return^2, 32))
ZN$Risk <-  (0.3 * runMean(ema, 252) + 0.7 * ema) * 16 # one year instead of ten
ZN$Position = 1 * 0.20 / ZN$Risk
ZN <- na.omit(ZN)
ZN$Excess <- ZN$Return * ZN$Position 
summary_statistics(ZN$Date, ZN$Excess) %>% unlist
RP <- merge(SP500, ZN, by="Date")
RP$Excess.z <- (RP$Excess.x+RP$Excess.y) / 2 * 1.32
with(RP, matplot2(cbind(cumsum(Excess.x), cumsum(Excess.y), cumsum((RP$Excess.z)))))
summary_statistics(RP$Date, RP$Excess.z) %>% unlist
}
# All weather
{
N <- 2
equity <- backadjust_future(Futures[["VI"]], N = N)
bond_long <- backadjust_future(Futures[["ZB"]], N = N)
bond_short <- backadjust_future(Futures[["G"]], N = N)
energy <- backadjust_future(Futures[["RB"]], N = N)
soft <- backadjust_future(Futures[["SW"]], N = N)
gold <- backadjust_future(Futures[["GC"]], N = N)
assets <- list(equity=equity, bond_long=bond_long, bond_short=bond_short, energy=energy, soft=soft, gold=gold)
assets$equity$Return <- -assets$equity$Return
for(n in names(assets)) {
  x <- assets[[n]]
  x$Return[is.na(x$Return)] <- 0
  ema <- sqrt(EMA(x$Return^2, 32))
  x$Risk <-  (0.3 * runMean(ema, 252) + 0.7 * ema) * 16 # one year instead of ten
  x$Position = 1 * 0.20 / x$Risk
  x$Excess <- x$Return * x$Position 
  assets[[n]] <- select(x, Date, Excess)
}
portfolio <- merge_portfolio_list(assets)
portfolio_summary(as.matrix(portfolio[,-1]), dates = portfolio$Date, plot_stats = TRUE) %>% unlist
}
# Jumbo
{
jumbo <- list()
IDM = 2.47
for(n in names(BackAdj)) {
  df <- BackAdj[[n]]
  df$Position = lag(target_vol / calculate_volatility(df$Return))
  df$Excess <- df$Return * df$Position * IDM
  jumbo[[n]] <- select(df, Date, Excess)
}
portfolio <- merge_portfolio_list(jumbo)
res <- portfolio_summary(as.matrix(portfolio[,-1]), dates = portfolio$Date, plot_stats = TRUE, symbol_wise = FALSE, benchmark.dates = benchmark.dates, benchmark.returns = benchmark.returns) 
print(res$Aggregate %>% unlist)
benchmark.dates <- portfolio[,1] 
benchmark.returns <- portfolio[,-1] %>% rowMeans()
}
}
## Stategy 5
{
# SP only
{
SP500 <- backadjust_future(Futures[["ES"]], N = 5)
SP500$Return[is.na(SP500$Return)] <- 0
ema <- sqrt(EMA(SP500$Return^2, 32))
SP500$Risk <-  (0.3 * runMean(ema, 252) + 0.7 * ema) * 16 # one year instead of ten
SP500$Position = 0.20 / SP500$Risk
SP500$Trend <- EMA(SP500$Backadjusted, 64) -  EMA(SP500$Backadjusted, 256)
SP500$Trade <- lag(ifelse(SP500$Trend > 0, 1, 0))
SP500 <- na.omit(SP500)
SP500$Excess <- SP500$Return * SP500$Position * SP500$Trade
with(SP500, plot(Date,cumsum(Excess)))
summary_statistics(SP500$Date, SP500$Excess) %>% unlist
}
# Jumbo
{
jumbo <- list()
IDM = 2.47
for(n in names(BackAdj)) {
  df <- BackAdj[[n]]
  df$Position = lag(target_vol / calculate_volatility(df$Return))
  df$Trend <- EMA(df$Backadjusted, 64) -  EMA(df$Backadjusted, 256)
  df$Trade <- lag(ifelse(df$Trend > 0, 1, 0))
  df$Excess <- df$Return * df$Position * df$Trade * IDM
  jumbo[[n]] <- select(df, Date, Excess)
}
portfolio <- merge_portfolio_list(jumbo)
res <- portfolio_summary(as.matrix(portfolio[,-1]), dates = portfolio$Date, plot_stats = TRUE, symbol_wise = TRUE) 
print(res$Aggregate %>% unlist)
}
}
## Stategy 6
# Jumbo
{
jumbo <- list()
IDM = 1#2.41
for(n in names(BackAdj)) {
  df <- BackAdj[[n]]
  df$Position = lag(target_vol / calculate_volatility(df$Return))
  df$Trend <- EMA(df$Backadjusted, 64) -  EMA(df$Backadjusted, 256)
  #df$Trend <- AbsoluteStrength(df$Backadjusted, 250)
  df$Trade <- lag(ifelse(df$Trend > 0, 1, -1))
  df$Excess <- df$Return * df$Position * df$Trade * IDM
  jumbo[[n]] <- select(df, Date, Excess)
}
portfolio <- merge_portfolio_list(jumbo)
res <- portfolio_summary(as.matrix(portfolio[,-1]), dates = portfolio$Date, plot_stats = TRUE, symbol_wise = TRUE) 
print(res$Aggregate %>% unlist)
}

## Stategy 7
# Jumbo
{
jumbo <- list()
IDM = 1 #2.41
for(n in names(BackAdj)) {
  df <- BackAdj[[n]]
  df$Risk = calculate_volatility(df$Return)
  df$Position = lag(target_vol / df$Risk)
  df$Trend <- EMA(df$Backadjusted, 64) -  EMA(df$Backadjusted, 256)
  df$Forecast <- df$Trend / (df$Nearest * df$Risk / 16) * 1.9
  # df$Trend <- AbsoluteStrength(df$Backadjusted, 250)
  df$Trade <- lag(ifelse(df$Forecast > 20, 20, ifelse(df$Forecast < -20, -20, df$Forecast )) / 10)
  df$Excess <- df$Return * df$Position * df$Trade * IDM
  jumbo[[n]] <- select(df, Date, Excess)
}
portfolio <- merge_portfolio_list(jumbo)
res <- portfolio_summary(as.matrix(portfolio[,-1]), dates = portfolio$Date, plot_stats = TRUE, symbol_wise = TRUE) 
print(res$Aggregate %>% unlist)
}

## Stategy 8
# Jumbo
{
jumbo <- list()
IDM = 1
for(n in names(BackAdj)) {
  df <- BackAdj[[n]]
  df$Risk = calculate_volatility(df$Return)
  df$Position = lag(target_vol / df$Risk)
  df$Trend <- EMA(df$Backadjusted, 16) -  EMA(df$Backadjusted, 64)
  df$Forecast <- df$Trend / (df$Nearest * df$Risk / 16) * 1.9
  # df$Trend <- AbsoluteStrength(df$Backadjusted, 100)
  # df$Forecast <- df$Trend / (df$Nearest * df$Risk / 16) * 50
  df$Trade <- lag(ifelse(df$Forecast > 20, 20, ifelse(df$Forecast < -20, -20, df$Forecast )) / 10)
  df$Excess <- df$Return * df$Position * df$Trade * IDM
  jumbo[[n]] <- select(df, Date, Excess)
}
portfolio <- merge_portfolio_list(jumbo)
res <- portfolio_summary(as.matrix(portfolio[,-1]), dates = portfolio$Date, plot_stats = TRUE, symbol_wise = TRUE) 
print(res$Aggregate %>% unlist)
}

## Stategy 9
multiple_ema <- function(df) {
  df$Trend2 <- EMA(df$Backadjusted, 2) -  EMA(df$Backadjusted, 8)
  df$Trend4 <- EMA(df$Backadjusted, 4) -  EMA(df$Backadjusted, 16)
  df$Trend8 <- EMA(df$Backadjusted, 8) -  EMA(df$Backadjusted, 32)
  df$Trend16 <- EMA(df$Backadjusted, 16) -  EMA(df$Backadjusted, 64)
  df$Trend32<- EMA(df$Backadjusted, 32) -  EMA(df$Backadjusted, 128)
  df$Trend64 <- EMA(df$Backadjusted, 64) -  EMA(df$Backadjusted, 256)
  df$Forecast2 <- df$Trend2 / (df$Nearest * df$Risk / 16) * 12.1
  df$Forecast4 <- df$Trend4 / (df$Nearest * df$Risk / 16) * 8.53
  df$Forecast8 <- df$Trend8 / (df$Nearest * df$Risk / 16) * 5.95
  df$Forecast16 <- df$Trend16 / (df$Nearest * df$Risk / 16) * 4.1
  df$Forecast32 <- df$Trend32 / (df$Nearest * df$Risk / 16) * 2.79
  df$Forecast64 <- df$Trend64 / (df$Nearest * df$Risk / 16) * 1.91
  df$Trade2 <- ifelse(df$Forecast2 > 20, 20, ifelse(df$Forecast2 < -20, -20, df$Forecast2 ))
  df$Trade4 <- ifelse(df$Forecast4 > 20, 20, ifelse(df$Forecast4 < -20, -20, df$Forecast4 ))
  df$Trade8 <- ifelse(df$Forecast8 > 20, 20, ifelse(df$Forecast8 < -20, -20, df$Forecast8 ))
  df$Trade16 <- ifelse(df$Forecast16 > 20, 20, ifelse(df$Forecast16 < -20, -20, df$Forecast16 ))
  df$Trade32 <- ifelse(df$Forecast32 > 20, 20, ifelse(df$Forecast16 < -20, -20, df$Forecast16 ))
  df$Trade64 <- ifelse(df$Forecast64 > 20, 20, ifelse(df$Forecast64 < -20, -20, df$Forecast64 )) 
  df$Trade <- rowMeans(cbind(df$Trade2 , df$Trade4 , df$Trade8 , df$Trade16 , df$Trade32 , df$Trade64 ))
  return(df)
}
multiple_as <- function(df) {
  df$Trend2 <- AbsoluteStrength(df$Backadjusted, 5)
  df$Trend4 <- AbsoluteStrength(df$Backadjusted, 25)
  df$Trend8 <- AbsoluteStrength(df$Backadjusted, 100)
  df$Trend16 <- AbsoluteStrength(df$Backadjusted, 175)
  df$Trend32 <- AbsoluteStrength(df$Backadjusted, 250)
  df$Trend64 <- AbsoluteStrength(df$Backadjusted, 500)
  df$Forecast2 <- df$Trend2 / (df$Nearest * df$Risk / 16) * 13.5
  df$Forecast4 <- df$Trend4 / (df$Nearest * df$Risk / 16) * 30
  df$Forecast8 <- df$Trend8 / (df$Nearest * df$Risk / 16) * 60
  df$Forecast16 <- df$Trend16 / (df$Nearest * df$Risk / 16) * 80
  df$Forecast32 <- df$Trend32 / (df$Nearest * df$Risk / 16) * 95
  df$Forecast64 <- df$Trend64 / (df$Nearest * df$Risk / 16) * 135
  df$Trade2 <- ifelse(df$Forecast2 > 20, 20, ifelse(df$Forecast2 < -20, -20, df$Forecast2 ))
  df$Trade4 <- ifelse(df$Forecast4 > 20, 20, ifelse(df$Forecast4 < -20, -20, df$Forecast4 ))
  df$Trade8 <- ifelse(df$Forecast8 > 20, 20, ifelse(df$Forecast8 < -20, -20, df$Forecast8 ))
  df$Trade16 <- ifelse(df$Forecast16 > 20, 20, ifelse(df$Forecast16 < -20, -20, df$Forecast16 ))
  df$Trade32 <- ifelse(df$Forecast32 > 20, 20, ifelse(df$Forecast16 < -20, -20, df$Forecast16 ))
  df$Trade64 <- ifelse(df$Forecast64 > 20, 20, ifelse(df$Forecast64 < -20, -20, df$Forecast64 )) 
  df$Trade <- rowMeans(cbind(df$Trade2 , df$Trade4 , df$Trade8 , df$Trade16 , df$Trade32 , df$Trade64 ))
  return(df)
}
multiple_dc <- function(df) {
  dc1 <- TTR::DonchianChannel(df$Backadjusted, n=5) 
  dc2 <- TTR::DonchianChannel(df$Backadjusted, n=21) 
  dc3 <- TTR::DonchianChannel(df$Backadjusted, n=63) 
  dc4 <- TTR::DonchianChannel(df$Backadjusted, n=252) 
  dc5 <- TTR::DonchianChannel(df$Backadjusted, n=504) 
  df$Forecast1 <- (df$Backadjusted - dc1[,2]) / abs(dc1[,1] - dc1[,3])
  df$Forecast2 <- (df$Backadjusted - dc2[,2]) / abs(dc2[,1] - dc2[,3])
  df$Forecast3 <- (df$Backadjusted - dc3[,2]) / abs(dc3[,1] - dc3[,3])
  df$Forecast4 <- (df$Backadjusted - dc4[,2]) / abs(dc4[,1] - dc4[,3])
  df$Forecast5 <- (df$Backadjusted - dc5[,2]) / abs(dc5[,1] - dc5[,3])
  df$Trade <- rowMeans(cbind(df$Forecast1 , df$Forecast2 , df$Forecast3 , df$Forecast4 , df$Forecast5 )) * 40
  return(df)
}

multiple_rsi <- function(df) {
df$Forecast1 <- ((RSI(df$Backadjusted, 5, maType = EMA)-50)/2.5)
df$Forecast2 <- ((RSI(df$Backadjusted, 21, maType = EMA)-50)/2.5)
df$Forecast3 <- ((RSI(df$Backadjusted, 63, maType = EMA)-50)/2.5)
df$Forecast4 <- ((RSI(df$Backadjusted, 252, maType = EMA)-50)/2.5)
df$Forecast5 <- ((RSI(df$Backadjusted, 504, maType = EMA)-50)/2.5)
df$Trade <- rowMeans(cbind(df$Forecast1 , df$Forecast2 , df$Forecast3 , df$Forecast4 , df$Forecast5 ))
return(df)
}
# Jumbo
{
  jumbo <- list()
  IDM = 2.41
  FDM <- 1.39
  for(n in names(Symbols)) {
    df <- Symbols[[n]]
    df$Risk = calculate_volatility(df$Return)
    df$Position = lag(target_vol / df$Risk)
    df <- multiple_ema(df)
    #df <- multiple_as(df)
    #df <- multiple_dc(df)
    #df <- multiple_rsi(df)
    df$Trade <- df$Trade  * FDM 
    df$Trade <- lag(ifelse(df$Trade > 20, 20, ifelse(df$Trade < -20, -20, df$Trade )) / 10)
    df$Excess <- df$Return * df$Position * df$Trade * IDM 
    jumbo[[n]] <- select(df, Date, Excess)
  }
  portfolio <- merge_portfolio_list(jumbo)
  res <- portfolio_summary(as.matrix(portfolio[,-1]), dates = portfolio$Date, plot_stats = TRUE, symbol_wise = FALSE, benchmark.dates = benchmark.dates, benchmark.returns = benchmark.returns) 
  print(res$Aggregate %>% unlist)
  # > cor(cbind(ema, as, dc, rsi), use="pairwise.complete.obs")
  # ema        as        dc       rsi
  # ema 1.0000000 0.9624574 0.9182595 0.8290662
  # as  0.9624574 1.0000000 0.9505449 0.8494622
  # dc  0.9182595 0.9505449 1.0000000 0.9370206
  # rsi 0.8290662 0.8494622 0.9370206 1.0000000
}

multiple_carry <- function(df, scalar=30) {
  df$Carry <- (df$Basis / (df$Basis_distance / 12)) / ( df$Risk )
  df$Carry <- na.locf(df$Carry, na.rm=FALSE)
  df$Carry5 <- EMA(df$Carry, 5)
  df$Carry20 <- EMA(df$Carry, 20)
  df$Carry60 <- EMA(df$Carry, 60)
  df$Carry120 <- EMA(df$Carry, 120)
  df$Forecast5 <- df$Carry5 * scalar
  df$Forecast20 <- df$Carry20 * scalar
  df$Forecast60 <- df$Carry60 * scalar
  df$Forecast120 <- df$Carry120 * scalar
  df$Trade5 <- ifelse(df$Forecast5 > 20, 20, ifelse(df$Forecast5 < -20, -20, df$Forecast5 ))
  df$Trade20 <- ifelse(df$Forecast20 > 20, 20, ifelse(df$Forecast20 < -20, -20, df$Forecast20 ))
  df$Trade60 <- ifelse(df$Forecast60 > 20, 20, ifelse(df$Forecast60 < -20, -20, df$Forecast60 ))
  df$Trade120 <- ifelse(df$Forecast120 > 20, 20, ifelse(df$Forecast120 < -20, -20, df$Forecast120 ))
  df$Trade <- rowMeans(cbind(df$Trade5 , df$Trade20 , df$Trade60 , df$Trade120  ))
  return(df)
}
# Strategy 10
{
jumbo <- list()
IDM = 2.41
FDM <- 1.04
tt <- list()
for(n in names(BackAdj)) {
  df <- BackAdj[[n]]
  df$Risk = calculate_volatility(df$Return)
  df$Position = lag(target_vol / df$Risk)
  df <- multiple_carry(df, scalar=30)
  df$Trade <- df$Trade * FDM
  df$Trade <- lag(ifelse(df$Trade > 20, 20, ifelse(df$Trade < -20, -20, df$Trade ))) / 10
  tt[[n]] <- df$Trade
  df$Excess <- df$Return * df$Position * df$Trade * IDM 
  jumbo[[n]] <- select(df, Date, Excess)
}
portfolio <- merge_portfolio_list(jumbo)
res <- portfolio_summary(as.matrix(portfolio[,-1]), dates = portfolio$Date, plot_stats = TRUE, symbol_wise = FALSE) 
print(res$Aggregate %>% unlist)
}

# Strategy 11
{
  jumbo <- list()
  IDM = 2.41
  FDM <- 1.35 
  tt <- list()
  for(n in names(BackAdj)) {
    df <- BackAdj[[n]]
    df$Risk = calculate_volatility(df$Return)
    df$Position = lag(target_vol / df$Risk)
    df <- multiple_ema(df)
    df$ForecastTrend <- df$Trade
    df <- multiple_carry(df, scalar=30)
    df$ForecastCarry <- df$Trade
    df$Trade <- (0.6 * df$ForecastTrend + 0.4 * df$ForecastCarry) * FDM
    df$Trade <- lag(ifelse(df$Trade > 20, 20, ifelse(df$Trade < -20, -20, df$Trade ))) / 10
    tt[[n]] <- df$Trade
    df$Excess <- df$Return * df$Position * df$Trade * IDM 
    jumbo[[n]] <- select(df, Date, Excess)
  }
  portfolio <- merge_portfolio_list(jumbo)
  res <- portfolio_summary(as.matrix(portfolio[,-1]), dates = portfolio$Date, plot_stats = TRUE, symbol_wise = FALSE, benchmark.dates = benchmark.dates, benchmark.returns = benchmark.returns) 
  print(res$Aggregate %>% unlist)
}

# Strategy 12
{
  # Figure 46
  # I don't observe the same mean-reverting phenomenon at EMWA2
  jumbo <- list()
  f <- 2
  hs <- c(98.5, 50.2, 25.4, 13.4, 7.6, 5.2)
  h <- hs[1]
  h <- ceiling(252/h)
  scalar <- 12.1
  for(n in names(BackAdj)) {
    df <- BackAdj[[n]]
    df$Return[is.na(df$Return)] <- 0
    df$Risk = calculate_volatility(df$Return)
    df$Trend <- EMA(df$Backadjusted, f) -  EMA(df$Backadjusted, f*4)
    df$Forecast <- df$Trend / (df$Nearest * df$Risk / 16) * scalar
    #df$Forecast <- ifelse(df$Forecast > 20, 20, ifelse(df$Forecast < -20, -20, df$Forecast )) 
    for(i in 1:nrow(df)) {
      df$FR[i] <- df$Forecast[i]
      df$NR[i] <- mean(df$Return[(i+1):(i+1+h)], na.rm=TRUE) / (df$Risk[i] / 16) 
    }
    jumbo[[n]] <- select(df, FR, NR)
  }
  res <- do.call(rbind, jumbo)
  a <- group_by(res %>% filter(between(FR, -30, 30)), bin=cut(FR, 15)) %>% summarize(M=median(NR, na.rm=TRUE), S=2*(sd(NR, na.rm=TRUE))/sqrt(n())) 
  ggplot(a)+geom_errorbar(aes(bin, ymin=M-S, ymax=M+S))+geom_hline(yintercept = 0)
   
}

# Strategy 13
{
  # Figure 54
  df <- BackAdj[["ES"]]
  df$Risk = calculate_volatility(df$Return)
  df$RV <- df$Risk / runMean(df$Risk, 256)
  plot(df$Date, df$RV)
  # Figure 55
  RVs <- list()
  for(n in names(BackAdj)) {
    df <- BackAdj[[n]]
    df$Risk = calculate_volatility(df$Return)
    RVs[[n]] <- df$Risk / runMean(df$Risk, 256)
  }
  RVs %>% unlist %>% hist(xlim = c(0,5), 100)
  # jumbo
  {
    jumbo <- list()
    IDM = 2.41
    tt <- list()
    for(n in names(BackAdj)) {
      df <- BackAdj[[n]]
      df$Risk = calculate_volatility(df$Return)
      df$RV <- df$Risk / runMean(df$Risk, 256)
      Q <- sapply(1:length(df$RV), function(i) sum(df$RV[i] > df$RV[1:i], na.rm=TRUE) / i)
      M <- EMA(2-1.5*Q)
      df$Position = lag(target_vol / df$Risk)
      df <- multiple_ema(df)
      df$ForecastTrend <- df$Trade * M
      df <- multiple_carry(df, scalar=30)
      df$ForecastCarry <- df$Trade * M
      df$Trade <- (0.5 * df$ForecastTrend +  0.5 * df$ForecastCarry) 
      df$Trade <- lag(ifelse(df$Trade > 20, 20, ifelse(df$Trade < -20, -20, df$Trade ))) / 10
      tt[[n]] <- df$Trade
      df$Excess <- df$Return * df$Position * df$Trade * IDM 
      jumbo[[n]] <- select(df, Date, Excess)
    }
    portfolio <- merge_portfolio_list(jumbo)
    res <- portfolio_summary(as.matrix(portfolio[,-1]), dates = portfolio$Date, plot_stats = TRUE, symbol_wise = FALSE, benchmark.dates = benchmark.dates, benchmark.returns = benchmark.returns) 
    print(res$Aggregate %>% unlist)
  }
  
  
}


# Strategy 14
{
  # figure 58
  df <- BackAdj[["ZN"]]
  df$Carry <- (df$Basis_price / (df$Basis_distance / 12)) 
  df$Carry_accrued <- df$Carry * 0.00391
  df$Spot <- df$Backadjusted[1]
  for(i in 2:nrow(df)) {
    df$Spot[i] <- df$Spot[i-1] + df$Backadjusted[i] - df$Backadjusted[i-1] - df$Carry_accrued[i]
    if(is.na(df$Spot[i]))
      df$Spot[i] <- df$Spot[i-1]
  }
  df$Carry_cum_return <- cumsum(replace(df$Carry_accrued, is.na(df$Carry_accrued), 0)) + df$Backadjusted[1]
  with(df, matplot2(cbind(Carry_cum_return, Backadjusted, Spot)))  
  
}


# Strategy 15
# nothing worth testing here, but interesting for the future model

# Strategy 16
# nothing worth testing here

# Strategy 17
{
  # Figure 69
  df <- BackAdj[["ES"]]
  df$Risk = calculate_volatility(df$Difference)
  df$NP <- 0
  for(i in 2:nrow(df)) {
    df$NP[i] <-  (100 * (df$Backadjusted[i] - df$Backadjusted[i-1]) / (df$Risk[i] )) + df$NP[i-1]
    if(is.na(df$NP[i]))
      df$NP[i] <- df$NP[i-1]
  }
  with(df, matplot2(cbind(Backadjusted, NP*3)))
  # jumbo
  {
    jumbo <- list()
    IDM = 2.41
    FDM <- 1.26
    for(n in names(BackAdj)) {
      df <- BackAdj[[n]]
      df$Risk = calculate_volatility(df$Return)
      df$Position = lag(target_vol / df$Risk)
      df$NP <- 0
      for(i in 2:nrow(df)) {
        df$NP[i] <-  (100 * (df$Backadjusted[i] - df$Backadjusted[i-1]) / (df$Risk[i] * df$Nearest[i] / 16)) + df$NP[i-1]
        if(is.na(df$NP[i]))
          df$NP[i] <- df$NP[i-1]
      }
      df$Backadjusted <- df$NP # we are replace backadj with the normalized price here
      df <- multiple_ema(df)
      df$Trade <- df$Trade * FDM
      df$Trade <- lag(ifelse(df$Trade > 20, 20, ifelse(df$Trade < -20, -20, df$Trade )) / 10)
      df$Excess <- df$Return * df$Position * df$Trade * IDM 
      jumbo[[n]] <- select(df, Date, Excess)
    }
    portfolio <- merge_portfolio_list(jumbo)
    res <- portfolio_summary(as.matrix(portfolio[,-1]), dates = portfolio$Date, plot_stats = TRUE, symbol_wise = FALSE, benchmark.dates = benchmark.dates, benchmark.returns = benchmark.returns) 
    print(res$Aggregate %>% unlist)
  }
  
  
}

# Strategy 18
{
  # Figures 70 and 71
  {
    NPs <- list()
    for(n in names(BackAdj)) {
      df <- BackAdj[[n]]
      df$Risk = calculate_volatility(df$Return)
      df$NP <- 0
      for(i in 2:nrow(df)) {
        df$NP[i] <-  (100 * (df$Backadjusted[i] - df$Backadjusted[i-1]) / (df$Risk[i] * df$Nearest[i] / 16)) + df$NP[i-1]
        if(is.na(df$NP[i]))
          df$NP[i] <- df$NP[i-1]
      }
      df$dNP = c(0, diff(df$NP))
      NPs[[n]] <- select(df, Date, Symbol, Class, NP, dNP)
    }
    allNPs <- do.call(rbind, NPs)
    R <- group_by(allNPs, Class, Date) %>% summarise(Symbol=Symbol, R=mean(dNP)) %>% arrange(Class, Date) %>% select(-Symbol)  %>%  unique %>% group_by( Class) %>% mutate(A=cumsum(R)) %>% ungroup()    
    ggplot(R) + geom_line(aes(Date, A, col=Class), linewidth=4) + scale_color_viridis(discrete = TRUE)
  }
  # jumbo
  {
    jumbo <- list()
    IDM = 2.41
    FDM <- 1.39
    for(n in names(BackAdj)) {
      df <- BackAdj[[n]]
      df$Risk = calculate_volatility(df$Return)
      df$Position = lag(target_vol / df$Risk)
      df <- merge(df, filter(R, Class==df$Class[1]) %>%  select(Date, A), by="Date") # R obtained from before
      df$Backadjusted  <- df$A
      df <- multiple_ema(df)
      df$Trade <- df$Trade * FDM
      df$Trade <- lag(ifelse(df$Trade > 20, 20, ifelse(df$Trade < -20, -20, df$Trade )) / 10)
      df$Excess <- df$Return * df$Position * df$Trade * IDM 
      jumbo[[n]] <- select(df, Date, Excess)
    }
    portfolio <- merge_portfolio_list(jumbo)
    res <- portfolio_summary(as.matrix(portfolio[,-1]), dates = portfolio$Date, plot_stats = TRUE, symbol_wise = FALSE, benchmark.dates = benchmark.dates, benchmark.returns = benchmark.returns) 
    print(res$Aggregate %>% unlist)
  }
}


# Strategy 19
{
  # Figure 72
  df <- BackAdj[["ES"]]
  df$Risk = calculate_volatility(df$Return)
  df$NP <- 0
  for(i in 2:nrow(df)) {
    df$NP[i] <-  (100 * (df$Backadjusted[i] - df$Backadjusted[i-1]) / (df$Risk[i] * df$Nearest[i] / 16)) + df$NP[i-1]
    if(is.na(df$NP[i]))
      df$NP[i] <- df$NP[i-1]
  }
  df <- merge(df, filter(R, Class==df$Class[1]) %>%  select(Date, A), by="Date") # R obtained from before
  df$R  <- df$NP - df$A
  matplot2(cbind(df$NP, df$A, df$R))
  # jumbo (the result is ok, not bad as in the book in table 85)
  {
    jumbo <- list()
    IDM = 2.41 
    FDM <- 1.49
    for(n in names(BackAdj)) {
      df <- BackAdj[[n]]
      df$Risk = calculate_volatility(df$Return)
      df$Position = lag(target_vol / df$Risk)
      df$NP <- 0
      for(i in 2:nrow(df)) {
        df$NP[i] <-  (100 * (df$Backadjusted[i] - df$Backadjusted[i-1]) / (df$Risk[i] * df$Nearest[i] / 16)) + df$NP[i-1]
        if(is.na(df$NP[i]))
          df$NP[i] <- df$NP[i-1]
      }
      df <- merge(df, filter(R, Class==df$Class[1]) %>%  select(Date, A), by="Date") # R obtained from before
      df$R  <- (df$NP - df$A) / 100
      df$O1 <- EMA(c(rep(NA, 5), diff(df$R, lag=5) / 5), 2) * 56.1
      df$O2 <- EMA(c(rep(NA, 10), diff(df$R, lag=10) / 10), 3) * 79.0
      df$O3 <- EMA(c(rep(NA, 20), diff(df$R, lag=20) / 20), 5) * 108.5
      df$O4 <- EMA(c(rep(NA, 40), diff(df$R, lag=40) / 40), 10) * 153.5
      df$O5 <- EMA(c(rep(NA, 160), diff(df$R, lag=160) / 160), 40) * 296.8
      df$Trade <- rowMeans(cbind(df$O1, df$O2, df$O3, df$O4, df$O5))
      df$Trade <- df$Trade * FDM
      df$Trade <- lag(ifelse(df$Trade > 20, 20, ifelse(df$Trade < -20, -20, df$Trade )) / 10)
      df$Excess <- df$Return * df$Position * df$Trade * IDM 
      jumbo[[n]] <- select(df, Date, Excess)
    }
    portfolio <- merge_portfolio_list(jumbo)
    res <- portfolio_summary(as.matrix(portfolio[,-1]), dates = portfolio$Date, plot_stats = TRUE, symbol_wise = FALSE, benchmark.dates = benchmark.dates, benchmark.returns = benchmark.returns) 
    print(res$Aggregate %>% unlist)
  }
}

# Strategy 20
# Same as before but applied to carry

breakout <- function(p, h=1, scalar=1) {
  if(sum(is.na(p)) > 0)
    stop("breakout: series contains NAs")
  minimum <- runMin(p, h)
  maximum <- runMax(p, h)
  mid <- (maximum + minimum) / 2
  raw <- na.locf(40 * (p - mid) / (maximum - minimum), na.rm=FALSE)
  forecast <- EMA(raw, ceiling(h/4)) * scalar
}
# Strategy 21
# Jumbo
{
  jumbo <- list()
  IDM = 2.41
  FDM <- 1.33
  for(n in names(BackAdj)) {
    df <- BackAdj[[n]]
    df$Risk = calculate_volatility(df$Return)
    df$Position = lag(target_vol / df$Risk)
    df$Backadjusted <- na.locf(df$Backadjusted, na.rm = FALSE)
    b1 <- breakout(df$Backadjusted, 10, 0.60)
    b2 <- breakout(df$Backadjusted, 20, 0.67)
    b3 <- breakout(df$Backadjusted, 40, 0.70)
    b4 <- breakout(df$Backadjusted, 80, 0.73)
    b5 <- breakout(df$Backadjusted, 160, 0.74)
    b6 <- breakout(df$Backadjusted, 320, 0.74)
    df$Trade <- rowMeans(cbind(b1, b2, b3, b4, b5, b6))
    df$Trade <- df$Trade  * FDM 
    df$Trade <- lag(ifelse(df$Trade > 20, 20, ifelse(df$Trade < -20, -20, df$Trade )) / 10)
    df$Excess <- df$Return * df$Position * df$Trade * IDM 
    jumbo[[n]] <- select(df, Date, Excess)
  }
  portfolio <- merge_portfolio_list(jumbo)
  res <- portfolio_summary(as.matrix(portfolio[,-1]), dates = portfolio$Date, plot_stats = TRUE, symbol_wise = FALSE, benchmark.dates = benchmark.dates, benchmark.returns = benchmark.returns) 
  print(res$Aggregate %>% unlist)
}

# Strategy 22
{
  # jumbo (the result is ok, not bad as in the book in table 85)
  {
    jumbo <- list()
    IDM = 2.41 
    FDM <- 1
    for(n in names(BackAdj)) {
      df <- BackAdj[[n]]
      if(sum(!is.na(df$Return)) < 256*10)
        next
      df$Risk = calculate_volatility(df$Return)
      df$Position = lag(target_vol / df$Risk)
      df$NP <- 0
      for(i in 2:nrow(df)) {
        df$NP[i] <-  (100 * (df$Backadjusted[i] - df$Backadjusted[i-1]) / (df$Risk[i] * df$Nearest[i] / 16)) + df$NP[i-1]
        if(is.na(df$NP[i]))
          df$NP[i] <- df$NP[i-1]
      }
      df <- merge(df, filter(R, Class==df$Class[1]) %>%  select(Date, A), by="Date") # R obtained from before
      df$R  <- (df$NP - df$A) 
      df$O <- -EMA(c(rep(NA, 256*5), diff(df$R, lag=256*5) / 256*5), 30) * 7.27
      df$Trade <- lag(df$O)
      df$Trade <- df$Trade * FDM
      df$Trade <- lag(ifelse(df$Trade > 20, 20, ifelse(df$Trade < -20, -20, df$Trade )) / 10)
      df$Excess <- df$Return * df$Position * df$Trade * IDM 
      jumbo[[n]] <- select(df, Date, Excess)
    }
    portfolio <- merge_portfolio_list(jumbo)
    res <- portfolio_summary(as.matrix(portfolio[,-1]), dates = portfolio$Date, plot_stats = TRUE, symbol_wise = FALSE, benchmark.dates = benchmark.dates, benchmark.returns = benchmark.returns) 
    print(res$Aggregate %>% unlist)
  }
}

# Strategy 23
# Jumbo
{
  jumbo <- list()
  IDM = 2.41
  FDM <- 1.39
  for(n in names(BackAdj)) {
    df <- BackAdj[[n]]
    df$Risk = calculate_volatility(df$Return)
    df$Position = lag(target_vol / df$Risk)
    df$Forecast8 <- (EMA(df$Backadjusted, 8) -  EMA(df$Backadjusted, 32) ) / (df$Nearest * df$Risk / 16) * 5.95
    df$Forecast16 <- (EMA(df$Backadjusted, 16) -  EMA(df$Backadjusted, 64)) / (df$Nearest * df$Risk / 16) * 2.79
    df$Forecast32 <- (EMA(df$Backadjusted, 32) -  EMA(df$Backadjusted, 128)) / (df$Nearest * df$Risk / 16) * 2.79
    df$Forecast64 <- (EMA(df$Backadjusted, 64) -  EMA(df$Backadjusted, 256)) / (df$Nearest * df$Risk / 16) * 1.91
    df$Acc8 <- c(rep(NA, 8), diff(df$Forecast8, 8)) * 1.87
    df$Acc16 <- c(rep(NA, 16), diff(df$Forecast16, 16)) * 1.90
    df$Acc32 <- c(rep(NA, 32), diff(df$Forecast32, 32)) * 1.98
    df$Acc64 <- c(rep(NA, 64), diff(df$Forecast64, 64)) * 2.05
    df$Trade <- rowMeans(cbind(df$Acc8 , df$Acc16 , df$Acc32 , df$Acc64 ))
    df$Trade <- df$Trade  * FDM 
    df$Trade <- lag(ifelse(df$Trade > 20, 20, ifelse(df$Trade < -20, -20, df$Trade )) / 10)
    df$Excess <- df$Return * df$Position * df$Trade * IDM 
    jumbo[[n]] <- select(df, Date, Excess)
  }
  portfolio <- merge_portfolio_list(jumbo)
  res <- portfolio_summary(as.matrix(portfolio[,-1]), dates = portfolio$Date, plot_stats = TRUE, symbol_wise = FALSE, benchmark.dates = benchmark.dates, benchmark.returns = benchmark.returns) 
  print(res$Aggregate %>% unlist)
}


# Strategy 24
# Jumbo
{
  jumbo <- list()
  IDM = 2.41
  FDM <- 1.18
  for(n in names(BackAdj)) {
    df <- BackAdj[[n]]
    df$Risk = calculate_volatility(df$Return)
    df$Position = lag(target_vol / df$Risk)
    skew1 <- c(rep(NA, 60),sapply((60+1):length(df$Return), function(i) -skewness(df$Return[(i-60-1):i], na.rm = TRUE)))
    skew2 <- c(rep(NA, 120),sapply((120+1):length(df$Return), function(i) -skewness(df$Return[(i-120-1):i], na.rm = TRUE)))
    skew3 <- c(rep(NA, 240),sapply((240+1):length(df$Return), function(i) -skewness(df$Return[(i-240-1):i], na.rm = TRUE)))
    df$Skew1 <- EMA(skew1 %>% na.locf(na.rm=FALSE), ceiling(60/4)) * 33.3
    df$Skew2 <- EMA(skew2 %>% na.locf(na.rm=FALSE), ceiling(120/4)) * 37.2
    df$Skew3 <- EMA(skew3 %>% na.locf(na.rm=FALSE), ceiling(240/4)) * 39.2
    df$Trade <- rowMeans(cbind(df$Skew1 , df$Skew2 , df$Skew3))
    df$Trade <- df$Trade  * FDM 
    df$Trade <- lag(ifelse(df$Trade > 20, 20, ifelse(df$Trade < -20, -20, df$Trade )) / 10)
    df$Excess <- df$Return * df$Position * df$Trade * IDM 
    jumbo[[n]] <- select(df, Date, Excess)
  }
  portfolio <- merge_portfolio_list(jumbo)
  res <- portfolio_summary(as.matrix(portfolio[,-1]), dates = portfolio$Date, plot_stats = TRUE, symbol_wise = FALSE, benchmark.dates = benchmark.dates, benchmark.returns = benchmark.returns) 
  print(res$Aggregate %>% unlist)
}

### Production strategy

# The backbone will be strategy 11
{
  adjclose <- df$AdjClose; close <- df$Close; risk <- df$Risk; basis <- df$Basis; expiry_difference <- df$Basis_distance
  cap_forecast <- function(x, cap=20) {
    return(ifelse(x > cap, cap, ifelse(x < -cap, -cap, x ) ))
  }
  multiple_EMA <- function(adjclose, close, risk, spans=c(4, 8, 16, 32, 64), scalars=c(8.53, 5.95, 4.1, 2.79, 1.91), mult=4, cap=20, period=252) {
    n <- length(spans)
    EWMACs <- lapply(1:n, function(i) EMA(adjclose, spans[i]) -  EMA(adjclose, spans[i]*mult))
    EWMACs <- lapply(1:n, function(i) EWMACs[[i]] / (close * risk / sqrt(period)) * scalars[i] )
    EWMACs <- lapply(1:n, function(i) ifelse(EWMACs[[i]] > cap, cap, ifelse(EWMACs[[i]] < -cap, -cap, EWMACs[[i]] ) ))
    forecast <- rowMeans(do.call(cbind, EWMACs))
    return(forecast)
  }
  multiple_AS <- function(adjclose, close, risk, spans=c(21, 63, 126, 252, 504), scalars=c(28.53, 49.09, 68.99, 97.00, 136.61), cap=20, period=252) {
    n <- length(spans)
    ASs <- lapply(1:n, function(i) AbsoluteStrength(adjclose, spans[i]))
    ASs <- lapply(1:n, function(i) ASs[[i]] / (close * risk / sqrt(period)) * scalars[i] )
    ASs <- lapply(1:n, function(i) ifelse(ASs[[i]] > cap, cap, ifelse(ASs[[i]] < -cap, -cap, ASs[[i]] ) ))
    forecast <- rowMeans(do.call(cbind, ASs))
    return(forecast)
  }
  multiple_DC <- function(adjclose, close, risk, spans=c(21, 63, 126, 252, 504), scalars=c(31.2, 33.4, 34.3, 34.9, 35.3), cap=20, period=252) {
    n <- length(spans)
    DCs <- lapply(1:n, function(i) {dc <- DonchianChannel(adjclose, spans[i]); (adjclose - dc[,2]) / abs(dc[,1] - dc[,3])})
    DCs <- lapply(1:n, function(i) DCs[[i]] * scalars[i] )
    DCs <- lapply(1:n, function(i) ifelse(DCs[[i]] > cap, cap, ifelse(DCs[[i]] < -cap, -cap, DCs[[i]] ) ))
    forecast <- rowMeans(do.call(cbind, DCs))
    return(forecast)
  }
  multiple_KF <- function(adjclose, close, risk, spans=c(0.5, 1, 2, 5, 10), scalars=c(66, 55, 46, 37, 31), cap=20, period=252) {
    n <- length(spans)
    KFs <- lapply(1:n, function(i) KalmanFilterIndicator(adjclose, sharpness = 1, K = spans[i])[,2])
    KFs <- lapply(1:n, function(i) KFs[[i]] / ((close * risk / sqrt(period))) * scalars[i] )
    KFs <- lapply(1:n, function(i) ifelse(KFs[[i]] > cap, cap, ifelse(KFs[[i]] < -cap, -cap, KFs[[i]] ) ))
    forecast <- rowMeans(do.call(cbind, KFs))
    return(forecast)
  }
  
  multiple_TII <- function(adjclose, close, risk, spans=c(21, 63, 126, 252, 504)) {
    n <- length(spans)
    TII <- lapply(1:n, function(i) TII(adjclose, P = spans[i]) / 5)
    forecast <- rowMeans(do.call(cbind, TII))
    return(forecast)
  }
  # basis and risk are in percentage
  multiple_Carry <- function(basis, expiry_difference, risk, spans=c(21, 63, 126), scalar=30, expiry_span=12) {
    n <- length(spans)
    Carry <- (basis / (expiry_difference / expiry_span)) / ( risk )
    Carry <- na.locf(Carry, na.rm=FALSE); Carry[is.na(Carry)] <- 0
    EMAs <- lapply(1:n, function(i) EMA(Carry, spans[i]) * scalar)
    EMAs <- lapply(1:n, function(i) ifelse(EMAs[[i]] > cap, cap, ifelse(EMAs[[i]] < -cap, -cap, EMAs[[i]] ) ))
    forecast <- rowMeans(do.call(cbind, EMAs))
    return(forecast)
  }
  relative_volatility <- function(volatility, period=2520) {
    return(unlist(Map(function(i) mean(tail(volatility[1:i], period), na.rm=TRUE), 1:length(volatility))))
  }
  
  
  # Execution
  {
    Assets <- BackAdj
    results <- list()
    target_vol <- 0.25
    IDM = 2.5
    FDMtrend <- 1.33
    FDMcarry <- 1.05
    weights <- c(1, 0)
    for(symbol in names(Assets)) {
      print(symbol)
      df <- Assets[[symbol]]
      df$Volatility = calculate_volatility(df$Return)
      df$Position = lag(target_vol / df$Volatility)
      # Relative volatility
      df$M <- 1
      #df$RV <- relative_volatility(df$Volatility) # quite slow, you can replace it with df$Volatility / runMean(df$Volatility, 2520))  
      # df$RV <- df$Volatility / runMean(df$Volatility, 252)  
      # df$Q <- sapply(1:length(df$RV), function(i) sum(df$RV[i] > df$RV[1:i], na.rm=TRUE) / i)
      # df$M <- EMA(2 - 1.5 * df$Q, 10)
      # Trend-following
      #df$ForecastEMA <- multiple_EMA(df$AdjClose, df$Close, df$Volatility) 
      df$ForecastAS <- multiple_AS(df$AdjClose, df$Close, df$Volatility) 
      df$ForecastDC <- multiple_DC(df$AdjClose, df$Close, df$Volatility) 
      #df$ForecastRSI <- multiple_RSI(df$AdjClose, df$Close, df$Volatility) 
      df$ForecastKF <- multiple_KF(df$AdjClose, df$Close, df$Volatility) 
      df$ForecastTII <- multiple_TII(df$AdjClose, df$Close, df$Volatility)
      df$ForecastTrend <- rowMeans(cbind(df$ForecastAS, df$ForecastDC, df$ForecastKF, df$ForecastTII)) * FDMtrend * df$M
      df$ForecastTrend <- cap_forecast(df$ForecastTrend)
      # Carry
      df$ForecastCarry <- multiple_Carry(df$Basis, df$Basis_distance, df$Volatility)  * FDMcarry
      df$ForecastCarry <- cap_forecast(df$ForecastCarry)
      # Final trade
      df$Trade <- (weights[1] * df$ForecastTrend + weights[2] * df$ForecastCarry) / 10 
      df$Trade <- lag(df$Trade)
      df$Excess <- df$Return * df$Position * df$Trade * IDM 
      results[[symbol]] <- select(df, Date, Excess)
    }
    portfolio <- merge_portfolio_list(results)
    res <- portfolio_summary(as.matrix(portfolio[,-1]), dates = portfolio$Date, plot_stats = TRUE, symbol_wise = FALSE, benchmark.dates = benchmark.dates  , benchmark.returns = benchmark.returns  ) 
    print(res$Aggregate %>% unlist)
  }
  # > cor(cbind(ema, as, dc, rsi, tii, carry), use="pairwise.complete.obs")
  #               ema          as        dc        rsi         kf       tii       carry
  # ema    1.00000000  0.98087784 0.9569068  0.9774590 0.77753903 0.7802132 -0.06468951
  # as     0.98087784  1.00000000 0.9815585  0.9973434 0.74095885 0.8442622 -0.02987404
  # dc     0.95690678  0.98155851 1.0000000  0.9819828 0.81395836 0.7945836  0.25337216
  # rsi    0.97745899  0.99734341 0.9819828  1.0000000 0.73057155 0.8526681 -0.01424110
  # kf     0.77753903  0.74095885 0.8139584  0.7305715 1.00000000 0.4338501  0.08733133
  # tii    0.78021325  0.84426222 0.7945836  0.8526681 0.43385009 1.0000000  0.35867119
  # carry -0.06468951 -0.02987404 0.2533722 -0.0142411 0.08733133 0.3586712  1.00000000
  # 
  # Best SR: DC
  # Best skew: KF
  # Best Tails: TII
  # Best correlation: KF
  # Best GPR: DC
}


