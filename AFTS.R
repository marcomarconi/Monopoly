{
library(tidyverse)
library(moments)
library(TTR)
  library(lubridate)
  library(tsibble)
  library(zoo)
}
portfolio_summary <- function(portfolio, dates=NULL, period=252, benchmark.dates=NULL, benchmark.returns=NULL, plot_stats=FALSE) {
  if(any(is.na(portfolio)))
    stop("portfolio_summary: NAs are not allowed, replace them with zeros") 
  if(!is.null(dates) & class(dates) != "Date")
    stop(paste("portfolio_summary: dates should be of class Date"))
  if(class(portfolio) != "matrix")
    stop(paste("portfolio_summary: portfolio should be of class matrix"))
  if(any(is.na(portfolio)))
    stop(paste("portfolio_summary: portfolio cannot contain NAs"))
  if(is.null(dates)) 
    dates <- seq(as.Date("1970/01/01"), by = "day", length.out = length(log_returns))
  returns <- rowMeans(portfolio)
  df <- data.frame(Dates=dates, Returns=returns) %>% arrange(Dates)
  annual_returns <- group_by(df, year(Dates)) %>% summarise(Dates=first(year(Dates)), Returns = sum(Returns)) 
  monthly_returns <- group_by(df, yearmonth(Dates)) %>% summarise(Dates=first(yearmonth(Dates)), Returns = sum(Returns)) 
  mean_ann_ret <- mean(annual_returns$Returns) * 100
  ann_sd <- sd(df$Returns) * sqrt(period) * 100
  sr <- mean(df$Returns) / sd(df$Returns) * sqrt(period)
  skew <- skewness(monthly_returns$Returns)
  q <- quantile(df$Returns[df$Returns!=0], probs=c(0.01, 0.3, 0.7, 0.99))
  lower_tail <- as.numeric(q[1] / q[2] / 4.43)
  upper_tail <- as.numeric(q[4] / q[3] / 4.43)
  cum_returns = cumsum(df$Returns) 
  peak = cummax(cum_returns)
  drawdown = peak - cum_returns
  max_drawdown <- -(exp(drawdown[which.max(drawdown)])-1)*100
  avg_drawdown <- -(exp(mean(drawdown))-1)*100
  gpr <-  sum(df$Returns) / sum(abs(df$Returns[df$Returns<0]))
  r2 <- summary(lm(1:length(cum_returns) ~ 0+cum_returns))$adj.r.squared
  alpha <- NA
  beta <- NA
  if(!is.null(benchmark.dates)) {
    benchmark <- data.frame(Dates=benchmark.dates, Returns=benchmark.returns)
    z <- merge(benchmark, df, by="Dates")
    z <- group_by(z, yearmonth(Dates)) %>% summarise(X=sum(Returns.x), Y=sum(Returns.y))
    fit <- (lm(Y ~ X, z))
    alpha <- as.numeric(coef(fit)[1]) * 100
    beta <- as.numeric(coef(fit)[2]) 
  }
  if(plot_stats) {
    par(mfrow=c(2,1), mar=c(2,4,1,0.5))
    plot(sort(as.Date(dates)), cumsum(returns)*100, ylab="Equity curve %")
    chunks <- group_by(data.frame(Date=as.Date(dates), ret=returns), year(Date)) %>% 
      summarise(sum=round(sum(ret)*100,1), first=first(Date), .groups = 'drop')
    abline(v=chunks$first, lty=2, lwd=0.5)
    text(x=chunks$first+period/2, y=max(cumsum(returns*100)), labels=chunks$sum, cex = 0.75)
    matplot(apply(portfolio, 2, cumsum)*100, type = "l", lwd=2, lty=1, ylab="Assets curves %", xaxt="n")
    abline(h=0, lwd=2)
    axis(side = 1, labels=dates, at=seq(1, length(dates)), tick = FALSE)
  }
  results <- list("Mean annual return"=mean_ann_ret, 
       "Annualized standard deviation"=ann_sd, 
       "Sharpe ratio"=sr, 
       "Skew"=skew, 
       "Lower tail"=lower_tail, 
       "Upper tail"=upper_tail, 
       "Max drawdown"=max_drawdown, 
       "Average drawdown"=avg_drawdown,
       "GPR"=gpr,
       "R2"=r2,
       "alpha"=alpha,
       "beta"=beta
       )
  return(lapply(results, round, 2))
}

merge_portfolio_list <- function(portfolio_list) {
  full_df <- Reduce(function(...) full_join(..., by="Date", all=TRUE, incomparables = NA), portfolio_list) %>% arrange(Date)
  colnames(full_df) <- c("Date", names(portfolio_list))
  full_df[is.na(full_df)] <- 0
  return(full_df)
}

Futures <- read_rds("/home/marco/trading/Historical Data/Barchart/Futures.RDS")
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
## Stategy 2 ????
{SP500 <- backadjust_future(Futures[["ES"]], N = 5)
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
SP500 <- backadjust_future(Futures[["ES"]], N = 5)
SP500$Return[is.na(SP500$Return)] <- 0
SP500$Nearest <- na.locf(SP500$Nearest, na.rm = FALSE )
ema <- sqrt(EMA(SP500$Return^2, 32))
SP500$Risk <-  (0.3 * runMean(ema, 252) + 0.7 * ema) * 16 # one year instead of ten
SP500$Position = 0.20 / SP500$Risk
#P500$Position[is.na(SP500$Position)] <- 0
SP500 <- na.omit(SP500)
SP500$Excess <- SP500$Return * SP500$Position 
with(SP500, plot(Date,cumsum(Excess)))
summary_statistics(SP500$Date, SP500$Excess) %>% unlist

## Stategy 4
# Risk parity
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
# All weather
{
N <- 1
equity <- backadjust_future(Futures[["VI"]], N = N)
bond_long <- backadjust_future(Futures[["ZB"]], N = N)
bond_short <- backadjust_future(Futures[["ZT"]], N = N)
energy <- backadjust_future(Futures[["RB"]], N = N)
soft <- backadjust_future(Futures[["SW"]], N = N)
gold <- backadjust_future(Futures[["GC"]], N = N)
assets <- list(equity=equity, bond_long=bond_long, bond_short=bond_short, energy=energy, soft=soft, gold=gold)
assets$ES$Return <- -assets$ES$Return
for(n in names(assets)) {
  x <- assets[[n]]
  x$Return[is.na(x$Return)] <- 0
  ema <- sqrt(EMA(x$Return^2, 32))
  x$Risk <-  (0.3 * runMean(ema, 252) + 0.7 * ema) * 16 # one year instead of ten
  x$Position = 1 * 0.20 / x$Risk
  x$Excess <- x$Return * x$Position 
  assets[[n]] <- x
}
full_df <- Reduce(function(...) full_join(..., by="Date", all=TRUE, incomparables = NA), assets)
full_df <- full_df[,grep("Date|Excess", colnames(full_df))] %>% na.omit
colnames(full_df) <- c("Date", names(assets))
full_df$Aggregate <- rowMeans(full_df[,-1]) * 1.81
summary_statistics(full_df$Date, full_df$Aggregate) %>% unlist
apply(full_df[,-1], 2, cumsum)[seq(1, nrow(full_df), 20),] %>% matplot
}
# Jumbo
{
jumbo <- list()
IDM = 2.47
for(n in names(BackAdj)) {
  df <- BackAdj[[n]]
  df$Return[is.na(df$Return)] <- 0
  ema <- sqrt(EMA(df$Return^2, 32))
  df$Risk <-  (0.3 * runMean(ema, 252) + 0.7 * ema) * 16 # one year instead of ten
  df$Position = 0.20 / (df$Risk)
  df$Position[is.na(df$Position)] <- 0
  df$Excess <- df$Return * df$Position * IDM
  jumbo[[n]] <- select(df, Date, Excess)
}
portfolio <- merge_portfolio_list(jumbo)
portfolio_summary(as.matrix(portfolio[,-1]), dates = portfolio$Date, plot_stats = TRUE) %>% unlist
benchmark.dates <- portfolio[,1] 
benchmark.returns <- portfolio[,-1] %>% rowMeans()
}
## Stategy 5
# SP only
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
# Jumbo
{
jumbo <- list()
IDM = 2.47
for(n in names(BackAdj)) {
  df <- BackAdj[[n]]
  df$Return[is.na(df$Return)] <- 0
  ema <- sqrt(EMA(df$Return^2, 32))
  df$Risk <-  (0.3 * runMean(ema, 252) + 0.7 * ema) * 16 # one year instead of ten
  df$Position <- 0.20 / df$Risk
  df$Trend <- EMA(df$Backadjusted, 64) -  EMA(df$Backadjusted, 256)
  df$Trade <- lag(ifelse(df$Trend > 0, 1, 0))
  df <- na.omit(df)
  df$Excess <- df$Return * df$Position * df$Trade * IDM
  jumbo[[n]] <- select(df, Date, Excess)
}
portfolio <- merge_portfolio_list(jumbo)
portfolio_summary(as.matrix(portfolio[,-1]), dates = portfolio$Date, plot_stats = TRUE, benchmark.dates = benchmark.dates, benchmark.returns = benchmark.returns) %>% unlist}
## Stategy 6
# Jumbo
{
jumbo <- list()
IDM = 2.47
for(n in names(BackAdj)) {
  df <- BackAdj[[n]]
  df$Return[is.na(df$Return)] <- 0
  ema <- sqrt(EMA(df$Return^2, 32))
  df$Risk <-  (0.3 * runMean(ema, 252) + 0.7 * ema) * 16 # one year instead of ten
  df$Position <- 0.20 / df$Risk
  df$Trend <- EMA(df$Backadjusted, 64) -  EMA(df$Backadjusted, 256)
  #df$Trend <- AbsoluteStrength(df$Backadjusted, 250)
  df$Trade <- lag(ifelse(df$Trend > 0, 1, -1))
  df <- na.omit(df)
  df$Excess <- df$Return * df$Position * df$Trade * IDM
  jumbo[[n]] <- select(df, Date, Excess)
}
portfolio <- merge_portfolio_list(jumbo)
portfolio_summary(as.matrix(portfolio[,-1]), dates = portfolio$Date, plot_stats = TRUE, benchmark.dates = benchmark.dates, benchmark.returns = benchmark.returns) %>% unlist}

}

## Stategy 7
# Jumbo
{
  jumbo <- list()
IDM = 2.47
for(n in names(BackAdj)) {
  df <- BackAdj[[n]]
  df$Nearest <- na.locf(df$Nearest, na.rm = FALSE )
  df$Return[is.na(df$Return)] <- 0
  ema <- sqrt(EMA(df$Return^2, 32))
  df$Risk <-  (0.3 * runMean(ema, 252) + 0.7 * ema) * 16 # one year instead of ten
  df$Position <- 0.20 / df$Risk
  df$Trend <- EMA(df$Backadjusted, 64) -  EMA(df$Backadjusted, 256)
  df$Forecast <- df$Trend / (df$Nearest * df$Risk / 16) * 1.9
  # df$Trend <- AbsoluteStrength(df$Backadjusted, 250)
  # df$Forecast <- df$Trend / (df$Nearest * df$Risk / 16) * 100
  df$Trade <- lag(ifelse(df$Forecast > 20, 20, ifelse(df$Forecast < -20, -20, df$Forecast )) / 10)
  df <- na.omit(df)
  df$Excess <- df$Return * df$Position * df$Trade * IDM
  jumbo[[n]] <- select(df, Date, Excess)
}
full_df <- Reduce(function(...) full_join(..., by="Date", all=TRUE, incomparables = NA), jumbo) %>% na.omit
colnames(full_df) <- c("Date", names(jumbo))
summary_statistics(full_df$Date, full_df[,-1] %>% rowMeans()) %>% unlist
full_df[,-1] %>% rowMeans() %>% na.omit %>% cumsum %>% plot.ts
}

## Stategy 8
# Jumbo
{
jumbo <- list()
IDM = 2.47
for(n in names(BackAdj)) {
  df <- BackAdj[[n]]
  df$Nearest <- na.locf(df$Nearest, na.rm = FALSE )
  df$Return[is.na(df$Return)] <- 0
  ema <- sqrt(EMA(df$Return^2, 32))
  df$Risk <-  (0.3 * runMean(ema, 252) + 0.7 * ema) * 16 # one year instead of ten
  df$Position <- 0.20 / df$Risk
  df$Trend <- EMA(df$Backadjusted, 16) -  EMA(df$Backadjusted, 64)
  df$Forecast <- df$Trend / (df$Nearest * df$Risk / 16) * 4.1
  # df$Trend <- AbsoluteStrength(df$Backadjusted, 100)
  # df$Forecast <- df$Trend / (df$Nearest * df$Risk / 16) * 50
  df$Trade <- lag(ifelse(df$Forecast > 20, 20, ifelse(df$Forecast < -20, -20, df$Forecast )) / 10)
  df <- na.omit(df)
  df$Excess <- df$Return * df$Position * df$Trade * IDM
  jumbo[[n]] <- select(df, Date, Excess)
}
full_df <- Reduce(function(...) full_join(..., by="Date", all=TRUE, incomparables = NA), jumbo) %>% na.omit
colnames(full_df) <- c("Date", names(jumbo))
summary_statistics(full_df$Date, full_df[,-1] %>% rowMeans()) %>% unlist
full_df[,-1] %>% rowMeans() %>% na.omit %>% cumsum %>% plot.ts
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
# Jumbo
{
  jumbo <- list()
  IDM = 2.47
  FDM <- 1.26
  for(n in names(BackAdj)) {
    df <- BackAdj[[n]]
    df$Nearest <- na.locf(df$Nearest, na.rm = FALSE )
    df$Return[is.na(df$Return)] <- 0
    ema <- sqrt(EMA(df$Return^2, 32))
    df$Risk <-  (0.3 * runMean(ema, 252) + 0.7 * ema) * 16 # one year instead of ten
    df$Position <- 0.20 / df$Risk
    

    # df$Trend2 <- AbsoluteStrength(df$Backadjusted, 5)
    # df$Trend4 <- AbsoluteStrength(df$Backadjusted, 25)
    # df$Trend8 <- AbsoluteStrength(df$Backadjusted, 100)
    # df$Trend16 <- AbsoluteStrength(df$Backadjusted, 175)
    # df$Trend32 <- AbsoluteStrength(df$Backadjusted, 250)
    # df$Trend64 <- AbsoluteStrength(df$Backadjusted, 500)
    # df$Forecast2 <- df$Trend2 / (df$Nearest * df$Risk / 16) * 13.5
    # df$Forecast4 <- df$Trend4 / (df$Nearest * df$Risk / 16) * 30
    # df$Forecast8 <- df$Trend8 / (df$Nearest * df$Risk / 16) * 60
    # df$Forecast16 <- df$Trend16 / (df$Nearest * df$Risk / 16) * 80
    # df$Forecast32 <- df$Trend32 / (df$Nearest * df$Risk / 16) * 95
    # df$Forecast64 <- df$Trend64 / (df$Nearest * df$Risk / 16) * 135

    df <- multiple_ema(df)
    df$Trade <- df$Trade * FDM
    df$Trade <- lag(ifelse(df$Trade > 20, 20, ifelse(df$Trade < -20, -20, df$Trade )) / 10)
    df <- na.omit(df)
    df$Excess <- df$Return * df$Position * df$Trade * IDM 
    jumbo[[n]] <- select(df, Date, Excess)
  }
  full_df <- Reduce(function(...) full_join(..., by="Date", all=TRUE, incomparables = NA), jumbo) 
  colnames(full_df) <- c("Date", names(jumbo))
  summary_statistics(full_df$Date, full_df[,-1] %>% rowMeans(., na.rm=TRUE), benchmark.dates = benchmark.dates, benchmark.returns = benchmark.returns) %>% unlist
  full_df[,-1] %>% rowMeans(., na.rm=TRUE) %>% na.omit %>% cumsum %>% plot.ts
}

multiple_carry <- function(df) {
  df$Carry <- na.locf(df$Carry, na.rm=FALSE)
  df$Carry5 <- EMA(df$Carry, 5)
  df$Carry20 <- EMA(df$Carry, 20)
  df$Carry60 <- EMA(df$Carry, 60)
  df$Carry120 <- EMA(df$Carry, 120)
  df$Forecast5 <- df$Carry5 * 30
  df$Forecast20 <- df$Carry20 * 30
  df$Forecast60 <- df$Carry60 * 30
  df$Forecast120 <- df$Carry120 * 30
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
IDM = 2.47
FDM <- 1.04
tt <- list()
for(n in names(BackAdj)) {
  df <- BackAdj[[n]]
  df$Nearest <- na.locf(df$Nearest, na.rm = FALSE )
  df$Return[is.na(df$Return)] <- 0
  ema <- sqrt(EMA(df$Return^2, 32))
  df$Risk <-  (0.3 * runMean(ema, 252) + 0.7 * ema) * 16 # one year instead of ten
  df$Position <- 0.20 / df$Risk
  df$Carry <- df$Basis / (df$Basis_distance / 12) / (df$Risk )
  df <- multiple_carry(df)
  #df$Trade <- df$Carry
  df$Trade <- lag(ifelse(df$Trade > 20, 20, ifelse(df$Trade < -20, -20, df$Trade ))) / 10
  tt[[n]] <- df$Trade
  df <- na.omit(df)
  df$Excess <- df$Return * df$Position * df$Trade * IDM * FDM
  jumbo[[n]] <- select(df, Date, Excess)
}
full_df <- Reduce(function(...) full_join(..., by="Date", all=TRUE, incomparables = NA), jumbo) 
colnames(full_df) <- c("Date", names(jumbo))
summary_statistics(full_df$Date, full_df[,-1] %>% rowMeans(., na.rm=TRUE)) %>% unlist
full_df[,-1] %>% rowMeans(., na.rm=TRUE)  %>% cumsum %>% plot.ts
}


