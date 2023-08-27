{
  library(tidyverse)
  library(Rfast)
  library(TTR)
  library(lubridate)
  library(tsibble)
  library(zoo)
  library(moments)
  library(ggthemes)
  library(Rfast)
  source("/home/marco/trading/Systems//Common/Indicators.R")
  source("/home/marco/trading/Systems//Common/Reports.R")
  
}


Futures <- read_rds("/home/marco/trading/Historical Data/Barchart/Futures.RDS")
BackAdj <- read_rds("/home/marco/trading/Historical Data/Barchart/BackAdj.RDS")
target_vol <- 0.2


## Stategy 1
{SP500 <- backadjust_future(Futures[["ES"]], N = 5)
SP500$Return[is.na(SP500$Return)] <- 0
ggplot(SP500) + geom_line(aes(Date, Nearest)) + geom_line(aes(Date, AdjClose), color="gray")
with(SP500, matplot2(cbind(-Nearest+ AdjClose)))
with(SP500, plot(Date,cumsum(Return*100)))
summary_statistics(SP500$Date, SP500$Return) %>% unlist
ZN <- backadjust_future(Futures[["ZN"]], N = 5)
ZN$Return[is.na(ZN$Return)] <- 0
ggplot(ZN) + geom_line(aes(Date, Nearest)) + geom_line(aes(Date, AdjClose), color="gray")
with(ZN, matplot2(cbind(-Nearest+ AdjClose)))
with(ZN, plot(Date,cumsum(Return*100)))
summary_statistics(ZN$Date, ZN$Return) %>% unlist
}
## Stategy 2 (not very useful)
{ºSP500 <- backadjust_future(Futures[["ES"]], N = 5)
SP500$Return[is.na(SP500$Return)] <- 0
SP500$Close <- na.locf(SP500$Close, na.rm = FALSE )
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
SP500$Close <- na.locf(SP500$Close, na.rm = FALSE )
ema <- sqrt(EMA(SP500$Return^2, 32))
SP500$Volatility <-  (0.3 * runMean(ema, 252) + 0.7 * ema) * 16 # one year instead of ten
SP500$Position = 0.20 / SP500$Volatility
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
SP500$Volatility <-  (0.3 * runMean(ema, 252) + 0.7 * ema) * 16 # one year instead of ten
SP500$Position = 1 * 0.20 / SP500$Volatility
SP500 <- na.omit(SP500)
SP500$Excess <- SP500$Return * SP500$Position 
summary_statistics(SP500$Date, SP500$Excess) %>% unlist
ZN <- backadjust_future(Futures[["ZN"]], N = 5)
ZN$Return[is.na(ZN$Return)] <- 0
ema <- sqrt(EMA(ZN$Return^2, 32))
ZN$Volatility <-  (0.3 * runMean(ema, 252) + 0.7 * ema) * 16 # one year instead of ten
ZN$Position = 1 * 0.20 / ZN$Volatility
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
  x$Volatility <-  (0.3 * runMean(ema, 252) + 0.7 * ema) * 16 # one year instead of ten
  x$Position = 1 * 0.20 / x$Volatility
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
SP500$Volatility <-  (0.3 * runMean(ema, 252) + 0.7 * ema) * 16 # one year instead of ten
SP500$Position = 0.20 / SP500$Volatility
SP500$Trend <- EMA(SP500$AdjClose, 64) -  EMA(SP500$AdjClose, 256)
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
  df$Trend <- EMA(df$AdjClose, 64) -  EMA(df$AdjClose, 256)
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
  df$Trend <- EMA(df$AdjClose, 64) -  EMA(df$AdjClose, 256)
  #df$Trend <- AbsoluteStrength(df$AdjClose, 250)
  df$Trade <- lag(ifelse(df$Trend > 0, 1, -1))
  df$Excess <- df$Return * df$Position * df$Trade * IDM
  jumbo[[n]] <- select(df, Date, Excess)
}
portfolio <- merge_portfolio_list(jumbo)
res <- portfolio_summary(as.matrix(portfolio[,-1]), dates = portfolio$Date, plot_stats = TRUE, symbol_wise = TRUE) 
print(res$Aggregate %>% unlist)
}

## Stategy 7
{
  # Jumbo
jumbo <- list()
IDM = 1 #2.41
for(n in names(BackAdj)) {
  df <- BackAdj[[n]]
  df$Volatility = calculate_volatility(df$Return)
  df$Position = lag(target_vol / df$Volatility)
  df$Trend <- EMA(df$AdjClose, 64) -  EMA(df$AdjClose, 256)
  df$Forecast <- df$Trend / (df$Close * df$Volatility / 16) * 1.9
  # df$Trend <- AbsoluteStrength(df$AdjClose, 250)
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
  df$Volatility = calculate_volatility(df$Return)
  df$Position = lag(target_vol / df$Volatility)
  df$Trend <- EMA(df$AdjClose, 16) -  EMA(df$AdjClose, 64)
  df$Forecast <- df$Trend / (df$Close * df$Volatility / 16) * 1.9
  # df$Trend <- AbsoluteStrength(df$AdjClose, 100)
  # df$Forecast <- df$Trend / (df$Close * df$Volatility / 16) * 50
  df$Trade <- lag(ifelse(df$Forecast > 20, 20, ifelse(df$Forecast < -20, -20, df$Forecast )) / 10)
  df$Excess <- df$Return * df$Position * df$Trade * IDM
  jumbo[[n]] <- select(df, Date, Excess)
}
portfolio <- merge_portfolio_list(jumbo)
res <- portfolio_summary(as.matrix(portfolio[,-1]), dates = portfolio$Date, plot_stats = TRUE, symbol_wise = TRUE) 
print(res$Aggregate %>% unlist)
}

## Stategy 9
{
multiple_ema <- function(df) {
  df$Trend2 <- EMA(df$AdjClose, 2) -  EMA(df$AdjClose, 8)
  df$Trend4 <- EMA(df$AdjClose, 4) -  EMA(df$AdjClose, 16)
  df$Trend8 <- EMA(df$AdjClose, 8) -  EMA(df$AdjClose, 32)
  df$Trend16 <- EMA(df$AdjClose, 16) -  EMA(df$AdjClose, 64)
  df$Trend32<- EMA(df$AdjClose, 32) -  EMA(df$AdjClose, 128)
  df$Trend64 <- EMA(df$AdjClose, 64) -  EMA(df$AdjClose, 256)
  df$Forecast2 <- df$Trend2 / (df$Close * df$Volatility / 16) * 12.1
  df$Forecast4 <- df$Trend4 / (df$Close * df$Volatility / 16) * 8.53
  df$Forecast8 <- df$Trend8 / (df$Close * df$Volatility / 16) * 5.95
  df$Forecast16 <- df$Trend16 / (df$Close * df$Volatility / 16) * 4.1
  df$Forecast32 <- df$Trend32 / (df$Close * df$Volatility / 16) * 2.79
  df$Forecast64 <- df$Trend64 / (df$Close * df$Volatility / 16) * 1.91
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
  df$Trend2 <- AbsoluteStrength(df$AdjClose, 5)
  df$Trend4 <- AbsoluteStrength(df$AdjClose, 25)
  df$Trend8 <- AbsoluteStrength(df$AdjClose, 100)
  df$Trend16 <- AbsoluteStrength(df$AdjClose, 175)
  df$Trend32 <- AbsoluteStrength(df$AdjClose, 250)
  df$Trend64 <- AbsoluteStrength(df$AdjClose, 500)
  df$Forecast2 <- df$Trend2 / (df$Close * df$Volatility / 16) * 13.5
  df$Forecast4 <- df$Trend4 / (df$Close * df$Volatility / 16) * 30
  df$Forecast8 <- df$Trend8 / (df$Close * df$Volatility / 16) * 60
  df$Forecast16 <- df$Trend16 / (df$Close * df$Volatility / 16) * 80
  df$Forecast32 <- df$Trend32 / (df$Close * df$Volatility / 16) * 95
  df$Forecast64 <- df$Trend64 / (df$Close * df$Volatility / 16) * 135
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
  dc1 <- TTR::DonchianChannel(df$AdjClose, n=5) 
  dc2 <- TTR::DonchianChannel(df$AdjClose, n=21) 
  dc3 <- TTR::DonchianChannel(df$AdjClose, n=63) 
  dc4 <- TTR::DonchianChannel(df$AdjClose, n=252) 
  dc5 <- TTR::DonchianChannel(df$AdjClose, n=504) 
  df$Forecast1 <- (df$AdjClose - dc1[,2]) / abs(dc1[,1] - dc1[,3])
  df$Forecast2 <- (df$AdjClose - dc2[,2]) / abs(dc2[,1] - dc2[,3])
  df$Forecast3 <- (df$AdjClose - dc3[,2]) / abs(dc3[,1] - dc3[,3])
  df$Forecast4 <- (df$AdjClose - dc4[,2]) / abs(dc4[,1] - dc4[,3])
  df$Forecast5 <- (df$AdjClose - dc5[,2]) / abs(dc5[,1] - dc5[,3])
  df$Trade <- rowMeans(cbind(df$Forecast1 , df$Forecast2 , df$Forecast3 , df$Forecast4 , df$Forecast5 )) * 40
  return(df)
}
multiple_rsi <- function(df) {
df$Forecast1 <- ((RSI(df$AdjClose, 5, maType = EMA)-50)/2.5)
df$Forecast2 <- ((RSI(df$AdjClose, 21, maType = EMA)-50)/2.5)
df$Forecast3 <- ((RSI(df$AdjClose, 63, maType = EMA)-50)/2.5)
df$Forecast4 <- ((RSI(df$AdjClose, 252, maType = EMA)-50)/2.5)
df$Forecast5 <- ((RSI(df$AdjClose, 504, maType = EMA)-50)/2.5)
df$Trade <- rowMeans(cbind(df$Forecast1 , df$Forecast2 , df$Forecast3 , df$Forecast4 , df$Forecast5 ))
return(df)
}
# Jumbo
{
  jumbo <- list()
  IDM = 2.41
  FDM <- 1.39
  for(n in names(BackAdj)) {
    df <- BackAdj[[n]]
    df$Volatility = calculate_volatility(df$Return)
    df$Position = lag(target_vol / df$Volatility)
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
}



# Strategy 10
{
  multiple_carry <- function(df, scalar=30) {
    df$Carry <- (df$Basis / (df$Basis_distance / 12)) / ( df$Volatility )
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
jumbo <- list()
IDM = 2.41
FDM <- 1.04
tt <- list()
for(n in names(BackAdj)) {
  df <- BackAdj[[n]]
  df$Volatility = calculate_volatility(df$Return)
  df$Position = lag(target_vol / df$Volatility)
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
    df$Volatility = calculate_volatility(df$Return)
    df$Position = lag(target_vol / df$Volatility)
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
  f <- 4
  hs <- c(98.5, 50.2, 25.4, 13.4, 7.6, 5.2)
  h <- hs[2]
  h <- ceiling(252/h)
  scalar <- 8.53
  for(n in names(BackAdj)) {
    df <- BackAdj[[n]]
    df$Return[is.na(df$Return)] <- 0
    df$Volatility = calculate_volatility(df$Return)
    df$Trend <- EMA(df$AdjClose, f) -  EMA(df$AdjClose, f*4)
    df$Forecast <- df$Trend / (df$Close * df$Volatility / 16) * scalar
    df$Forecast <- ifelse(df$Forecast > 20, 20, ifelse(df$Forecast < -20, -20, df$Forecast )) 
    for(i in 1:nrow(df)) {
      df$FR[i] <- df$Forecast[i]
      df$NR[i] <- mean(df$Return[(i+1):(i+h)], na.rm=TRUE) / (df$Volatility[i] / 16) 
    }
    jumbo[[n]] <- select(df, FR, NR)
  }
  res <- do.call(rbind, jumbo)
  a <- group_by(res %>% filter(between(FR, -20, 20)), bin=cut(FR, 10)) %>% 
    summarize(M=median(NR, na.rm=TRUE), S=2*(mad(NR, na.rm=TRUE))/sqrt(n())) 
  ggplot(a)+geom_errorbar(aes(bin, ymin=M-S, ymax=M+S))+geom_hline(yintercept = 0)
   
}

# Strategy 13
{
  # Figure 54
  df <- BackAdj[["ES"]]
  df$Volatility = calculate_volatility(df$Return)
  df$RV <- df$Volatility / runMean(df$Volatility, 256)
  plot(df$Date, df$RV)
  # Figure 55
  RVs <- list()
  for(n in names(BackAdj)) {
    df <- BackAdj[[n]]
    df$Volatility = calculate_volatility(df$Return)
    RVs[[n]] <- df$Volatility / runMean(df$Volatility, 256)
  }
  RVs %>% unlist %>% hist(xlim = c(0,5), 100)
  # jumbo
  {
    jumbo <- list()
    IDM = 2.41
    tt <- list()
    for(n in names(BackAdj)) {
      df <- BackAdj[[n]]
      df$Volatility = calculate_volatility(df$Return)
      df$RV <- df$Volatility / runMean(df$Volatility, 256)
      Q <- sapply(1:length(df$RV), function(i) sum(df$RV[i] > df$RV[1:i], na.rm=TRUE) / i)
      M <- EMA(2-1.5*Q)
      df$Position = lag(target_vol / df$Volatility)
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
  df$Spot <- df$AdjClose[1]
  for(i in 2:nrow(df)) {
    df$Spot[i] <- df$Spot[i-1] + df$AdjClose[i] - df$AdjClose[i-1] - df$Carry_accrued[i]
    if(is.na(df$Spot[i]))
      df$Spot[i] <- df$Spot[i-1]
  }
  df$Carry_cum_return <- cumsum(replace(df$Carry_accrued, is.na(df$Carry_accrued), 0)) + df$AdjClose[1]
  with(df, matplot2(cbind(Carry_cum_return, AdjClose, Spot)))  
  
}


# Strategy 15
# nothing worth testing here, but interesting for the future model

# Strategy 16
# nothing worth testing here

# Strategy 17
{
  # Figure 69
  df <- BackAdj[["ES"]]
  df$Volatility = calculate_volatility(df$Difference)
  df$NP <- 0
  for(i in 2:nrow(df)) {
    df$NP[i] <-  (100 * (df$AdjClose[i] - df$AdjClose[i-1]) / (df$Volatility[i] )) + df$NP[i-1]
    if(is.na(df$NP[i]))
      df$NP[i] <- df$NP[i-1]
  }
  with(df, matplot2(cbind(AdjClose, NP*3)))
  # jumbo
  {
    jumbo <- list()
    IDM = 2.41
    FDM <- 1.26
    for(n in names(BackAdj)) {
      df <- BackAdj[[n]]
      df$Volatility = calculate_volatility(df$Return)
      df$Position = lag(target_vol / df$Volatility)
      df$NP <- 0
      for(i in 2:nrow(df)) {
        df$NP[i] <-  (100 * (df$AdjClose[i] - df$AdjClose[i-1]) / (df$Volatility[i] * df$Close[i] / 16)) + df$NP[i-1]
        if(is.na(df$NP[i]))
          df$NP[i] <- df$NP[i-1]
      }
      df$AdjClose <- df$NP # we are replace backadj with the normalized price here
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
    # Calculate asset class indices
    NPs <- list()
    for(n in names(BackAdj)) {
      df <- BackAdj[[n]]
      df$Volatility = calculate_volatility(df$Return)
      df$NP <- 0
      for(i in 2:nrow(df)) {
        df$NP[i] <-  (100 * (df$AdjClose[i] - df$AdjClose[i-1]) / (df$Volatility[i] * df$Close[i] / 16)) + df$NP[i-1]
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
      df$Volatility = calculate_volatility(df$Return)
      df$Position = lag(target_vol / df$Volatility)
      df <- merge(df, filter(R, Class==df$Class[1]) %>%  select(Date, A), by="Date") # R obtained from before
      df$AdjClose  <- df$A
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
  df$Volatility = calculate_volatility(df$Return)
  df$NP <- 0
  for(i in 2:nrow(df)) {
    df$NP[i] <-  (100 * (df$AdjClose[i] - df$AdjClose[i-1]) / (df$Volatility[i] * df$Close[i] / 16)) + df$NP[i-1]
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
      df$Volatility = calculate_volatility(df$Return)
      df$Position = lag(target_vol / df$Volatility)
      df$NP <- 0
      for(i in 2:nrow(df)) {
        df$NP[i] <-  (100 * (df$AdjClose[i] - df$AdjClose[i-1]) / (df$Volatility[i] * df$Close[i] / 16)) + df$NP[i-1]
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

# Strategy 21
{
  breakout <- function(p, h=1, scalar=1) {
    if(sum(is.na(p)) > 0)
      stop("breakout: series contains NAs")
    minimum <- runMin(p, h)
    maximum <- runMax(p, h)
    mid <- (maximum + minimum) / 2
    raw <- na.locf(40 * (p - mid) / (maximum - minimum), na.rm=FALSE)
    forecast <- EMA(raw, ceiling(h/4)) * scalar
  }
  jumbo <- list()
  IDM = 2.41
  FDM <- 1.33
  for(n in names(BackAdj)) {
    df <- BackAdj[[n]]
    df$Volatility = calculate_volatility(df$Return)
    df$Position = lag(target_vol / df$Volatility)
    df$AdjClose <- na.locf(df$AdjClose, na.rm = FALSE)
    b1 <- breakout(df$AdjClose, 10, 0.60)
    b2 <- breakout(df$AdjClose, 20, 0.67)
    b3 <- breakout(df$AdjClose, 40, 0.70)
    b4 <- breakout(df$AdjClose, 80, 0.73)
    b5 <- breakout(df$AdjClose, 160, 0.74)
    b6 <- breakout(df$AdjClose, 320, 0.74)
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
  {
    # Calculate asset class indices
    NPs <- list()
    for(n in names(BackAdj)) {
      df <- BackAdj[[n]]
      df$Volatility = calculate_volatility(df$Return)
      df$NP <- 0
      for(i in 2:nrow(df)) {
        df$NP[i] <-  (100 * (df$AdjClose[i] - df$AdjClose[i-1]) / (df$Volatility[i] * df$Close[i] / 16)) + df$NP[i-1]
        if(is.na(df$NP[i]))
          df$NP[i] <- df$NP[i-1]
      }
      df$dNP = c(0, diff(df$NP))
      NPs[[n]] <- select(df, Date, Symbol, Class, NP, dNP)
    }
    allNPs <- do.call(rbind, NPs)
    R <- group_by(allNPs, Class, Date) %>% summarise(Symbol=Symbol, R=mean(dNP)) %>% arrange(Class, Date) %>% select(-Symbol)  %>%  unique %>% group_by( Class) %>% mutate(A=cumsum(R)) %>% ungroup()    
    ggplot(R) + geom_line(aes(Date, A, col=Class), linewidth=4) + scale_color_colorblind()
  }
  # jumbo
  {
    jumbo <- list()
    IDM = 2.41 
    FDM <- 1
    for(n in names(BackAdj)) {
      df <- BackAdj[[n]]
      if(sum(!is.na(df$Return)) < 256*10)
        next
      df$Volatility = calculate_volatility(df$Return)
      df$Position = lag(target_vol / df$Volatility)
      df$NP <- 0
      for(i in 2:nrow(df)) {
        df$NP[i] <-  (100 * (df$AdjClose[i] - df$AdjClose[i-1]) / (df$Volatility[i] * df$Close[i] / 16)) + df$NP[i-1]
        if(is.na(df$NP[i]))
          df$NP[i] <- df$NP[i-1]
      }
      df <- merge(df, filter(R, Class==df$Class[1]) %>%  select(Date, A), by="Date") # R obtained from before
      df$R  <- (df$NP - df$A) 
      df$O <- c(rep(NA, 256*5), diff(df$R, lag=256*5)) / 256*5
      df$Forecast <- EMA(-df$O, 30) * 7.27
      df$Trade <- lag(df$Forecast)
      df$Trade <- df$Trade * FDM
      df$Trade <- lag(ifelse(df$Trade > 20, 20, ifelse(df$Trade < -20, -20, df$Trade )) / 10)
      df$Excess <- df$Return * df$Position * df$Trade * IDM 
      jumbo[[n]] <- select(df, Date, Excess)
    }
    portfolio <- merge_portfolio_list(jumbo)
    res <- portfolio_summary(as.matrix(portfolio[,-1]), dates = portfolio$Date, plot_stats = TRUE, symbol_wise = TRUE, benchmark.dates = benchmark.dates, benchmark.returns = benchmark.returns) 
    print(res$Aggregate %>% unlist)
  }
}

# Strategy 23
{
  jumbo <- list()
  IDM = 2.41
  FDM <- 1.55
  for(n in names(BackAdj)) {
    df <- BackAdj[[n]]
    df$Volatility = calculate_volatility(df$Return)
    df$Position = lag(target_vol / df$Volatility)
    df$Forecast8 <- (EMA(df$AdjClose, 8) -  EMA(df$AdjClose, 32) ) / (df$Close * df$Volatility / 16) * 5.95
    df$Forecast16 <- (EMA(df$AdjClose, 16) -  EMA(df$AdjClose, 64)) / (df$Close * df$Volatility / 16) * 2.79
    df$Forecast32 <- (EMA(df$AdjClose, 32) -  EMA(df$AdjClose, 128)) / (df$Close * df$Volatility / 16) * 2.79
    df$Forecast64 <- (EMA(df$AdjClose, 64) -  EMA(df$AdjClose, 256)) / (df$Close * df$Volatility / 16) * 1.91
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
    df$Volatility = calculate_volatility(df$Return)
    df$Position = lag(target_vol / df$Volatility)
    df$Return[is.na(df$Return)] <- 0
    skew1 <- -rollapply(df$Return, width=60, skew,  fill=NA, align="right")
    skew2 <- -rollapply(df$Return, width=120, skew,  fill=NA, align="right")
    skew3 <- -rollapply(df$Return, width=240, skew,  fill=NA, align="right")
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

### Production strategy VERY IMPORTANT this is the backtest of the production strategy  

# The backbone will be strategy 11
{
  { # functions
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
  multiple_DC <- function(adjclose, close, risk, spans=c(20, 40, 80, 160, 320), scalars=c(0.67, 0.70, 0.73, 0.74, 0.74), cap=20, period=252) {
    n <- length(spans)
    DCs <- lapply(1:n, function(i) {dc <- DonchianChannel(adjclose, spans[i]); (adjclose - dc[,2]) / abs(dc[,1] - dc[,3])})
    DCs <- lapply(1:n, function(i) EMA(na.locf(DCs[[i]], na.rm=F) * 40, spans[i]/4) * scalars[i] )
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
  multiple_TII <- function(adjclose, close, risk, spans=c(21, 63, 126, 252)) {
    n <- length(spans)
    TII <- lapply(1:n, function(i) TII(adjclose, P = spans[i]) / 5)
    forecast <- rowMeans(do.call(cbind, TII))
    return(forecast)
  }
  # basis and risk are in percentage
  multiple_Carry <- function(basis, expiry_difference, risk, spans=c(21, 63, 126), scalar=30, expiry_span=12, cap=20) {
    n <- length(spans)
    Carry <- (basis / (expiry_difference / expiry_span)) / ( risk )
    Carry <- na.locf(Carry, na.rm=FALSE); Carry[is.na(Carry)] <- 0
    EMAs <- lapply(1:n, function(i) EMA(Carry, spans[i]) * scalar)
    EMAs <- lapply(1:n, function(i) ifelse(EMAs[[i]] > cap, cap, ifelse(EMAs[[i]] < -cap, -cap, EMAs[[i]] ) ))
    forecast <- rowMeans(do.call(cbind, EMAs))
    return(forecast)
  }
  multiple_cor <- function(return.x, return.y, spans=c(20, 60, 120, 250)) {
    n <- length(spans)
    COR <- lapply(1:n, function(i) runCor(return.x, return.y, n = spans[i]) %>% na.locf(., na.rm=FALSE))
    forecast <- rowMeans(do.call(cbind, COR))
    return(forecast)
  }
  relative_volatility <- function(volatility, period=2520) {
    return(rollapply(na.locf(volatility, fromLast = TRUE), width=period, FUN=mean, partial=TRUE, fill=NA,align="right"))
  }
  normalize_price <- function(adjclose, close, volatility, period=252) {
    np <- rep(NA, length(close))
    np[1] <- 0
    for(i in 2:length(close)) {
      np[i] <-  (100 * (adjclose[i] - adjclose[i-1]) / (close[i] * volatility[i] / sqrt(period))) + np[i-1]
      if(is.na(np[i]))
        np[i] <- np[i-1]
    }
    return(np)
  }
  cross_sectional_momentum <- function(instrument_np, class_np, spans=c(20, 40, 80, 160, 320), scalars=c(108.5, 153.5, 217.1, 296.8, 376.3), cap=20) {
    n <- length(spans)
    relative_np  <- (instrument_np - class_np) / 100
    Os <- lapply(1:n, function(i) EMA(c(rep(NA, spans[i]), diff(relative_np, lag=spans[i]) / spans[i]), ceiling(spans[i]/4)) * scalars[i])
    Os <- lapply(1:n, function(i) ifelse(Os[[i]] > cap, cap, ifelse(Os[[i]] < -cap, -cap, Os[[i]] ) ))
    forecast <- rowMeans(do.call(cbind, Os))
    return(forecast)
  }
  returns_skew <- function(returns, spans=c(60, 120, 240), scalars=c(33.3, 37.2, 39.2), cap=20) {
    skewf <- Rfast::skew
    n <- length(spans)
    returns[is.na(returns)] <- 0
    Skews <- lapply(1:n, function(i) -rollapply(returns, width=spans[i], skew,  fill=NA, align="right"))
    Skews <-  lapply(1:n, function(i) replace(Skews[[i]], is.na(Skews[[i]]), 0))
    Skews <-  lapply(1:n, function(i) EMA(Skews[[i]], ceiling(spans[i]/4)) * scalars[i])
    Skews <- lapply(1:n, function(i) cap_forecast(Skews[[i]], cap))
    forecast <- rowMeans(do.call(cbind, Skews))
    return(forecast)
  }
  returns_skew_2 <- function(returns, spans=c(60, 120, 240), scalars=c(23, 24, 25), cap=20) {
    n <- length(spans)
    returns[is.na(returns)] <- 0
    Skews <- lapply(1:n, function(i) {
      mult <- min(spans[i]*10, length(returns)) 
      x <- rollapply(returns, width=spans[i], skew,  fill=NA, align="right") ;
      x[is.na(x)] <- 0
      x - runMean(x, n = mult)
    })
    Skews <-  lapply(1:n, function(i) replace(Skews[[i]], is.na(Skews[[i]]), 0))
    Skews <-  lapply(1:n, function(i) EMA(-Skews[[i]], ceiling(spans[i]/4)) * scalars[i])
    Skews <- lapply(1:n, function(i) cap_forecast(Skews[[i]], cap))
    forecast <- rowMeans(do.call(cbind, Skews))
    return(forecast)
  }
  returns_kurtosis <- function(returns, spans=c(60, 120, 240), scalars=c(8, 5.70, 3.75), cap=20) {
    n <- length(spans)
    returns[is.na(returns)] <- 0
    Kurtosis <- lapply(1:n, function(i) -rollapply(returns, width=spans[i], kurt,  fill=NA, align="right"))
    Kurtosis <-  lapply(1:n, function(i) replace(Kurtosis[[i]], is.na(Kurtosis[[i]]), 0))
    Kurtosis <-  lapply(1:n, function(i) EMA(Kurtosis[[i]], ceiling(spans[i]/4)) * scalars[i])
    Kurtosis <- lapply(1:n, function(i) cap_forecast(Kurtosis[[i]], cap))
    forecast <- rowMeans(do.call(cbind, Kurtosis))
    return(forecast)
  }
  multiple_ER <- function(close, spans=c(20, 60, 120, 250, 500), scalars=c(70.1, 90.2, 105.2, 117.2, 127.37), avgs=c(0.197, 0.112, 0.079, 0.053, 0.038), cap=20) {
    n <- length(spans)
    ERs <- lapply(1:n, function(i) sqrt(rollapply(close, width=spans[i], ER, fill=NA, align="right")) - sqrt(avgs[i]) )
    ERs <- lapply(1:n, function(i) ERs[[i]]  * scalars[i] )
    ERs <- lapply(1:n, function(i) cap_forecast(ERs[[i]]))
    forecast <- rowMeans(do.call(cbind, ERs))
    return(forecast)
  }

  
}
    
  
  # load all CMC and other cash data (to test with CMC data uhmm....)
  {
  dir <- "/home/marco/trading/Systems/Monopoly/Data/Scraping"
  instruments_file  <- paste0(dir, "/", "INSTRUMENTS.csv")
  df <- read_csv(instruments_file, show_col_types = FALSE, col_names = FALSE)
  CMC_data <- list()
  for(i in 1:nrow(df)) {
    symbol <- df[i,2,drop=TRUE]
    platform <- df[i,3,drop=TRUE]
    lagged <- ifelse(platform == "CMC", TRUE, FALSE)
    print(symbol)
    CMC_data[[symbol]] <- load_cmc_cash_data(symbol, dir, TRUE)
  }
  }
  
  # Final Backtest
  {
    # A subset of instrument I might actually trade
    CMC_selection <- c("ZN","GG","CC","KC","HG","ZC","CT","CL","IM","GC","HE","LE","LS","NG","ZO","OJ","ZR","ZS","ES","SB","DX","ZW","D6")
    Assets <- BackAdj# or BackAdj[CMC_selection]
    results <- list()
    forecasts <- list()
    exposures <- list()
    returns <- list()
    vols <- list()
    target_vol <- 0.25
    IDM = 2.5
    FDMtrend <- 1.33
    FDMcarry <- 1.05 # so small?
    FDMcsm <- 1.4
    FDMskew <- 1.18
    FDM <- 1
    # Apply relative volatility
    relative_vol <- FALSE
    # Apply Marker Correlation
    market_cor <- FALSE
    # Symbol-wise results
    symbol_wise <- FALSE
    # Strategies weights
    weights <- list("Long"=0, "Trend"=0, "Carry"=0, "CSM"=0, "Skew"=0, "Test"=1)
    if(sum(unlist(weights)) != 1)
      warning("Strategy weights do not sum to zero")
    # Asset class indices
    if(weights[["CSM"]] > 0) {
      NPs <- list()
      for(n in names(Assets)) {
        df <- Assets[[n]]
        df$Volatility = calculate_volatility(df$Return)
        df$NP <- normalize_price(df$AdjClose, df$Close, df$Volatility)
        df$dNP = c(0, diff(df$NP))
        NPs[[n]] <- select(df, Date, Symbol, Class, NP, dNP)
      }
      allNPs <- do.call(rbind, NPs)
      Asset_class_indices <- group_by(allNPs, Class, Date) %>% summarise(Symbol=Symbol, R=mean(dNP)) %>% 
        arrange(Class, Date) %>% select(-Symbol)  %>%  unique %>% group_by( Class) %>% mutate(A=cumsum(R)) %>% ungroup()
    }
    # iterate over symbols
    for(symbol in names(Assets)) {
      cat(paste(symbol, ""))
      df <- Assets[[symbol]]
      df$Volatility = calculate_volatility(df$Return)
      df$Position = lag(target_vol / df$Volatility)
      df$Return[is.na(df$Return)] <- 0
      df$ForecastTrend <- df$ForecastCarry <- df$ForecastCSM <- df$ForecastSkew <- df$ForecastTest <- 0
      
      # Relative volatility (strategy 13, improvement is minimal, and we only apply it to trend)
      df$M <- 1
      if(relative_vol) {
        df$RV <- relative_volatility(df$Volatility) 
        df$Q <- sapply(1:length(df$RV), function(i) sum(df$RV[i] > df$RV[1:i], na.rm=TRUE) / i)
        df$M <- EMA(2 - 1.5 * df$Q, 10)
      }
      
      # Correlation with market
      df$Cor <- 1
      if(market_cor) {
        df <- merge(df, select(BackAdj$ES, Date, Return) %>% mutate(ES=Return) %>% select(-Return), by="Date") 
        df$Cor <- multiple_cor(df$Return %>% na.locf(na.rm=F), df$ES %>% na.locf(na.rm=F))
        df$Cor <- -df$Cor * 0.75 + 1.25
      }
      
      # Trend-following (strategy 9)
      if(weights[["Trend"]]  > 0) {
        df$ForecastEMA <- multiple_EMA(df$AdjClose, df$Close, df$Volatility) 
        df$ForecastDC <- multiple_DC(df$AdjClose, df$Close, df$Volatility) 
        df$ForecastKF <- multiple_KF(df$AdjClose, df$Close, df$Volatility) 
        df$ForecastTII <- multiple_TII(df$AdjClose, df$Close, df$Volatility)
        df$ForecastTrend <- rowMeans(cbind(df$ForecastEMA, df$ForecastDC, df$ForecastKF, df$ForecastTII)) * FDMtrend * df$M * df$Cor
        df$ForecastTrend <- cap_forecast(df$ForecastTrend) 
      }
      # Carry (strategy 10)
      if(weights[["Carry"]]  > 0) {
        df$ForecastCarry <- multiple_Carry(df$Basis, df$Basis_distance, df$Volatility)  * FDMcarry 
        df$ForecastCarry <- cap_forecast(df$ForecastCarry)
      }
      # Cross-sectional momentum (strategy 19)
      if(weights[["CSM"]]  > 0) {
        df <- merge(df, filter(Asset_class_indices, Class==df$Class[1]) %>%  select(Date, A), by="Date") # Asset_class_indices obtained from before
        df$NP <- normalize_price(df$AdjClose, df$Close, df$Volatility)
        df$ForecastCSM <- cross_sectional_momentum(df$NP, df$A) * FDMcsm 
        df$ForecastCSM <- cap_forecast(df$ForecastCSM)
      }
      # Skewness (strategy 24)
      if(weights[["Skew"]]  > 0) {
        df$ForecastSkew <- returns_skew(df$Return) * FDMskew  
        df$ForecastSkew <- cap_forecast(df$ForecastSkew)
      }
      
      if(weights[["Test"]]  > 0) {
        logitm <- 30; logitr <- 25
        rsi <- multiple_RSI(df$Close%>% na.locf(na.rm=F)) / 20
        rsi <- -((invlogit(logitm*rsi-logitr) + invlogit(logitm*rsi+25))-1) * 20
        forecast <- EMA(rsi , 5)     
        forecast <- multiple_RSI(df$Close%>% na.locf(na.rm=F))
        df$ForecastTest <-   forecast
      }   
      
      {  # Tests
      # # Kurtosis
      # if(weights[["Test"]]  > 0) {
      #   df$ForecastTest <- returns_kurtosis(df$Return) * FDMkurtosis
      #   forecast <- rollapply(df$Return, width=20, kurt,  fill=NA, align="right")  
      #   df$ForecastTest <- cap_forecast(df$ForecastTest)
      # }
      # Acceleration
      # if(weights[["Test"]]  > 0) {
      #   df$Forecast16 <- (EMA(df$AdjClose, 16) -  EMA(df$AdjClose, 64)) / (df$Close * df$Volatility / 16) * 4.1
      #   df$Forecast32 <- (EMA(df$AdjClose, 32) -  EMA(df$AdjClose, 128)) / (df$Close * df$Volatility / 16) * 2.79
      #   df$Forecast64 <- (EMA(df$AdjClose, 64) -  EMA(df$AdjClose, 256)) / (df$Close * df$Volatility / 16) * 1.91
      #   df$Forecast128 <- (EMA(df$AdjClose, 128) -  EMA(df$AdjClose, 512)) / (df$Close * df$Volatility / 16) * 1.50
      #   df$Acc16 <- c(rep(NA, 16), diff(df$Forecast16, 16)) * 1.90
      #   df$Acc32 <- c(rep(NA, 32), diff(df$Forecast32, 32)) * 1.98
      #   df$Acc64 <- c(rep(NA, 64), diff(df$Forecast64, 64)) * 2.05
      #   df$Acc128 <- c(rep(NA, 128), diff(df$Forecast128, 128)) * 2.10
      #   df$ForecastTest <- cap_forecast(rowMeans(cbind(df$Acc16 , df$Acc32 , df$Acc64, df$Acc128)) * 1.55)
      # }
      
      # COT
      # COT <- read_csv("/home/marco/trading/Systems/Monopoly/COT.csv")
      # select(COT, c("Market and Exchange Names", "As of Date in Form YYYY-MM-DD", "Noncommercial Positions-Long (All)","Noncommercial Positions-Short (All)", "Commercial Positions-Long (All)","Commercial Positions-Short (All)")) -> a
      # colnames(a) <- c("Name", "Date", "NC_long", "NC_short", "C_long", "C_short")
      # mutate(a, NC=NC_long/(NC_long+NC_short),C=C_long/(C_long+C_short) ) -> COT
      # if(weights[["Test"]]  > 0) {
      #     df$COT <- to_load[to_load$Symbol==symbol,]$COT %>% gsub('\"', '', .)
      #     if(is.na(df$COT[1]) | symbol =="CL"| symbol =="KO"| symbol =="TG") {
      #         df$ForecastTest <- 0
      #         
      #     } else {
      #     cot <- filter(COT, Name==df$COT[1])  %>%  select(Date, C)
      #     df_ <- df
      #     df_ <- merge(df_, cot, by="Date", all = TRUE, incomparables = NA)
      #     df <- arrange(df_, Date) %>% mutate(C = na.locf(C, na.rm=F))
      #     df$C <- lag(df$C );
      #     r <- c(0,diff(log(df$C))); r[r==0] <- NA; r = na.locf(r, na.rm=F)
      #     f <- AbsoluteStrength(df$C, 20) /  calculate_volatility(r) * 28
      #     f <- multiple_AS(df$C, df$C, calculate_volatility(r))
      #     forecast <- f
      #     df$ForecastTest <- (forecast)
      #     }
      # }
      }  
      # Final trade
      df$Forecast <- ( weights[["Trend"]] * df$ForecastTrend + 
                       weights[["Carry"]] * df$ForecastCarry + 
                       weights[["CSM"]] * df$ForecastCSM + 
                       weights[["Skew"]] * df$ForecastSkew + 
                       weights[["Test"]] * df$ForecastTest) / 10 
      df$Forecast <- lag(df$Forecast)
      df$Excess <- df$Return * df$Position * df$Forecast * IDM 
      forecasts[[symbol]]  <-   select(df, Date, Forecast) 
      exposures[[symbol]]  <-  mutate(df, Exposure=Position * Forecast) %>% select(Date, Exposure) 
      returns[[symbol]]  <-  select(df, Date, Return)
      vols[[symbol]]  <-  select(df, Date, Volatility)
      results[[symbol]] <- select(df, Date, Excess)
    } # end of symbol sloop
    
    print("")
    portfolio <- merge_portfolio_list(results)
    weights <- 1 / length(names(Assets)) # equal weights per instrument
    res <- portfolio_summary(as.matrix(portfolio[,-1]) * weights, dates = portfolio$Date, plot_stats = TRUE, symbol_wise = symbol_wise  ) 
    print(res$Aggregate %>% unlist)
    if(symbol_wise) {
      res$Symbols$Class <- lapply(Assets[names(results)], function(x) x$Class[1]) %>% unlist
      group_by(res$Symbols, Class) %>% summarise(SR_mean=mean(`Sharpe ratio`, na.rm=T), SR_sd=sd(`Sharpe ratio`, na.rm=T))
    }
    ## Some figures takes from the Risk Management section
    # Figure 97: Portfolio volatility, check it is in line with target volatility
    # full_df_exposures <- Reduce(function(...) full_join(..., by = "Date", all = TRUE, incomparables = NA), exposures) %>% arrange(Date)
    # full_df_returns <- Reduce(function(...) full_join(..., by = "Date", all = TRUE, incomparables = NA), returns) %>% arrange(Date)
    # full_df_vols<- Reduce(function(...) full_join(..., by = "Date", all = TRUE, incomparables = NA), vols) %>% arrange(Date)
    # a <- sapply(181:nrow(full_df_returns),  function(i) { w <- as.numeric(full_df_exposures[i,-1]); w[is.na(w)] <- 0; S <-  cov(full_df_returns[(i-180):i,-1], use="pairwise.complete.obs"); S[is.na(S)] <- 0; sqrt( w %*% S %*% w  )  } )
    # plot.ts(a*100); abline(h=target_vol*100)
    # a <- rowSums(abs(full_df_exposures[,-1] * full_df_vols[,-1]), na.rm=T)
  }
  
}
# Correlation between trend filters
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
#
# Correlation between strategies
#                         benchmark       trend           carry           skew          acc
# benchmark                1.00           -0.01           -0.15           0.33         -0.05
# trend                   -0.01            1.00            0.26          -0.02          0.81
# carry                   -0.15            0.26            1.00           0.17          0.00
# skew                     0.33           -0.02            0.17           1.00         -0.26
# acc                     -0.05            0.81            0.00          -0.26          1.00





## Carver post on Skew/Kurtosis
# https://qoppac.blogspot.com/2019/10/skew-and-expected-returns.html
# General plotting by symbol
func <- carver_ratio2
Assets <- Symbols_D1_barchart
res <- list()
for(symbol in names(Assets)) {
  print(symbol)
  df <- Assets[[symbol]]
  x <- na.omit(df$Return)
  res[[df$Name[1]]] <- montecarlo_resampler(x, 100, func) 
}
res[["RANDOM"]] <- montecarlo_resampler(rnorm(100000, 0, 0.005), 100, func)
full <- do.call(cbind, res) 
full <- full[,order(colMedians(full, na.rm=T))]
boxplot(full, las=2, cex.axis=0.75); abline(h=0)
# Global plot feature vs return
func <- carver_ratio
a <- lapply(Assets, function(df){x <- na.omit(df$Return); r <- na.omit(df$Return); c(func(x), mean(r))}) %>% do.call(rbind,.)
plot(a, type="n"); text(a[,1], a[,2], rownames(a));abline(h=0)
a <- lapply(Assets, function(df){x <- na.omit(df$Return); r <- na.omit(df$Return); c(func(x), mean(r)/sd(r)*16)}) %>% do.call(rbind,.)
rownames(a) <- sapply(Assets, function(x)x$Name[1])
plot(a, type="n" ); text(a[,1], a[,2], rownames(a));abline(h=0)
# Rolling skew vs adjusted returns
func <- carver_ratio2
# this is to calculate 10 years long rolling values
assets_long_term_values <- list()
for(symbol in names(Assets)) {
  df <- Assets[[symbol]] 
  assets_long_term_values[[symbol]] <- data.frame(Date=as.Date(df$Date), 
                                                  long_term_values=rollapply(df$Return, width=2520, func, fill=NA, align="right"))
}
# merge them and get the 10 years long average of all assets. To be used later 
full_long_term <- Reduce(function(...) full_join(..., by = "Date", all = TRUE, incomparables = NA), assets_long_term_values) %>% arrange(Date)
colnames(full_long_term) <- c("Date", names(Assets))
long_term_cs <- data.frame(Date=full_long_term[,1], LTCS=rowMedians(full_long_term[,-1] %>% as.matrix(), na.rm = T))
res_func <- list()
for(freqs in c("10", "20", "60", "120", "250", "500")){ 
  freq <- as.numeric(freqs)
  res_func[[freqs]]  <- data.frame(matrix(NA, ncol=6, nrow=1))
  colnames(res_func[[freqs]])  <- c("Symbol", "Date", "Values", "Means", "Vols", "LTV")
  print(freqs)
  for(symbol in names(Assets)) {
    df <- Assets[[symbol]] 
    x <- df$Return # either Close or AdjClose
    return <- df$Return
    return[is.na(return)] <- 0
    date <- as.Date(df$Date)
    long_term_values <- 0#assets_long_term_values[[symbol]][,2]
    #values <- rollapply(x, width=freq, func, fill=NA, align="right") 
    values <- func(return, n=freq) 
    means <- runMean(return, freq)
    vols <- runSD(return, freq)
    means <- lead(means, freq)
    vols <- lead(vols, freq)
    res_func[[freqs]]  <- rbind(res_func[[freqs]] , data.frame(Symbol=symbol, Date=as.Date(date), Values=values, Means=means, Vols=vols, LTV=long_term_values))
  }  
  res_func[[freqs]]  <- data.frame(f=freq, res_func[[freqs]])
}
full <- do.call(rbind, res_func)
# full <- group_by(full, f, symbol) %>% 
#   summarise(f=f, symbol=symbol, date=as.Date(date),means=means, vols=vols, values=values, total_avg=median(values, na.rm=T), moving_values = AbsoluteStrength(values %>% na.locf(na.rm=F), f))
full <- merge(full, long_term_cs, by="Date")
full$SR <- (full$Means / full$Vols * 16) 
# Absolute: versus against some meaningful fixed value (zero?)
full$Cond_abs <- full$Values > 0
# Relative to this particular assets history (10 years)
full$Cond_ts <- full$Values > full$LTV
# Relative to the current cross sectional average across all assets 
full$Cond_cs <- full$Values > full$LTCS
# Choose one
full$h <- full$Cond_abs
#full <- filter(full, !is.na(h))
a <- full %>% filter(!is.na(h)) %>% group_by(f, h) %>% summarize(SR_m=median(SR, na.rm=T), SR_s=mad(SR, na.rm=T)/sqrt(n()))
ggplot(a ) + geom_line(aes(f, SR_m, color=h)) +  geom_errorbar(aes(f, y=SR_m, ymin=SR_m-SR_s, ymax=SR_m+SR_s, color=h)) + theme(text = element_text(size=32))
full %>% NaRV.omit() -> full; lapply(unique(full$f), function(x) t.test(full$sr[full$f==x & full$h], full$sr[full$f==x & !full$h])$statistic  ) %>% unlist

