## This file contains important procedures to process futures contracts data and create back-adjusted prices

{
  library(tidyverse)
  library(mvtnorm)
  library(quantmod)
  library(forecast)
  library(mvtnorm)
  library(reshape2)
  library(tsibble)
  library(data.table)
  source("/home/marco/trading/Systems//Common/Common.R")
  source("/home/marco/trading/Systems//Common/Reports.R")
  source("/home/marco/trading/Systems//Common/Indicators.R")
  source("/home/marco/trading/Systems//Common/RiskManagement.R")
  setwd("/home/marco/trading/Systems/Monopoly/")
  Sys.setlocale("LC_TIME", "en_US.UTF-8")
  theme_set(theme_bw(base_size = 32))
}


# Load future contracts, we expect all the contract to be in the directory, and the contract order in the file order.txt
load_future_contracts <- function(symbol, dir, order_years=c(80:99,0:30), order_months=c("f", "g", "h", "j", "k", "m", "n", "q", "u", "v", "x", "z")) {
  order_years <- sapply(order_years, function(x) ifelse(x < 10, paste0("0", as.character(x)), as.character(x)))
  order_comb <- apply(expand.grid(order_months, order_years), 1, function(x) tolower(paste0(symbol, paste0(x, collapse = ""))))
  files <- list()
  # load all the contracts, adding an infinite before and after each contract (so you can recognize start and end)
  for(l in list.files(dir, pattern = ".csv")) {
    f <- fread(paste0(dir, "/", l)) %>% select(Time, Last) %>% rename(Date=Time)
    f$Date <- as.Date(f$Date, format="%m/%d/%Y") 
    f <- arrange(f, Date)
    f <- rbind(data.frame(Date=f$Date[1]-1, Last=NaN), f, data.frame(Date=f$Date[length(f$Date)]+1, Last=NaN))
    files[[sub(".csv", "", l)]] <- f
  }
  # Join all the contracts and sort them by Date and contracts order
  df <- Reduce(function(...) full_join(..., by="Date", all=T), files) %>% arrange(Date)
  colnames(df) <- c("Date", names(files))
  df <- as.data.frame(df)
  #order <- scan(paste0(dir, "/", order_file), what="string") %>% tolower()
  order <- colnames(df)[-1][na.omit(match(order_comb, colnames(df)[-1]))]
  df <- df[,c("Date", order)] %>% arrange(Date)
  return(df)
}

# create a backadjusted future contract
# N: days to expiration to rollover
backadjust_future <- function(df, N=1, period=365) {
  month_code <- setNames(1:12,c("f", "g", "h", "j", "k", "m", "n", "q", "u", "v", "x", "z"))
  m <- as.matrix(df[,-1]) # the first column is supposed to be the Date
  d <- as.Date(df[,1]) # dates
  sc <- rep(NA, ncol(m)) # contracts starts
  ec <- rep(NA, ncol(m)) # contracts ends
  ym <- rep(yearmonth(0), ncol(m)) # contracts name in yearmonth
  for(j in 1:ncol(m)) {
    a <- which(is.nan(m[,j]))
    sc[j] <- a[1]+1
    ec[j] <- a[2]-1
    code_j <- colnames(m)[j]
    year_j <- as.numeric(substr(code_j, nchar(code_j)-1, nchar(code_j))); year_j <- ifelse(year_j > 40, year_j + 1900, year_j + 2000)
    month_j <- as.numeric(month_code[substr(code_j, nchar(code_j)-2, nchar(code_j)-2)]);
    ym[j] <- yearmonth(paste0(year_j, "-", month_j))
  }
  j <- 1; 
  i <- 1; 
  adjclose <- rep(NA, nrow(m)); # the final continous backadjusted price
  close <- rep(NA, nrow(m)); # the current contract unadjusted price
  basis <- rep(NA, nrow(m)); # basis as log differences
  basis_price <- rep(NA, nrow(m)); # basis as price differences
  basis_distance <- rep(NA, nrow(m)); # months between basis contracts
  basis_gordon <- rep(NA, nrow(m)); # months between basis contracts as defined in gorton et. al 2013 
  spot <- rep(NA, nrow(m)); # implied spot price as defined in gorton et. al 2013 
  rollover <- rep(FALSE, nrow(m)); # rollover dates
  contract <- rep(NA, nrow(m)); # current contract
  maturity <- rep(NA, nrow(m)); # days to maturity
  difference <- rep(NA, nrow(m)); # returns considering rollover (in price differences)
  ret <- rep(NA, nrow(m)); # returns considering rollover (in log price)
  first <- NA # first value to use to adjust
  last <- 0 # last value to use to adjust
  for(i in 3:(nrow(m)-1)) { # this assume first and last entries are Inf
    ret[i] <- log(m[i,j] / m[i-1,j])
    difference[i] <- m[i,j] - m[i-1,j]  
    if(j < ncol(m) && i >= ec[j]-N+2) { # we have not reached the last contract and we have not reached the last holding day
      j <- j + 1;
      rollover[i] <- TRUE
      ret[i] <- log(m[i,j] / m[i-1,j])
      difference[i] <- m[i,j] - m[i-1,j]  
    }
    close[i] <- m[i,j]
    adjclose[i] <- m[i,j] - m[i-1,j]
    contract[i] <- colnames(m)[j]
    maturity[i] <- ec[j] - i
    if(j < ncol(m)) {
      k <- ifelse(j+1 > ncol(m), ncol(m), j+1 )
      basis[i] <- log(m[i,j]) - log(m[i,k]) # simple log difference between contracts
      basis_price[i] <- m[i,j] - m[i,k] # simple price difference between contracts
      basis_distance[i] <- ym[k] - ym[j] # distance in months
      basis_gordon[i] <- period * ((m[i,j] / m[i,k]) - 1) / ((ec[k]-i) - (ec[j]-i)) # as defined in Gorton et al. 2013
      #spot[i] <-  m[i,j] * (1 + basis[i] / period * (ec[j]-i)) # as defined in Gorton et al. 2013
    }
    if(!(is.na(m[i,j]) | is.nan(m[i,j]))) {
      last <- m[i,j]
      if(is.na(first))
        first <- m[i,j]
    }
  } 
  adjclose[is.na(adjclose)] <- 0
  adjclose <- first + cumsum(adjclose)
  adjclose <- adjclose + (last - adjclose[length(adjclose)])
  final <- data.frame(
    Date=df[,1], Close=close, AdjClose=adjclose, Return=ret, Difference=difference, Adjs=adjclose-close, 
    Contract=contract, Rollover=rollover, Maturity=maturity,
    Basis=basis, Basis_price=basis_price, Basis_gordon=basis_gordon, Basis_distance=basis_distance, Spot=spot )
  delete <- c()
  for(i in 1:nrow(m)) # if a row wall all NAs (probably because it just stored a Nan) in the original data, remove it from the final result
    if(all(is.na(m[i,])))
      delete <- c(delete, i)
  if(length(delete) > 0)
    final <- final[-delete,]
  return(final)
}

# create a backadjusted future spread contract
backadjust_spread <- function(df1, df2, N1=1, N2=1, mult=c(1,1), func=backadjust_future) {
  c1 <- func(df1, N1)
  c2 <- func(df2, N2)
  z <- merge(c1, c2, by="Date", all=TRUE)
  z$Adjs.x <- zoo::na.locf.default(z$Adjs.x, na.rm = FALSE, fromLast = TRUE)
  z$Adjs.y <- zoo::na.locf.default(z$Adjs.y, na.rm = FALSE, fromLast = TRUE)
  close <- z$Close.x*mult[1] - z$Close.y*mult[2]
  backadj <- close + (z$Adjs.x*mult[1] - z$Adjs.y*mult[2])
  z$Return.x <- z$Return.x * mult
  z$Return.y <- z$Return.y * mult
  return(data.frame(Date=z[,1], Close1=z$Close.x, Close2=z$Close.y, 
                    Backadj1=z$AdjClose.x, Backadj2=z$AdjClose.y, 
                    Rollover1=z$Rollover.x, Rollover2=z$Rollover.y, 
                    Adj1=z$Adjs.x , Adj2=z$Adjs.y, 
                    Return1=z$Return.x, Return2=z$Return.y,
                    Close=close, AdjClose=backadj, Return=z$Return.x-z$Return.y))
}

# Load future contracts, we expect all the contract to be in the directory, and the contract order in the file order.txt
load_cash_contract <- function(f) {
  df <- read_csv(f, show_col_types = FALSE) %>% 
    select(Time, Last) %>% rename(Date=Time) %>% mutate(Date = as.Date(Date, format="%m/%d/%Y")) %>%    arrange(f, Date) %>% 
    mutate(Symbol=toupper(sub("y00.csv", "", f)), Return = c(0,diff(log(Last))) ) 
  
  return(df)
}



# build up the rollover curve given a df from load_future_contracts, assume only N x T data.frame
rollover_curve <- function(df, forward=2, lm=FALSE) { 
  curve_diff <- rep(NA, nrow(df))
  curve_ret1 <- rep(NA, nrow(df))
  curve_ret2 <- rep(NA, nrow(df))
  if(lm) {
    curve_lm <- rep(NA, nrow(df))
    curve_err <- rep(NA, nrow(df))
  }
  m <- log(as.matrix(df[,-1]))
  for(i in 2:nrow(m)) {
    b <- na.omit(m[i, ])
    b_prev <- na.omit(m[i-1, ])
    if(length(b) < forward) 
      next
    b <- b[1:forward]
    x <- 1:length(b)
    if(lm) {
      fit <- lm(b ~ x)
      curve_lm[i] <- -12*coef(fit)[2]
      curve_err[i] <- sqrt(diag(vcov(fit)))[2]
    }
    curve_diff[i] <- (b[forward] - b[1])
  }
  if(lm)
    res <- data.frame(Date=df$Date, Basis=curve_diff, Lm=curve_lm, Err=curve_err)
  else
    res <- data.frame(Date=df$Date, Basis=curve_diff)
  return(res)
  
}




# create a intramarket future spread
intramarket_spread <- function(df, N=1, D=1) {
  m <- as.matrix(df[,-1]) # the first column is supposed to be the Date
  d <- as.Date(df[,1])
  sc <- rep(NA, ncol(m))
  ec <- rep(NA, ncol(m))
  for(j in 1:ncol(m)) {
    a <- which(is.nan(m[,j]))
    sc[j] <- a[1]+1
    ec[j] <- a[2]-1
  }
  j <- 1; 
  i <- 1; 
  rollover <- rep(NA, nrow(m)); 
  spread <- rep(NA, nrow(m)); 
  ret <- rep(NA, nrow(m)); 
  for(i in 2:nrow(m)) { 
    ret[i] <- log(m[i,j] / m[i-1,j])
    if(j < ncol(m))
      spread[i] <- m[i,j] - m[i,j+D]
    if(j < (ncol(m)-D) && (i >= ec[j]-N+2)) {
      j <- j + 1;
      rollover[i] <- TRUE
      ret[i] <- log(m[i,j] / m[i-1,j])
      spread[i] <- m[i,j] - m[i,j+D]
    }
    
  } 
  return(data.frame(Date=df[,1], Spread=spread, Return=ret, Rollover=rollover))
}




# Load futures data and calculate stuff
{
  stop()
  setwd( "/home/marco/trading/Historical Data/Barchart/")
  to_load <- read_csv("Instrument_List.csv")
  # load the full futures contracts
  Futures <- list()
  for(i in 1:nrow(to_load) ){
    symbol <- as.character(to_load[i,1])
    if(!is.null(Futures[[symbol]]))
      next
    print(symbol)
    dir <- as.character(to_load[i,2])
    Futures[[symbol]] <- load_future_contracts(symbol, dir)
  }
  write_rds(Futures, "/home/marco/trading/Historical Data/Barchart/Futures.RDS")
  # Backadjust the prices
  BackAdj <- list()
  for(symbol in names(Futures)) {
    if(!is.null(BackAdj[[symbol]])) {
      next
    }
    print(symbol)
    BackAdj[[symbol]] <- backadjust_future(Futures[[symbol]], N=2)
    BackAdj[[symbol]]$Symbol <- symbol
    BackAdj[[symbol]]$Name <- to_load$Name[to_load$Symbol == symbol]
    BackAdj[[symbol]]$Class <- to_load$Class[to_load$Symbol == symbol]
  }
  write_rds(BackAdj, "/home/marco/trading/Historical Data/Barchart/BackAdj.RDS")
  # Calculate the forward curve ?
  Basis <- list()
  for(a in  names(Futures)) 
    Basis[[a]] <- rollover_curve(Futures[[a]], forward = 2, lm = FALSE) # linear fitting can be wierd 
  # Load cash data
  setwd( "/home/marco/trading/Historical Data/Barchart/OTHER/Cash/")
  Cash <- list()
  for(f in list.files(".", ".csv")) {
    symbol <- toupper(sub("y00.csv", "", f))
    if(system(paste("wc -l ", f, " | cut -f 1 -d \" \""), intern = TRUE) == "1")
      next
    Cash[[symbol]] <- load_cash_contract(f)
  }
  
  # create summary long-only plots
  setwd("/home/marco/trading/Systems/Monopoly/")
  dir.create("Plots/LongOnly")
  setwd("Plots/LongOnly")
  for(symbol in names(BackAdj)){
    r <- BackAdj[[symbol]]$Return
    d <- BackAdj[[symbol]]$Date
    b <- BackAdj[[symbol]]$Basis
    name <- to_load$Name[which(to_load$Symbol == symbol)]
    r[is.na(r)] <- 0
    bb <-  round(sum(na.omit(b) > 0, na.rm=T) / length(na.omit(b)) * 100, 1)
    png(paste0(name, ".png"))
    plot(d, cumsum(r), main=name, xlab="Date", ylab="Log Return", col=(b > 0 )+ 1, pch=16)
    mtext(paste("Backwardation: ", bb, "%"), side = 3, padj = 2, adj=0.1)
    mtext(paste("Sharpe Ratio: ", mean(r) / sd(r) * sqrt(252), "%"), side = 3, padj = 3, adj=0.1)
    dev.off()
  }
  # Run a simple daily stategy based on basis
  setwd("/home/marco/trading/Systems/Monopoly/")
  dir.create("Plots/BasisStrategyDaily")
  setwd("Plots/BasisStrategyDaily")
  n <- length(BackAdj)
  res <- data.frame(Symbol=rep(NA, n), Name=rep(NA, n), Backwardation=rep(NA, n), SR=rep(NA, n))
  for(symbol in names(BackAdj)){
    r <- BackAdj[[symbol]]$Return
    d <- BackAdj[[symbol]]$Date
    name <- to_load$Name[which(to_load$Symbol == symbol)]
    b <- BackAdj[[symbol]]$Basis
    b[is.na(b)] <- 0
    r <- r * lag(ifelse(b < 0, -1, 1))
    r[is.na(r)] <- 0
    bb <-  round(sum(b > 0, na.rm=T) / nrow(BackAdj[[symbol]]) * 100, 1)
    sr <- round(mean(r) / sd(r) * sqrt(252), 2)
    #png(paste0(name, ".png"))
    plot(d, cumsum(r), main=name, xlab="Date", ylab="Log Return")
    mtext(paste("Backwardation: ", bb, "%"), side = 3, padj = 2, adj=0.1)
    mtext(paste("Sharpe Ratio: ", sr, "%"), side = 3, padj = 4, adj=0.1)
    #dev.off()
    res[which(names(BackAdj)==symbol),] <- c(symbol, name, bb, sr)
  }
  # Run a simple montly stategy based on basis
  setwd("/home/marco/trading/Systems/Monopoly/")
  dir.create("Plots/BasisStrategyMonthly")
  setwd("Plots/BasisStrategyMonthly")
  n <- length(BackAdj)
  res <- data.frame(Symbol=rep(NA, n), Name=rep(NA, n), Backwardation=rep(NA, n), SR=rep(NA, n))
  for(symbol in names(BackAdj)){
    df <- BackAdj[[symbol]] %>% mutate(ym = yearmonth(Date))
    df <- group_by(df, ym) %>% summarize(Date=last(Date), Return=sum(Return), Basis=last(Basis)) %>% mutate(LastReturn=lag(Return))
    r <- df$Return
    date <- df$Date
    name <- to_load$Name[which(to_load$Symbol == symbol)]
    basis <- df$Basis
    basis[is.na(basis)] <- 0
    r <- r * lag(ifelse(basis < 0, -1, 1))
    r[is.na(r)] <- 0
    bb <-  round(sum(basis > 0, na.rm=T) / nrow(df) * 100, 1)
    sr <- round(mean(r) / sd(r) * sqrt(12), 2)
    png(paste0(name, ".png"))
    plot(date, cumsum(r), main=name, xlab="Date", ylab="Log Return")
    mtext(paste("Backwardation: ", bb, "%"), side = 3, padj = 2, adj=0.1)
    mtext(paste("Sharpe Ratio: ", sr), side = 3, padj = 4, adj=0.1)
    dev.off()
    png(paste0(name, "_.png"))
    plot(df$Basis, df$Return, main=name, xlab="Basis", ylab="Return")
    dev.off()
  }
  # Create ACF and PACF or year-to-year monthly log differences 
  setwd("/home/marco/trading/Systems/Monopoly/")
  dir.create("Plots/Acf_weekly")
  setwd("Plots/Acf_weekly")
  for(symbol in names(BackAdj)){
    print(symbol)
    df <- BackAdj[[symbol]]
    df <-  mutate(df, Date = yearweek(Date)) %>% group_by(Date) %>%  summarise(Return=sum(Return, na.rm=T))
    r <- df$Return
    name <- to_load$Name[which(to_load$Symbol == symbol)]
    png(paste0(name, "_Acf.png"))
    Acf(r, main=name, lag.max = 100)
    dev.off()
    png(paste0(name, "_Pacf.png"))
    Pacf(r, main=name, lag.max = 100)
    dev.off()
  }
  # Previous return effect on returns
  n <- length(BackAdj)
  period <- 12
  res <- data.frame(Symbol=rep(NA, n), Name=rep(NA, n), m=as.numeric(rep(NA, n)), s=as.numeric(rep(NA, n)))
  for(symbol in names(BackAdj)){
    df <- BackAdj[[symbol]] %>% mutate(ym = yearmonth(Date))
    # Comment this for daily
    df <- group_by(df, ym) %>% summarize(Date=last(Date), Return=sum(Return, na.rm=T), Basis=last(Basis), Vol=mean(Return^2, na.rm=T)) %>% mutate(LastReturn=lag(Return))
    r <- df$Return * 100
    #v <- df$Vol - runMean(df$Vol)
    q <- df$Basis
    b <- lag(ifelse(r > 0, 1, -1))
    #b <- lag(ifelse(v > 0, 1, -1))
    #b <- 1
    b <- lag(ifelse(q > 0, 1, -1))
    #b <- lag(ifelse(q > 0 & r > 0 & v > 0, 1, ifelse(q < 0 & r < 0 & v < 0, -1, 0)))
    r <- r * b
    r[is.na(r)] <- 0
    m <- mean(r) * sqrt(period)
    s <- sd(r) * sqrt(period)
    name <- to_load$Name[which(to_load$Symbol == symbol)]
    res[which(names(BackAdj)==symbol),] <- c(symbol, name,  m, s)
  }
  # Plotting basis forward curves
  setwd("/home/marco/trading/Systems/Monopoly/")
  dir.create("Plots/Basis") # select nearest of backadj
  setwd("Plots/Basis")
  s <- 25
  for(symbol in names(Futures)){
    # Basis curve and seasonal
    df <- BackAdj[[symbol]] %>% filter(year(Date) > 2000)
    p <- ggplot() + geom_line(data=df, aes(Date, Basis))  + geom_hline(yintercept = 0) + xlab("") + ylab("") +ggtitle(symbol)  + theme(text =  element_text(size = 12))
    ggsave(paste(symbol, "_curve.png"), p, width=12, height = 9, dpi=100)
    df <- group_by(df, M=as.integer(month(Date))) %>% summarise(m=mean(Basis, na.rm=TRUE),s=sd(Basis, na.rm=TRUE))
    p <- ggplot(df) + geom_line(aes(M, m)) + geom_errorbar(aes(x=M, ymin=m-s, ymax=m+s), width=0.5) + geom_hline(yintercept = 0) + xlab("") + ylab("") +ggtitle(symbol)  + theme(text =  element_text(size = 12))
    ggsave(paste(symbol, "_seasonal.png"), p, width=12, height = 9, dpi=100)
    # Basis by contract
    df <- Futures[[symbol]] %>% filter(year(Date) > 2000)
    m <- log(as.matrix(df[,-1]))
    r <- matrix(NA, nrow = nrow(m), ncol=s-1)
    for(i in 1:nrow(m)){
      na <- !is.na(m[i,])
      if(sum(na)>1)
        r[i,] <- abs(m[i  ,na][2:s] - m[i  ,na][1]) / sd(m[i,na], na.rm=TRUE)
    }
    a1 <- apply(r, 2, median, na.rm=T)
    a2 <- apply(r, 2, mad, na.rm=T) #/ sqrt(nrow(r)) 
    p <- ggplot(data.frame(a1, a2)) + geom_line(aes(x = 1:(s-1), y=a1)) + geom_errorbar(aes(x = 1:(s-1), ymin=a1-a2, ymax=a1+a2), width=0.5) + theme(text = element_text(size = 14))
    ggsave(filename = paste0(symbol, "_contract.png"), p, dpi=150)
  }
  # volatility forward curve and by contract
  setwd("/home/marco/trading/Systems/Monopoly/")
  dir.create("Plots/Volatility") # select nearest of backadj
  setwd("Plots/Volatility")
  s <- 24
  for(symbol in names(Futures)){
    df <- Futures[[symbol]] %>% filter(year(Date) > 2000)
    d <- df[,1]
    m <- log(as.matrix(df[,-1]))
    m <- apply(m, 2, function(x) c(0, diff(x)))^2
    r <- matrix(NA, nrow = nrow(m), ncol=(s-1))
    f <- rep(NA, nrow(m))
    for(i in 11:nrow(m)){
      na <- !is.na(m[i,])
      if(sum(na)>1) {
        a <- m[(i-10):i  ,na] %>% colMeans(., na.rm=T) 
        r[i,] <- log(a[2:s] / a[1])
        f[i] <- log(a[2] / a[1])
      }
    }
    a1 <- apply(r, 2, median, na.rm=T)
    a2 <- apply(r, 2, mad, na.rm=T) #/ sqrt(nrow(r))
    p <- ggplot(data.frame(a1, a2)) + geom_line(aes(x = 1:(s-1), y=a1))+ geom_errorbar(aes(x = 1:(s-1), ymin=a1-a2, ymax=a1+a2), width=0.5) + theme(text = element_text(size = 14))
    ggsave(filename = paste0(symbol, "_contracts.png"), p, height = 9, width = 12, dpi=150)
    p <- ggplot(data.frame(exp(f))) + geom_line(aes(x = as.Date(d), y=f))+ theme(text = element_text(size = 14))
    ggsave(filename = paste0(symbol, "_curve.png"), p,  height = 9, width = 12,dpi=150)
    # png(paste0(symbol, "_boxplot.png"))
    # boxplot(r, main=symbol)
    # dev.off()
  }
  # Convergence to barchart cash
  setwd("/home/marco/trading/Systems/Monopoly/")
  dir.create("Plots/Convergence_cash") 
  setwd("Plots/Convergence_cash")
  s <- 252
  for(symbol in names(Futures)){
    if(!symbol %in% names(Cash))
      next
    df <- Futures[[symbol]] %>% filter(year(Date) > 2000)
    df <- merge(Cash[[symbol]], df, by="Date") %>% select(-c(Symbol,Return ))
    r <- matrix(NA, nrow = s, ncol=ncol(df)-2) # cash - future spread over s days
    v <- rep(NA, ncol(df)-2) # cash - future spread at expiration
    w <- rep(NA, ncol(df)-2) # cash - future average spread
    for(j in 3:ncol(df)){ # ingore date, last, symbol and return
      m <- cbind(df[,2], df[,j]) %>% na.omit %>% as.matrix
      if(nrow(m) == 0)
        next
      z <- log(m[,2] / m[,1])
      a <- abs(tail(z, s))
      if(length(a) < s)
        a <- c(rep(NA, s-length(a)), a)
      r[,j-2] <- a
      v[j-2] <- tail(z, 1)
      w[j-2] <- mean(abs(z), na.rm=T)
    }
    if(all(is.na(r)))
      next
    a1 <- apply(r, 1, median, na.rm=T)
    a2 <- apply(r, 1, mad, na.rm=T) #/ sqrt(nrow(r)) 
    p <- ggplot(data.frame(a1, a2)) + geom_line(aes(x = 1:s, y=a1)) + geom_errorbar(aes(x = 1:s, ymin=a1-a2, ymax=a1+a2), width=0.5) + theme(text = element_text(size = 14))
    ggsave(filename = paste0(symbol, "_convergence.png"), p, dpi=150)
    p <- ggplot(data.frame((v))) + geom_histogram(aes(v)) + geom_vline(xintercept = mean(w,na.rm=T)) + xlab("") + ylab("")    
    ggsave(filename = paste0(symbol, "_spread.png"), p, dpi=150)
  }
  # Convergence to expected spot
  setwd("/home/marco/trading/Systems/Monopoly/")
  dir.create("Plots/Convergence_spot") 
  setwd("Plots/Convergence_spot")
  s <- 252
  for(symbol in names(Futures)){
    df <- Futures[[symbol]] %>% filter(year(Date) > 2000)
    df <- merge(BackAdj[[symbol]], df, by="Date") %>% select(-c(Nearest, Backadjusted, Return,Adjs, Rollover,Basis,Spread ))
    r <- matrix(NA, nrow = s, ncol=ncol(df)-2) # cash - future spread over s days
    for(j in 3:ncol(df)){ # ingore date, last, symbol and return
      m <- cbind(df[,2], df[,j]) %>% na.omit %>% as.matrix
      if(nrow(m) == 0)
        next
      z <- log(m[,2] / m[,1])
      a <- abs(tail(z, s))
      if(length(a) < s)
        a <- c(rep(NA, s-length(a)), a)
      r[,j-2] <- a
    }
    if(all(is.na(r)))
      next
    a1 <- apply(r, 1, median, na.rm=T)
    a2 <- apply(r, 1, mad, na.rm=T) #/ sqrt(nrow(r)) 
    p <- ggplot(data.frame(a1, a2)) + geom_line(aes(x = 1:s, y=a1)) + geom_errorbar(aes(x = 1:s, ymin=a1-a2, ymax=a1+a2), width=0.5) + theme(text = element_text(size = 14))
    ggsave(filename = paste0(symbol, "_convergence.png"), p, width=12, height=9,dpi=150)
  }
  # Seasonality
  setwd("/home/marco/trading/Systems/Monopoly/")
  dir.create("Plots/Seasonalty") 
  setwd("Plots/Seasonalty")
  for(symbol in names(BackAdj)){
    print(symbol)
    df <- BackAdj[[symbol]] %>% filter(year(Date) > 2000)
    df$Volatility <- calculate_volatility(df$Return)
    df$Return <- df$Return / df$Volatility * sqrt(252) # Seasonality adjusted, comment it if unwanted
    a <- group_by(df, week(Date)) %>% summarise(Week=first(week(Date)), Mean=mean(Return, na.rm=T), SD=2*sd(Return, na.rm=T)/sqrt(n()))
    p <- ggplot(a) + geom_bar(aes(Week, Mean), stat="identity") + geom_errorbar(aes(x=Week, ymin=Mean-SD, ymax=Mean+SD), width=0.5)
    ggsave(filename = paste0(symbol, "_yearly.png"), p, width=12, height=9,dpi=150)
    a <- group_by(df, mday(Date)) %>% summarise(Day=first(mday(Date)), Mean=mean(Return, na.rm=T), SD=2*sd(Return, na.rm=T)/sqrt(n()))
    p <- ggplot(a) + geom_bar(aes(Day, Mean), stat="identity") + geom_errorbar(aes(x=Day, ymin=Mean-SD, ymax=Mean+SD), width=0.5)
    ggsave(filename = paste0(symbol, "_mday.png"), p, width=12, height=9,dpi=150)
    a <- group_by(df, wday(Date)) %>% summarise(Day=first(wday(Date)), Mean=mean(Return, na.rm=T), SD=2*sd(Return, na.rm=T)/sqrt(n()))
    p <- ggplot(a) + geom_bar(aes(Day, Mean), stat="identity") + geom_errorbar(aes(x=Day, ymin=Mean-SD, ymax=Mean+SD), width=0.5)
    ggsave(filename = paste0(symbol, "_wday.png"), p, width=12, height=9,dpi=150)
    a <- mutate(df, dom = mday(Date)) %>% mutate(W = case_when(dom <= 7 ~ 1, dom > 7 & dom <= 14 ~ 2, dom > 14 & dom <= 21 ~ 3, dom > 21 & dom <= 31 ~ 4, TRUE ~ 0)) %>% group_by(W) %>% summarise(Week=first(W), Mean=mean(Return, na.rm=T), SD=2*sd(Return, na.rm=T)/sqrt(n()))
    p <- ggplot(a) + geom_bar(aes(Week, Mean), stat="identity") + geom_errorbar(aes(x=Week, ymin=Mean-SD, ymax=Mean+SD), width=0.5)
    ggsave(filename = paste0(symbol, "_weekmonth.png"), p, width=12, height=9,dpi=150)
  }
  # Done fot RobotWealth
  all <- list()
  for(symbol in names(BackAdj)){
    print(symbol)
    df <- BackAdj[[symbol]] %>% filter(year(Date) >= 2000)
    name <-  BackAdj[[symbol]]$Name[1]
    df$Volatility <- calculate_volatility(df$Return)
    df$Return <- df$Return / df$Volatility * sqrt(252) # Seasonality adjusted, comment it if unwanted
    all[[name]] <- df
  }
  df <- do.call(rbind, all)
  {
    setwd("~")
    type<-"Debt"
    a <- group_by(df, Name, wday=lubridate::wday(Date), year=year(Date)) %>%filter(Class==type & wday %in% 2:6) %>%
      summarise(Mean=mean(Return, na.rm=T))
    b <- group_by(a, Name, wday) %>% na.omit %>% summarize(M=mean(Mean), SD=2*sd(Mean)/sqrt(n()))
    p <- ggplot(a) + geom_point(aes(x=jitter(wday), y=Mean), fill="grey90", color="gray20", pch=21, size=2) +  scale_x_continuous(labels=c("Mon","Tue","Wed","Thu","Fri")) +
      geom_errorbar(data=b, aes(x=wday, ymin=M-SD, ymax=M+SD), width=0.25, linewidth=2) +  geom_hline(yintercept = 0)  + facet_wrap(~Name)+
      theme(legend.position = "None", axis.title = element_blank(), axis.text = element_text(size=12), plot.title = element_text(hjust = 0.5, size=18, face = "bold"),panel.grid.minor = element_line(linewidth = 0.5), panel.grid.major = element_line(linewidth = 0.5), strip.text = element_text(size=10, margin = margin(0.1,0,0.1,0, "cm"))) + ggtitle(type ) +
      ylim(c(-0.5, 0.5))
    ggsave(filename = paste0(type, "_week.png"), p, width=16, height=12,dpi=300)
    a <- mutate(df, Name, dom = mday(Date), year=year(Date)) %>%filter(Class==type)  %>% mutate(W = case_when(dom <= 7 ~ 1, dom > 7 & dom <= 14 ~ 2, dom > 14 & dom <= 21 ~ 3, dom > 21 & dom <= 31 ~ 4, TRUE ~ 0)) %>% 
      group_by(Name, W, year) %>% summarise( Mean=mean(Return, na.rm=T)) %>% na.omit
    b <- group_by(a, Name, W) %>% na.omit %>% summarize(M=mean(Mean), SD=2*sd(Mean)/sqrt(n()))
    p <- ggplot(a) +  geom_point(aes(x=jitter(W), y=Mean), fill="grey90", color="gray20", pch=21, size=2)  + 
      facet_wrap(~Name)+
      geom_errorbar(data=b, aes(x=W, ymin=M-SD, ymax=M+SD), width=0.25, linewidth=2) + geom_hline(yintercept = 0)  + 
      theme(legend.position = "None", axis.title = element_blank(), axis.text = element_text(size=12), plot.title = element_text(hjust = 0.5, size=18, face = "bold"),panel.grid.minor = element_line(linewidth = 0.5), panel.grid.major = element_line(linewidth = 0.5), strip.text = element_text(size=10, margin = margin(0.1,0,0.1,0, "cm"))) + ggtitle(type )+
      ylim(c(-0.5, 0.5))
    ggsave(filename = paste0(type, "_month.png"), p, width=16, height=12,dpi=300)
    
  }
  
  
  # Create data.frames to plot animated forward curve
  forward <- 12
  final <- list()
  for(symbol in names(Futures)){
    print(symbol)
    df <- Futures[[symbol]] %>% filter(year(Date) > 2000)
    returns <- data.frame(Date=df$Date, apply(df[,-1], 2, function(x) c(0, diff(log(x)))^2))
    returns <- returns %>% mutate(Date = yearmonth(Date)) %>% group_by(Date) %>%   
      summarise(across(where(is.numeric), function(x) sum(x, na.rm=TRUE)))  %>% mutate(Date=as.Date(Date))
    price <-  df %>% mutate(Date = yearmonth(Date)) %>% group_by(Date) %>%  summarise_all(last) %>% mutate(Date=as.Date(Date))
    basis <- matrix(NA, nrow(price), forward)
    volatility <- matrix(NA, nrow(returns), forward)
    for(i in 1:nrow(price)){
      a <- as.vector(na.omit(unlist(price[i,-1])))[1:forward]
      a <- ((a / a[1]) - 1) * 100
      b <- as.vector(na.omit(unlist(returns[i,-1])))[1:forward]
      b <- log(b) - log(b[1])
      basis[i,] <- a
      volatility[i,] <- b
    }
    basis <- as.data.frame(basis)
    basis$Date <- sub("-01$", "", price$Date)
    volatility <- as.data.frame(volatility)
    volatility$Date <- sub("-01$", "", returns$Date)
    df1 <- melt(basis, id.vars = "Date") 
    colnames(df1 ) <- c("Date", "Forward", "Value")
    df2 <- melt(volatility, id.vars = "Date") 
    colnames(df2 ) <- c("Date", "Forward", "Value")
    final[[symbol]] <- df1
    final[[symbol]]$Volatility <- df2$Value
    final[[symbol]]$Forward <- as.integer(final[[symbol]]$Forward)
  }
  fig1 <- final[[symbol]]  %>%
    plot_ly(
      x = ~Forward, 
      y = ~Value, # or Value
      frame = ~Date, 
      hoverinfo = "text",
      type = 'scatter',
      mode = 'markers',
      size = 5
    ) %>% layout(
      yaxis = list(
        range=c(-20,20)
      ))
  fig1
}

# Volatiliy spread
{
vol <- merge(BackAdj$VI, BackAdj$DV, by="Date")
vol$Close <- (vol$Close.x - vol$Close.y*42 )%>% na.locf(na.rm=F)
vol$Return <- vol$Return.x - vol$Return.y
vol$Volatility <- calculate_volatility(vol$Return)
vol$Position <- 0.4 / vol$Volatility
cost <- 0.008
d <- BBands(vol$Close , 32, maType = SMA)
vol$Trade <- 0
vol$Cost <- 0
for(i in 2:nrow(vol)) {
  vol$Trade[i] <- vol$Trade[i-1]
  if(d[i,2] %>% is.na) next;
  if(vol$Trade[i] == 0) {
    if(vol$Close[i] < d[i,1]) {
      vol$Trade[i] <- 1; 
      vol$Cost[i] <- cost 
    }
    if(vol$Close[i] > d[i,3]) {
      vol$Trade[i] <- -1; 
      vol$Cost[i] <- cost
    }
  } else {
    if(vol$Trade[i] == 1 & vol$Close[i] > d[i,2]) 
      vol$Trade[i] <- 0; 
    if(vol$Trade[i] == -1 & vol$Close[i] < d[i,2]) 
      vol$Trade[i] <- 0;
  }
}
vol$Trade <- lag(vol$Trade)
vol$Cost <- lag(vol$Cost)
i <- 250
f <- tail(vol, i)
plot(f$Close, col=f$Trade+2, pch=16, cex=2 )
lines(f$Close, col="gray")
matplot2(d[,1:3] %>% tail(i), add=T)
vol$Excess <- vol$Return * vol$Trade * vol$Position - vol$Cost
vol$Excess[is.na(vol$Excess)] <- 0
cumsum(vol$Excess %>% na.omit) %>% plot
strategy_performance(vol$Excess, vol$Date) %>% unlist
}

# ES/YM spread
{
  vol <- merge(BackAdj$ES, BackAdj$YM, by="Date")
  vol$Close <- (vol$Close.x - vol$Close.y )%>% na.locf(na.rm=F)
  vol$Return <- vol$Return.x - vol$Return.y
  vol$Volatility <- calculate_volatility(vol$Return)
  vol$Position <- 0.4 / vol$Volatility
  cost <- 0.00
  d <- BBands(vol$Close, 32, maType = SMA, sd = 2)
  vol$Trade <- 0
  vol$Cost <- 0
  for(i in 2:nrow(vol)) {
    vol$Trade[i] <- vol$Trade[i-1]
    if(d[i,2] %>% is.na) next;
    if(vol$Trade[i] == 0) {
      if(vol$Close[i] < d[i,1]) {
        vol$Trade[i] <- 1; 
        vol$Cost[i] <- cost 
      }
      if(vol$Close[i] > d[i,3]) {
        vol$Trade[i] <- -1; 
        vol$Cost[i] <- cost
      }
    } else {
      if(vol$Trade[i] == 1 & vol$Close[i] > d[i,2]) 
        vol$Trade[i] <- 0; 
      if(vol$Trade[i] == -1 & vol$Close[i] < d[i,2]) 
        vol$Trade[i] <- 0;
    }
  }
  vol$Trade <- lag(vol$Trade)
  vol$Cost <- lag(vol$Cost)
  i <- 250
  f <- tail(vol, i)
  plot(f$Close, col=f$Trade+2, pch=16, cex=2 )
  lines(f$Close, col="gray")
  matplot2(d[,1:3] %>% tail(i), add=T)
  vol$Excess <- vol$Return * vol$Trade * vol$Position - vol$Cost
  vol$Excess[is.na(vol$Excess)] <- 0
  cumsum(vol$Excess %>% na.omit) %>% plot
  strategy_performance(vol$Excess, vol$Date) %>% unlist
}


