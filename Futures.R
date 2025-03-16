## This file contains important procedures to process futures contracts data and create back-adjusted prices
## Go to the "Load futures data and calculate stuff" to update RDS files for future backadjusted data

{
  library(tidyverse)
  library(mvtnorm)
  library(quantmod)
  library(forecast)
  library(mvtnorm)
  library(reshape2)
  library(tsibble)
  library(data.table)
  library(ggthemes)
  library(caTools)
  source("/home/marco/trading/Systems//Common/Common.R")
  source("/home/marco/trading/Systems//Common/Reports.R")
  source("/home/marco/trading/Systems//Common/Indicators.R")
  source("/home/marco/trading/Systems//Common/RiskManagement.R")
  setwd("/home/marco/trading/Systems/Monopoly/")
  Sys.setlocale("LC_TIME", "en_US.UTF-8")
  theme_set(theme_bw(base_size = 32))
}


# Load future contracts in wide format, we expect all the contract to be in the directory
load_future_contracts_wide <- function(symbol, dir, order_years=c(80:99,0:30), order_months=c("f", "g", "h", "j", "k", "m", "n", "q", "u", "v", "x", "z")) {
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
    if(!all(table(f$Date)==1))
      warning(paste("Double entry date found in contract: ", l))
  }
  # Join all the contracts and sort them by Date and contracts order
  df <- Reduce(function(...) full_join(..., by="Date"), files) %>% arrange(Date)
  colnames(df) <- c("Date", names(files))
  df <- as.data.frame(df)
  #order <- scan(paste0(dir, "/", order_file), what="string") %>% tolower()
  order <- colnames(df)[-1][na.omit(match(order_comb, colnames(df)[-1]))]
  df <- df[,c("Date", order)] %>% arrange(Date)
  return(df)
}


# Load future contracts in long format, we expect all the contract to be in the directory
load_future_contracts_long <- function(symbol, dir, expiry_df=NULL, order_years=c(80:99,0:30), order_months=c("f", "g", "h", "j", "k", "m", "n", "q", "u", "v", "x", "z")) {
  order_years <- sapply(order_years, function(x) ifelse(x < 10, paste0("0", as.character(x)), as.character(x)))
  order_comb <- apply(expand.grid(order_months, order_years), 1, function(x) tolower(paste0(symbol, paste0(x, collapse = ""))))
  files <- list()
  # load all the contracts
  for(l in list.files(dir, pattern = ".csv")) {
    symbol <- sub(".csv", "", l)
    df <- fread(paste0(dir, "/", l))  %>% rename(Date=Time, Close=Last) %>% mutate(Symbol = symbol)
    df$Date <- as.Date(df$Date, format="%m/%d/%Y") 
    df <- arrange(df, Date) %>% mutate(ReturnLog = log(Close/lag(Close)), ReturnPoint = Close-lag(Close))
    files[[symbol]] <- df
  }
  # Concatenate all the contracts and sort them by contracts order
  df <- do.call(rbind, files[order_comb])
  # Calculate DTE if expiry file is provided, otherwise use contract length (for non-yet expired contract this will return a wrong DTE)
  if(is.null(expiry_df))
    df <- group_by(df, Symbol) %>% arrange(Date) %>% mutate(Expiry=last(Date), DTE = as.numeric(Expiry - Date)) %>% relocate(DTE, .before=Expiry)
  else {
    df <- left_join(df, expiry_df %>% rename(Expiry=Date), by="Symbol") %>%  group_by(Symbol) %>% arrange(Date) %>% mutate(DTE = as.numeric(Expiry - Date)) %>% relocate(DTE, .before=Expiry)
  }
  return(df)
}

# Load the expire file
load_expiry_file <- function(file){
  f <- read_csv(file, show_col_types = FALSE, col_names = FALSE) %>% rename(Symbol=X1, Date=X2) %>% mutate(Date=as.Date(Date, format="%m/%d/%y")) %>% arrange(Date)
  return(f)
}

# create a backadjusted future contract from futures contracts in wide format as returned by load_future_contracts_wide
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
  constant <- rep(NA, nrow(m)); # hypothetical constant maturity contract (using the next two contracts)
  constant_ret <- rep(NA, nrow(m)); # hypothetical constant maturity contract log return (using the next two contracts)
  first <- NA # first value to use to adjust
  last <- 0 # last value to use to adjust
  for(i in 3:(nrow(m)-1)) { # this assume first and last entries are Inf
    ret[i] <- log(m[i,j] / m[i-1,j])
    difference[i] <- m[i,j] - m[i-1,j]  
    # we have not reached the last contract but we have reached the last holding day, we roll to the next contract
    if(j < ncol(m) && i >= ec[j]-N+2) { 
      j <- j + 1;
      rollover[i] <- TRUE
      ret[i] <- log(m[i,j] / m[i-1,j])
      difference[i] <- m[i,j] - m[i-1,j]  
    }
    close[i] <- m[i,j]
    adjclose[i] <- m[i,j] - m[i-1,j]
    contract[i] <- colnames(m)[j]
    maturity[i] <- ec[j] - i
    # we have not reached the last contract
    if(j < ncol(m)) {
      k <- ifelse(j+1 > ncol(m), ncol(m), j+1 )
      basis[i] <- log(m[i,j]) - log(m[i,k]) # simple log difference between contracts
      basis_price[i] <- m[i,j] - m[i,k] # simple price difference between contracts
      basis_distance[i] <- ym[k] - ym[j] # distance in months
      basis_gordon[i] <- period * ((m[i,j] / m[i,k]) - 1) / ((ec[k]-i) - (ec[j]-i)) # as defined in Gorton et al. 2013
      spot[i] <-  m[i,j] * (1 + basis[i] / period * (ec[j]-i)) # as defined in Gorton et al. 2013
      maturity1 <- maturity[i] - 1 
      maturity2 <- ec[j+1] - i - 1
      dist1 <- abs(maturity1 - 30) 
      dist2 <- abs(maturity2 - 30) 
      if(min(dist1, dist2) >= 30)
        w <- 1
      else
        w <- ((dist1 + dist2) - dist1) / (dist1 + dist2) 
      constant[i] <- w * m[i,j] + (1-w) * m[i,j+1]
      constant_ret[i] <- w * log(m[i,j]/m[i-1,j]) + (1-w) * log(m[i,j+1]/m[i-1,j+1])
    }
    if(!(is.na(m[i,j]) | is.nan(m[i,j]))) {
      last <- m[i,j]
      if(is.na(first))
        first <- m[i,j]
    }
  } 
  # backadjust the price
  adjclose[is.na(adjclose)] <- 0
  adjclose <- first + cumsum(adjclose)
  adjclose <- adjclose + (last - adjclose[length(adjclose)])
  # the final data frame
  final <- data.frame(
    Date=df[,1], Close=close, AdjClose=adjclose, Return=ret, Difference=difference, Adjs=adjclose-close, 
    Contract=contract, Rollover=rollover, Maturity=maturity,
    Basis=basis, Basis_price=basis_price, Basis_gordon=basis_gordon, Basis_distance=basis_distance, Spot=spot, Constant=constant, Constant_return=constant_ret)
  # if a row has all NAs (probably because it just stored a Nan in the original data), remove it from the final result
  delete <- c()
  for(i in 1:nrow(m))
    if(all(is.na(m[i,])))
      delete <- c(delete, i)
  if(length(delete) > 0)
    final <- final[-delete,]
  return(final)
}

# build spreads from futures contracts in long format as returned by load_future_contracts_long
build_spreads <- function(df, C=list(c(1,2))) {
    N <- max(sapply(C, max))
    front_c <- sapply(C, function(x)x[1]) 
    back_c <- sapply(C, function(x)x[2]) 
    contracts <- df %>% group_by(Date) %>% mutate(Contract = row_number()) %>%  filter(Contract <= N)
    if(max(contracts$Contract) < N)
      stop(paste("Max contract is lower than supplied", N))
    # Turn the prices and returns into a wide format
    symbols <- contracts %>%  select(Symbol, Date, Contract) %>%
      pivot_wider(names_from = Contract, names_prefix = 'c_', values_from = Symbol)
    prices <- contracts %>%  select(Symbol, Date, Contract, Close) %>%
      pivot_wider(id_cols = -Symbol, names_from = Contract, names_prefix = 'c_', values_from = Close)
    dte <- contracts %>%  select(Symbol, Date, Contract, DTE) %>%
      pivot_wider(id_cols = -Symbol, names_from = Contract, names_prefix = 'c_', values_from = DTE)
    returnspoint <- contracts %>% select(Symbol, Date, Contract, ReturnPoint) %>%
      pivot_wider(id_cols = -Symbol, names_from = Contract, names_prefix = 'c_', values_from = ReturnPoint)
    returnslog <- contracts %>% select(Symbol, Date, Contract, ReturnLog) %>%
      pivot_wider(id_cols = -Symbol, names_from = Contract, names_prefix = 'c_', values_from = ReturnLog)
    volume <- contracts %>%   select(Symbol, Date, Contract, Volume) %>%
      pivot_wider(id_cols = -Symbol, names_from = Contract, names_prefix = 'c_', values_from = Volume)
    expiry <- contracts %>%    select(Symbol, Date, Contract, Expiry) %>%
      pivot_wider(id_cols = -Symbol, names_from = Contract, names_prefix = 'c_', values_from = Expiry)
    dates <- prices[,1]
    labels <- paste0("c_",paste0(front_c, back_c))
    symbol1 <- data.frame(dates, symbols[, front_c+1])
    symbol2 <- data.frame(dates, symbols[, back_c+1])
    price1 <- data.frame(dates, prices[, front_c+1])
    price2 <- data.frame(dates, prices[, back_c+1])
    returnpoint1 <- data.frame(dates, returnspoint[, front_c+1])
    returnpoint2 <- data.frame(dates, returnspoint[, back_c+1])
    returnlog1 <- data.frame(dates, returnslog[, front_c+1])
    returnlog2 <- data.frame(dates, returnslog[, back_c+1])
    dte1 <- data.frame(dates, dte[, front_c+1])
    dte2 <- data.frame(dates, dte[, back_c+1])
    volume1 <- data.frame(dates, volume[, front_c+1])
    volume2 <- data.frame(dates, volume[, back_c+1])
    expiry1 <- data.frame(dates, expiry[, front_c+1])
    expiry2 <- data.frame(dates, expiry[, back_c+1])
    spreadpoints <- data.frame(dates, price2[,-1] - price1[,-1])
    spreadlogs <- data.frame(dates, log(price2[,-1] / price1[,-1]))
    spreadreturnspoint <- data.frame(dates,  returnpoint1[,-1] - returnpoint2[,-1])
    spreadreturnslog <- data.frame(dates,  returnlog1[,-1] - returnlog2[,-1])
    colnames(symbol1) <- colnames(symbol2) <- 
    colnames(price1) <- colnames(price2) <- colnames(returnpoint1) <- colnames(returnpoint2)  <-  colnames(returnlog1) <- colnames(returnlog2)  <- 
      colnames(spreadpoints) <-   colnames(spreadlogs) <- colnames(spreadreturnspoint) <- colnames(spreadreturnslog) <-
      colnames(dte1)<- colnames(dte2)  <- colnames(volume1)  <- colnames(volume2) <- colnames(expiry1) <- colnames(expiry2) <- c("Date", labels)
    symbol1_l <- pivot_longer(symbol1, -Date, names_to = "Contracts", values_to = "Symbol1")
    symbol2_l <- pivot_longer(symbol2, -Date, names_to = "Contracts", values_to = "Symbol2")
    price1_l <- pivot_longer(price1, -Date, names_to = "Contracts", values_to = "Price1")
    price2_l <- pivot_longer(price2, -Date, names_to = "Contracts", values_to = "Price2")
    returnpoint1_l <- pivot_longer(returnpoint1, -Date, names_to = "Contracts", values_to = "ReturnPoint1")
    returnpoint2_l <- pivot_longer(returnpoint2, -Date, names_to = "Contracts", values_to = "ReturnPoint2")
    returnlog1_l <- pivot_longer(returnlog1, -Date, names_to = "Contracts", values_to = "ReturnLog1")
    returnlog2_l <- pivot_longer(returnlog2, -Date, names_to = "Contracts", values_to = "ReturnLog2")
    spreadpoints_l <- pivot_longer(spreadpoints, -Date, names_to = "Contracts", values_to = "SpreadPoint")
    spreadlogs_l <- pivot_longer(spreadlogs, -Date, names_to = "Contracts", values_to = "SpreadLog")
    spreadreturnspoint_l <- pivot_longer(spreadreturnspoint, -Date, names_to = "Contracts", values_to = "SpreadReturnPoint")
    spreadreturnslog_l <- pivot_longer(spreadreturnslog, -Date, names_to = "Contracts", values_to = "SpreadReturnLog")
    dtes1_l <- pivot_longer(dte1, -Date, names_to = "Contracts", values_to = "DTE1")
    dtes2_l <- pivot_longer(dte2, -Date, names_to = "Contracts", values_to = "DTE2")
    volumes1_l <- pivot_longer(volume1, -Date, names_to = "Contracts", values_to = "Volume1")
    volumes2_l <- pivot_longer(volume2, -Date, names_to = "Contracts", values_to = "Volume2")
    expiries1_l <- pivot_longer(expiry1, -Date, names_to = "Contracts", values_to = "Expiry1")
    expiries2_l <- pivot_longer(expiry2, -Date, names_to = "Contracts", values_to = "Expiry2")
    spreads <- Reduce(function(...) full_join(..., by = c("Date", "Contracts")), 
                      list(symbol1_l, symbol2_l, price1_l, price2_l, returnpoint1_l, returnpoint2_l, returnlog1_l, returnlog2_l, 
                           spreadpoints_l, spreadlogs_l, spreadreturnspoint_l, spreadreturnslog_l, dtes1_l, dtes2_l, 
                           volumes1_l, volumes2_l, expiries1_l, expiries2_l)) %>% arrange(Date)
    return(spreads)
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

# create a intramarket future spread
intramarket_spread <- function(df, N=1, D1=1, D2=2) {
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
      spread[i] <- m[i,j+D1] - m[i,j+D2]
    if(j < (ncol(m)-D2) && (i >= ec[j]-N+2)) {
      j <- j + 1;
      rollover[i] <- TRUE
      ret[i] <- log(m[i,j] / m[i-1,j])
      spread[i] <- m[i,j+D1] - m[i,j+D2]
    }
    
  } 
  return(data.frame(Date=df[,1], Spread=spread, Return=ret, Rollover=rollover))
}

# Load future contracts, we expect all the contract to be in the directory, and the contract order in the file order.txt
load_cash_contract <- function(f) {
  df <- read_csv(f, show_col_types = FALSE) %>% 
    select(Time, Last) %>% rename(Date=Time) %>% mutate(Date = as.Date(Date, format="%m/%d/%Y")) %>%    arrange(f, Date) %>% 
    mutate(Symbol=toupper(sub("y00.csv", "", f)), Return = c(0,diff(log(Last))) ) 
  
  return(df)
}

# build up the rollover curve given a df from load_future_contracts_wide, assume only N x T data.frame
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

### Load futures data and calculate stuff
{
  stop("Do not execute.")
  ## Load futures contracts and backadjust them
  {
  setwd( "/home/marco/trading/HistoricalData/Barchart/Futures/")
  to_load <- read_csv("Instrument_List.csv", show_col_types = FALSE)
  # load the full futures contracts
  Futures <- list()
  for(i in 1:nrow(to_load) ){
    symbol <- as.character(to_load[i,1])
    if(!is.null(Futures[[symbol]]))
      next
    dir <- as.character(to_load[i,2])
    print(c(symbol, dir))
    Futures[[symbol]] <- load_future_contracts_wide(symbol, dir)
  }
  write_rds(Futures, "/home/marco/trading/HistoricalData/Barchart/Futures.RDS")
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
  write_rds(BackAdj, "/home/marco/trading/HistoricalData/Barchart/BackAdj.RDS")
  Futures <- read_rds("/home/marco/trading/HistoricalData/Barchart/Futures.RDS")
  BackAdj <- read_rds("/home/marco/trading/HistoricalData/Barchart/BackAdj.RDS")
  }
  ## Load cash data
  {
  setwd( "/home/marco/trading/HistoricalData/Barchart/OTHER/Cash//")
  Cash <- list()
  for(f in list.files(".", ".csv")) {
    symbol <- toupper(sub("y00.csv", "", f))
    if(system(paste("wc -l ", f, " | cut -f 1 -d \" \""), intern = TRUE) == "1")
      next
    Cash[[symbol]] <- load_cash_contract(f)
  }
  }
  ## Long-only the front contract
  {
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
  }
  ## Correlation between contracts
  {
  setwd("/home/marco/trading/Systems/Monopoly/")
  dir.create("Plots/Correlations")
  setwd("Plots/Correlations")
  for(symbol in names(Futures)){
    m <- Futures[[symbol]] %>% pivot_longer(-Date, names_to = "Symbol", values_to = "Close") %>% group_by(Date) %>% na.omit %>% mutate(Contract = row_number()) %>% filter(Contract <= 10) %>% group_by(Contract) %>% mutate(Return = log(Close/lag(Close))) %>%  select(-c(Symbol, Close))  %>% pivot_wider(names_from = Contract, values_from = Return) %>% select(-Date) %>% cor(use = "pairwise.complete.obs")
    png(paste(symbol, "_Correlation.png"), width = 800, height = 800)
    corrplot::corrplot(m, type = "lower", method = "number", cl.cex = 2, number.cex = 2, tl.cex = 2)
    dev.off()
  }
  }
  ## Spreads info by symbol
  {
  setwd("/home/marco/trading/Systems/Monopoly/")
  dir.create("Plots/Spreads")
  setwd("Plots/Spreads")
  Futures_long <- list()
  Spreads <- list()
  # Create spreads data
  for(i in 1:nrow(to_load) ){
    symbol <- as.character(to_load[i,1])
    name <- as.character(to_load[i,2])
    if(symbol %in% names(Futures_long))
      next
    print(paste(symbol, name))
    dir <- paste0("/home/marco/trading/HistoricalData/Barchart/", name)
    expiry_df <- load_expiry_file(paste0(dir, "/", "expirations.txt"))
    df <- load_future_contracts_long(symbol, dir, expiry_df)
    Futures_long[[symbol]] <- df
    # calculate the contracts combinations to pass to build_spreads
    contracts <- df %>%  group_by(Symbol) %>% arrange(Date) %>% group_by(Date) %>% mutate(Contract = row_number())
    max_level <- min(c(5, max(contracts$Contract)))
    contract_spreads <- cbind(1:(max_level-1), 2:max_level) %>% apply(., 1, c, simplify = F)
    Spreads[[symbol]] <- build_spreads(df, C = contract_spreads) 
  }
  write_rds(Futures_long, "/home/marco/trading/HistoricalData/Barchart/Futures_long.RDS")
  write_rds(Spreads, "/home/marco/trading/HistoricalData/Barchart/Spreads.RDS")
  # Plot spreads stuff
  Futures_long <- read_rds( "/home/marco/trading/HistoricalData/Barchart/Futures_long.RDS")
  Spreads <- read_rds( "/home/marco/trading/HistoricalData/Barchart/Spreads.RDS")
  for(symbol in names(Spreads)){
    print(symbol)
    #if(symbol != "GR") next
    spreads <- Spreads[[symbol]] 
    # Calculate vol adj spread returns, ignore the last trading day and contracts shorter than the running window
    spreads_returns <- spreads %>% filter(Date > "2000-01-01" &  DTE1 > 0) %>%  group_by(Contracts) %>% arrange(Date) %>% na.omit %>% filter(n() > 32 ) %>% 
      mutate(
             SpreadVol = runSD(SpreadReturnLog, 32), SpreadVol = na.locf(SpreadVol, na.rm=F),
             FrontVol = runSD(ReturnLog1, 32), FrontVol = na.locf(FrontVol, na.rm=F),
             scaledreturns = 0.2 / lag(SpreadVol) * SpreadReturnLog * sqrt(252),
             scaledreturns = replace(scaledreturns, is.infinite(scaledreturns) | is.na(scaledreturns), 0),
             SpreadLogAdj = SpreadLog / SpreadVol
             ) %>%   ungroup %>%    na.omit()
    # Plot the spreads
    # p <- ggplot(spreads_returns, aes(x=Date, y=SpreadLog, color=Contracts)) + geom_line(linewidth=1) + ggtitle(symbol) + scale_color_colorblind()
    # ggsave(paste(symbol, "_Spreads.png"), p, width=15, height = 9, dpi=100)
    # res <-  spreads_returns %>% group_by(Contracts) %>%  arrange(Date) %>%  mutate(SpreadCumulative = cumsum(SpreadReturn))
    # p <- ggplot(res, aes(x=Date, y=SpreadCumulative, color=Contracts)) + geom_line(linewidth=2) + ggtitle(symbol) + scale_color_colorblind()
    # ggsave(paste(symbol, "_SpreadCumulative.png"), p, width=15, height = 9, dpi=100)
    # # Volatility of the spreads compared to the front outright contract
    # res <- rbind(spreads_returns %>% mutate(Volatility = runSD(Return1, 32)) %>% select(Volatility) %>% mutate(Contracts=" Front"), 
    #            spreads_returns  %>% mutate(Volatility=SpreadVol) %>% select(Contracts, Volatility)) %>% mutate(Volatility=Volatility*sqrt(252)) %>% na.omit %>%
    #   group_by(Contracts) %>% reframe(M=mean(Volatility), S=sd(Volatility)*1) 
    # p <- ggplot(res, aes(x=Contracts, y=M, ymin=M-S, ymax=M+S)) + geom_pointrange(linewidth=1, size=1)
    # ggsave(paste(symbol, "_VolSpread.png"), p, width=15, height = 9, dpi=100)
    ## Performance of trading the front spread by DTE
    # res <-  spreads_returns %>%  mutate(Decile = round(DTE1 / 10)) %>% filter(Decile < 10) %>%
    #   group_by(Contracts, Decile) %>% reframe(M=mean(scaledreturns, na.rm=T), S=sd(scaledreturns, na.rm=T)/sqrt(n())*2)
    # p <- ggplot(res) + geom_errorbar(aes(x=Decile, ymin=M-S, ymax=M+S), width=0.5) + facet_wrap(~Contracts, scales = "free")  + geom_hline(yintercept = 0)+scale_x_continuous(breaks = 0:9)
    # ggsave(paste(symbol, "_DTE.png"), p, width=15, height = 9, dpi=100)
    ## Performance of trading the front spread, conditional on predictors
    lagging <- 2
    res_ <- spreads_returns %>%  group_by(Contracts) %>%  arrange(Date) %>%  filter(n() > 252+32) %>% mutate(
      Level_Z = runZscore(Price1, 252),
      LevelDecile = ntile(Level_Z, 5),
      Slope_Z = runZscore(SpreadLog, 252),
      SlopeDecile = ntile(Slope_Z, 5),
      Momentum = EMA(SpreadReturnLog, 32),
      Momentum_Z = runZscore(Momentum, 252), 
      MomentumDecile = ntile(Momentum_Z, 5),
      Carry = SpreadLog / (abs(DTE2-DTE1) / 252),
      CarryDecile = ntile(Carry, 5),
      ReturnLead = lead(scaledreturns, lagging)
      ) %>% na.omit
    res <- res_ %>% group_by(Contracts, LevelDecile) %>% reframe(M=mean(ReturnLead, na.rm=T), S=sd(ReturnLead, na.rm=T)/sqrt(n())*2)
    p <- ggplot(res) + geom_errorbar(aes(x=LevelDecile, ymin=M-S, ymax=M+S), width=0.25) + facet_wrap(~Contracts, scales = "free")  + geom_hline(yintercept = 0)+scale_x_continuous(breaks = 0:9)
    ggsave(paste(symbol, "_Level.png"), p, width=15, height = 9, dpi=100)
    res <- res_ %>% group_by(Contracts, SlopeDecile) %>% reframe(M = mean(ReturnLead), S = sd(ReturnLead)/sqrt(n())*2) %>% na.omit
    p <- ggplot(res) + geom_errorbar(aes(x=SlopeDecile, ymin=M-S, ymax=M+S), width=0.25) + facet_wrap(~Contracts, scales = "free")  + geom_hline(yintercept = 0)+scale_x_continuous(breaks = 0:9)
    ggsave(paste(symbol, "_Slope.png"), p, width=15, height = 9, dpi=100)
    res <- res_ %>% group_by(Contracts, MomentumDecile) %>% reframe(M = mean(ReturnLead), S = sd(ReturnLead)/sqrt(n())*2)
    p <- ggplot(res) + geom_errorbar(aes(x=MomentumDecile, ymin=M-S, ymax=M+S), width=0.25) + facet_wrap(~Contracts, scales = "free")  + geom_hline(yintercept = 0)+scale_x_continuous(breaks = 0:9)
    ggsave(paste(symbol, "_Momentum.png"), p, width=15, height = 9, dpi=100)
    res <- res_ %>% group_by(Contracts, CarryDecile) %>% reframe(M=mean(ReturnLead, na.rm=T), S=sd(ReturnLead, na.rm=T)/sqrt(n())*2)
    p <- ggplot(res) + geom_errorbar(aes(x=CarryDecile, ymin=M-S, ymax=M+S), width=0.25) + facet_wrap(~Contracts, scales = "free")  + geom_hline(yintercept = 0)+scale_x_continuous(breaks = 0:9)
    ggsave(paste(symbol, "_Carry.png"), p, width=15, height = 9, dpi=100)
    next
    ## Various pnl plots
    res <-  spreads_returns %>% group_by(Contracts) %>%  arrange(Date) %>%  mutate(cumreturns = cumsum(-scaledreturns))
    p <- ggplot(res, aes(x=Date, y=cumreturns, color=Contracts)) + geom_line(linewidth=2) + ggtitle(symbol) + scale_color_colorblind()
    ggsave(paste(symbol, "_PnL_VolAdj.png"), p, width=15, height = 9, dpi=100)
    res_ <- spreads_returns %>%  group_by(Contracts) %>%  arrange(Date) %>%  filter(n() > 252+32) %>% mutate(
      LevelLag = run_ntile(lag(Price1), 252, 5),
      Slope_Z = runZscore(SpreadLog, 252), SlopeLag = lag(Slope_Z),
      Momentum = EMA(SpreadReturn, 32), Momentum_Z = runZscore(Momentum, 252), MomentumLag = lag(Momentum_Z))
    res <- mutate(res_, weightedreturns = (scaledreturns * SlopeLag)  %>% replace(., is.na(.), 0), cumreturns = cumsum(weightedreturns))
    p <- ggplot(res, aes(x=Date, y=cumreturns, color=Contracts)) + geom_line(linewidth=2) + ggtitle(symbol) + scale_color_colorblind() + ylab("Returns Slope Adjusted")
    ggsave(paste(symbol, "_PnL_SlopeAdj.png"), p, width=15, height = 9, dpi=100)
    res <- mutate(res_, weightedreturns = (scaledreturns * MomentumLag)  %>% replace(., is.na(.), 0), cumreturns = cumsum(weightedreturns))
    p <- ggplot(res, aes(x=Date, y=cumreturns, color=Contracts)) + geom_line(linewidth=2) + ggtitle(symbol) + scale_color_colorblind() + ylab("Returns Momentum Adjusted")
    ggsave(paste(symbol, "_PnL_MomentumAdj.png"), p, width=15, height = 9, dpi=100)
  }
  }
  ## Spreads info general summary tables 
  {
  final_dte <- list()
  final_slope <- list()
  final_pred <- list()
  for(symbol in names(Spreads)){
    print(symbol)
    spreads <- Spreads[[symbol]] 
    spreads_returns <- spreads %>% filter(Date > "2000-01-01" &  DTE1 > 0) %>%  group_by(Contracts) %>% arrange(Date) %>% na.omit %>% filter(n() > 32 ) %>% 
      mutate(SpreadVol = runSD(SpreadReturnLog, 32),SpreadVol = na.locf(SpreadVol, na.rm=F), scaledreturns = 0.2 / lag(SpreadVol) * SpreadReturnLog * sqrt(252), scaledreturns = replace(scaledreturns, is.infinite(scaledreturns), 0),
      ) %>% filter(abs(scaledreturns) < sd(scaledreturns, na.rm=T)*2) %>%   ungroup %>%    na.omit()
    infos <- to_load[to_load$Symbol==symbol,][,2:3] %>% unlist
    lagging <- 2
    res_ <- spreads_returns %>%  group_by(Contracts) %>%  arrange(Date) %>%  filter(n() > 252+32) %>% mutate(
      Level_Z = runZscore(Price1, 252),
      LevelDecile = ntile(Level_Z, 5),
      Slope_Z = runZscore(SpreadLog, 252),
      SlopeDecile = ntile(Slope_Z, 5),
      Momentum = EMA(SpreadReturnLog, 32),
      Momentum_Z = runZscore(Momentum, 252), 
      MomentumDecile = ntile(Momentum_Z, 5),
      Carry = SpreadLog / (abs(DTE2-DTE1) / 252),
      CarryDecile = ntile(Carry, 5),
      ReturnLead = lead(scaledreturns, lagging)
    ) %>% na.omit
    final_pred[[symbol]] <- res_
    final_slope[[symbol]] <- group_by(res_, Contracts) %>%  reframe(Symbol=symbol, Name=infos[1], Class=infos[2], M=mean(scaledreturns*SlopeLag, na.rm=T), S=sd(scaledreturns*SlopeLag, na.rm=T)/sqrt(n())*2)
  }
  # Check front contracts spread adjusted returns and plot by class
  ret <- do.call(rbind, final_slope) %>% filter(Contracts == "c_23") 
  ggplot(ret, aes(Class, M, ymin=M-S, ymax=M+S, color=Class)) + geom_pointrange(position=position_jitter(width=0.45)) +  geom_hline(yintercept = 0) + scale_color_colorblind()   
  }
  ## Run a simple daily strategy based on basis
  {
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
  }
  ## Create ACF and PACF or year-to-year monthly log differences 
  {
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
  }
  ## Basis forward curves and basis seasonality
  {
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
    M <- apply(r, 2, mean, na.rm=T)
    S <- apply(r, 2, sd, na.rm=T) #/ sqrt(nrow(r))
    df <- data.frame(x=1:length(M), M, S) %>% na.omit
    p <- ggplot(df) + geom_line(aes(x = x, y=M)) + geom_errorbar(aes(x = x, ymin=M-S, ymax=M+S), width=0.5) + theme(text = element_text(size = 14))
    ggsave(filename = paste0(symbol, "_contract.png"), p, width = 8, height = 6)
  }
  }
  ## Slopes between contracts in points and log
  {
  setwd("/home/marco/trading/Systems/Monopoly/")
  dir.create("Plots/Slopes") 
  setwd("Plots/Slopes")
  s <- 25
  for(symbol in names(Futures)){
    df <- Futures[[symbol]] %>% filter(year(Date) > 2000)
    d <- df[,1]
    m <- as.matrix(df[,-1])
    r_p <- matrix(NA, nrow = nrow(m), ncol=ncol(m))
    r_l <- matrix(NA, nrow = nrow(m), ncol=ncol(m))
    for(i in 1:nrow(m)) {
      r_p[i,] <- (lead(m[i,]) - m[i,])
      r_l[i,] <- (log(lead(m[i,]) / m[i,]))
    }
    r <- apply(r_l, 1, function(x) na.omit(x)[1:s]) %>% t
    melt(r) %>% group_by(Var2) %>% na.omit %>% reframe(M=median(value), S=sd(value)/sqrt(n())*2) %>% ggplot(aes(Var2, ymin=M-S, ymax=M+S)) + geom_errorbar(width=0.5) + xlab("Contract") + ggtitle("Slope in Logs") -> p
    ggsave(filename = paste0(symbol, "_log.png"), p, width = 8, height = 6)
    r <- apply(r_p, 1, function(x) na.omit(x)[1:s]) %>% t
    melt(r) %>% group_by(Var2) %>% na.omit %>% reframe(M=median(value), S=sd(value)/sqrt(n())*2) %>% ggplot(aes(Var2, ymin=M-S, ymax=M+S)) + geom_errorbar(width=0.5) + xlab("Contract") + ggtitle("Slope in Points")  -> p
    ggsave(filename = paste0(symbol, "_points.png"), p, width = 8, height = 6)
  }
  }
  ## Volatility forward curve and by contract
  {
  setwd("/home/marco/trading/Systems/Monopoly/")
  dir.create("Plots/Volatility") 
  setwd("Plots/Volatility")
  s <- 24
  for(symbol in names(Futures)){
    df <- Futures[[symbol]] %>% filter(year(Date) > 2000)
    res <- pivot_longer(df, -Date, names_to = "Symbol", values_to = "Close") %>% na.omit %>% 
      group_by(Date) %>% mutate(Contract=row_number()) %>% group_by(Symbol) %>% 
      mutate(Return = log(Close / lag(Close)), N = n()) %>% filter(N>32) %>% na.omit %>% mutate(Volatility=runSD(Return, 32)*sqrt(252)) 
    res %>% 
      group_by(Contract) %>% reframe(M = median(Volatility, na.rm=T), S = sd(Volatility, na.rm=T)/sqrt(n())*2) %>% 
       ggplot(aes(Contract, ymin=M-S, ymax=M+S)) + geom_errorbar(width=0.5) + xlab("Contract") + ggtitle("Volatility")  -> p
    ggsave(filename = paste0(symbol, "_contract.png"), p,  height = 9, width = 12,dpi=150)
  }
  }
  ## Convergence to barchart cash
  {
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
  }
  ## Convergence to expected spot
  {
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
  }
  ## Seasonality
  {
  setwd("/home/marco/trading/Systems/Monopoly/")
  dir.create("Plots/Seasonalty") 
  setwd("Plots/Seasonalty")
  for(symbol in names(BackAdj)){
    print(symbol)
    df <- BackAdj[[symbol]] %>% filter(year(Date) > 2000)
    df$Volatility <- calculate_volatility(df$Return)
    df$Return <- df$Return / df$Volatility * sqrt(252) # Seasonality adjusted, comment it if unwanted
    a <- mutate(df, dom = mday(Date)) %>% mutate(W = case_when(dom <= 5 ~ "BOM", dom >= 27 ~ "EOM", TRUE ~ NA)) %>% group_by(W) %>% summarise(Mean=mean(Return, na.rm=T), SD=2*sd(Return, na.rm=T)/sqrt(n())) %>% na.omit
    p <- ggplot(a) + geom_bar(aes(W, Mean), stat="identity") + geom_errorbar(aes(x=W, ymin=Mean-SD, ymax=Mean+SD), width=0.5)
    ggsave(filename = paste0(symbol, "_TOM.png"), p, width=12, height=9,dpi=150)
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
  }
  
  ## Create data.frames to plot animated forward curve
  {
  forward <- 20
  final <- list()
  for(symbol in names(Futures)){
    if(symbol != "GC") next
    df <- Futures[[symbol]] %>% filter(year(Date) > 2000)
    returns <- data.frame(Date=df$Date, apply(df[,-1], 2, function(x) c(0, diff(log(x)))^2))
    returns <- returns %>% mutate(Date = yearmonth(Date)) %>% group_by(Date) %>%   
      summarise(across(where(is.numeric), function(x) sum(x, na.rm=TRUE)))  %>% mutate(Date=as.Date(Date))
    price <-  df %>% mutate(Date = yearmonth(Date)) %>% group_by(Date) %>%  summarise_all(last) %>% mutate(Date=as.Date(Date))
    basis <- matrix(NA, nrow(price), forward)
    volatility <- matrix(NA, nrow(returns), forward)
    for(i in 1:nrow(price)){
      a <- as.vector(na.omit(unlist(price[i,-1])))[1:forward]
      a <- (log(a) - log(a[1])) * 100
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
  symbol <- "GC"
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
        range=c(-15,15)
      ))
  fig1
  }
}

