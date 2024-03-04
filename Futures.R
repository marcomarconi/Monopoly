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
  library(ggthemes)
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
load_future_contracts_long <- function(symbol, dir, order_years=c(80:99,0:30), order_months=c("f", "g", "h", "j", "k", "m", "n", "q", "u", "v", "x", "z")) {
  order_years <- sapply(order_years, function(x) ifelse(x < 10, paste0("0", as.character(x)), as.character(x)))
  order_comb <- apply(expand.grid(order_months, order_years), 1, function(x) tolower(paste0(symbol, paste0(x, collapse = ""))))
  files <- list()
  # load all the contracts
  for(l in list.files(dir, pattern = ".csv")) {
    symbol <- sub(".csv", "", l)
    f <- fread(paste0(dir, "/", l))  %>% rename(Date=Time, Close=Last) %>% mutate(Symbol = symbol)
    f$Date <- as.Date(f$Date, format="%m/%d/%Y") 
    f <- arrange(f, Date) %>% mutate(Return = log(Close/lag(Close)))
    files[[symbol]] <- f
  }
  # Concatenate all the contracts and sort them by contracts order
  df <- do.call(rbind, files[order_comb])
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
build_spreads <- function(df, C=list(c(1,2)), expiry_df=NULL) {
    N <- max(sapply(C, max))
    front_c <- sapply(C, function(x)x[1]) 
    back_c <- sapply(C, function(x)x[2]) 
    # Calculate DTE if expiry file is provided, otherwise use contract length (for non-yet expired contract this will return a wrong DTE)
    if(is.null(expiry_df))
      contracts <- df %>%  group_by(Symbol) %>% arrange(Date) %>% mutate(DTE = (n()-1):0) 
    else {
      contracts <- left_join(df, expiry_df %>% rename(Expiry=Date), by="Symbol") %>%  group_by(Symbol) %>% arrange(Date) %>% mutate(DTE = as.numeric(Expiry - Date))
    }
    contracts <- contracts %>% group_by(Date) %>% mutate(Contract = row_number()) %>%  filter(Contract <= N)
    if(max(contracts$Contract) < N)
      stop(paste("Max contract is lower than supplied", N))
    # Turn the prices and returns into a wide format
    symbols <- contracts %>%  select(Symbol, Date, Contract) %>%
      pivot_wider(names_from = Contract, names_prefix = 'c_', values_from = Symbol)
    prices <- contracts %>%  select(Symbol, Date, Contract, Close) %>%
      pivot_wider(id_cols = -Symbol, names_from = Contract, names_prefix = 'c_', values_from = Close)
    dte <- contracts %>%  select(Symbol, Date, Contract, DTE) %>%
      pivot_wider(id_cols = -Symbol, names_from = Contract, names_prefix = 'c_', values_from = DTE)
    returns <- contracts %>% select(Symbol, Date, Contract, Return) %>%
      pivot_wider(id_cols = -Symbol, names_from = Contract, names_prefix = 'c_', values_from = Return)
    volume <- contracts %>%   select(Symbol, Date, Contract, Volume) %>%
      pivot_wider(id_cols = -Symbol, names_from = Contract, names_prefix = 'c_', values_from = Volume)
    oi <- contracts %>%    select(Symbol, Date, Contract, `Open Int`) %>%
      pivot_wider(id_cols = -Symbol, names_from = Contract, names_prefix = 'c_', values_from = `Open Int`)
    expiry <- contracts %>%    select(Symbol, Date, Contract, Expiry) %>%
      pivot_wider(id_cols = -Symbol, names_from = Contract, names_prefix = 'c_', values_from = Expiry)
    dates <- prices[,1]
    labels <- paste0("c_",paste0(front_c, back_c))
    symbol1 <- data.frame(dates, symbols[, front_c+1])
    symbol2 <- data.frame(dates, symbols[, back_c+1])
    price1 <- data.frame(dates, prices[, front_c+1])
    price2 <- data.frame(dates, prices[, back_c+1])
    return1 <- data.frame(dates, returns[, front_c+1])
    return2 <- data.frame(dates, returns[, back_c+1])
    dtes <- data.frame(dates, dte[, front_c+1])
    volumes <- data.frame(dates, volume[, front_c+1] + volume[, back_c+1])
    ois <- data.frame(dates, oi[, front_c+1] + oi[, back_c+1])
    expiries <- data.frame(dates, expiry[, front_c+1])
    dtes <- data.frame(dates, dte[, front_c+1])
    spreadpoints <- data.frame(dates, price2[,-1] - price1[,-1])
    spreadlogs <- data.frame(dates, log(price2[,-1] / price1[,-1]))
    spreadreturns <- data.frame(dates,  returns[,front_c+1] - returns[,back_c+1])
    colnames(symbol1) <- colnames(symbol2) <- 
    colnames(price1) <- colnames(price2) <- colnames(return1) <- colnames(return2)  <- 
      colnames(spreadpoints) <-   colnames(spreadlogs) <- colnames(spreadreturns) <-
      colnames(dtes)  <- colnames(volumes) <- colnames(ois)<- colnames(expiries) <- c("Date", labels)
    symbol1_l <- pivot_longer(symbol1, -Date, names_to = "Contracts", values_to = "Symbol1")
    symbol2_l <- pivot_longer(symbol2, -Date, names_to = "Contracts", values_to = "Symbol2")
    price1_l <- pivot_longer(price1, -Date, names_to = "Contracts", values_to = "Price1")
    price2_l <- pivot_longer(price2, -Date, names_to = "Contracts", values_to = "Price2")
    return1_l <- pivot_longer(return1, -Date, names_to = "Contracts", values_to = "Return1")
    return2_l <- pivot_longer(return2, -Date, names_to = "Contracts", values_to = "Return2")
    spreadpoints_l <- pivot_longer(spreadpoints, -Date, names_to = "Contracts", values_to = "SpreadPoint")
    spreadlogs_l <- pivot_longer(spreadlogs, -Date, names_to = "Contracts", values_to = "SpreadLog")
    spreadreturns_l <- pivot_longer(spreadreturns, -Date, names_to = "Contracts", values_to = "SpreadReturn")
    dtes_l <- pivot_longer(dtes, -Date, names_to = "Contracts", values_to = "DTE")
    volumes_l <- pivot_longer(volumes, -Date, names_to = "Contracts", values_to = "Volume")
    ois_l <- pivot_longer(ois, -Date, names_to = "Contracts", values_to = "OpenInt")
    expiries_l <- pivot_longer(expiries, -Date, names_to = "Contracts", values_to = "Expiry")
    spreads <- Reduce(function(...) full_join(..., by = c("Date", "Contracts")), 
                      list(symbol1_l, symbol2_l, price1_l, price2_l, return1_l, return2_l, 
                           spreadpoints_l, spreadlogs_l, spreadreturns_l, dtes_l, volumes_l, ois_l, expiries_l)) %>% arrange(Date)
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

runZscore <- function(x, n=10) {
  return((x-runMean(x, n))/runSD(x, n))
}


### Load futures data and calculate stuff
{
  stop("Do not execute.")
  ## Load futures contracts and backadjust them
  {
  setwd( "/home/marco/trading/HistoricalData/Barchart/")
  to_load <- read_csv("Instrument_List.csv")
  # load the full futures contracts
  Futures <- list()
  for(i in 1:nrow(to_load) ){
    symbol <- as.character(to_load[i,1])
    if(!is.null(Futures[[symbol]]))
      next
    print(symbol)
    dir <- as.character(to_load[i,2])
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
    df <- load_future_contracts_long(symbol, dir)
    Futures_long[[symbol]] <- df
    expiry_df <- load_expiry_file(paste0(dir, "/", "expirations.txt"))
    # calculate the contracts combinations to pass to build_spreads
    contracts <- df %>%  group_by(Symbol) %>% arrange(Date) %>% group_by(Date) %>% mutate(Contract = row_number())
    max_level <- min(c(5, max(contracts$Contract)))
    contract_spreads <- cbind(1:(max_level-1), 2:max_level) %>% apply(., 1, c, simplify = F)
    Spreads[[symbol]] <- build_spreads(df, C = contract_spreads, expiry_df=expiry_df) 
  }
  write_rds(Futures_long, "/home/marco/trading/HistoricalData/Barchart/Futures_long.RDS")
  write_rds(Spreads, "/home/marco/trading/HistoricalData/Barchart/Spreads.RDS")
  # Plot spreads stuff
  for(symbol in names(Spreads)){
    print(symbol)
    if(symbol != "GR") next
    spreads <- Spreads[[symbol]] 
    # Calculate vol adj spread returns
    spreads_returns <- spreads %>% filter(Date > "2000-01-01" &  DTE > 0) %>%  group_by(Contracts) %>% arrange(Date) %>% na.omit %>% filter(n() > 32 ) %>% 
      mutate(
             SpreadVol = runSD(SpreadReturn, 32),
             SpreadVol = na.locf(SpreadVol, na.rm=F),
             scaledreturns = 0.2 / lag(SpreadVol) * SpreadReturn * sqrt(252),
             scaledreturns = replace(scaledreturns, is.infinite(scaledreturns), 0),
             ) %>%   ungroup %>%    na.omit()
    # Performance of shorting the front spread by spread level (SpreadLog or SpreadPoint, not the column Level)
    #res <-  spreads_returns %>%  mutate(SpreadLag = ntile(lag(SpreadLog), 5)) %>%
    res <-  spreads_returns %>%  mutate(SpreadLag = run_ntile(lag(SpreadLog), 252, 5)) %>%
      group_by(Contracts, SpreadLag) %>% reframe(M=mean(scaledreturns, na.rm=T), S=sd(scaledreturns, na.rm=T)/sqrt(n())*2)
    p <- ggplot(res) + geom_errorbar(aes(x=SpreadLag, ymin=M-S, ymax=M+S), width=0.5) + facet_wrap(~Contracts, scales = "free")  + geom_hline(yintercept = 0)+scale_x_continuous(breaks = 0:9)
    ggsave(paste(symbol, "_Level.png"), p, width=15, height = 9, dpi=100)
    # Plot the spreads
    p <- ggplot(spreads_returns, aes(x=Date, y=SpreadLog, color=Contracts)) + geom_line(linewidth=1) + ggtitle(symbol) + scale_color_colorblind()
    ggsave(paste(symbol, "_Spreads.png"), p, width=15, height = 9, dpi=100)
    # Performance of shorting the front spread, in log returns
    res <-  spreads_returns %>% group_by(Contracts) %>%  arrange(Date) %>%  mutate(cumreturns = cumsum(-SpreadReturn))
    p <- ggplot(res, aes(x=Date, y=cumreturns, color=Contracts)) + geom_line(linewidth=2) + ggtitle(symbol) + scale_color_colorblind()
    ggsave(paste(symbol, "_PnL.png"), p, width=15, height = 9, dpi=100)
    # Performance of shorting the front spread, in vol adjusted returns
    res <-  spreads_returns %>% group_by(Contracts) %>%  arrange(Date) %>%  mutate(cumreturns = cumsum(-scaledreturns))
    #group_by(res, Contracts) %>% reframe(mean(-scaledreturns)/sd(scaledreturns)*16)
    p <- ggplot(res, aes(x=Date, y=cumreturns, color=Contracts)) + geom_line(linewidth=2) + ggtitle(symbol) + scale_color_colorblind()
    ggsave(paste(symbol, "_PnLAdj.png"), p, width=15, height = 9, dpi=100)
    # Performance of shorting the front spread by DTE
    res <-  spreads_returns %>%  mutate(Decile = round(DTE / 10)) %>% filter(Decile < 10) %>%
      group_by(Contracts, Decile) %>% reframe(M=mean(scaledreturns, na.rm=T), S=sd(scaledreturns, na.rm=T)/sqrt(n())*2)
    p <- ggplot(res) + geom_errorbar(aes(x=Decile, ymin=M-S, ymax=M+S), width=0.5) + facet_wrap(~Contracts, scales = "free")  + geom_hline(yintercept = 0)+scale_x_continuous(breaks = 0:9)
    ggsave(paste(symbol, "_DTE.png"), p, width=15, height = 9, dpi=100)
    # Performance of shorting the front spread, conditional on basis slope and momentum
    res <- spreads_returns %>%  group_by(Contracts) %>%  arrange(Date) %>%  filter(n() > 252) %>% mutate(
      Slope_Z = (SpreadLog - runMean(SpreadLog, 252))/runSD(SpreadLog, 252),
      SlopeLag = ntile(lag(Slope_Z), 5)) %>%
      group_by(Contracts, SlopeLag) %>% reframe(M = mean(scaledreturns), S = sd(scaledreturns)/sqrt(n())*2) %>% na.omit
    p <- ggplot(res, aes(x=SlopeLag, y=M)) + geom_bar(stat='identity') + geom_errorbar(aes(ymin=M-S,ymax=M+S), width=0.25)+ facet_wrap(~Contracts)
    ggsave(paste(symbol, "_Slope.png"), p, width=15, height = 9, dpi=100)
    res_ <- spreads_returns %>%  group_by(Contracts) %>% arrange(Date) %>%  filter(n() > 32+252) %>%  mutate(
      Momentum = EMA(SpreadReturn, 32), Momentum_Z = runZscore(Momentum, 252), MomentumLag = ntile(lag(Momentum_Z), 5)) %>% na.omit
    res <- res_ %>% group_by(Contracts, MomentumLag) %>% reframe(M = mean(scaledreturns), S = sd(scaledreturns)/sqrt(n())*2)
    p <- ggplot(res, aes(x=MomentumLag, y=M)) + geom_bar(stat='identity') + geom_errorbar(aes(ymin=M-S,ymax=M+S), width=0.25)+ facet_wrap(~Contracts)
    ggsave(paste(symbol, "_Momentum.png"), p, width=15, height = 9, dpi=100)
    res_ <- spreads_returns %>%  group_by(Contracts) %>%  arrange(Date) %>%  filter(n() > 252+32) %>% mutate(
      Slope_Z = runZscore(SpreadLog, 252), SlopeLag = lag(Slope_Z),
      Momentum = EMA(SpreadReturn, 32), Momentum_Z = runZscore(Momentum, 252), MomentumLag = -lag(Momentum_Z))
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
  for(symbol in names(Spreads)){
    print(symbol)
    spreads <- Spreads[[symbol]] 
    spreads_returns <- spreads %>% filter(Date > "2000-01-01" &  DTE > 0) %>%  group_by(Contracts) %>% arrange(Date) %>% na.omit %>% filter(n() > 32 ) %>% 
      mutate(SpreadVol = runSD(SpreadReturn, 32),SpreadVol = na.locf(SpreadVol, na.rm=F), scaledreturns = 0.2 / lag(SpreadVol) * SpreadReturn * sqrt(252), scaledreturns = replace(scaledreturns, is.infinite(scaledreturns), 0),
      ) %>%   ungroup %>%    na.omit()
    infos <- to_load[to_load$Symbol==symbol,][,2:3] %>% unlist
    res <-  spreads_returns %>%  mutate(Decile = round(DTE / 10),
                                                    Slope_Z = runZscore(SpreadLog, 252), SlopeLag = ntile(lag(Slope_Z), 5))  %>% na.omit
    final_dte[[symbol]] <- res %>% filter(Decile < 5) %>% group_by(Contracts, Decile) %>% reframe(Symbol=symbol, Name=infos[1], Class=infos[2], M=mean(scaledreturns, na.rm=T), S=sd(scaledreturns, na.rm=T)/sqrt(n())*2)
    final_slope[[symbol]] <- group_by(res, Contracts) %>%  reframe(Symbol=symbol, Name=infos[1], Class=infos[2], M=mean(scaledreturns*SlopeLag, na.rm=T), S=sd(scaledreturns*SlopeLag, na.rm=T)/sqrt(n())*2)
  }
  # Check front contracts spread adjusted returns and plot by class
  ret <- do.call(rbind, final_dte) %>% filter(Contracts == "c_12", Decile == 0) 
  ggplot(ret, aes(Class, M, ymin=M-S, ymax=M+S, color=Class)) + geom_pointrange(position=position_jitter(width=0.45)) +  geom_hline(yintercept = 0) + scale_color_colorblind() + ylim(c(-5,5))  
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
      mutate(Return = log(Close / lag(Close)), N = n()) %>% filter(N>20) %>% na.omit %>% mutate(Volatility=runSD(Return, 20)*sqrt(252)) %>% 
      group_by(Date) %>% mutate(SpreadReturn = log(Close / lag(Close)), Spread = paste(lag(Contract), Contract, sep=" "))   %>% 
      na.omit %>% group_by(Spread) %>% mutate(M=n()) %>% filter(M>20) %>%  
      mutate(SpreadVolatility=runSD(SpreadReturn, 20)*sqrt(252)) 
    res %>% 
      group_by(Contract) %>% reframe(M = median(Volatility, na.rm=T), S = sd(Volatility, na.rm=T)/sqrt(n())*2) %>% 
       ggplot(aes(Contract, ymin=M-S, ymax=M+S)) + geom_errorbar(width=0.5) + xlab("Contract") + ggtitle("Volatility")  -> p
    ggsave(filename = paste0(symbol, "_contract.png"), p,  height = 9, width = 12,dpi=150)
    
    res  %>% 
      group_by(Spread) %>% reframe(M = median(SpreadVolatility, na.rm=T), S = sd(SpreadVolatility, na.rm=T)/sqrt(n())*2) %>% separate(Spread, into = c("Spread1", "Spread2"), sep = " ") %>% mutate(Spread1=as.numeric(Spread1)) %>% arrange(Spread1) %>% 
      ggplot(aes(Spread1, ymin=M-S, ymax=M+S)) + geom_errorbar(width=0.5) + xlab("Spread") + ggtitle("Volatility")  -> p
    ggsave(filename = paste0(symbol, "_spread.png"), p,  height = 9, width = 12,dpi=150)
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
  ## Done fot RobotWealth
  {
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


