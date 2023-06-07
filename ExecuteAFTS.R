{
  library(tidyverse)
  library(moments)
  library(TTR)
  library(lubridate)
  library(tsibble)
  library(zoo)
  library(moments)
  library(ggthemes)
}



# Functions
{
  decimalnumcount<-function(x){
    stopifnot(class(x)=="numeric")
    x <- as.character(x)
    x<-gsub("(.*)(\\.)|([0]*$)","",x)
    nchar(x)
  }
# for some reason, scrapped CMC daily data are leaded one day, check for example https://www.cmcmarkets.com/en-gb/instruments/coffee-arabica-jul-2023?search=1
# weekly data is leaded 2 days
load_cmc_cash_data <- function(symbol,  dir, lagged=TRUE){
  symbol_dir <- paste0(dir, "/", symbol)
  # load daily data, lag date by one day
  files <- list()
  for (l in list.files(symbol_dir, pattern = "daily")) {
    f <- read_csv(paste0(symbol_dir, "/", l), show_col_types = FALSE, col_names = FALSE)
    colnames(f) <- c("Date", "Close")
    if(lagged) 
      f <- f %>% mutate(Date = as_date((ifelse(wday(Date) == 5, Date+2, Date+1  ))))
    files[[l]] <- f
  }
  # merge into one continous time series, remove possible dubplicates by keeping the last one
  df_daily = do.call(rbind, files) %>% arrange(Date) %>% group_by(Date) %>% summarize(Date=last(Date), Close=last(Close)) %>% ungroup
  # load weekly data, lag date by two days
  files <- list()
  for (l in list.files(symbol_dir, pattern = "weekly")) {
    f <- read_csv(paste0(symbol_dir, "/", l), show_col_types = FALSE, col_names = FALSE)
    colnames(f) <- c("Date", "Close")
    if(lagged) {
      f[,1] <- f[,1]+2
      f <- f[-1,]
    }
    files[[l]] <- f
  }
  # merge into one continous time series, remove possible dubplicates by keeping the last one
  df_weekly = do.call(rbind, files) %>% arrange(Date) %>% group_by(Date) %>% summarize(Date=last(Date), Close=last(Close)) %>% ungroup
  # only keep weekly data up to the start of daily data
  df_weekly <- dplyr::filter(df_weekly, Date < df_daily$Date[7])
  # interpolate weekly data to create daily data
  # first, recreate full daily Date excluding weekends
  dates <- seq(df_weekly$Date[1], df_weekly$Date[length(df_weekly$Date)], by=1) %>% as_tibble() %>% mutate(Date=value) %>% select(-value) %>% dplyr::filter(!(wday(Date, label = TRUE) %in% c("Sat", "Sun")))
  # then interpolate
  df_weekly <- merge(df_weekly, dates, by="Date", all=TRUE) %>% mutate(Close=na.approx(Close))
  # merge all data
  df <- rbind(df_weekly, df_daily) %>% group_by(Date) %>% summarize(Date=last(Date), Close=last(Close)) %>% ungroup %>% arrange(Date)
  
  # load last holding cost
  l <- tail(sort(list.files(symbol_dir, pattern = "holding_cost")), 1)
  f <- read_csv(paste0(symbol_dir, "/", l), show_col_types = FALSE, col_names = FALSE)
  if(dim(f)[1] == 0) {
    warning(paste("Holding cost file empty for symbol:", symbol))
    hc <- c(0,0)        
  } else {
    hc <- unlist(f[,-1])
  }
  return(list("Price"=df, "HC"=hc))
}
cap_forecast <- function(x, cap=20) {
  return(ifelse(x > cap, cap, ifelse(x < -cap, -cap, x ) ))
}
multiple_EMA <- function(adjclose, close, risk, spans=c(4, 8, 16, 32, 64), scalars=c(8.53, 5.95, 4.1, 2.79, 1.91), mult=4, cap=20, period=252) {
  n <- length(spans)
  EWMACs <- lapply(1:n, function(i) EMA(adjclose, spans[i]) -  EMA(adjclose, spans[i]*mult))
  EWMACs <- lapply(1:n, function(i) EWMACs[[i]] / (close * risk / sqrt(period)) * scalars[i] )
  EWMACs <- lapply(1:n, function(i) cap_forecast(EWMACs[[i]], cap))
  forecast <- rowMeans(do.call(cbind, EWMACs))
  return(forecast)
}
multiple_AS <- function(adjclose, close, risk, spans=c(21, 63, 126, 252, 504), scalars=c(28.53, 49.09, 68.99, 97.00, 136.61), cap=20, period=252) {
  n <- length(spans)
  ASs <- lapply(1:n, function(i) AbsoluteStrength(adjclose, spans[i]))
  ASs <- lapply(1:n, function(i) ASs[[i]] / (close * risk / sqrt(period)) * scalars[i] )
  ASs <- lapply(1:n, function(i) cap_forecast(ASs[[i]], cap))
  forecast <- rowMeans(do.call(cbind, ASs))
  return(forecast)
}
multiple_DC <- function(adjclose, close, risk, spans=c(21, 63, 126, 252, 504), scalars=c(31.2, 33.4, 34.3, 34.9, 35.3), cap=20, period=252) {
  n <- length(spans)
  DCs <- lapply(1:n, function(i) {dc <- DonchianChannel(adjclose, spans[i]); (adjclose - dc[,2]) / abs(dc[,1] - dc[,3])})
  DCs <- lapply(1:n, function(i) DCs[[i]] * scalars[i] )
  DCs <- lapply(1:n, function(i) cap_forecast(DCs[[i]], cap))
  forecast <- rowMeans(do.call(cbind, DCs))
  return(forecast)
}
multiple_KF <- function(adjclose, close, risk, spans=c(0.5, 1, 2, 5, 10), scalars=c(66, 55, 46, 37, 31), cap=20, period=252) {
  n <- length(spans)
  KFs <- lapply(1:n, function(i) KalmanFilterIndicator(adjclose, sharpness = 1, K = spans[i])[,2])
  KFs <- lapply(1:n, function(i) KFs[[i]] / ((close * risk / sqrt(period))) * scalars[i] )
  KFs <- lapply(1:n, function(i) cap_forecast(KFs[[i]], cap))
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
  EMAs <- lapply(1:n, function(i)  cap_forecast(EMAs[[i]], cap))
  forecast <- rowMeans(do.call(cbind, EMAs))
  return(forecast)
}
relative_volatility <- function(volatility, period=2520) {
  return(unlist(Map(function(i) mean(tail(volatility[1:i], period), na.rm=TRUE), 1:length(volatility))))
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
cross_sectional_momentum <- function(instrument_np, class_np, spans=c(20, 40, 80, 160, 320), scalars=c(108.5, 153.5, 217.1, 296.8, 376.3)) {
  n <- length(spans)
  relative_np  <- (instrument_np - class_np) / 100
  Os <- lapply(1:n, function(i) EMA(c(rep(NA, spans[i]), diff(relative_np, lag=spans[i]) / spans[i]), ceiling(spans[i]/4)) * scalars[i])
  Os <- lapply(1:n, function(i) ifelse(Os[[i]] > cap, cap, ifelse(Os[[i]] < -cap, -cap, Os[[i]] ) ))
  forecast <- rowMeans(do.call(cbind, Os))
  return(forecast)
}
returns_skew <- function(returns, spans=c(60, 120, 240), scalars=c(33.3, 37.2, 39.2), cap=20) {
  n <- length(spans)
  returns[is.na(returns)] <- 0
  Skews <- lapply(1:n, function(i) -rollapply(returns, width=spans[i], skewness,  fill=NA, align="right"))
  Skews <-  lapply(1:n, function(i) replace(Skews[[i]], is.na(Skews[[i]]), 0))
  Skews <-  lapply(1:n, function(i) EMA(Skews[[i]], ceiling(spans[i]/4)) * scalars[i])
  Skews <- lapply(1:n, function(i) cap_forecast(Skews[[i]], cap))
  forecast <- rowMeans(do.call(cbind, Skews))
  return(forecast)
}



}



# Parameters (maybe pu them in a config file?)
{
main_dir <- "/home/marco/trading/Systems/Monopoly/Execution/"
instrument_file <- "/home/marco/trading/Systems/Monopoly/Execution/INSTRUMENTS.csv"
FX_file <- "/home/marco/trading/Systems/Monopoly/Execution/FX.csv"
scrape_dir <- "/home/marco/trading/Systems/Monopoly/Execution/Scraping/"
FX_dir <- "/home/marco/trading/Systems/Monopoly/Execution/FX/"
scrape_script <- "scrape_data.sh"
target_vol <- 0.50
IDM = 2.5
FDMtrend <- 1.3
FDMcarry <- 3.3
FDMskew <- 1.1
strategy_weights <- list("Trend" = 0.4, "Carry" = 0.5, "Skew" = 0.1)
buffering <- 0.1
trade_shadow_cost <- 10
}

### TODO
# check instruments_data is always well arranged by Date, check last row is yesterday's trading day

{
  # scrape price and FX data
  setwd(main_dir)
  system(paste("bash", scrape_script, scrape_dir, instrument_file, FX_dir, FX_file))
  
  # instruments infos
  instruments_info <- read_csv(instrument_file, col_names = TRUE, show_col_types = FALSE)
  # calculate instruments_info weights (could be coded a little better maybe?)
  instruments_info <- instruments_info %>% arrange(Symbol)
  instruments_info$Weight <- instruments_info %>% group_by(Symbol, Class1, Class2) %>% 
    summarise(Symbol=Symbol, n0=length(unique(instruments_info$Class1)), n1=length(unique(instruments_info$Class2[instruments_info$Class1==Class1])) , n2=length((instruments_info$Class2[instruments_info$Class2==Class2]))) %>% 
    ungroup %>% mutate(Weight=1/n0/n1/n2) %>% pull(Weight)
  
  # load price data from previous scrape
  instruments_data <- list()
  for(symbol in instruments_info$Symbol)
    instruments_data[[symbol]] <- load_cmc_cash_data(symbol, scrape_dir)
  
  # load FX data from previous scrape
  FX_names <- read_csv(FX_file, col_names = FALSE, show_col_types = FALSE) %>% unlist 
  files <- list()
  for(fx in FX_names) {
    f <- read_csv(paste0(FX_dir, "/", fx, ".csv"), col_names = FALSE, show_col_types = FALSE)
    colnames(f) <- c("Date", "Rate")
    files[[fx]] <- arrange(f, Date) %>% tail(2) %>% head(1) # we take yesterday's rate, to match CMC data
  }
  FX_rates <- do.call(rbind, files) %>% mutate(FX=toupper(sub("eur", "", FX_names))) 
  colnames(FX_rates) <- c("Date", "Rate", "FX")
  
  # the covariance matrix
  closes <- lapply(instruments_data, function(x)x[[1]])
  vols <- unlist(lapply(instruments_data, function(x) tail(calculate_volatility(x[[1]][,2]), 1)))
  closes_merged <- Reduce(function(...) full_join(..., by="Date"), closes) %>% arrange(Date) %>% select(-Date)
  colnames(closes_merged) <- names(instruments_data)
  returns_merged <- apply(closes_merged, 2, function(x) c(0,diff(log(x)))) 
  cor_matrix <- cor(returns_merged, use="pairwise.complete.obs")
  cov_matrix <- diag(vols) %*% cor_matrix %*% diag(vols)
  
  # iterate over data and calculate positions
  results <- list()
  for(symbol in names(instruments_data)) {
    print(symbol)
    df <- instruments_data[[symbol]][[1]]
    hc <- instruments_data[[symbol]][[2]]
    df$Symbol <- symbol
    df$PositionRaw <- df$PositionRounded <- df$PositionDynamic <- df$PositionDynamic <- df$Forecast <- df$ForecastTrend <- df$ForecastCarry <- df$ForecastSkew <- 0
    df$Return <- c(0, diff(log(df$Close)))
    df$Volatility = calculate_volatility(df$Return)
    fx <- dplyr::filter(instruments_info, Symbol == symbol) %>% pull(FX)
    if(fx == "EUR") {
      df$FX <- 1 
    } else {
      df$FX <- dplyr::filter(FX_rates, FX == fx) %>% pull(Rate)
    }
    df$Weight <- dplyr::filter(instruments_info, Symbol == symbol) %>% pull(Weight)
    df$Product <- dplyr::filter(instruments_info, Symbol == symbol) %>% pull(Product)
    df$ContractSize <- dplyr::filter(instruments_info, Symbol == symbol) %>% pull(ContractSize)
    df$MinPosition <- dplyr::filter(instruments_info, Symbol == symbol) %>% pull(MinPosition)
    df$TickSize <- dplyr::filter(instruments_info, Symbol == symbol) %>% pull(TickSize)
    df$Decimals <- dplyr::filter(instruments_info, Symbol == symbol) %>% pull(Decimals)
    df$Spread <- dplyr::filter(instruments_info, Symbol == symbol) %>% pull(Spread)
    df$Commission <- dplyr::filter(instruments_info, Symbol == symbol) %>% pull(Commission)
    df$CostPerTrade <- (df$Spread / 2 + df$Commission) 
    
    # Relative volatility (strategy 13, it does not seems to add much)
    # df$M <- 1
    # df$RV <- relative_volatility(df$Volatility) # quite slow, you can replace it with df$Volatility / runMean(df$Volatility, 2520))
    # df$RV <- df$Volatility / runMean(df$Volatility, 252)
    # df$Q <- sapply(1:length(df$RV), function(i) sum(df$RV[i] > df$RV[1:i], na.rm=TRUE) / i)
    # df$M <- EMA(2 - 1.5 * df$Q, 10)
    
    # Trend-following (strategy 9)
    df$ForecastTrend <- rowMeans(do.call(cbind, lapply(list(multiple_EMA, multiple_DC, multiple_KF, multiple_TII), function(f) f(df$Close, df$Close, df$Volatility))))
    df$ForecastTrend <- cap_forecast(df$ForecastTrend * FDMtrend) 
    
    # Carry
    # It is based on cash contract interest rate, we remove the 3% commission, so low volatility assets like
    # bonds are not negatively affected in this analysis. Of course low volatility assets must be played on the future contract
    # if you want to obtain the carry, as interest rates of the cash are always negative (because of the 3% commission that
    # is higher than the volatility itself). For high volatility assets you can choose between cash or future.
    hc_max <- which.max(hc)
    hc_commission <- ifelse(df$Product[1] == "Cash", 0, 0.03)
    df$ForecastCarry <- ifelse(hc_max == 1, 1, -1) * (hc[hc_max]+hc_commission) / df$Volatility * 10 
    df$ForecastCarry <- cap_forecast(df$ForecastCarry * FDMcarry)
    
    # Skewness (strategy 24)
    df$ForecastSkew <- returns_skew(df$Return) 
    df$ForecastSkew <- cap_forecast(df$ForecastSkew * FDMskew)

    # Final trade
    df$Forecast <- (
        strategy_weights$Trend * df$ForecastTrend + 
        strategy_weights$Carry * df$ForecastCarry + 
        strategy_weights$Skew  * df$ForecastSkew   ) 
    df$Forecast <- cap_forecast(df$Forecast)
    df$InstCapital <- capital * df$Weight * IDM * target_vol/df$Volatility
    df$Exposure <- capital * df$Weight * IDM * target_vol/df$Volatility * 1/df$FX  
    df$PositionRaw <-  (df$Exposure * df$Forecast/10) /
                    (df$ContractSize * df$Close) 
    df$PositionRounded <- ifelse(abs(df$PositionRaw) < df$MinPosition,  0, round(df$PositionRaw, df$Decimals) )
    # df$Buffer <-  df$Exposure * buffering /
    #   (df$ContractSize * df$Close) 
    # df$BufferUp <- df$Position + df$Buffer
    # df$BufferLow <- df$Position - df$Buffer
    # A little simulation of position buffering, it can be removed
    # df$PositionSim <- NA
    # df$PositionSim[1] <- 0
    # for(i in 2:nrow(df)) {
    #   df$PositionSim[i] <- df$PositionSim[i-1]
    #   if(is.na(df$BufferUp[i]) | is.na(df$Position[i]))
    #     next
    #   if(df$PositionSim[i-1] > df$BufferUp[i] | df$PositionSim[i-1] < df$BufferLow[i])
    #     df$PositionSim[i] <- df$Position[i]
    # }
    df <- arrange(df, desc(Date))
    #write_csv(df, paste0("/home/marco/trading/Systems/Monopoly/Execution/Logs/Detailed/", symbol, ".csv"))
    results[[symbol]] <- df[1,]
  }
  # Final table
  today_trading <- do.call(rbind, results)
  # Dynamic portfolio
  optimal_positions <- with(today_trading, PositionRaw)
  notional_exposures <- with(today_trading, ContractSize * Close / FX)
  costs_per_contract <- with(today_trading, ContractSize * (Spread/2) / FX)
  digits <- decimalnumcount(today_trading$TickSize)-1
  today_trading$PositionDynamic <- dynamic_portfolio(capital, optimal_positions[1:j], notional_exposures[1:j],  cov_matrix[1:j,1:j], NULL,
                                                     costs_per_contract= 0*costs_per_contract, trade_shadow_cost = 0*trade_shadow_cost, digits = digits)
}

dynamic_portfolio <- function(capital, optimal_positions, notional_exposures, cov_matrix, previous_position=NULL, costs_per_contract=NULL, trade_shadow_cost=1, fractional=NULL) {
  calculate_costs <- function(weights) {
    trade_gap <- weights - weights_previous
    trade_costs <- sum(abs(costs_per_contract_in_weight * trade_gap * trade_shadow_cost))
    return(trade_costs)
  }
  evaluate <- function(weights_optimal, weights, cov_matrix) {
    solution_gap <- weights_optimal - weights
    track_error <- as.numeric(sqrt(t(solution_gap) %*%  cov_matrix %*% solution_gap))
    trade_costs = calculate_costs(weights)
    return(track_error + trade_costs)
  }
  find_possible_new_best <- function(weights_optimal, per_contract_value, direction, best_solution, best_value, cov_matrix){
    new_best_value = best_value
    new_solution = best_solution
    count_assets = length(best_solution)
    for(i in 1:count_assets) {
      temp_step <- best_solution
      temp_step[i] = temp_step[i] + per_contract_value[i] * direction[i]
      temp_objective_value <-  evaluate(weights_optimal, temp_step, cov_matrix)
      if(temp_objective_value < new_best_value) {
        new_best_value = temp_objective_value
        new_solution = temp_step
      }
    }
    return (list(new_best_value, new_solution))
  }
  n <- nrow(cov_matrix)
  if(is.null(previous_position))
    previous_position <- rep(0, n)
  if(is.null(costs_per_contract))
    costs_per_contract <- rep(0, n)
  # fractional is a new element. In the original algorithm you are force to move by one integer contract,
  # but with CFDs you can move by fractions. So later "weights_per_contract" is actually "weights_per_fraction"
  if(is.null(fractional))
    fractional <- rep(1, n)

  weights_per_contract <- notional_exposures  / capital 
  weights_optimal <- optimal_positions * weights_per_contract 
  weights_previous <- previous_position * weights_per_contract 
  costs_per_contract_in_weight <- (costs_per_contract / fractional / capital) / weights_per_contract
  best_solution <- rep(0, n)
  best_value <- evaluate(weights_optimal, best_solution, cov_matrix)
  while(1) {
    res <- find_possible_new_best(weights_optimal, weights_per_contract, sign(weights_optimal), best_solution, best_value, cov_matrix)
    new_best_value <- res[[1]]
    new_solution <- res[[2]]
    if(new_best_value < best_value) {
      best_value = new_best_value
      best_solution = new_solution
    } else
      break
    #print(best_value)
  }
  return(best_solution / (weights_per_contract))
}

dynamic_portfolio <- function(capital, optimal_positions, notional_exposures, cov_matrix,
                              previous_position = NULL, costs_per_contract = NULL, trade_shadow_cost = 1, digits = NULL) {
  calculate_costs <- function(weights) {
    trade_gap <- weights - weights_previous
    trade_costs <- sum(abs(costs_per_contract * trade_gap * trade_shadow_cost))
    return(trade_costs)
  }
  evaluate <- function(weights_optimal, weights, cov_matrix) {
    solution_gap <- weights_optimal - weights
    track_error <- as.numeric(sqrt(t(solution_gap) %*% cov_matrix %*% solution_gap))
    trade_costs <- calculate_costs(weights)
    # print(paste(track_error, trade_costs))
    return(track_error + trade_costs)
  }
  find_possible_new_best <- function(weights_optimal, per_contract_value, direction, best_solution, best_value, cov_matrix) {
    new_best_value <- best_value
    new_solution <- best_solution
    count_assets <- length(best_solution)
    for (i in 1:count_assets) {
      # print(i)
      temp_step <- best_solution
      temp_step[i] <- temp_step[i] + per_contract_value[i] * direction[i]
      temp_objective_value <- evaluate(weights_optimal, temp_step, cov_matrix)
      if (temp_objective_value < new_best_value) {
        new_best_value <- temp_objective_value
        new_solution <- temp_step
        # print("acepted")
      }
    }
    return(list(new_best_value, new_solution))
  }
  n <- nrow(cov_matrix)
  if (is.null(previous_position)) {
    previous_position <- rep(0, n)
  }
  if (is.null(costs_per_contract)) {
    costs_per_contract <- rep(0, n)
  }
  if (is.null(digits)) {
    multiplier <- rep(1, n)
  } else {
    multiplier <- 10^digits
  }
  weights_per_contract <- notional_exposures * multiplier / capital
  weights_optimal <- optimal_positions * weights_per_contract / multiplier
  weights_previous <- previous_position * weights_per_contract / multiplier
  costs_per_trade_in_weight <- (costs_per_contract / multiplier / capital) / weights_per_contract
  print(weights_optimal)
  best_solution <- rep(0, n)
  best_value <- evaluate(weights_optimal, best_solution, cov_matrix)
  while (1) {
    res <- find_possible_new_best(weights_optimal, weights_per_contract, sign(weights_optimal), best_solution, best_value, cov_matrix)
    new_best_value <- res[[1]]
    new_solution <- res[[2]]
    if (new_best_value < best_value) {
      best_value <- new_best_value
      best_solution <- new_solution
    } else {
      break
    }
    # print(best_value)
  }
  return(best_solution / (weights_per_contract / multiplier))
}



res <- list()
for(symbol in names(instruments_data)) {
  print(symbol)
  df <- instruments_data[[symbol]][[1]]
  hc <- instruments_data[[symbol]][[2]]
  df$Return <- c(0, diff(log(df$Close)))
  df$Volatility = calculate_volatility(df$Return)
  df$Position = target_vol / df$Volatility
  res[[symbol]] <- (max(hc)+0.03)  / df$Volatility 
  plot.ts(res[[symbol]], main=symbol)#, ylim=c(-30,30)); abline(h=c(-20, 0, 20))
}


