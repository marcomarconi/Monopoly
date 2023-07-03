{
  suppressMessages(library(tidyverse))
  suppressMessages(library(moments))
  suppressMessages(library(TTR))
  suppressMessages(library(lubridate))
  suppressMessages(library(tsibble))
  suppressMessages(library(zoo))
  suppressMessages(library(moments))
  suppressMessages(library(ggthemes))
}



# Functions
{
  decimalplaces <- function(x) {
    if ((x %% 1) != 0) {
      nchar(strsplit(sub('0+$', '', as.character(x)), ".", fixed=TRUE)[[1]][[2]])
    } else {
      return(0)
    }
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
  
  calculate_volatility <- function(returns, long_span=252, short_span=35,  weights=c(0.3, 0.7), period=252){
    vol_short <- sqrt(EMA(replace(returns, is.na(returns), 0)^2, short_span))
    vol_long <- runMean(vol_short, long_span)
    vol <-  (weights[1] * vol_long + weights[2] * vol_short) * sqrt(period) # one year instead of ten
    return(vol)
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
  
  KalmanFilterIndicator <- function(x, sharpness=1, K=1) {
    n <- length(x)
    value <- rep(NA, n)
    distance <- rep(NA, n)
    velocity <- rep(NA, n)
    error <- rep(NA, n)
    value[1] <- x[1]
    velocity[1] <- 0
    distance[1] <- 0
    error[1] <- 0
    for(i in 2:length(x)){
      distance[i] <- x[i] - value[i-1]
      error[i] <- value[i-1] + distance[i] * sqrt(sharpness * K / 100)
      velocity[i] <- velocity[i-1] + distance[i]*K/100
      value[i] <- error[i]+velocity[i]
    }
    return(cbind(value=value, velocity=velocity, distance=distance, error=error))
  }
  
  multiple_KF <- function(adjclose, close, risk, spans=c(0.5, 1, 2, 5, 10), scalars=c(66, 55, 46, 37, 31), cap=20, period=252) {
    n <- length(spans)
    KFs <- lapply(1:n, function(i) KalmanFilterIndicator(adjclose, sharpness = 1, K = spans[i])[,2])
    KFs <- lapply(1:n, function(i) KFs[[i]] / ((close * risk / sqrt(period))) * scalars[i] )
    KFs <- lapply(1:n, function(i) cap_forecast(KFs[[i]], cap))
    forecast <- rowMeans(do.call(cbind, KFs))
    return(forecast)
  }
  TII <- function(x, P = 60, ma=TTR::EMA) {
    ma_p <- ma(x, P)
    diff <- x - ma_p
    pos_count <- runSum(diff>0, floor(P/2))
    return(400 * (pos_count) / P - 100)
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
  
  round_position <- function(position, min_position, ticksize) {
    return(ifelse(abs(position) < min_position,  0, round(position, sapply(ticksize, decimalplaces ))))
  }
  

  ## greedy algorithm to find a set of positions closest to the optimal provided. 
  # capital : your money
  # optimal_positions : the best unrounded positions (in contracts) corresponding to your forecast
  # max_positions : the max allowed positions (in absolute contracts), usually corresponding to a forecast of 20
  # notional_exposures : the value of one contract (usually ContractSize * Price / FX)
  # cov_matrix : covariance matrix of assets (daily or weekly) returns, usually last 6 months
  # previous_position : the previous optimal positions. Zero if not provided
  # costs_per_contract : the cost to trade one contract, in price scale. 
  # trade_shadow_cost : the expected number of trades in one year.
  # fractional : TRUE is your broker allow fractional contracts, like for CFDs. FALSE for futures.
  # max_factor : maximum multiple of optimal position allowed (e.g. if optimal position = 2, the optimized position will be <= 4).
  dynamic_portfolio <- function(capital, optimal_positions, max_positions, notional_exposures, cov_matrix,
                                previous_position = NULL, costs_per_contract = NULL, trade_shadow_cost = 1, fractional=TRUE, max_factor=2) {
    # Calculate the cost of making trades. trade_shadow_cost represents the number of expected trades in year, set it zero to ignore trade costs  
    calculate_costs <- function(weights) {
      trade_gap <- abs(weights_previous - weights)
      trade_costs <- trade_shadow_cost * sum(trade_gap * costs_per_trade_in_weight)
      return(trade_costs)
    }
    # Calculate the error of given weights from the optimal weights considering instruments correlations, plus optional costs
    evaluate <- function(weights_optimal, weights, cov_matrix) {
      solution_gap <- weights_optimal - weights
      track_error <- as.numeric(sqrt(t(solution_gap) %*% cov_matrix %*% solution_gap))
      trade_costs <- calculate_costs(weights)
      return(track_error + trade_costs)
    }
    find_possible_new_best <- function(weights_optimal, weights_max, per_contract_value, direction, best_solution, best_value, cov_matrix, max_factor) {
      new_best_value <- best_value
      new_solution <- best_solution
      count_assets <- length(best_solution)
      for (i in sample(1:count_assets)) {
        temp_step <- best_solution
        temp_step[i] <- temp_step[i] + per_contract_value[i] * fractional[i] * direction[i]
        if(abs(temp_step[i]) > weights_max[i])
          temp_step[i] <- weights_max[i] * sign(temp_step[i])
        else if (abs(temp_step[i]) > max_factor * abs(weights_optimal[i]))
          temp_step[i] <- max_factor * weights_optimal[i] 
        temp_objective_value <- evaluate(weights_optimal, temp_step, cov_matrix)
        if (temp_objective_value < new_best_value) {
          new_best_value <- temp_objective_value
          new_solution <- temp_step
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
    if (!fractional) {
      fractional <- rep(1, n)
    } else {
      fractional <-  10^(floor(log10(abs(optimal_positions)))-1)
    }
    weights_per_contract <- notional_exposures / capital
    weights_optimal <- optimal_positions * weights_per_contract 
    weights_max <- max_positions * weights_per_contract
    weights_previous <- previous_position * weights_per_contract
    costs_per_trade_in_weight <- (costs_per_contract  / capital) / weights_per_contract
    best_solution <- rep(0, n)
    best_value <- evaluate(weights_optimal, best_solution, cov_matrix)
    while (1) {
      res <- find_possible_new_best(weights_optimal, weights_max, weights_per_contract, sign(weights_optimal), best_solution, best_value, cov_matrix, max_factor)
      new_best_value <- res[[1]]
      new_solution <- res[[2]]
      if (new_best_value < best_value) {
        best_value <- new_best_value
        best_solution <- new_solution
      } else {
        break
      }
    }
    return(best_solution / weights_per_contract)
  }
  
  buffering_portfolio <- function(capital, optimized_position, previous_position, notional_exposures, cov_matrix, portfolio_buffering_level, target_vol) {
    weights_per_contract <- notional_exposures / capital
    optimized_portfolio_weight <- optimized_position * weights_per_contract 
    previous_portfolio_weight <- previous_position * weights_per_contract 
    tracking_error_current_weight <- optimized_portfolio_weight - previous_portfolio_weight
    tracking_error <- as.numeric(sqrt(t(tracking_error_current_weight) %*% cov_matrix %*% tracking_error_current_weight))
    adjustment_factor <- max((tracking_error - portfolio_buffering_level/2 * target_vol) / tracking_error, 0)
    required_trade <- adjustment_factor * (optimized_position - previous_position) 
    return(list(required_trade, tracking_error, adjustment_factor))
  }
  
}


# Parameters (maybe pu them in a config file?)
{
  main_dir <- "/home/marco/trading/Systems/Monopoly/Execution/"
  positions_file <- paste0(main_dir, "POSITIONS.csv")
  instrument_file <- paste0(main_dir, "INSTRUMENTS.csv")
  FX_file <- paste0(main_dir, "FX.csv")
  scrape_dir <- paste0(main_dir, "Scraping/")
  FX_dir <- paste0(main_dir, "FX/")
  logs_dir <- paste0(main_dir, "Logs/")
  logs_instruments_dir <- paste0(logs_dir, "Instruments/")
  scrape_script <- "SCRAPE_DATA.sh"
  target_vol <- 0.50
  IDM = 2.5
  FDMtrend <- 1.3
  FDMcarry <- 3.3
  FDMskew <- 1.1
  strategy_weights <- list("Trend" = 0.4, "Carry" = 0.5, "Skew" = 0.1)
  corr_days <- 120
  portfolio_buffering_level <- 0.1
  position_buffering_level <- 0.1
  trade_shadow_cost <- 0
}

### TODO
# check instruments_data is always well arranged by Date, check last row is yesterday's trading day

# Execute
args = commandArgs(trailingOnly=TRUE)
# recent lines of data to ignore, in case you skipped a day
capital <- 0
if(length(args) == 1) {
  capital <- as.numeric(args[1])  
}else{
  stop("Provide capital as arguments.")
}
if(capital <= 0 | is.na(capital))
  stop("Capital must be positive number")

{
  print(paste("Capital:", capital, "Target Volatility:", target_vol, "IDM:", IDM, "Buffering level:", portfolio_buffering_level))
  # create dirs&files
  today_string <- gsub("-", "", today())
  now_string <- gsub("-| |:", "", now())
  if(!dir.exists(logs_dir))
    dir.create(logs_dir)
  if(!dir.exists(logs_instruments_dir))
    dir.create(logs_instruments_dir)
  if(!dir.exists(scrape_dir))
    dir.create(scrape_dir)
  if(!dir.exists(FX_dir))
    dir.create(FX_dir)
  
  # load instruments infos and calculate instruments weights from asset classes groups (could be coded a little better maybe?)
  print("Loading symbols info and previous positions file...")
  instruments_info <- read_csv(instrument_file, col_names = TRUE, show_col_types = FALSE) %>% arrange(Symbol)
  instruments_info$Weight <- instruments_info %>% group_by(Symbol) %>% 
    summarise(Symbol=Symbol, 
              n0=length(unique(instruments_info$Class1)), 
              n1=length(unique(instruments_info$Class2[instruments_info$Class1==Class1])), 
              n2=length((instruments_info$Class2[instruments_info$Class2==Class2]))) %>% 
    ungroup %>% mutate(Weight=1/n0/n1/n2) %>% pull(Weight)
  
  # load previous positions file
  if(!file.exists(positions_file))
    stop("Previous positions file does not exists.")
  previous_trading <- read_csv(positions_file, col_names = TRUE, show_col_types = FALSE) %>% arrange(Symbol)

  # scrape price and FX data
  print("Scraping price and FX data...")
  setwd(main_dir)
  system(paste("bash", scrape_script, scrape_dir, instrument_file, FX_dir, FX_file))

  # load price data from previous scrape
  print("Loading price data...")
  instruments_data <- list()
  for(symbol in instruments_info$Symbol) {
    cat(paste(symbol, ""))
    instruments_data[[symbol]] <- load_cmc_cash_data(symbol, scrape_dir)
    nas <- sum(is.na(instruments_data[[symbol]]$Price$Close))
    if(nas > 0) {
      warning(paste(symbol, "price data has", nas, "NAs. They have been filled"))
      instruments_data[[symbol]]$Price$Close <- na.locf(instruments_data[[symbol]]$Price$Close, na.rm=FALSE)
    }
  }
  
  # load FX data from previous scrape
  print("")
  print("Loading FX data...")
  FX_names <- read_csv(FX_file, col_names = FALSE, show_col_types = FALSE) %>% unlist 
  files <- list()
  for(fx in FX_names) {
    f <- read_csv(paste0(FX_dir, "/", fx, ".csv"), col_names = TRUE, show_col_types = FALSE)
    colnames(f) <- c("Date", "Rate")
    files[[fx]] <- arrange(f, Date) %>% na.locf(na.rm=FALSE) %>% tail(2) %>% head(1) # we take yesterday's rate, to match CMC data
  }
  FX_rates <- do.call(rbind, files) %>% mutate(FX=toupper(sub("eur", "", FX_names))) 
  colnames(FX_rates) <- c("Date", "Rate", "FX")
  
  # the covariance matrix
  print("Calculate covariance matrix...")
  closes <- lapply(instruments_data, function(x)x[[1]])
  closes_merged <- Reduce(function(...) full_join(..., by="Date"), closes) %>% arrange(Date) 
  colnames(closes_merged) <- c("Date", names(instruments_data))
  returns_merged <- data.frame(Date=closes_merged$Date, apply(closes_merged[,-1], 2, function(x) c(0,diff(log(x))))) 
  vols <- data.frame(Date=returns_merged$Date, apply(returns_merged[,-1], 2, function(x) calculate_volatility(x))) 
  cor_matrix <- cor(tail(returns_merged[,-1], corr_days), use="pairwise.complete.obs")
  last_day_vol <- tail(vols, 1)[-1]
  cov_matrix <- diag(last_day_vol) %*% cor_matrix %*% diag(last_day_vol)
  rownames(cov_matrix) <- colnames(cov_matrix) <- names(instruments_data)
  

  # iterate over data and calculate positions
  print("Calculate new positions...")
  results <- list()
  for(symbol in names(instruments_data)) {
    print(symbol)
    df <- instruments_data[[symbol]][[1]]
    hc <- instruments_data[[symbol]][[2]]
    df$Symbol <- symbol
    df$ForecastTrend <- df$ForecastCarry <- df$ForecastSkew <- df$Forecast <- df$PositionMax <- df$PositionOptimal <- df$PositionOptimized <- df$AdjFactor  <- df$RequiredTrade <- df$Buffer <- df$Trading <- df$PositionPrevious <- df$PositionUnrounded <- df$Position <- df$PositionRisk  <- 0
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
    {
    # df$M <- 1
    # df$RV <- relative_volatility(df$Volatility) # quite slow, you can replace it with df$Volatility / runMean(df$Volatility, 2520))
    # df$RV <- df$Volatility / runMean(df$Volatility, 252)
    # df$Q <- sapply(1:length(df$RV), function(i) sum(df$RV[i] > df$RV[1:i], na.rm=TRUE) / i)
    # df$M <- EMA(2 - 1.5 * df$Q, 10)
    }
    # Trend-following (strategy 9)
    df$ForecastTrend <- rowMeans(do.call(cbind, lapply(list(multiple_EMA, multiple_DC, multiple_KF, multiple_TII), function(f) f(df$Close, df$Close, df$Volatility))))
    df$ForecastTrend <- cap_forecast(df$ForecastTrend * FDMtrend) 
    
    # Carry 
    # It is based on cash contract interest rate, we remove the 3% commission, so low volatility assets like
    # bonds are not negatively affected in this analysis. Of course low volatility assets must be played on the future contract
    # if you want to obtain the carry, as interest rates of the cash are always negative (because of the 3% commission that
    # is higher than the volatility itself). For high volatility assets you can choose between cash or future.
    hc_max <- which.max(hc)
    hc_commission <- case_when(
                              df$Product[1] == "Cash" ~ 0,
                              df$Product[1] == "Index" ~ 0.03,
                              df$Product[1] == "Future" ~ 0.03,
                              TRUE ~ NA_real_)
    hc_value <- (hc[hc_max]+hc_commission)
    if(hc_value < 0)
      hc_value <- 0
    df$ForecastCarry <- ifelse(hc_max == 1, 1, -1) * hc_value / df$Volatility * 10 
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
    df$InstCapital <- capital * df$Weight * IDM 
    df$Exposure <- df$InstCapital * target_vol/df$Volatility 
    df$PositionOptimal <-  (df$Exposure * df$FX * df$Forecast/10) /
      (df$ContractSize * df$Close  ) 
    df$PositionMax <- (df$Exposure * df$FX * 2) /
      (df$ContractSize * df$Close  ) 
    # Be careful, now it is reverse-date sorted, you cannot run any other function like EMA etc..
    df <- arrange(df, desc(Date))
    write_csv(df, paste0(logs_instruments_dir, "/", symbol, ".csv"))
    results[[symbol]] <- df[1,]
  }
  # Final table
  today_trading <- do.call(rbind, results)
  if(!all(previous_trading$Symbol %in% today_trading$Symbol)) {
    missing_prev <- previous_trading$Symbol[!(previous_trading$Symbol %in% today_trading$Symbol)]
    missing_today <- today_trading$Symbol[!(today_trading$Symbol %in% previous_trading$Symbol)]
    stop(paste("Previous position symbols (POSITION file) and current symbols (INSTRUMENTS file) do not match. Missing in current: ", missing_prev, ", missing in previous: ", missing_today, "\nFix it manually."))
  }
  # Dynamic portfolio
  optimal_positions <- with(today_trading, PositionOptimal)
  max_positions <- with(today_trading, PositionMax)
  notional_exposures <- with(today_trading, ContractSize * Close / FX)
  costs_per_contract <- with(today_trading, ContractSize * (Spread/2) / FX)
  previous_position <- previous_trading$Position
  position_dynamic <- dynamic_portfolio(capital, optimal_positions, max_positions, notional_exposures,  cov_matrix, 
                                        previous_position = previous_position,
                                        costs_per_contract=costs_per_contract, trade_shadow_cost = trade_shadow_cost)
  position_optimized <- round_position(position_dynamic, today_trading$MinPosition,  today_trading$TickSize) 
  # Buffering
  res <- buffering_portfolio(capital, position_optimized, previous_position, notional_exposures, cov_matrix, portfolio_buffering_level, target_vol)
  required_trades <- res[[1]]
  portfolio_tracking_error <- res[[2]]
  adjustment_factor <- res[[3]]
  print(paste("Portfolio tracking error:", round(portfolio_tracking_error, 3), "against buffer level", portfolio_buffering_level/2*target_vol, "adjustment factor:", round(adjustment_factor, 3)))
  # Update final positions
  today_trading$PositionPrevious <- previous_position
  today_trading$PositionOptimized <- position_optimized
  today_trading$AdjFactor <- adjustment_factor
  today_trading$Buffer <- with(today_trading, position_buffering_level * (Exposure * FX * 10/10) / (ContractSize * Close)) # buffering is the minimal position change allowed, equal to 1 forecast
  today_trading$RequiredTrade <- required_trades
  today_trading$Trading <- with(today_trading, abs(RequiredTrade) > Buffer & abs(RequiredTrade) > TickSize & abs(RequiredTrade) > MinPosition)
  today_trading$PositionUnrounded <- with(today_trading,  ifelse(Trading, PositionPrevious +  RequiredTrade, PositionPrevious))  
  today_trading$Position <- with(today_trading,  round_position(PositionUnrounded, MinPosition, TickSize))  
  today_trading$Trading <- with(today_trading, ifelse(Position != PositionPrevious, TRUE, FALSE))
  today_trading$PositionRisk <- abs(with(today_trading, Position * ContractSize * (Close / FX) * Volatility)) %>% round(2)
  print("Positions to update:")
  trades <- today_trading %>% filter(Trading == TRUE) %>% select(Date, Close, Symbol, Position, PositionUnrounded, PositionPrevious, RequiredTrade, PositionOptimized, PositionOptimal, Forecast)
  print(trades, n=nrow(trades))
  write_csv(previous_trading, paste0(logs_dir, "/", now_string, ".POSITIONS.csv"))
  write_csv(today_trading, "POSITIONS.csv")
}



