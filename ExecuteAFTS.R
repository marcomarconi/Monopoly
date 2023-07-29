#TODO:
# minium position in dynamic portfolio: it should be first increment in the greedy algorithm
{
  suppressMessages(library(tidyverse))
  suppressMessages(library(moments))
  suppressMessages(library(TTR))
  suppressMessages(library(lubridate))
  suppressMessages(library(tsibble))
  suppressMessages(library(zoo))
  suppressMessages(library(moments))
  suppressMessages(library(ggthemes))
  suppressMessages(library(data.table))
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
  
  round_position <- function(position, min_position, position_tick) {
    return(ifelse(abs(position) < min_position,  0, round(position, sapply(position_tick, decimalplaces ))))
  }
  
  runCorMatrix <- function(M, n=25) {
    run_corr_matrices <- lapply(n:nrow(M), function(i) cor(M[(i-n):i,], use="pairwise.complete.obs"))
    run_corr_vectors <- lapply(run_corr_matrices, as.vector) 
    corr_by_date <- do.call(cbind, run_corr_vectors)
    ema_corr_by_date <- apply(corr_by_date, 1, EMA, n) %>% t 
    Q <- lapply(1:ncol(ema_corr_by_date), 
                function(i) matrix(ema_corr_by_date[,i], ncol=ncol(M), dimnames = list(colnames(M), colnames(M))))
    return(Q)
  }



  
  
  # for some reason, scrapped CMC daily data are leaded one day, check for example https://www.cmcmarkets.com/en-gb/instruments/coffee-arabica-jul-2023?search=1
  # weekly data is leaded 2 days
  load_cmc_cash_data <- function(symbol,  dir, lagged=TRUE){
    symbol_dir <- paste0(dir, "/", symbol)
    # # load daily data, lag date by one day
    # files <- list()
    # for (l in list.files(symbol_dir, pattern = "daily")) {
    #   #f <- read_csv(paste0(symbol_dir, "/", l), show_col_types = FALSE, col_names = FALSE)
    #   f <- fread(paste0(symbol_dir, "/", l), header= FALSE)
    #   colnames(f) <- c("Date", "Close")
    #   if(lagged) 
    #     f <- f %>% mutate(Date = as_date((ifelse(wday(Date) == 5, Date+2, Date+1  ))))
    #   files[[l]] <- f
    # }
    # # merge into one continous time series, remove possible dubplicates by keeping the last one
    # df_daily = do.call(rbind, files) %>% arrange(Date) %>% group_by(Date) %>% summarize(Date=last(Date), Close=last(Close)) %>% ungroup
    system(paste("cat", paste(list.files(symbol_dir, pattern = "daily", full.names = TRUE), collapse = " "),  " | sort -u  > _tmp"))
    df_daily <- fread("_tmp", header= FALSE)
    colnames(df_daily) <- c("Date", "Close")
    df_daily <- arrange(df_daily, Date)
    if(lagged) 
      df_daily <- df_daily %>% mutate(Date = as_date((ifelse(wday(Date) == 5, Date+2, Date+1  ))))
    # load weekly data, lag date by two days
    # files <- list()
    # for (l in list.files(symbol_dir, pattern = "weekly")) {
    #   #f <- read_csv(paste0(symbol_dir, "/", l), show_col_types = FALSE, col_names = FALSE)
    #   f <- fread(paste0(symbol_dir, "/", l), header= FALSE)
    #   colnames(f) <- c("Date", "Close")
    #   if(lagged) {
    #     f[,1] <- f[,1]+2
    #     f <- f[-1,]
    #   }
    #   files[[l]] <- f
    # }
    # # merge into one continous time series, remove possible dubplicates by keeping the last one
    # df_weekly = do.call(rbind, files) %>% arrange(Date) %>% group_by(Date) %>% summarize(Date=last(Date), Close=last(Close)) %>% ungroup
    # 
    system(paste("cat", paste(list.files(symbol_dir, pattern = "weekly", full.names = TRUE), collapse = " "),  " | sort -u  > _tmp"))
    df_weekly <- fread("_tmp", header= FALSE)
    colnames(df_weekly) <- c("Date", "Close")
    df_weekly <- arrange(df_weekly, Date)
    if(lagged) {
      df_weekly[,1] <- df_weekly[,1]+2
      df_weekly <- df_weekly[-1,]
    }
    # only keep weekly data up to the start of daily data
    df_weekly <- mutate(df_weekly, Date=as.Date(Date)) %>% dplyr::filter(Date < df_daily$Date[7])
    # interpolate weekly data to create daily data
    # first, recreate full daily Date excluding weekends
    dates <- seq(df_weekly$Date[1], df_weekly$Date[length(df_weekly$Date)], by=1) %>% 
      as_tibble() %>% mutate(Date=value) %>% select(-value) %>% dplyr::filter(!(lubridate::wday(Date, label = TRUE) %in% c("Sat", "Sun")))
    # then interpolate
    df_weekly <- merge(df_weekly, dates, by="Date", all=TRUE) %>% mutate(Close=na.approx(Close))
    # merge all data
    df <- rbind(df_weekly, df_daily) %>% group_by(Date) %>% summarize(Date=last(Date), Close=last(Close)) %>% ungroup %>%
      arrange(Date)
    
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
  

  

  ## Greedy algorithm to find a set of positions closest to the optimal provided. Adapted from "Advanced futures trading strategies (2022)".
  ## refer to the book for details. 
  # capital : your money in your account currency
  # optimal_positions : vector of the best un-rounded positions (in contracts) corresponding to your forecast. 
  #                     Forecasts are usually from -20 (max short position) to 20. Optimal positions in contracts are calculated as following:
  #                     (capital * instrument_weight * IDM  * target_volatility / instrument_volatility  * FX_rate * Forecast/10) / (contract_size * price) 
  # notional_exposures : vector of the values in account currency of one contract (usually contract_size * price / FX_rate)
  # cov_matrix : covariance matrix of instruments returns, usually calculated from the last 6 months of (daily or weekly) returns.
  # previous_position : vector of the previous optimal positions. All zeroes if not provided.
  # max_positions : vector of the max allowed positions (in absolute contracts), usually corresponding to a forecast of 20 (see above formula). Ignored if NULL
  # min_positions : vector of the min allowed positions (in absolute contracts). If NULL, it is set to the minimum incremental step (1 contract for futures).
  # costs_per_contract : vector of the costs to trade one contract, in price scale. 
  # trade_shadow_cost : a factor multiplier of the cost per contracts.
  # fractional : TRUE is your broker allow fractional contracts, like for CFDs. The algorithm will use the decimal part of the positions as incremental step 
  #              in the greedy algorithm. If you are trading futures where all contracts are 1, just set it to FALSE.
  # max_factor : maximum multiple of optimal position allowed (e.g. if optimal position == 2 and max_factor == 2, the optimized position will be <= 4). 
  #
  # returned value: a vector of optimized positions according to the dynamic portfolio algorithm.
  dynamic_portfolio <- function(capital, optimal_positions, notional_exposures, cov_matrix, 
                                previous_position = NULL, min_positions=NULL, max_positions=NULL, costs_per_contract = NULL, trade_shadow_cost = 1, fractional=TRUE, max_factor=2) {
    # Calculate the cost of making trades. trade_shadow_cost represents the number of expected trades in year
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
    # The greedy algorithm (see https://qoppac.blogspot.com/2021/10/mr-greedy-and-tale-of-minimum-tracking.html)
    find_possible_new_best <- function(weights_optimal, weights_max, weights_per_contract, direction, best_solution, best_value, cov_matrix, max_factor) {
      new_best_value <- best_value
      new_solution <- best_solution
      count_assets <- length(best_solution)
      for (i in sample(1:count_assets)) {
        temp_step <- best_solution
        if(temp_step[i] == 0) {
          temp_step[i] <- temp_step[i] + weights_min[i] * direction[i]
        } else {
          temp_step[i] <- temp_step[i] + weights_per_contract[i] * fractional[i] * direction[i]
        }
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
    
    # Number os instruments
    n <- nrow(cov_matrix)
    # Set previous positions as zero if not specified
    if (is.null(previous_position)) {
      previous_position <- rep(0, n)
    }
    # Set trading costs to zero if not specified
    if (is.null(costs_per_contract)) {
      costs_per_contract <- rep(0, n)
    }
    # Find a fractional increments from positions (e.g. if position == 1.2 then the increment is 0.1)
    if (!fractional) {
      fractional <- rep(1, n)
    } else {
      fractional <-  10^(floor(log10(abs(optimal_positions)))-1)
    }
    weights_per_contract <- notional_exposures / capital
    weights_optimal <- optimal_positions * weights_per_contract 
    weights_max <- if(!is.null(max_positions)) max_positions * weights_per_contract else Inf
    weights_min <- if(!is.null(min_positions)) min_positions * weights_per_contract else weights_per_contract * fractional
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
  
  ## Dynamic portfolio buffering.  Adapted from "Advanced futures trading strategies (2022)".
  # capital : your money in your account currency
  # optimized_positions : vector of optimized positions returned from the function "dynamic_portfolio"
  # previous_position : vector of the previous optimal positions. All zeroes if not provided.
  # notional_exposures : vector of the values in account currency of one contract (usually contract_size * price / FX_rate)
  # cov_matrix : covariance matrix of instruments returns, usually calculated from the last 6 months of (daily or weekly) returns.
  # target_volatility : your portfolio volatility target (e.g. 0.25)
  # portfolio_buffering_level : the deviance representing the edges of the buffering. The highest this number the less frequent the portfolio updates.
  #
  # returned value: a list of: a vector of required positions updates to take (all zero if the adjustment factor is negative), 
  #                            the tracking error of the portfolio 
  #                            the adjustment factor (the percentage of position to adjust from the current to the optimized position)  
  buffering_portfolio <- function(capital, optimized_positions, previous_positions, notional_exposures, cov_matrix, target_volatility, portfolio_buffering_level=0.1) {
    weights_per_contract <- notional_exposures / capital
    optimized_portfolio_weight <- optimized_positions * weights_per_contract 
    previous_portfolio_weight <- previous_positions * weights_per_contract 
    tracking_error_current_weight <- optimized_portfolio_weight - previous_portfolio_weight
    tracking_error <- as.numeric(sqrt(t(tracking_error_current_weight) %*% cov_matrix %*% tracking_error_current_weight))
    adjustment_factor <- max((tracking_error - portfolio_buffering_level/2 * target_volatility) / tracking_error, 0)
    required_trade <- adjustment_factor * (optimized_positions - previous_positions) 
    return(list(required_trade, tracking_error, adjustment_factor))
  }
}


# Parameters (maybe put them in a config file?)
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
  target_vol <- 0.33
  IDM = 2.5
  FDMtrend <- 1.3
  FDMcarry <- 3.3
  FDMskew <- 1.2
  FDM <- 1.5
  strategy_weights <- list("Trend" = 0.4, "Carry" = 0.5, "Skew" = 0.1)
  corr_length <- 25
  portfolio_buffering_level <- 0.1
  position_buffering_level <- 0.2
  trade_shadow_cost <- 0
}


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
  print(paste("Capital:", capital, "Target Volatility:", target_vol, "IDM:", IDM, "Portfolio Buffering Level:", portfolio_buffering_level, "Position Buffering Level:", position_buffering_level))
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
    cat(paste(fx, ""))
    f <- read_csv(paste0(FX_dir, "/", fx, ".csv"), col_names = TRUE, show_col_types = FALSE)
    colnames(f) <- c("Date", "Rate")
    if(nrow(f) > 1) {
      files[[fx]] <- arrange(f, Date) %>% na.locf(na.rm=FALSE) %>% tail(2) %>% head(1) ### we take yesterday's rate, to match CMC data
    }else{
      files[[fx]] <- arrange(f, Date) %>% na.locf(na.rm=FALSE) %>% tail(1)
    }
  }
  FX_rates <- do.call(rbind, files) %>% mutate(FX=toupper(sub("eur", "", FX_names))) 
  colnames(FX_rates) <- c("Date", "Rate", "FX")
  
  # the covariance matrix
  print("Calculate covariance matrix...")
  closes <- lapply(instruments_data, function(x)x[[1]])
  closes_merged <- Reduce(function(...) full_join(..., by="Date"), closes) %>% arrange(Date) 
  colnames(closes_merged) <- c("Date", names(instruments_data))
  daily_returns <- data.frame(Date=closes_merged$Date, apply(closes_merged[,-1], 2, function(x) c(0,diff(log(x))))) 
  weekly_returns <- mutate(daily_returns, Date=yearweek(Date)) %>% group_by(Date) %>% summarise(across(everything(), ~mean(.x,na.rm=TRUE)))
  vols <- data.frame(Date=daily_returns$Date, apply(daily_returns[,-1], 2, function(x) calculate_volatility(x))) 
  #cor_matrix <- cor(tail(weekly_returns[,-1], corr_length), use="pairwise.complete.obs")
  cor_matrix <- runCorMatrix(as.matrix(weekly_returns[,-1]))[[ncol(weekly_returns[,-1])]]
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
    df$PositionMin <- dplyr::filter(instruments_info, Symbol == symbol) %>% pull(PositionMin)
    df$PositionTick <- dplyr::filter(instruments_info, Symbol == symbol) %>% pull(PositionTick)
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
        strategy_weights$Skew  * df$ForecastSkew   ) * FDM
    df$Forecast <- cap_forecast(df$Forecast)
    df$InstCapital <- capital * df$Weight * IDM 
    df$Exposure <- df$InstCapital * target_vol/df$Volatility 
    df$PositionOptimal <-  (df$Exposure * df$FX * df$Forecast/10) /
      (df$ContractSize * df$Close  ) 
    df$PositionMax <- (df$Exposure * df$FX * 20/10) /
      (df$ContractSize * df$Close  ) 
    # Be careful, now it is reverse-date sorted, you cannot run any other function like EMA etc..
    df <- arrange(df, desc(Date))
    #write_csv(df, paste0(logs_instruments_dir, "/", symbol, ".csv"))
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
  min_positions <- with(today_trading, PositionMin)
  notional_exposures <- with(today_trading, ContractSize * Close / FX)
  costs_per_contract <- with(today_trading, ContractSize * (Spread/2) / FX)
  previous_position <- previous_trading$Position
  position_dynamic <- dynamic_portfolio(capital, optimal_positions, notional_exposures,  cov_matrix, 
                                        previous_position = previous_position, max_positions = max_positions, min_positions = min_positions,
                                        costs_per_contract=costs_per_contract, trade_shadow_cost = trade_shadow_cost)
  position_optimized <- round_position(position_dynamic, today_trading$PositionMin,  today_trading$PositionTick) 
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
  # buffering is the minimal position change allowed, equal to 1 forecast
  today_trading$Buffer <- with(today_trading, position_buffering_level * (Exposure * FX * 10/10) / (ContractSize * Close))
  # if the previous position is 0 we override the required trade obtained from the buffering portfolio, and instead
  # we trade straight to the optimized position. Otherwise some positions never take place (like when the minimum position is very high) 
  today_trading$RequiredTrade <- required_trades
  today_trading$RequiredTrade <- with(today_trading, ifelse(abs(RequiredTrade) > 0 & PositionPrevious == 0, PositionOptimized, RequiredTrade))
  # trade only if the required trade if bigger than buffer, bigger than position tick size and bigger than minumum position
  today_trading$Trading <- with(today_trading, abs(RequiredTrade) > Buffer & abs(RequiredTrade) > PositionTick & abs(RequiredTrade) > PositionMin)
  today_trading$PositionUnrounded <- with(today_trading,  ifelse(Trading, PositionPrevious +  RequiredTrade, PositionPrevious))
  today_trading$Position <- with(today_trading,  round_position(PositionUnrounded, PositionMin, PositionTick))  
  today_trading$PositionRisk <- abs(with(today_trading, Position * ContractSize * (Close / FX) * Volatility)) %>% round(2)
  # Portfolio volatility
  w <- with(today_trading, Position * ContractSize * Close / FX / capital) %>% as.numeric
  portfolio_volatility <- round(sqrt(w %*% cov_matrix %*% w) * 100, 2)
  print(paste("Portfolio volatility:", portfolio_volatility, "%"))
  # Portfolio jump risk
  jump <- lapply(results, function(x) quantile(x$Volatility, probs=0.99)) %>% unlist
  jump_cov_matrix <- diag(jump) %*% cor_matrix %*% diag(jump)
  jump_risk <- round(sqrt(w %*% jump_cov_matrix %*% w) * 100, 2)
  print(paste("Portfolio jump risk:", jump_risk, "%"))
  # Portfolio correlation risk
  risks <- lapply(1:length(results), function(i) w[i] * tail(results[[i]]$Volatility, 1)) %>% unlist
  correlation_shock_portfolio <- round(sum(abs(risks)), 2) * 100
  print(paste("Portfolio correlation risk:", correlation_shock_portfolio, "%"))
  # Active positions
  print(paste("Active positions:", sum(today_trading$Position != 0), "total symbols:", nrow(today_trading)))
  print("Positions to update:")
  trades <- today_trading %>% filter(Trading == TRUE) %>% select(Date, Close, Symbol, Position, PositionUnrounded, PositionPrevious, RequiredTrade, PositionOptimized, PositionOptimal, Forecast)
  print(trades, n=nrow(trades))
  
  # Write to file
  write_csv(previous_trading, paste0(logs_dir, "/", now_string, ".POSITIONS.csv"))
  write_csv(today_trading, "POSITIONS.csv")
}



