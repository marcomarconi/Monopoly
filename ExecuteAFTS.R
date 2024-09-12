
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
  suppressMessages(library(Rfast))
  suppressMessages(library(optparse))
  suppressMessages(library(crayon))
  source("/home/marco/trading/Systems/Common/RiskManagement.R")
  
}



# Functions
{

  # for some reason, scrapped CMC daily data are leaded one day, check for example https://www.cmcmarkets.com/en-gb/instruments/coffee-arabica-jul-2023?search=1
  # weekly data is leaded 2 days
  # IMPORTANT: currently we only consider intraday data from CMC, daily and weekly is not used
  load_scraped_cmc_data <- function(symbol,  scrape_dir, load_daily=TRUE, load_weekly=TRUE, lagged=TRUE){
    symbol_dir <- paste0(scrape_dir, "/", symbol)
    df <- data.frame(Date=Date(), Close=as.numeric(), Period=as.character())
    # # load intra-day data
    df_intraday <- data.frame(Date=NA, Close=NA)
    system(paste("cat", paste(list.files(symbol_dir, pattern = "intraday", full.names = TRUE), collapse = " "),  " | sort -u  > _tmp"))
    df_intraday <- fread("_tmp", header= FALSE)
    file.remove("_tmp")
    colnames(df_intraday) <- c("Date", "Close")
    df_intraday$Date <- as_date(df_intraday$Date)
    df_intraday <- arrange(df_intraday, Date) %>% group_by(Date) %>% summarize(Date=last(Date), Close=last(Close))
    #df_intraday <- tail(df_intraday, 1)
    df_intraday$Period <- "Intraday"
    # # load daily data, lag date by one day
    df_daily <- data.frame(Date=Date(), Close=as.numeric(), Period=as.character())
    if(load_daily) {
      system(paste("cat", paste(list.files(symbol_dir, pattern = "daily", full.names = TRUE), collapse = " "),  " | sort -u  > _tmp"))
      df_daily <- fread("_tmp", header= FALSE)
      colnames(df_daily) <- c("Date", "Close")
      df_daily <- arrange(df_daily, Date)
      if(lagged) 
        df_daily <- df_daily %>% mutate(Date = as_date((ifelse(wday(Date) == "Fri", Date+2, Date+1  ))))
      # only keep daily data up to the last element of intradaily data
      df_daily <-  dplyr::filter(df_daily, Date < df_intraday$Date[1])
      df_daily$Period <- "Daily"
      file.remove("_tmp")
    }
    # load weekly data, lag date by two days
    df_weekly <- data.frame(Date=Date(), Close=as.numeric(), Period=as.character())
    if(load_weekly) {
      system(paste("cat", paste(list.files(symbol_dir, pattern = "weekly", full.names = TRUE), collapse = " "),  " | sort -u  > _tmp"))
      df_weekly <- fread("_tmp", header= FALSE)
      colnames(df_weekly) <- c("Date", "Close")
      df_weekly <- arrange(df_weekly, Date)
      if(lagged) {
        df_weekly[,1] <- df_weekly[,1]+2
        df_weekly <- df_weekly[-1,]
      }
      # only keep weekly data up to the start of daily data
      df_weekly <- mutate(df_weekly, Date=as.Date(Date)) %>% dplyr::filter(Date < df_daily$Date[1])
      # interpolate weekly data to create daily data
      # first, recreate full daily Date excluding weekends
      dates <- seq(df_weekly$Date[1], df_weekly$Date[length(df_weekly$Date)], by=1) %>% 
        as_tibble() %>% mutate(Date=value) %>% select(-value) %>% dplyr::filter(!(lubridate::wday(Date, label = TRUE) %in% c("Sat", "Sun")))
      # then interpolate
      df_weekly <- merge(df_weekly, dates, by="Date", all=TRUE) %>% mutate(Close=na.approx(Close), Date=as_date(Date))
      df_weekly$Period <- "Weekly"
      file.remove("_tmp")
    }
    if(load_daily & load_weekly)
      df <- rbind(df_weekly, df_daily) %>% group_by(Date) %>% summarize(Date=last(Date), Close=last(Close), Period=last(Period)) %>% ungroup %>% arrange(Date) 
    df <- rbind(df, df_intraday) %>% group_by(Date) %>% summarize(Date=last(Date), Close=last(Close), Period=last(Period)) %>% ungroup %>% arrange(Date) 
    #df <- rbind(df, mutate(df_intraday, Date=as_date(Date))) %>% mutate(Date=as.Date(Date))
    df <- na.omit(df) %>% dplyr::select(Date, Close, Period)
    if((length(unique(df$Date)) != length(df$Date)))
      stop(paste("Duplicate dates in ", symbol))
    if(tail(df$Date, 1) != today()) {
      cat(red("\nLast date does not correspond to today in ", symbol,"\n"))
      warning(paste("Last date does not correspond to today in ", symbol))
    }
    df$Close <- as.numeric(df$Close) # sometimes it is loaded as character
    return(df)
  }

  load_scraped_cmc_hc <- function(symbol,  dir){
    symbol_dir <- paste0(dir, "/", symbol)
    l <- tail(sort(list.files(symbol_dir, pattern = "holding_cost")), 1)
    f <- read_csv(paste0(symbol_dir, "/", l), show_col_types = FALSE, col_names = FALSE)
    if(dim(f)[1] == 0) {
      cat(red("\nHolding cost file empty for symbol:", symbol,"\n"))
      warning(paste("Holding cost file empty for symbol:", symbol))
      hc <- c(0,0)        
    } else {
      hc <- unlist(f[,-1])
    }
    return(hc)
  }
  
  load_current_cmc_data <- function(symbol, dir){
    symbol_file <- paste0(dir, "/", symbol, ".csv")
    df <- fread(symbol_file) %>% mutate(Date=as.Date(Date), Period="Daily") %>% arrange(Date) %>% select(Date, Close, Period)
    return(df)
  }
  
  # load_historical_cmc_data <- function(symbol, dir){
  #   symbol_file <- paste0(dir, "/", symbol, ".csv")
  #   df <- fread(symbol_file) %>% mutate(Date=as.Date(Date)) %>% arrange(Date) 
  #   return(df)
  # }
  
  
  multiple_EMA <- function(adjclose, close, volatility, spans=c(2, 4, 8, 16, 32, 64), scalars=c(12.1, 8.53, 5.95, 4.1, 2.79, 1.91), mult=4, cap=20, period=252) {
    n <- length(spans)
    EWMACs <- lapply(1:n, function(i) EMA(adjclose, spans[i]) -  EMA(adjclose, spans[i]*mult))
    EWMACs <- lapply(1:n, function(i) EWMACs[[i]] / (close * volatility / sqrt(period)) * scalars[i] )
    EWMACs <- lapply(1:n, function(i) cap_forecast(EWMACs[[i]], cap))
    forecast <- rowMeans(do.call(cbind, EWMACs))
    return(forecast)
  }
  multiple_DC <- function(adjclose, close, volatility, spans=c(10, 20, 40, 80, 160, 320), scalars=c(0.60, 0.67, 0.70, 0.73, 0.74, 0.74), cap=20, period=252) {
    n <- length(spans)
    DCs <- lapply(1:n, function(i) {dc <- DonchianChannel(adjclose, spans[i]); (adjclose - dc[,2]) / abs(dc[,1] - dc[,3])})
    DCs <- lapply(1:n, function(i) EMA(na.locf(DCs[[i]], na.rm=F) * 40, spans[i]/4) * scalars[i] )
    DCs <- lapply(1:n, function(i) cap_forecast(DCs[[i]], cap))
    forecast <- rowMeans(do.call(cbind, DCs))
    return(forecast)
  }
  
  # basis and volatility are in percentage
  multiple_Carry <- function(basis, expiry_difference, volatility, spans=c(5, 20, 60, 120), scalar=30, expiry_span=12, cap=20) {
    n <- length(spans)
    Carry <- (basis / (expiry_difference / expiry_span)) / ( volatility )
    Carry <- na.locf(Carry, na.rm=FALSE); Carry[is.na(Carry)] <- 0
    EMAs <- lapply(1:n, function(i) EMA(Carry, spans[i]) * scalar)
    EMAs <- lapply(1:n, function(i)  cap_forecast(EMAs[[i]], cap))
    forecast <- rowMeans(do.call(cbind, EMAs))
    return(forecast)
  }
  relative_volatility <- function(volatility, period=2520) {
    return(unlist(Map(function(i) mean(tail(volatility[1:i], period), na.rm=TRUE), 1:length(volatility))))
  }

  multiple_Skew <- function(returns, spans=c(60, 120, 240), scalars=c(33.3, 37.2, 39.2), cap=20) {
    n <- length(spans)
    returns[is.na(returns)] <- 0
    Skews <- lapply(1:n, function(i) -rollapply(returns, width=spans[i], skew,  fill=NA, align="right"))
    Skews <-  lapply(1:n, function(i) replace(Skews[[i]], is.na(Skews[[i]]), 0))
    Skews <-  lapply(1:n, function(i) EMA(Skews[[i]], ceiling(spans[i]/4)) * scalars[i])
    Skews <- lapply(1:n, function(i) cap_forecast(Skews[[i]], cap))
    forecast <- rowMeans(do.call(cbind, Skews))
    return(forecast)
  }
}


# Parameters (maybe put them in a config file?)
{
  main_dir <- "/home/marco/trading/Systems/Monopoly/ExecuteATFS/"
  positions_file <- paste0(main_dir, "POSITIONS.csv")
  instrument_file <- paste0(main_dir, "INSTRUMENTS.csv")
  portfolio_file <- paste0(main_dir, "PORTFOLIO.csv")
  FX_file <- paste0(main_dir, "FX.csv")
  FX_file_manual <- paste0(main_dir, "FX_data.csv")
  scrape_dir <- paste0(main_dir, "Data/Scrape/")
  #historical_dir <- paste0(main_dir, "Data/Historical/")
  current_dir <- paste0(main_dir, "Data/Current/")
  FX_dir <- paste0(main_dir, "FX/")
  logs_dir <- paste0(main_dir, "Logs/")
  plots_dir <- paste0(main_dir, "Logs/Plots/")
  scrape_script <- "SCRAPE_DAILY_DATA.sh"
  target_vol <- 0.3
  IDM <- 2.3
  FDMtrend <- 1.75 # 
  FDMcarry <- 2.5 # 
  FDMskew <- 1.0 # 
  FDM <- 1.75
  strategy_weights <- list("Trend" = 0.5, "Carry" = 0.25, "Skew" = 0.25)
  corr_length <- 25 # weekly correlation window
  position_buffering_level <- 2.5 # in backtest daily SD is ~1
  short_penality <- 0.75 # Penalize short positions (NULL to disable)
  use_dynamic_portfolio <- FALSE
  portfolio_buffering_level <- 0.1
  trade_shadow_cost <- 0
  dry_run <- FALSE
  skip_download <- FALSE
}


# Read command arguments
option_list = list(
    make_option(c("-c", "--capital"),  type="double", help="Account Capital."),
    make_option(c("-d", "--dryrun"), action="store_true", default=FALSE, help="Do not write any file."),
    make_option(c("-s", "--skipdownload"), action="store_true", default=FALSE, help="Do not download data.")
);
opt_parser = OptionParser(option_list=option_list);
opt = parse_args(opt_parser);
capital <- opt$capital
dry_run <- opt$dryrun
skip_download <- opt$skipdownload



{
  print(paste("Capital:", capital, "Target Volatility:", target_vol, "IDM:", IDM, "FDM:", FDM,
               "Position Buffering Level:", position_buffering_level, "Short Penalty:", short_penality,
               "Dry Run:", dry_run, "Skip Downloading Data:", skip_download
              ))
  # create dirs&files
  today_string <- gsub("-", "", today())
  now_string <- gsub("-| |:", "", now())
  if(!dry_run) {
    if(!dir.exists(logs_dir))
      dir.create(logs_dir)
    if(!dir.exists(current_dir))
      dir.create(current_dir)
    if(!dir.exists(FX_dir))
      dir.create(FX_dir)
  }
  
  setwd(main_dir)
  
  # load instruments infos and calculate instruments weights from asset classes groups 
  print("Loading symbols info and previous positions file...")
  instruments_info <- read_csv(instrument_file, col_names = TRUE, show_col_types = FALSE) %>% arrange(Symbol)
  tradable_symbols <- filter(instruments_info, Tradable==TRUE) %>% pull(Symbol)
  instruments_info <- mutate(instruments_info, Weight = ifelse(Symbol %in% tradable_symbols, 1/length(tradable_symbols), 0)) # uniform weight
  # instruments_info$Weight <- instruments_info %>% group_by(Symbol) %>%   # class-based weight
  #   summarise(Symbol=Symbol, 
  #             n0=length(unique(instruments_info$Class1)), 
  #             n1=length(unique(instruments_info$Class2[instruments_info$Class1==Class1])), 
  #             n2=length((instruments_info$Class2[instruments_info$Class2==Class2]))) %>% 
  #   ungroup %>% mutate(Weight=1/n0/n1/n2) %>% pull(Weight)

  # load previous positions file
  if(!file.exists(positions_file))
    stop("Previous positions file does not exists.")
  previous_trading <- read_csv(positions_file, col_names = TRUE, show_col_types = FALSE) %>% arrange(Symbol)

  # scrape price and FX data
  if(!skip_download){
    print("Scraping price and FX data...")
    system(paste("bash", scrape_script, scrape_dir, instrument_file, FX_dir, FX_file))
  }

  # load price data from previous scrape, we merge it we the current data. 
  # IMPORTANT: we expect the current dir contains already some data (with Date and Close columns), download it manually the very first time.
  print("Loading price data...")
  instruments_data <- list()
  for(symbol in instruments_info$Symbol) {
    cat(paste(symbol, ""))
    current_data <- load_current_cmc_data(symbol, current_dir) 
    scraped_data <- load_scraped_cmc_data(symbol, scrape_dir, load_daily = FALSE, load_weekly = FALSE)
    symbol_data <- rbind(current_data, scraped_data) %>% group_by(Date) %>% summarize(Date=last(Date), Close=last(Close), Period=last(Period)) %>% ungroup %>% arrange(Date) 
    scraped_hc <- load_scraped_cmc_hc(symbol, scrape_dir)
    instruments_data[[symbol]] <- list(Price=symbol_data, HC=scraped_hc)
    nas <- sum(is.na(instruments_data[[symbol]]$Price$Close))
    if(nas > 0) {
      warning(paste(symbol, "price data has", nas, "NAs. They have been filled"))
      instruments_data[[symbol]]$Price$Close <- na.locf(instruments_data[[symbol]]$Price$Close, na.rm=FALSE)
    }
  }
  
  # load FX data from previous scrape
  # print("")
  # print("Loading FX data...")
  # FX_names <- read_csv(FX_file, col_names = FALSE, show_col_types = FALSE) %>% unlist 
  # files <- list()
  # for(fx in FX_names) {
  #   cat(paste(fx, ""))
  #   f <- read_csv(paste0(FX_dir, "/", fx, ".csv"), col_names = TRUE, show_col_types = FALSE)
  #   colnames(f) <- c("Date", "Rate")
  #   files[[fx]] <- arrange(f, Date) %>% na.locf(na.rm=FALSE) %>% tail(1)
  # }
  # FX_rates <- do.call(rbind, files) %>% mutate(FX=toupper(sub("eur", "", FX_names))) 
  # colnames(FX_rates) <- c("Date", "Rate", "FX")
  
  FX_rates <- read_csv(FX_file_manual, show_col_types = FALSE)
  
  # the covariance matrix
  print("Calculate covariance matrix...")
  closes <- lapply(instruments_data, function(x)x[[1]] %>% select(Date, Close))
  closes_merged <- Reduce(function(...) full_join(..., by="Date"), closes) %>% arrange(Date) %>% na.locf(na.rm=F)
  colnames(closes_merged) <- c("Date", names(instruments_data))
  daily_returns <- data.frame(Date=as.Date(closes_merged$Date), apply(closes_merged[,-1], 2, function(x) c(NA, diff(log(x)))))
  daily_returns <- na.omit(daily_returns) # Potentially dangerous?
  weekly_returns <- mutate(daily_returns, Date=yearweek(Date)) %>% group_by(Date) %>% summarise(across(everything(), ~mean(.x,na.rm=TRUE)))
  vols <- data.frame(Date=daily_returns$Date, apply(daily_returns[,-1], 2, function(x) calculate_volatility(x))) 
  #cor_matrix <- cor(tail(daily_returns[,-1], corr_length), use="pairwise.complete.obs") # static last corr matrix
  Q <- runCorMatrix(as.matrix(weekly_returns[,-1]), n = corr_length) # running corr matrix (note: if we use absolute correlation we get an error in the dynamic portfolio)
  cor_matrix <- Q[[length(Q)]] 
  last_day_vol <- tail(vols, 1)[-1]
  cov_matrix <- diag(last_day_vol) %*% cor_matrix %*% diag(last_day_vol)
  rownames(cov_matrix) <- colnames(cov_matrix) <-  names(instruments_data)
  if(!dry_run) {
    png(paste0(plots_dir, "/", today_string, "_Cor_matrix_all.png"), width = 1200, height = 1200)
    corrplot::corrplot(cor_matrix, method = "number")
    dev.off()
    png(paste0(plots_dir, "/", today_string, "_Cor_matrix_trading.png"), width = 1200, height = 1200)
    corrplot::corrplot(cor_matrix[tradable_symbols,tradable_symbols], method = "number")
    dev.off()
  }
  
  # iterate over data and calculate positions
  print("Calculate new positions...")
  all_days <- list()
  last_day <- list()
  for(symbol in names(instruments_data)) {
    cat(paste(symbol, ""))
    df <- instruments_data[[symbol]][[1]]
    hc <- instruments_data[[symbol]][[2]]
    df$Symbol <- symbol
    df$ForecastTrend <- df$ForecastCarry <- df$ForecastSkew <- df$Forecast <- df$PositionMax <- df$PositionOptimal <- df$PositionOptimized <- df$AdjFactor  <- df$RequiredTrade <- df$Buffer <- df$Trading <- df$PositionPrevious <- df$PositionUnrounded <- df$Position <- df$PositionChange <- df$PositionRisk  <- 0
    df$Return <- c(0, diff(log(df$Close)))
    df$Return[df$Period=="Weekly"]  <- df$Return[df$Period=="Weekly"] * 4.84 # adjust weekly data to daily volatility (4.84=252/52). Not sure if correct and/or necessary
    df$Volatility <- calculate_volatility(df$Return)
    df$Period <- NULL
    
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
    df$Spread <- dplyr::filter(instruments_info, Symbol == symbol) %>% pull(Spread)
    df$Commission <- dplyr::filter(instruments_info, Symbol == symbol) %>% pull(Commission)
    df$RiskAdjCost <- df$Spread / df$Close / df$Volatility
    
    # Relative volatility (strategy 13, it does not seems to add much)
    {
      df$M <- 1
      # df$RV <- relative_volatility(df$Volatility) 
      # df$Q <- sapply(1:length(df$RV), function(i) sum(df$RV[i] > df$RV[1:i], na.rm=TRUE) / i)
      # df$M <- EMA(2 - 1.5 * df$Q, 10)
    }
    # Trend-following (strategy 9)
    df$multiple_EMA <- multiple_EMA(df$Close, df$Close, df$Volatility)
    df$multiple_DC <- multiple_DC(df$Close, df$Close, df$Volatility)
    df$ForecastTrend <- rowMeans(cbind(df$multiple_EMA, df$multiple_DC), na.rm=T) 
    df$ForecastTrend <- cap_forecast(df$ForecastTrend * FDMtrend * df$M) 
    
    # Carry 
    # It is based on cash contract interest rate, we remove the 3% commission, so low volatility assets like
    # bonds are not negatively affected in this analysis. Of course low volatility assets must be played on the future contract
    # if you want to obtain the carry, as interest rates of the cash are always negative (because of the 3% commission that
    # is higher than the volatility itself). For high volatility assets you can choose between cash or future.
    if(hc[1] == 0 | hc[2] == 0) {
      prev_hc_long <- previous_trading %>% filter(Symbol == symbol) %>% pull(HoldingCostLong)
      prev_hc_short <- previous_trading %>% filter(Symbol == symbol) %>% pull(HoldingCostShort)
      hc[1] <- prev_hc_long
      hc[2] <- prev_hc_short
      warning(paste("WARNING: Holding costs for symbol", symbol, "are zero. Using previous costs:", prev_hc_long, prev_hc_short))
    }
    hc_max <- which.max(hc)
    # We have to adjust the swap rate to guess the actual carry for futures (CMC add a 3% commission on future swap rates)
    hc_commission <- case_when(
                              df$Product[1] == "Cash" ~ 0,
                              df$Product[1] == "Index" ~ 0.03,
                              df$Product[1] == "Future" ~ 0.03,
                              df$Product[1] == "FX" ~ 0,
                              TRUE ~ NA_real_)
    hc_value <- (hc[hc_max]+hc_commission)
    # When carry is always against us (both long and short charge us), we assume it is zero
    if(hc_value < 0)
      hc_value <- 0
    df$HoldingCostLong <- hc[1]
    df$HoldingCostShort <- hc[2]
    df$ForecastCarry <- ifelse(hc_max == 1, 1, -1) * hc_value / df$Volatility * 10 
    df$ForecastCarry <- cap_forecast(df$ForecastCarry * FDMcarry)
    
    # Skewness (strategy 24)
    df$ForecastSkew <- multiple_Skew(df$Return) 
    df$ForecastSkew <- cap_forecast(df$ForecastSkew * FDMskew)
    
    # Final trade
    df$Forecast <- (
        strategy_weights$Trend * df$ForecastTrend + 
        strategy_weights$Carry * df$ForecastCarry + 
        strategy_weights$Skew  * df$ForecastSkew   ) * FDM
    df$Forecast <- cap_forecast(df$Forecast)
    df$InstCapital <- capital * df$Weight * IDM 
    df$Exposure <- df$InstCapital * target_vol/df$Volatility 
    # Penalize short positions
    if(!is.null(short_penality)) {
      df$Exposure <- ifelse(df$Forecast < 0, df$Exposure*short_penality, df$Exposure)
      df$Exposure <- ifelse(df$Forecast > 0, df$Exposure*(1+(1-short_penality)), df$Exposure)
    }
    df$PositionOptimal <-  (df$Exposure * df$FX * df$Forecast/10) /
      (df$ContractSize * df$Close) 
    df$PositionMax <- (df$Exposure * df$FX * 20/10) /
      (df$ContractSize * df$Close) 
    df$Buffer <- (df$Exposure * df$FX * position_buffering_level/10) / (df$ContractSize * df$Close)
    df$Buffer <-  ifelse(df$Buffer < df$PositionMin, df$PositionMin, df$Buffer)
    all_days[[symbol]] <- df
    df <- arrange(df, desc(Date))
    if(!dry_run)
      write_csv(df, paste0(current_dir, "/", symbol, ".csv"))
    # Be careful, now it is reverse-date sorted, you cannot run any other function like EMA etc..
    last_day[[symbol]] <- df[1,]
  }
  print("")
  # Final table
  today_trading <- do.call(rbind, last_day)
  if(!all(previous_trading$Symbol %in% today_trading$Symbol)) {
    missing_prev <- previous_trading$Symbol[!(previous_trading$Symbol %in% today_trading$Symbol)]
    missing_today <- today_trading$Symbol[!(today_trading$Symbol %in% previous_trading$Symbol)]
    stop(paste("Previous position symbols (POSITION file) and current symbols (INSTRUMENTS file) do not match. Missing in current: ", missing_prev, ", missing in previous: ", missing_today, "\nFix it manually."))
  } else if (!all(today_trading$Symbol %in% previous_trading$Symbol)) {
    missing_prev <- previous_trading$Symbol[!(previous_trading$Symbol %in% today_trading$Symbol)]
    missing_today <- today_trading$Symbol[!(today_trading$Symbol %in% previous_trading$Symbol)]
    stop(paste("Previous position symbols (POSITION file) and current symbols (INSTRUMENTS file) do not match. Missing in current: ", missing_prev, ", missing in previous: ", missing_today, "\nFix it manually."))
  }
  today_trading$PositionPrevious <- previous_trading$Position
  if(use_dynamic_portfolio) {
    # Dynamic portfolio
    optimal_positions <- with(today_trading, PositionOptimal)
    notional_exposures <- with(today_trading, ContractSize * Close / FX)
    costs_per_contract <- with(today_trading, ContractSize * (Spread/2) / FX)
    previous_positions <- previous_trading$Position
    fractionals <- with(today_trading, PositionMax / 20) 
    position_dynamic <- dynamic_portfolio(capital, optimal_positions, notional_exposures,  cov_matrix, 
                                          previous_position = previous_positions, max_positions = NULL, min_positions = NULL,
                                          costs_per_contract=costs_per_contract, trade_shadow_cost = trade_shadow_cost, fractional = fractionals)
    position_optimized <- round_position(position_dynamic, today_trading$PositionMin,  today_trading$PositionTick) 
    # Buffering
    res <- buffering_portfolio(capital, position_optimized, previous_positions, notional_exposures, cov_matrix, target_vol, portfolio_buffering_level)
    required_trades <- res[[1]]
    portfolio_tracking_error <- res[[2]]
    adjustment_factor <- res[[3]]
    print(paste("Portfolio tracking error:", round(portfolio_tracking_error, 3), "against buffer level", portfolio_buffering_level/2*target_vol, "adjustment factor:", round(adjustment_factor, 3)))
    # Update final positions
    today_trading$PositionOptimized <- position_optimized
    today_trading$AdjFactor <- adjustment_factor
    today_trading$RequiredTrade <- required_trades
    # if the optimal is zero, close the position. This is necessary otherwise sometimes open positions with high min position are never closed.
    today_trading$RequiredTrade <- with(today_trading, ifelse(abs(RequiredTrade) > 0 & PositionPrevious != 0 & PositionOptimized == 0, -PositionPrevious, RequiredTrade))
  } else {
    today_trading$PositionOptimized <- NULL
    today_trading$AdjFactor <- NULL
    today_trading$RequiredTrade <- today_trading$PositionOptimal - today_trading$PositionPrevious
  }
  # trade only if the required trade if bigger than buffer, bigger than position tick size and bigger than minimum position
  today_trading$Trading <- with(today_trading, 
                                (abs(RequiredTrade) >= Buffer & abs(RequiredTrade) >= PositionTick & abs(RequiredTrade) >= PositionMin) | 
                                (abs(PositionPrevious) != 0 & PositionOptimal == 0)
                                
  )
  today_trading$PositionUnrounded <- with(today_trading,  ifelse(Trading, PositionPrevious +  RequiredTrade, PositionPrevious))
  today_trading$Position <- with(today_trading,  round_position(PositionUnrounded, PositionMin, PositionTick))  
  today_trading$PositionChange <- today_trading$Position - today_trading$PositionPrevious
  today_trading$PositionRisk <- abs(with(today_trading, Position * ContractSize * (Close / FX) * Volatility)) %>% round(2)
  # Portfolio volatility
  w <- with(today_trading, Position * ContractSize * Close / FX / capital) %>% as.numeric
  portfolio_volatility <- round(as.numeric(sqrt(w %*% cov_matrix %*% w)) * 100, 2)
  print(paste("Portfolio volatility:", portfolio_volatility, "%"))
  # Portfolio correlation risk
  risks <- lapply(1:length(all_days), function(i)  tail(all_days[[i]]$Volatility, 1)) %>% unlist
  portfolio_correlation_risk <- round(sum(abs(w * risks)), 2) * 100
  print(paste("Portfolio correlation risk:", portfolio_correlation_risk, "%"))
  # Active positions
  portfolio_positions <- sum(today_trading$Position != 0)
  portfolio_symbols <- nrow(today_trading)
  print(paste("Active positions:", portfolio_positions, "total symbols:", portfolio_symbols))
  # The empirical current IDM using daily data
  current_idm <- round(calculate_IDM(tail(daily_returns[,-1], corr_length)), 2)
  print(paste("Empirical daily IDM:", current_idm))
  # Update position
  print("Positions to update:")
  trades <- today_trading %>% filter(Trading == TRUE) %>% 
    dplyr::select(Date, Close, Symbol, PositionChange, Position, PositionPrevious, PositionMax, PositionOptimal, Forecast, ForecastTrend, ForecastCarry, ForecastSkew)
  print(as.data.frame(trades))
  print("Forecast averages:")
  print(today_trading %>% dplyr::select(Forecast, ForecastTrend, ForecastCarry, ForecastSkew) %>% abs %>% colMeans())
  # Write to file
  if(!dry_run) {
    write_csv(previous_trading, paste0(logs_dir, "/", now_string, ".POSITIONS.csv"))
    write_csv(today_trading, "POSITIONS.csv")
    portfolio_info <- read_csv(portfolio_file, show_col_types  = FALSE) %>% as.data.frame()
    info <- tibble(Date=today_string, Capital=capital, Symbols=portfolio_symbols, 
                       Positions=portfolio_positions, Tracking_Error=NA, Volatility=portfolio_volatility, 
                       Jump_Risk=NA, Correlation_Risk=portfolio_correlation_risk, IDM=current_idm)
    write_csv(rbind(info, portfolio_info), portfolio_file)
  }
}

