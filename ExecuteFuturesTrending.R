{
  suppressMessages(library(tidyverse))
  suppressMessages(library(lubridate))
  suppressMessages(library(readr))
  suppressMessages(library(TTR))
  suppressMessages(library(crayon))
  suppressMessages(library(zoo))
  source("/home/marco/trading/Systems/Common/RiskManagement.R")
  source("/home/marco/trading/Systems/Common/Indicators.R")
  source("/home/marco/trading/Systems/Common/Common.R")
}

# Note of warning: this script uses yesterday's signal

writeLog <- function(s, log_dir = NULL, d = today(), print_date = TRUE) {
  if(is.null(dir))
    stop("Specify a log directory")
  if(substr(log_dir, nchar(log_dir),1) != "/")
    log_dir <- paste0(log_dir, "/")
  log_file <- paste0(log_dir, gsub("\\-", "", today()), ".txt")
  init_date <- now()
  if(print_date)
    init_date <- ""
  write(paste(init_date, s), log_file, append = TRUE)
}


dir="/home/marco/trading/Historical Data/Yahoo/Scraping/"
asset_file="/home/marco/trading/Historical Data/Yahoo/Scraping/Assets.txt"
scraper_file="/home/marco/trading/Historical Data/Yahoo/retrieve.sh"
log_dir <- "/home/marco/trading/Systems/Monopoly/Logs/"

load_all_data(dir, asset_file, scraper_file, download_investing = FALSE)
Closes_yahoo <- Closes_yahoo %>% na.locf
full_df <- apply(Closes_yahoo[,-1], 2,  function(x)AbsoluteStrength(x, 250,  sma = EMA)) 
last_value <- head(tail(full_df, 2), 1) # yesterday's value (NOT today, it produces errors for some instruments like ZR=F)
prev_last_value <- head(tail(full_df, 3), 1) # the day before yesterday's value 
trade <- sign(last_value) != sign(prev_last_value)
df <- data.frame(Symbol=colnames(trade), Change=as.vector(trade), Value=as.vector(last_value))
for(i in 1:nrow(df)) {
  s <- paste(df[i,], collapse = ' ')
  cat(ifelse(df$Change[i], blue(s), s), "\n")
}
log_file <- paste0(log_dir, "/", gsub("\\-", "", today()), ".txt")
write_delim(df, log_file)
