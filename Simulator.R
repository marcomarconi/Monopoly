library(tidyverse)
library(mvtnorm)

{
# Build the contracts calendar with maturities  
{  
N <- 365
dates <- 1:N 
dt <- 1/365  
contract_len <- 120
contract_expiry <- 30
contract_n <- sum(dates %% contract_expiry == 0)
contract_calendar <- matrix(NA, nrow=N, ncol=contract_n)
j <- 1
for(i in 1:N) {
    if(i %% contract_expiry == 0){
      s <- max(1, i - contract_len)
      contract_calendar[s:i, j] <- seq(i-s, 0)
      j <- j + 1
      if(j > ncol(contract_calendar))
        break
  }
}
}

# Parameters
{
sigma <- 0.01 
s0 <- log(100)
S <- diag(contract_n + 1); S[S==0] <- 0.9
converge_linear <- function(y, spot, maturity) {
  return((spot - y)/(maturity + 1))
}
}

{
E <- rmvnorm(N, mu = rep(0, contract_n + 1), sigma = S)
E_spot <- as.vector(E[,1]) * sigma
E_Y <- E[,-1] * sigma
spot_r <- rep(NA, N)
spot_p <- rep(NA, N)
Y_r <- matrix(NA, nrow=N, ncol=contract_n)
Y_p <- matrix(NA, nrow=N, ncol=contract_n)
Bias <- matrix(NA, nrow=N, ncol=contract_n)
spot_r[1] <- 0
spot_p[1] <- s0
Y_r[1,] <- 0
for(i in 2:N){
  spot_r[i] <- E_spot[i]   
  spot_p[i] <- spot_p[i-1] + spot_r[i] 
  for(j in 1:contract_n){
    if(is.na(Y_p[i-1,j]) & !is.na(contract_calendar[i,j])){
      Bias[i-1,j] <- 0.1
      Y_p[i-1,j] <- spot_p[i-1] + Bias[i-1,j]
    }
    Bias[i,j] <- Bias[i-1,j] - converge_linear(spot_p[i-1], Y_p[i-1,j], contract_calendar[i,j])
    Y_r[i,j] <- E_Y[i, j] - 1 * (Y_p[i-1,j] - Bias[i,j] - 1 * spot_p[i-1])
    Y_p[i,j] <- Y_p[i-1,j] + Y_r[i,j]
  }
}
Y_r[is.na(Y_r)] <- 0
#colnames(Y_gbm) <- contracts_symbol
spot_gbm <- exp(spot_p)
Y_gbm <- exp(Y_p)
ft_df_wide <- data.frame(Date=1:N, Spot=spot_gbm, Y_gbm) 
matplot2(ft_df_wide[,-1], lwd=2); abline(h=0)
}

}


{
  ft_df_long <- ft_df_wide %>% pivot_longer(-Date, names_to = "Symbol", values_to = "Close") %>% filter(!is.na(Close)) %>% group_by(Date) %>% mutate(Contract=row_number()-1) %>% group_by(Symbol) %>% mutate(Return=Close/lag(Close), Volume=0)
  ft_df_spreads <- build_spreads(ft_df_long %>% filter(Symbol != "Spot"), list(c(1,2), c(2,3), c(3,4), c(4,5)))
  # Term structure
  res <- ft_df_long %>% na.omit %>% group_by(Date) %>% mutate(Basis = abs(Close - first(Close))/sd(Close)) %>% group_by(Contract) %>% reframe(M=mean(Basis), S=sd(Basis)) %>% na.omit
  p1 <- ggplot(res, aes(Contract, y=M, ymin=M-S, ymax=M+S)) + geom_line() + geom_errorbar(width=0.25)
  # Contracts volatilities
  res <- ft_df_long %>% na.omit %>% group_by(Contract) %>% filter(n()>32)%>% mutate(Volatility = runSD(Return, 32)*sqrt(252)) %>% reframe(M = mean(Volatility, na.rm=T), S = sd(Volatility, na.rm=T))
  p2 <- ggplot(res, aes(Contract, y=M, ymin=M-S, ymax=M+S)) + geom_line() + geom_errorbar(width=0.25)
  # Spreads
  p3 <- ggplot(ft_df_spreads, aes(x=Date, y=SpreadPoint, color=Contracts)) + geom_line(linewidth=1) + ggtitle(symbol) + scale_color_colorblind()
}
  
  
  
{
x <- rep(NA, 1000)
x[1] <- 0
y <- seq(0, 3, length.out=1000)
for(i in 2:1000) {
  x[i] <- rnorm(1) - (cumsum(x[1:i]) - 0.1 * y[i])
}
plot.ts(cumsum(x))
}
# month_code <- setNames(1:12, c("F", "G", "H", "J", "K", "M", "N", "Q", "U", "V", "X", "Z"))  
# start_date <- as.Date("2022-01-01")
# end_date <- as.Date("2023-12-31")
# trading_years <- unique(year(date_range))
# contracts_month <- month_code[c(1,4,7,10)]
# contracts_n <- length(contract_month) * length(unique(year(date_range)))
# contracts_symbol <- paste0("A0", names(contract_month), trading_years)
# first_trade_offset <- 120
# last_trade_day <- 15

# date_range <- seq(start_date, end_date, 1)
# N <- length(date_range)
# trading_days <- matrix(FALSE, nrow=N, ncol=contracts_n)
# for(i in 1:N) {
#   y <- as.character(year(date_range[i]))
#   for(j in 1:contracts_n) {
#     c_m <- as.character(contract_month[j])
#     c_d <- as.character(last_trade_day)
#     last_day <- as.Date(paste(y, c_m, c_d, sep="-"))
#     contract_range <- seq(last_day-first_trade_offset, last_day, 1)
#     if(date_range[i] %in% contract_range)
#       trading_days[i,j] <- TRUE
#   }
# }
# trading_days <- data.frame(Date=date_range, trading_days)

{
VIX <- read_csv("/home/marco/trading/Systems/Options/Data//VIX.csv", show_col_types = F)  %>% select(Date, `Adj Close`) %>% rename(VIX=`Adj Close`)
df <- BackAdj$VI 
VI <- merge(df, VIX, by="Date") %>% mutate(VIX = (VIX), VX=(Close), Index = rep(1:nrow(VI), each=21, length.out=nrow(VI)))
b <- VI %>% group_by(Index) %>% reframe(VIX=first(VIX), VX=first(VX), PnL=sum(Difference)*-1000) %>% mutate(x=VIX, y=PnL)
plot(b$x, b$PnL)
}
