{
  library(tidyverse)
  library(rstan)
  library(loo)
  library(mvtnorm)
  library(cmdstanr)
  library(quantmod)
  library(forecast)
  library(mvtnorm)
  library(posterior)
  library(bayesplot)
  library(reshape2)
  library(plotly)
  library(dygraphs)
  library(tsibble)
  source("/home/marco/trading/Systems//Common/Common.R")
  setwd("/home/marco/trading/Systems/Monopoly/")
  Sys.setlocale("LC_TIME", "en_US.UTF-8")
  theme_set(theme_bw(base_size = 32))
}

# Load all CPI and IR data
{
  CPI <- read_csv("Data/OECD/CPI.csv") %>% mutate(Indicator="CPI") %>% rename(Country=LOCATION, Date=TIME) %>% select(Indicator, Country, Date, Value) 
  IR <- read_csv("Data/OECD/InterestRate.csv") %>% mutate(Indicator="IR") %>% rename(Country=LOCATION, Date=TIME) %>% select(Indicator, Country, Date, Value) 
  STIR <- read_csv("Data/OECD/STIR.csv") %>% mutate(Indicator="STIR") %>% rename(Country=LOCATION, Date=TIME) %>% select(Indicator, Country, Date, Value) 
  LTIR <- read_csv("Data/OECD/LTIR.csv") %>% mutate(Indicator="LTIR") %>% rename(Country=LOCATION, Date=TIME) %>% select(Indicator, Country, Date, Value) 
  YC <- full_join(STIR[,-1], LTIR[,-1],  by=c("Country", "Date")) %>% na.omit %>% mutate(Indicator="YC", Value=Value.y-Value.x)  %>% select(Indicator, Country, Date, Value) 
  GDP <- read_csv("Data/OECD/GDP.csv") %>% mutate(Indicator="GDP") %>% rename(Country=LOCATION, Date=TIME) %>% select(Indicator, Country, Date, Value) 
  GDP$Date <- sub("Q1", "03", GDP$Date) %>% sub("Q2", "06", .) %>% sub("Q3", "09", .) %>% sub("Q4", "12", .) 
  SP <- read_csv("Data/OECD/SharePrice.csv") %>% mutate(Indicator="SP") %>% rename(Country=LOCATION, Date=TIME) %>% select(Indicator, Country, Date, Value) 
  UR <- read_csv("Data/OECD/UnemploymentRate.csv") %>% mutate(Indicator="UR") %>% rename(Country=LOCATION, Date=TIME) %>% select(Indicator, Country, Date, Value) 
  Df <- rbind(CPI, IR, STIR, LTIR, YC, GDP, SP, UR)
  Df$Indicator <- factor(Df$Indicator)
  Df$Country <- factor(Df$Country)
  Df$Date <- as.Date(paste0(Df$Date, "-01"), format="%Y-%m-%d")
}

# Plotting previous data
{
  filter(Df, Country %in% c("USA") & Indicator %in% c("YC")) %>% #mutate(ValueL=SMA(zoo::na.locf(c(0,diff(log(Value))*100)))) %>% mutate(Value=ifelse(Indicator=="SP", ValueL, Value)) %>% 
    ggplot() + geom_line(aes(Date, Value , col=Indicator), lwd=1) + geom_hline(yintercept = 0) + facet_wrap(.~Country) + theme(text = element_text(size=32))
  filter(Df, (Indicator=="IR" | Indicator=="CPI") & Country %in% c("USA")) %>% dcast(Date~Indicator) %>% na.omit %>% 
    ggplot() + geom_point(aes(CPI, IR))
  filter(Df, (Indicator=="IR" | Indicator=="CPI")  & Country %in% c("USA")) %>% dcast(Date~Indicator) %>%  
    ggplot() + geom_point(aes(c(0, diff(CPI)), c(0, diff(IR))))
  filter(Df, (Indicator=="IR" | Indicator=="CPI") & Country %in% c("USA")) %>%
    ggplot() + geom_histogram(aes(Value)) + facet_wrap(~Indicator)
  usa <- filter(Df, Country %in% c("USA") & Indicator %in% c("IR", "CPI")) %>%  pivot_wider(names_from = Indicator, values_from = Value) %>% select(-Country, -Date) 
}


# Spot Prices taken from https://www.eia.gov/dnav/pet/pet_pri_spt_s1_m.htm
{
lista <- list()
lista[["oil"]] <- read_csv("Data/Oil/Prices/Oil.csv")
lista[["gasoline"]] <- read_csv("Data/Oil/Prices/Gasoline.csv")
lista[["heatingoil"]] <- read_csv("Data/Oil/Prices/HeatingOil.csv")
lista[["ULSD"]] <- read_csv("Data/Oil/Prices/ULSD.csv")
lista[["kerosene"]] <- read_csv("Data/Oil/Prices/Kerosene.csv")
lista[["propane"]] <- read_csv("Data/Oil/Prices/Propane.csv")
Prices <- Reduce(function(...) full_join(..., by="Date"), lista) 
Prices$Date <- paste0("01-", Prices$Date)
Prices$Date <- as.Date(Prices$Date, format="%d-%b-%Y")
Prices <- mutate(Prices, Season = case_when(month(Date) %in% c(12,1,2) ~ "Winter", month(Date) %in% c(3,4,5) ~ "Spring", month(Date) %in% c(6,7,8) ~ "Summer", month(Date) %in% c(9,10,11) ~ "Autumn"  ))
Prices <- arrange(Prices, Date)
Prices$GasolineSpread <- Prices$Gasoline*42 - Prices$Brent
Prices$HoSpread <- Prices$HeatingOil*42 - Prices$Brent
Prices$ULSDSpread <- Prices$NY_ULSD*42 - Prices$Brent
Prices$KeroseneSpread <- Prices$Kerosene*42 - Prices$Brent
ggplot(Prices %>% na.omit, aes(Date, HoSpread)) +  geom_line( col="gray") +geom_point( col=as.numeric(factor(na.omit(Prices)$Season)), size=3)
}

# Short term oil outlook data
{
outlook <- read_csv("/home/marco/trading/Systems/Monopoly/Data/Oil/ShortTermOutlook.csv" ,show_col_types = FALSE)
outlook$Date <- paste("01", outlook$Date)
outlook$Date <- as.Date(outlook$Date, format="%d %b %Y")
cols <- c(
            "WTIPUUS", 
            "BREPUUS",
            "MGWHUUS",
            "D2WHUUS",
            "DSWHUUS",
            "JKTCUUS",
            "NGHHMCF",#
            "PAPR_WORLD",
            "PAPR_OPEC...148",
            "PAPR_NONOPEC...969",
            "PATC_WORLD...224",
            "COPS_OPEC",
            "PADI_OPEC",
            "MGTCPUSX",
            "ORUTCUS",
            "MGPSPUS",
            "MGNIPUS",
            "PASC_US",
            "ELCOTWH",
            "WP57IUS",
            "SOTCBUS"
            )
newnames <- c( "WTI",
               "Brent", 
               "Gasoline", 
               "HeatingOil", 
               "DieselFuel",
               "JetFuel",
               "NaturalGas",
               "World_Oil_Production", # annual seasonal
               "OPEC_Oil_Production", # annual seasonal
               "NonOPEC_Oil_Production", # annual seasonal
               "World_Oil_Consumption", # annual seasonal
               "OPEC_Oil_Capacity", 
               "OPEC_unplanned_disruptions" ,# (bi)annual seasonal,
               "Motor_Gasoline_Supplied",
               "Refinery_Utilization_Factor",  
               "Motor_Gasoline_US_Inventory",
               "Motor_Gasoline_Net_Imports",
               "US_inventory",
               "Electricity_Consumption_US",
               "PPI_Petroleum",
               "Consumption_of_Solar_Energy" 
               
            )
oil <- select(outlook, c("Date", cols)) %>% rename_at(vars(cols), function(x) newnames)
oil$OilSpread <- oil$WTI - oil$Brent #
oil$GasolineSpread <- oil$Gasoline*0.42 - oil$Brent # 
oil$HoSpread <- oil$HeatingOil*0.42 - oil$Brent #
oil$DieselSpread <- oil$DieselFuel*0.42 - oil$Brent
oil$JetfuelSpread <- oil$JetFuel*0.42 - oil$Brent
oil$GasolineDieselSpread <- oil$Gasoline - oil$DieselFuel
oil$GasolineHoSpread <- oil$Gasoline - oil$HeatingOil
oil$HoDieselSpread <- oil$HeatingOil - oil$DieselFuel #
oil <- mutate(oil, Month=month(Date), Season = case_when(month(Date) %in% c(12,1,2) ~ 1, month(Date) %in% c(3,4,5) ~ 2, month(Date) %in% c(6,7,8) ~ 3, month(Date) %in% c(9,10,11) ~ 4))
LI <- read_csv("/home/marco/trading/Systems/Monopoly/Data/OECD//LeadingIndicators.csv", show_col_types = FALSE)
CPI <- read_csv("/home/marco/trading/Systems/Monopoly/Data/OECD//CPI.csv", show_col_types = FALSE)
GDP <- read_csv("/home/marco/trading/Systems/Monopoly/Data/OECD//GDP.csv", show_col_types = FALSE)
GDP$TIME <- sub("Q1", "03", GDP$TIME) %>% sub("Q2", "06", .) %>% sub("Q3", "09", .) %>% sub("Q4", "12", .) 
MA <- read_csv("/home/marco/trading/Systems/Monopoly/Data/OECD//MonetaryAggregates.csv", show_col_types = FALSE)
PPI <- read_csv("/home/marco/trading/Systems/Monopoly/Data/OECD//PPI.csv", show_col_types = FALSE)
TG <- read_csv("/home/marco/trading/Systems/Monopoly/Data/OECD//TradeInGoods.csv", show_col_types = FALSE)
UR <- read_csv("/home/marco/trading/Systems/Monopoly/Data/OECD//UnemploymentRate.csv", show_col_types = FALSE)
df <- rbind(LI, CPI,  PPI, TG, UR, MA)
df <- filter(df, LOCATION=="USA") %>% select(INDICATOR,TIME,Value) %>% mutate(TIME=as.Date(paste0(TIME, "-01"), format="%Y-%m-%d")) %>% rename(Date=TIME) %>% dcast(Date ~ INDICATOR, value.var = "Value") %>% arrange(Date)
Oil <- full_join(oil, df, by="Date")%>% arrange(Date) %>% mutate(Date = yearmonth(as.character(Date)))
cot <- read_csv("/home/marco/trading/Systems/Monopoly/Data/Oil/COT_gasoline.csv", show_col_types = FALSE)
cot <- mutate(cot, CommercialsNET = `Commercial Positions-Long (All)` /  (`Commercial Positions-Long (All)` + `Commercial Positions-Short (All)`), 
                   NonCommercialsNET = `Noncommercial Positions-Long (All)` /  (`Noncommercial Positions-Long (All)` + `Noncommercial Positions-Short (All)`)) %>% 
                   rename(Date = `As of Date in Form YYYY-MM-DD`) %>% select(Date, CommercialsNET, NonCommercialsNET) %>% 
                   mutate(Date = yearmonth(as.character(Date)))
cot <- group_by(cot, Date) %>% summarise(Date=last(Date), COT_Gasoline_Commercials=last(CommercialsNET),
                                         COT_Gasoline_NonCommercials=last(NonCommercialsNET))
Oil <- full_join(Oil, cot, by="Date")%>% arrange(Date)
cot <- read_csv("/home/marco/trading/Systems/Monopoly/Data/Oil/COT_heatingoil.csv", show_col_types = FALSE)
cot <- mutate(cot, CommercialsNET = `Commercial Positions-Long (All)` /  (`Commercial Positions-Long (All)` + `Commercial Positions-Short (All)`), NonCommercialsNET = `Noncommercial Positions-Long (All)` /  (`Noncommercial Positions-Long (All)` + `Noncommercial Positions-Short (All)`)) %>% rename(Date = `As of Date in Form YYYY-MM-DD`) %>% select(Date, CommercialsNET, NonCommercialsNET)%>% mutate(Date = yearmonth(as.character(Date)))
cot <- group_by(cot, Date) %>% summarise(Date=last(Date), COT_HeatingOil_Commercials=last(CommercialsNET),
                                         COT_HeatingOil_NonCommercials=last(NonCommercialsNET))
Oil <- full_join(Oil, cot, by="Date")%>% arrange(Date)
cot <- read_csv("/home/marco/trading/Systems/Monopoly/Data/Oil/COT_crudeoil.csv", show_col_types = FALSE)
cot <- mutate(cot, CommercialsNET = `Commercial Positions-Long (All)` /  (`Commercial Positions-Long (All)` + `Commercial Positions-Short (All)`), 
                   NonCommercialsNET = `Noncommercial Positions-Long (All)` /  (`Noncommercial Positions-Long (All)` + `Noncommercial Positions-Short (All)`)) %>% rename(Date = `As of Date in Form YYYY-MM-DD`) %>% select(Date, CommercialsNET, NonCommercialsNET)%>% mutate(Date = yearmonth(as.character(Date)))
cot <- group_by(cot, Date) %>% summarise(Date=last(Date), COT_CrudeOil_Commercials=last(CommercialsNET),
                                                           COT_CrudeOil_NonCommercials=last(NonCommercialsNET))
Oil <- full_join(Oil, cot, by="Date")%>% arrange(Date)
#for(i in 2:ncol(Oil)) Oil[,paste0(colnames(Oil)[i], "_S")] <- Oil[,i] - lag(Oil[,i])
p <- ggplot(Oil , aes(Date, Motor_Gasoline_Supplied)) +  
  geom_line( col="gray") +
  geom_point( col=as.numeric(factor(Oil$Season)), size=3)
ggplotly(p)  

}



# Load future contracts, we expect all the contract to be in the directory, and the contract order in the file order.txt
load_future_contracts <- function(symbol, dir, order_file="order.txt") {
  #cash <- read_csv("/home/marco/trading/Historical Data/Barchart/Cash/cty00.csv", show_col_types = FALSE) %>% select(Time, Last) %>% rename(Date=Time) %>% mutate(Date=as.Date(Date, format="%m/%d/%Y"))
  files <- list()
  for(l in list.files(dir, pattern = ".csv")) {
    f <- read_csv(paste0(dir, "/", l), show_col_types = FALSE) %>% select(Time, Last) %>% rename(Date=Time)
    f$Date <- as.Date(f$Date, format="%m/%d/%Y") 
    f <- arrange(f, Date)
    f <- rbind(data.frame(Date=f$Date[1]-1, Last=NaN), f, data.frame(Date=f$Date[length(f$Date)]+1, Last=NaN))
    files[[sub(".csv", "", l)]] <- f
  }
  df <- Reduce(function(...) full_join(..., by="Date", all=T), files) %>% arrange(Date)
  colnames(df) <- c("Date", names(files))
  order <- scan(paste0(dir, "/", order_file), what="string") %>% tolower()
  df <- df[,c("Date", order)] %>% arrange(Date)
  #df <- merge(cash, df, by="Date", all=T) %>% rename(Cash=Last) %>% arrange(Date)
  return(df)
}

# build up the rollover curve given a df from load_future_contracts
rollover_curve <- function(df, forward=6) { # assume only N x T matrix, no Date or other columns
  curve_diff <- rep(NA, nrow(df))
  curve_lm <- rep(NA, nrow(df))
  for(i in 1:nrow(df)) {
    a <- df[i, ]
    b <- na.omit(a)
    if(length(b) < forward) 
      next
    b_log <- log(b)
    x <- 1:length(b_log)
    curve_lm[i] <- -12 * coef(lm(b_log ~ x))[2]
    curve_diff[i] <- b[1] - b[forward] 
  }
  return(cbind(curve_lm, curve_diff))
}

# create a single future contract
backadjust_future <- function(df, N=1) {
  m <- as.matrix(df[,-1]) # the first column is supposed to be the Date
  sc <- rep(NA, ncol(m))
  ec <- rep(NA, ncol(m))
  for(j in 1:ncol(m))
    for(i in 1:nrow(m)){
      if(is.nan(m[i,j])) {
        if(is.na(sc[j]))
          sc[j] <- i+1
        else
          ec[j] <- i-1
      }
    }
  nearest <- rep(NA, nrow(m))
  continous <- rep(NA, nrow(m))
  adjs <- rep(0, nrow(m))
  jump <- 0
  j <- 1; 
  i <- 1; 
  while(i <= nrow(m)) {
    # take note of the current adjustment between the current contract and the previous one
    if(j < ncol(m)) {
      n_jump <-  m[i,j+1] - m[i,j] 
      if(!is.na(n_jump))
        jump <- n_jump
    }
    if(i>1)
      adjs[i] <- adjs[i-1] # keep the previous adjustment 
    # rollback contract if:
    # - we have not reached the last contract yet
    # - we have reached at least the number of days from the end of current contract 
    # - there is price on the next contract
    # - it is not the last day
    if(j < ncol(m) && i >= ec[j]-N+1 & !(is.na(m[i,j+1]) | is.nan(m[i,j+1])) & i < nrow(m)-1  ) {
      cc <- 0
      j <- j + 1
      adjs[i] <- adjs[i] + jump # update the adjustment now that we rollback the contract
    } 
    if(j == ncol(m)+1)
      break
    nearest[i] <-  m[i,j] 
    i <- i + 1
  }
  #adjs <- c(diff(adjs) %>% rev %>% cumsum %>% rev,0)
  continous <- nearest + adjs
  return(data.frame(Date=df[,1], Nearest=nearest, Continous=continous, Adjs=adjs))
}

# Load futures data and calculate rollover curves
{
  to_load <- list(c("CL", "/home/marco/trading/Historical Data/Barchart/CrudeOil/"),
                  c("CB", "/home/marco/trading/Historical Data/Barchart/Brent//"),
                  c("RB", "/home/marco/trading/Historical Data/Barchart/Gasoline/"),
                  c("HO", "/home/marco/trading/Historical Data/Barchart/ULSDNYHArbor/"),
                  c("LF", "/home/marco/trading/Historical Data/Barchart/Gasoil/"))
  Futures <- list()
  for(a in to_load) 
    Futures[[a[1]]] <- load_future_contracts(a[1], a[2])
  Curves <- list()
  for(a in  names(Futures)) {
    b <- as.data.frame(rollover_curve(as.matrix(Futures[[a]][,-1])))
    colnames(b) <- c(paste0(a[1], "_lm"), paste0(a[1], "_diff"))
    Curves[[a]] <- data.frame(Futures[[a]][,1], b)
  }

  
  Oil_extended2 <- 
    merge(Oil, Curves[["CL"]] %>%  mutate(Date=yearmonth(Date)) %>% group_by(Date) %>% 
            summarise(across(everything(), last)), by="Date") %>% 
    merge(., Curves[["RB"]] %>%  mutate(Date=yearmonth(Date)) %>% group_by(Date) %>% 
            summarise(across(everything(), last)), by="Date") %>% 
    merge(., Curves[["HO"]] %>%  mutate(Date=yearmonth(Date)) %>% group_by(Date) %>% 
            summarise(across(everything(), last)), by="Date") %>% 
    merge(., Curves[["LF"]] %>%  mutate(Date=yearmonth(Date)) %>% group_by(Date) %>% 
            summarise(across(everything(), last)), by="Date") 
}



# m0.1 simple spread model
{
  {
    N <- 500
    x <- matrix(NA, nrow=N, ncol=2)
    y <- matrix(NA, nrow=N, ncol=2)
    p <- rep(NA, N); p[1] <- 0; err_p <- rnorm(N, 0, 1) 
    x[1,] <- c(log(100), log(100) + log(42))
    y[1,] <- x[1,] 
    Z <- matrix(c(1, 0, 
                  1, 0), nrow = 2, byrow = TRUE)
    H <- diag(c(1,1))
    U <- c(0.05, log(42))  
    A <- c(0, 0)
    C <- matrix(c(0, 0, 0, 1), nrow = 2, byrow = TRUE) * 0
    Q <- matrix(c(0.01, 0,
                  0,      0),  byrow = T, ncol=2)
    R <- matrix(c(0.01, 0,
                  0, 0.01), byrow = T, ncol=2)
    st <- c(1); theta <- matrix(c(0.99, 0.01, 0.05, 0.95), byrow = T, ncol=2)
    for(i in 2:N) {
      st[i] <- rbernoulli(1, 1-theta[st[i-1], 1]) +1 
      p[i] <- c(0.0, 0.1)[st[i]] * err_p[i]^2  + 0.9 * p[i-1]
      x[i,] <- Z %*% x[i-1,] + U + C %*% c(0, p[i]) + t(rmvnorm(1, c(0,0), Q))
      y[i,] <- H %*% x[i,] + A + t(rmvt(n = 1, sigma = R, df=5))
    }
    matplot2(x); grid()
    matplot(y, type="p", pch=16, add=TRUE);
    plot(p, type="l")
  }
  
  m0.1_predict <- function(fit, delta=2) {
    m <- extract(fit, pars="m")[[1]]
    m_pred <- extract(fit, pars="m_pred")[[1]]
    P_pred <- extract(fit, pars="P_pred")[[1]]
    p_mean_1 <- colMeans(m_pred[,,1]) - lag(colMeans(m[,,1]))
    p_sd_1 <- colMeans(P_pred[,,1,1])
    p_mean_2 <- colMeans(m_pred[,,2]) - lag(colMeans(m[,,2]))
    p_sd_2 <- sqrt(colMeans(P_pred[,,2,2]))
    return(list(p_1 = cbind(p_mean_1-p_sd_1*delta, p_mean_1+p_sd_1*delta), 
                p_2 = cbind(p_mean_2-p_sd_2*delta, p_mean_2+p_sd_2*delta))
    )
  }
  
  m0.1 <- stan_model("Stan/m0.1.stan")
  fit_ m0.1<- sampling() m0.1, data=list(N=nrow(y), y=y, m0=c(0,0), P0=diag(c(1,1)), family=1, likelihood=1), chains=1, iter=500)
}


# SSM/LLT
{
{
  
par(mfrow=c(2,1), mar=c(2,2,1,0.5))
N <- 5000
v <- rep(0, N)
x <- rep(0, N)
y <- x
hl <- 20
ar_x <- 0.95 # ^0 to remove ar
ar_v <- 1
err_x <- rep(0, N)
err_v <- rep(0, N)
err_y <- rep(N)
sigma_x <- 0.25
sigma_y <- 0.2
sigma_v <- 0.0005   # 0 to remove velocity
u <- 0
for(i in 3:N) {
  err_v[i] <-  rnorm(1, 0, sigma_v)
  err_x[i] <-  rnorm(1, 0, sigma_x)
  err_y[i] <-  rnorm(1, 0, sigma_y)
  v[i] <- ar_v * v[i-1] + 0.99 * (v[i-1]- v[i-2]) + err_v[i]
  x[i] <- ar_x * (x[i-1] + u + v[i]) + err_x[i]
  y[i] <- x[i] + err_y[i]
}
plot(x, type="l", col="red"); grid()
points(y, pch=16, col="blue")
if(sigma_v > 0)
  plot(v, type="l", col="black"); abline(h=0); grid()
}

ssm_predict <- function(fit, delta=2) {
  m <- extract(fit, pars="m")[[1]]
  m_pred <- extract(fit, pars="m_pred")[[1]]
  P_pred <- extract(fit, pars="P_pred")[[1]]
  p_mean <- colMeans(m_pred) - lag(colMeans(m))
  p_sd <- colMeans(P_pred)
  return(cbind(p_mean-p_sd*delta, p_mean+p_sd*delta))  
}


# testing different model over the test
model_ssm <- stan_model("../../Stan/SSM.stan")
model_ar <- stan_model("../../Stan/SSM_ar.stan")
model_llt <- stan_model("../../Stan/LLT.stan")
fit_ssm <- sampling(model_ssm, data=list(N=length(y), y=y, m0=0, P0=1, trainset=length(y)), chains=1, iter=500)
fit_ar <- sampling(model_ar, data=list(N=length(y), y=y, m0=0, P0=1, trainset=length(y)), chains=1, iter=500)
fit_llt <- sampling(model_llt, data=list(N=length(y), y=y, m0=c(0,0), P0=diag(c(1,1)), family=0), chains=1, iter=500)
loo_compare(loo(fit_ssm), loo(fit_ar), loo(fit_llt))


# current filtered states
m_ar <- extract(fit_ar, pars="m")[[1]]
m_ssm <- extract(fit_ssm, pars="m")[[1]]
m_llt <- extract(fit_llt, pars="m")[[1]]
# prediction m
m_pred_ssm <- extract(fit_ssm, pars="m_pred")[[1]]
m_pred_ar <- extract(fit_ar, pars="m_pred")[[1]]
m_pred_llt <- extract(fit_llt, pars="m_pred")[[1]]
# prediciton P
P_pred_ssm <- extract(fit_ssm, pars="P_pred")[[1]]
P_pred_ar <- extract(fit_ar, pars="P_pred")[[1]]
P_pred_llt <- extract(fit_llt, pars="P_pred")[[1]]
# our positions will be the difference between the prediction and the current filtered state
p_ssm <- colMeans(m_pred_ssm) - lag(colMeans(m_ssm))
p_ar <- colMeans(m_pred_ar) - lag(colMeans(m_ar))
p_llt <- colMeans(m_pred_llt[,,1]) - lag(colMeans(m_llt[,,1]))
# discrete positions, long/shorts
p_ssm <- rowSums(t(replicate(N, c(-1, 0, 1))) * cbind(p_ssm<0, p_ssm==0, p_ssm>0))
p_ar <- rowSums(t(replicate(N, c(-1, 0, 1))) * cbind(p_ar<0, p_ar==0, p_ar>0))
p_llt <- rowSums(t(replicate(N, c(-1, 0, 1))) * cbind(p_llt<0, p_llt==0, p_llt>0))
# plotting the equities, the ar obviously wins
r <- c(0,diff(log(y+1000)))
matplot2(cbind(cumsum(na.omit(p_ssm*r)), cumsum(na.omit(p_ar*r)), cumsum(na.omit(p_llt*r))))

}



# Load Yields
{
  Yields_ratios <- list()
  setwd("/home/marco/trading/Systems/Monopoly/Data/")
  codes <- read.csv("Countries_codes.txt")
  GDP <- read_csv("OECD/GDP.csv", show_col_types = FALSE) %>% mutate(Indicator="GDP") %>% rename(Country=LOCATION, Date=TIME) %>% select(Indicator, Country, Date, Value) 
  GDP$Date <- sub("Q1", "03", GDP$Date) %>% sub("Q2", "06", .) %>% sub("Q3", "09", .) %>% sub("Q4", "12", .) 
  UR <- read_csv("OECD/UnemploymentRate.csv") %>% mutate(Indicator="UR") %>% rename(Country=LOCATION, Date=TIME) %>% select(Indicator, Country, Date, Value) 
  setwd("Yields/")
  files <- list.files(".")
  countries <- sub(" 10.*", "", files)  %>% sub(" 2.*", "", .) %>% unique
  dfs <- list()
  for(f in countries) 
  {
    print(f)
    f10 <- read_csv(paste(f, "10-Year Bond Yield Historical Data.csv"), show_col_types = FALSE)
    f2 <- read_csv(paste(f, "2-Year Bond Yield Historical Data.csv"), show_col_types = FALSE)
    m <- merge(f10, f2, by="Date")
    m <- select(m, Date, Price.x, Price.y) %>% rename(Y10=Price.x, Y2=Price.y) %>% mutate(Date=as.Date(Date, format="%b %d, %Y")) %>% arrange(Date)
    m$Month <- month(m$Date);m$Year <- year(m$Date);
    m <- group_by(m, Year, Month) %>% summarise(Date=first(Date),   Y10=first(Y10), Y2=first(Y2)) %>% ungroup() %>% select(-Year, -Month)
    line <- match(f,codes$Country)
    if(is.na(line)) {
      print(paste(f, "not found in codes"))
      next
    }
    g <- filter(UR, Country==codes[match(f,codes$Country),]$Code) %>% select(-Indicator, -Country) %>% rename(UR=Value)
    m$Date <- sub("\\-..$", "", m$Date)
    z <- full_join(m, g, by="Date") %>% mutate(Date= as.Date(paste0(Date, "-01"), format="%Y-%m-%d")) %>% arrange(Date) %>% na.omit()
    Yields_ratios[[f]] <- z
  }
 
}


# m1.0
# Inflation/CPI
{
  layout(matrix(c(1,1,2,3), 2, 2, byrow = TRUE))
  cpi <- filter(Df, Indicator=="CPI" & Country %in% c("USA")) %>% pull(Value)
  N <- length(cpi)
  ir <- rep(NA, N)
  ir_y <- rep(NA, N)
  sigma_ir <- 0.1
  sigma_obs <- 0.01
  ar_ir <- exp(-log(2) / 6) # ^0 to remove ar
  cpi_target <- 2.0
  response_delay <- 7
  C <- matrix(c(0.01, 1), nrow = 1, byrow = TRUE)
  w <- rnorm(N, 0, sigma_ir)
  v <- rnorm(N, 0, sigma_obs)
  ir[1:(response_delay+2)] <- cpi[1]; 
  f <- c()
  for(i in (response_delay+2):N) {
    if(cpi[i-1] > cpi_target) {
      C <- matrix(c(0.01, 0.5), nrow = 1, byrow = TRUE)
      Z <- matrix(1, nrow = 1, byrow = TRUE)
    }
    else {
      C <- matrix(c(0, 0), nrow = 1, byrow = TRUE)
      Z <- matrix(ar_ir, nrow = 1, byrow = TRUE)
    }
    #cpi[i] <- sqrt(0.01 + 0.02 * rnorm(1)^4 + 0.9 * cpi[i-1]^2 - 0.01 * ir[i-1])
    ir[i] <- Z %*% ir[i-1] +  C %*% c(cpi[i-response_delay] - cpi_target, (cpi[i-response_delay] - cpi[i-response_delay-1]) ) + w[i]
  }
  ir <- ir + v
  plot(cpi,  col="red", type="l"); grid()
  lines(ir, col="blue")
  hist(ir, 20); plot(cpi, ir);
}


# m2.0
# Inflation/Interest rate in a SSM-style
{
  logit_f <- function(x, m, b) {
    1 / (1 + exp(-b*(x-m)))
  }
  hl <- function(x) exp(-log(2)/x)
  log0 <- function(x) ifelse(x > 0, log(x), 0)
  layout(matrix(c(1,1,2,3), 2, 2, byrow = TRUE))
  N <- 500
  target <- 2
  thresholds <- c(0, 0)
  x <- matrix(NA, nrow=N, ncol=2)
  y <- matrix(NA, nrow=N, ncol=2)
  err_x <- rmvnorm(N, c(0,0), diag(c(0.2,0.01)))
  err_y <- rmvnorm(N, c(0,0), diag(c(1,1)))*0.01
  err_y[,2] <- 0
  ss <- 3
  x[1:ss,] <- c(0, 0)
  y[1:ss,] <- x[1,]
  for(i in ss:N) {
    Z11 <- ifelse(x[i-1,1] > 0, hl(1000), hl(1))
    Z22 <- ifelse(x[i-1,2] > 0, hl(5), hl(1))
    Z12 <- -0.01
    Z21 <- 0.5*logit_f(x[i-1,1], target, 2) / (x[i-1,1]+1e-6)
    Z <- matrix(c(Z11, Z12, 
                  Z21, Z22), 
                nrow = 2, byrow = TRUE) 
    #u <- (diag(c(1,1))-Z) %*% thresholds # MARSS chapter 14
    H <- matrix(c(1, 1), ncol = 2, byrow = TRUE)
    C <- matrix(c(0, 0, 
                  0.05, 0), ncol = 2, byrow = TRUE)*0
    x[i,] <- Z %*% x[i-1,] + u + C %*% (y[i-1,] - y[i-2,]) + err_x[i,]
    #x[i,1] <- cpi[i]
    y[i,] <- H * x[i,] + err_y[i,]
  }
  # x <- exp(x)
  # y <- exp(y)
  matplot2(x); grid()
  matplot(y, type="p", pch=16, add=TRUE);
  hist(y[,2], 20); plot(y);
}


# m2.1
# Yield ratio / GDP in a SSM-style
{
  logit_f <- function(x, m, b) {
    1 / (1 + exp(-b*(x-m)))
  }
  layout(matrix(c(1,1,2,2), 2, 2, byrow = TRUE))
  N <- 140
  ss <- 10
  x <- matrix(0, nrow=N, ncol=ss)
  y <- matrix(0, nrow=N, ncol=2)
  sigma_x <- rep(0, ss); sigma_x[c(1, ss)] <- 0.2^2
  sigma_y <- rep(0, 2); sigma_y[c(1, 2)] <- 0.1^2
  err_x <- rmvnorm(N, rep(0, ss), diag(sigma_x))
  err_y <- rmvnorm(N, rep(0, 2), diag(sigma_y))
  means <- matrix(c(rep(3, ss), 
                   rep(-1, ss)), byrow=T, ncol=ss)
  means[1,ss] <- 5
  means[2,ss] <- 5
  for(i in 1:ss) {
    x[i,] <- means[1,]
  }
  s <- rep(1, N)
  s[c(40:50, 100:110)] <- 2
  for(i in ss:N) {
    #s[i] <-  sample(c(s[i-1],s[i-1]%%2+1), 1, prob = c(theta[s[i-1], s[i-1]], theta[s[i-1], s[i-1]%%2+1])) 0.5*(-logit_f(x[i-(ss-1),1], 0, 3)+1)
    Z <- matrix(0, ncol=ss, nrow=ss)
    Z[1,1] <- 0.9
    Z[ss,ss] <- 0.9
    if(2 < ss)
      for(j in 2:(ss-1))
        Z[j, j-1] <- 1
    u <- (diag(c(rep(1, ss)))-Z) %*% means[s[i],] # MARSS chapter 14
    Z[ss, ss-1] <- ifelse(x[i-1,ss-1] < 0, -3, 0)
    x[i,] <- Z %*% x[i-1,] + u + err_x[i,]
    H <- matrix(0, ncol=ss, nrow=2); H[1,1] <- 1; H[2,ss] <- 1
    y[i,] <- H %*% x[i,] + err_y[i,]
    
  }
  matplot2(x, ylim=c(-2,30)); grid()
  matplot(y, type="p", pch=16, add=TRUE);
  plot(s, type="l")
}

# m3.0 simple univariate seasonal model (with discrete fourier) and covariates
# what about the peridogram?
{
  par(mfrow=c(2,1), mar=c(2,2,1,0.5))
  N <- 12
  sigma_y <- 0.01
  sigma_x <- 0.01
  CC <- c(-1.37, 1.79, 0.46, 0.53)
  cc <- matrix(0, ncol=length(CC), nrow=N)
  B <- c(1,-1)*0
  X <- matrix(0, ncol=length(B), nrow=N)
  err_b <- rmvnorm(N, c(0,0))*0.01
  # generates fourier coefficients and convariates
  for(t in 3:N) {
    for(i in seq(1, ncol(cc), 2))
      cc[t,i:(i+1)] <- c(cos(2 * pi * i * t / 12), sin(2 * pi * i * t / 12))
    X[t,] <- c(0.99, 0.85) * X[t-1,] + 0.9 * (X[t-1,] - X[t-2,])  +  err_b[t,]
  }
  x <- vector()
  x[1] <- 1
  y <- vector()
  y[1] <- 1
  for(t in 2:N) {
    x[t] <- x[t-1] + cc[t,] %*% CC + rnorm(1, 0, sigma_x) + X[t,] %*% B
    y[t] <- x[t] + rnorm(1, 0, sigma_y)
  }
  plot(x, col="red", type="l"); points(y, col="blue", pch=16);
  matplot2(X)
}




# m3.1 simple univariate seasonal model (with discrete fourier) and fixed level
{
  par(mfrow=c(2,1), mar=c(2,2,1,0.5))
  N <- 350
  sigma_y <- 0.25
  sigma_x <- 3.65
  ar <- 0.85
  level <- 3
  theta <- c(0.5, 0.25)
  CC <- c(-0.75,1.94)
  cc <- matrix(0, ncol=length(CC), nrow=N)
  for(t in 5:N) {
    for(i in seq(1, ncol(cc), 2))
      cc[t,i:(i+1)] <- c(cos(2 * pi * i * t / 12), sin(2 * pi * i * t / 12))
  }
  m <- 1:length(y)
  x <- vector()
  x[1:3] <- level / (1-ar)
  y <- vector()
  y[1:3] <- x[1:3]
  for(t in 4:N) {
    x[t] <- ar * x[t-1] + level + theta[1] * (x[t-1] - x[t-2]) + theta[2] * (x[t-2] - x[t-3]) + cc[t,] %*% CC + rnorm(1, 0, sigma_x)  
    y[t] <- x[t] + rnorm(1, 0, sigma_y)
  }
  plot(x, col="blue", type="l"); points(y, col="blue", pch=16);
  #lines( Oil$GasolineSpread %>% na.omit, type="l", col="red")
}

# m3.2 simple univariate seasonal model (with discrete fourier) and variable level
{
  par(mfrow=c(2,1), mar=c(2,2,1,0.5))
  N <- 12
  sigma_y <- 0.25
  sigma_x <- 3.65
  sigma_v <- 0.5 # amplitude variation
  CC <- c(1,-0.75)
  cc <- matrix(0, ncol=length(CC), nrow=N)
  for(t in 5:N) {
    for(i in seq(1, ncol(cc), 2))
      cc[t,i:(i+1)] <- c(cos(2 * pi * i * t / 12), sin(2 * pi * i * t / 12))
  }
  ar <- 0.8
  level <- 5
  H <- matrix(c(1,0), ncol=2, byrow=T)
  m <- 1:length(y)
  x <- matrix(NA, nrow=N, ncol=2)
  x[1,] <- c(0, 3) # the 2nd process starting level determines the initial amplitude
  y <- vector()
  y[1] <- 10
  for(t in 2:N) {
    # x[t,1] <- x[t-1,1] + x[t-1, 2] %*% (cc[t,] %*% CC) + rnorm(1, 0, sigma_x)  
    # x[t,2] <- x[t-1, 2] + rnorm(1, 0, sigma_v)  
    Fx <-  matrix(c(ar, cc[t,] %*% CC,0,1), ncol=2, byrow=T)
    x[t,] <- Fx %*% x[t-1, ] + c(ar*level, 0) + c(rnorm(1, 0, sigma_x),rnorm(1, 0, sigma_v)  )  
    y[t] <- H %*% x[t,]  + rnorm(1, 0, sigma_y)
  }
  matplot(x, col="red", type="l");
  plot(y, col="blue", pch=16, type="o");
  lines( Oil$GasolineSpread %>% na.omit, type="l", col="green")
}



# m4.0 bivariate model
{
cov_GPL2 <- function(x, sq_alpha, sq_rho, delta) {
  N = dim(x)[1];
  K <- matrix(NA, N, N)
  for (i in 1:(N-1)) {
    K[i, i] = sq_alpha + delta[i];
    for (j in (i + 1):N) {
      K[i, j] = sq_alpha * exp(-sq_rho * (x[i,j])^2 );
      K[j, i] = K[i, j];
    }
  }
  K[N, N] = sq_alpha + delta[N];
  return (K);
}  
  par(mfrow=c(2,1), mar=c(2,2,1,0.5))
  N <- 300
  x <- matrix(NA, ncol=2, nrow=N)
  y <- matrix(NA, ncol=2, nrow=N)
  ar_x <- c(0.95, 0.95)
  level <- c(3, 2)
  sigma_x <- c(3.5, 2)
  sigma_y <- c(2, 2)
  Dmat <- matrix(c(1,0.55,0.55,1), 2, 2, byrow=TRUE)
  Sx <- diag(sigma_x) %*% matrix(c(1, 0.5, 0.5, 1), ncol=2, byrow=TRUE) %*% diag(sigma_x)
  #Sx <- cov_GPL2(Dmat, 1, 3, sigma_x)
  Sy <- diag(sigma_y) %*% matrix(c(1, 0.0, 0.0, 1), ncol=2, byrow=TRUE) %*% diag(sigma_y)
  err_x <- rmvnorm(N, c(0,0), Sx)
  err_y <- rmvnorm(N, c(0,0), Sy)
  Z <- matrix(c(ar_x[1],0,0,ar_x[2]), ncol=2, byrow=TRUE)
  CC <- matrix(c(-0.75,1.94,1,-0.5), 2, 2, byrow=TRUE) 
  cc <- matrix(0, nrow=N, ncol=ncol(CC))
  for(t in 1:N) {
    for(i in seq(1, ncol(cc), 2))
      cc[t,i:(i+1)] <- c(cos(2 * pi * i * t / 12), sin(2 * pi * i * t / 12))
  }
  theta <- c(-0.25, 0.65)
  x[1:3,] <- level / (1-ar)
  y[1:3,] <- x[1:3,]
  for(i in 4:N) {
    x[i,] <-   x[i-1,]  + theta[1] * (y[i-1,] - y[i-2,]) + theta[2] * (y[i-2,] - y[i-3,])  + err_x[i,]#Z %*% x[i-1,] + level + CC %*% cc[i,] + err_x[i,]
    y[i,] <- x[i,] +   err_y[i,]
  }
  matplot(x, type="l", col="black"); grid()
  matplot(y, pch=16, col=c("blue", "red"), add=TRUE)
}

  




# Execute backtest univariate
{
df <- xx %>% tail(307)  
y <- df$GasolineSpread %>% na.omit
x1 <- tail(df$Gasoline, length(y))  
x2 <- tail(df$Brent, length(y))
d <- tail(df$Date, length(y))  
fit <- fit_
par(mfrow=c(3,1), mar=c(2,2,1,0.5))
# discrete positions
m_pred <-  fit$draws(variables = "y_hat")  %>% merge_chains %>% colMeans() %>% as.vector(); m_pred <- head(m_pred, length(y))
S <-  fit$draws(variables = "S")  %>% merge_chains %>% colMeans() %>% as.vector; S <- sqrt(head(S, length(y)))
df <- data.frame(Date=d, y=y, m=lead(m_pred))
df$trade <- (with(df, ifelse(m>y, 1, ifelse(m<y, -1, 0)))) * 1 / S^2;
# continuons positions
# m_pred <-  fit$draws(variables = "m_pred")  %>% merge_chains %>% matrix(nrow=2000); m_pred <- head(m_pred, length(y))
# p <- (lead(m_pred) - y) %>% apply(., 2, function(x) table(x>0)/length(x))
# p <- p[1,] - p[2,]
# df <- data.frame(Date=d, y=y, trade = p )
df$ret1 <- c(0, diff(log(x1)))
df$ret2 <-  c(0, diff(log(x2)))
df$pnl <- 0.5 * lag(df$trade) * (df$ret1 - df$ret2)
df$pnl[is.na(df$pnl)] <- 0
plot(cumsum(df$pnl))
colors <- df$pnl; colors[colors>0] <- "blue"; colors[colors<0] <- "red"
plot(df$Date, df$y %>% scale, pch=16, type="o", col=colors, cex=2); lines(df$Date, df$y %>% scale); abline(v=seq(df$Date[1], df$Date[nrow(df)] , by=365), lty=2)
print(mean(df$pnl) / sd(df$pnl) * sqrt(12))
plot.ts(SMA(df$pnl^2,3))
get_returns_statistics(data.frame(df$pnl), period = 12) %>% unlist
}


# Execute backtest multivariate
{
  # data
  df <- Oil_extended2  
  y <- cbind(df$OilSpread, df$GasolineSpread, df$HoSpread, df$DieselSpread, df$GasolineDieselSpread, df$GasolineHoSpread, df$HoDieselSpread) %>% na.omit
  y <- cbind(df$OilSpread, df$GasolineSpread, df$HoSpread, df$GasolineDieselSpread,  df$HoDieselSpread) %>% na.omit
  y <- cbind(df$OilSpread, df$GasolineSpread, df$HoSpread, df$HoDieselSpread) %>% na.omit
  #y <- cbind(df$GasolineSpread, df$HoSpread) %>% na.omit
  x1 <- cbind(
             df$WTI,
             df$Gasoline, 
             df$HeatingOil,
             #df$HeatingOil,
             #df$Gasoline, 
             #df$Gasoline,
             df$HeatingOil
            ) %>% tail(nrow(y))    
  x2 <- cbind(
              df$Brent, 
              df$Brent,
              df$Brent,
              #df$Brent,
              #df$DieselFuel,
              #df$HeatingOil,
              df$DieselFuel
            ) %>% tail(nrow(y))    
  d <- tail(df$Date, nrow(y))  
  fit <- fit_four_variational_garch_c__
  # go
  par(mfrow=c(3,1), mar=c(2,2,1,0.5))
  m_pred <-  fit$draws(variables = "y_hat")  %>% merge_chains %>% colMeans()  %>% unlist(); 
  S <-  fit$draws(variables = "S")  %>% merge_chains %>% colMeans() %>% sqrt  %>% unlist(); 
  m_preds <- list(); 
  Ss <- list()
  sds <- list()
  trades <- list()
  returns1 <- list()
  returns2 <- list()
  pnls <- list()
  for(i in 1:(ncol(y))) {
    index <- as.character(i)
    returns1[[i]] <- c(0, diff(log(x1[,i])))
    returns2[[i]] <- c(0, diff(log(x2[,i])))
    sds[[i]] <- sqrt(SMA((returns1[[i]] - returns2[[i]])^2, 3))
    m_preds[[i]] <-  lead(m_pred[ ((i-1)*nrow(y)+1) : (i*nrow(y)) ]) # leading because m_pred[t] is the prediction we make at time t-1
    Ss[[i]] <-  S[grep(paste0(",", index, ",", index), names(S))] 
    trades[[i]] <- ifelse(m_preds[[i]] > y[,i], 1, ifelse(m_preds[[i]] < y[,i], -1, 0)) / sds[[i]] / 100
    pnls[[i]] <- (1./ncol(y)) * lag(trades[[i]]) * (returns1[[i]] - returns2[[i]])
    pnls[[i]][is.na(pnls[[i]])] <- 0
  }
  portfolio <- do.call(cbind, pnls)
  get_returns_statistics(portfolio, period = 12, plot=TRUE) %>% unlist
}




# Execute backtest for arima/ets data
{
  #far <- function(x, h){forecast(Arima(x, order=c(3,1,4)), h=h)}
  #e <- tsCV(y, far, h=1)
  par(mfrow=c(3,1), mar=c(2,2,1,0.5))
  df <- data.frame(Date=d, y=y, m=y+e)
  df$trade <- (with(df, ifelse(m>y, 1, ifelse(m<y, -1, 0))));
  df$ret1 <- c(0, diff(log(tail(Oil$Gasoline, length(y)))))
  df$ret2 <-  c(0, diff(log(tail(Oil$Brent, length(y)))))
  df$pnl <- 0.5 * stats::lag(df$trade) * (df$ret1 - df$ret2)
  df$pnl[is.na(df$pnl)] <- 0
  plot(cumsum(df$pnl))
  colors <- df$pnl; colors[colors>0] <- "blue"; colors[colors<0] <- "red"
  plot(df$Date, df$y %>% scale, pch=16, type="o", col=colors, cex=2); lines(df$Date, df$y %>% scale); abline(v=seq(df$Date[1], df$Date[nrow(df)] , by=365), lty=2)
  print(mean(df$pnl) / sd(df$pnl) * sqrt(12))
  plot.ts(SMA(df$pnl^2,3))
}



# Full series CV
{
m_pred <- rep(NA, 100)
P_pred <- rep(NA, 100)
for(i in 100:(nrow(Oil)-1)) {
  h=1; y<-Oil$GasolineSpread[1:i]; m<-Oil$Month[1:(i+1)]; 
  fit <- m3.0$sample(data=list(N=length(y), y=y, m0=y[1], P0=1, month=m, period=12, dft=2, J=ncol(X), X=X, h=h), chains  =1, iter_warmup = 250, iter_sampling = 250)
  m_pred <- c(m_pred, fit$draws(variables = "m_pred")  %>% merge_chains %>% colMeans() %>% t %>% tail(1) %>% as.numeric())
  P_pred <- c(P_pred, fit$draws(variables = "P_pred")  %>% merge_chains %>% colMeans() %>% t %>% tail(1) %>% as.numeric())
}
}


# Random stuff
y <- Oil_extended$GasolineSpread %>% tail(307)
X0 <- array(0, dim=c(length(y), 0))
bb <- apply(Oil_extended2[,-1] %>% tail(length(y)), 2, function(x) {y <- c(0, diff(x)); y[is.na(y)] <- 0; (y-runMean(y, cumulative = TRUE))/runSD(y, cumulative = TRUE)  }); bb[is.na(bb)] <- 0
X2 <- tail(bb, 307)

fit_m3.0 <- m3.0$sample(data=list(N=length(y), y=y, m0=y[1], P0=sd(diff(y)), month=1:length(y), period=12, dft=2, 
                              J=ncol(X0), X=X0, h=0, level=1, ma=2,garch=1, sigma1=1, trainset=length(y)),     
                    parallel_chains = 4, iter_warmup = 500, iter_sampling = 500)
fit2_ <- m3.2$sample(data=list(N=length(y), y=y, m0=c(y[1],0), P0=diag(c(1,1)), month=1:length(y), period=12, dft=2, J=ncol(X), X=X, h=h,sigma1=1),     parallel_chains = 4, iter_warmup = 250, iter_sampling = 250)

fit_ <- m3.5$sample(data=list(N=length(y), y=y, m0=c(y[1], 0) , P0=diag(c(1,1)) , month=1:length(y), period=12, dft=2, h=0),      
                    chains  =1, iter_warmup = 500, iter_sampling = 500)


aa <- apply(outlook[,-1] %>% tail(324) %>% head(307), 2, function(x) {y <- c(0, diff(x)); y[is.na(y)] <- 0; (y-runMean(y, cumulative = TRUE))/runSD(y, cumulative = TRUE)  }); aa[is.na(aa)] <- 0
pca <- prcomp(aa)
X1 <- (pca$x[,1:30])

stanfit <- rstan::read_stan_csv(fit_ouch$output_files());plot(stanfit,pars=names(stanfit)[1:25])


