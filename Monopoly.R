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
# Download from https://www.eia.gov/outlooks/steo/data/browser/#/?v=6&f=M&s=vvvo5vvvv7m0477vo0gvvs83vq3gvv7rvvvvvvvvo00000000001g1fvvo30v0vo3vhu03vs4nvus07rg0vvpo024k20vvsvvnffvvvu0vo000rvv1vvjvufs103vvvvvvvvfuvvvv4vvvvtvvs00000000000000000000000000000000000000000000000000000000000000000000000000003vvvvvvvvmvvtvvvvvvsudnvjvvrvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv&start=199701&end=202212&id=&ctype=linechart&maptype=0&linechart=PAPR_OECD
# and transpose into ShortTermOutlook.csv
{
outlook <- read_csv("/home/marco/trading/Systems/Monopoly/Data/Oil/ShortTermOutlook.csv" ,show_col_types = FALSE, name_repair = FALSE)
outlook$Date <- paste("01", outlook$Date)
outlook$Date <- as.Date(outlook$Date, format="%d %b %Y")
cols <- c(
            "WTIPUUS", 
            "BREPUUS",
            "MGWHUUS",
            "D2WHUUS",
            "DSWHUUS",
            "JKTCUUS",
            "NGHHMCF",
            "PAPR_WORLD",
            "PAPR_OPEC",
            "PAPR_NONOPEC",
            "PATC_WORLD",
            "COPS_OPEC",
            "PADI_OPEC",
            "MGTCPUSX",
            "DFTCPUS",
            "PATCPUSX",
            "ORUTCUS",
            "MGPSPUS",
            "MGNIPUS",
            "PASC_US",
            "ELCOTWH",
            "WP57IUS",
            "SOTCBUS",
            "DFROPUS",
            "MGROPUS"
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
               "Distillate_Fuel_Oil_Supplied",
               "Total_Products_Supplied",
               "Refinery_Utilization_Factor",  
               "Motor_Gasoline_US_Inventory",
               "Motor_Gasoline_Net_Imports",
               "US_inventory",
               "Electricity_Consumption_US",
               "PPI_Petroleum",
               "Consumption_of_Solar_Energy" ,
               "Distillate_Fuel_Output",
               "Motor_Gasoline_Output"
               
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
## Economic indicators data
# LI <- read_csv("/home/marco/trading/Systems/Monopoly/Data/OECD//LeadingIndicators.csv", show_col_types = FALSE)
# CPI <- read_csv("/home/marco/trading/Systems/Monopoly/Data/OECD//CPI.csv", show_col_types = FALSE)
# GDP <- read_csv("/home/marco/trading/Systems/Monopoly/Data/OECD//GDP.csv", show_col_types = FALSE)
# GDP$TIME <- sub("Q1", "03", GDP$TIME) %>% sub("Q2", "06", .) %>% sub("Q3", "09", .) %>% sub("Q4", "12", .) 
# MA <- read_csv("/home/marco/trading/Systems/Monopoly/Data/OECD//MonetaryAggregates.csv", show_col_types = FALSE)
# PPI <- read_csv("/home/marco/trading/Systems/Monopoly/Data/OECD//PPI.csv", show_col_types = FALSE)
# TG <- read_csv("/home/marco/trading/Systems/Monopoly/Data/OECD//TradeInGoods.csv", show_col_types = FALSE)
# UR <- read_csv("/home/marco/trading/Systems/Monopoly/Data/OECD//UnemploymentRate.csv", show_col_types = FALSE)
# df <- rbind(LI, CPI,  PPI, TG, UR, MA)
# df <- filter(df, LOCATION=="USA") %>% select(INDICATOR,TIME,Value) %>% mutate(TIME=as.Date(paste0(TIME, "-01"), format="%Y-%m-%d")) %>% rename(Date=TIME) %>% dcast(Date ~ INDICATOR, value.var = "Value") %>% arrange(Date)
# Oil <- full_join(oil, df, by="Date")%>% arrange(Date) %>% mutate(Date = yearmonth(as.character(Date)))
Oil <- oil
## COT report data
# cot <- read_csv("/home/marco/trading/Systems/Monopoly/Data/Oil/COT_gasoline.csv", show_col_types = FALSE)
# cot <- mutate(cot, CommercialsNET = `Commercial Positions-Long (All)` /  (`Commercial Positions-Long (All)` + `Commercial Positions-Short (All)`), 
#                    NonCommercialsNET = `Noncommercial Positions-Long (All)` /  (`Noncommercial Positions-Long (All)` + `Noncommercial Positions-Short (All)`)) %>% 
#                    rename(Date = `As of Date in Form YYYY-MM-DD`) %>% select(Date, CommercialsNET, NonCommercialsNET) %>% 
#                    mutate(Date = yearmonth(as.character(Date)))
# cot <- group_by(cot, Date) %>% summarise(Date=last(Date), COT_Gasoline_Commercials=last(CommercialsNET),
#                                          COT_Gasoline_NonCommercials=last(NonCommercialsNET))
# Oil <- full_join(Oil, cot, by="Date")%>% arrange(Date)
# cot <- read_csv("/home/marco/trading/Systems/Monopoly/Data/Oil/COT_heatingoil.csv", show_col_types = FALSE)
# cot <- mutate(cot, CommercialsNET = `Commercial Positions-Long (All)` /  (`Commercial Positions-Long (All)` + `Commercial Positions-Short (All)`), NonCommercialsNET = `Noncommercial Positions-Long (All)` /  (`Noncommercial Positions-Long (All)` + `Noncommercial Positions-Short (All)`)) %>% rename(Date = `As of Date in Form YYYY-MM-DD`) %>% select(Date, CommercialsNET, NonCommercialsNET)%>% mutate(Date = yearmonth(as.character(Date)))
# cot <- group_by(cot, Date) %>% summarise(Date=last(Date), COT_HeatingOil_Commercials=last(CommercialsNET),
#                                          COT_HeatingOil_NonCommercials=last(NonCommercialsNET))
# Oil <- full_join(Oil, cot, by="Date")%>% arrange(Date)
# cot <- read_csv("/home/marco/trading/Systems/Monopoly/Data/Oil/COT_crudeoil.csv", show_col_types = FALSE)
# cot <- mutate(cot, CommercialsNET = `Commercial Positions-Long (All)` /  (`Commercial Positions-Long (All)` + `Commercial Positions-Short (All)`), 
#                    NonCommercialsNET = `Noncommercial Positions-Long (All)` /  (`Noncommercial Positions-Long (All)` + `Noncommercial Positions-Short (All)`)) %>% rename(Date = `As of Date in Form YYYY-MM-DD`) %>% select(Date, CommercialsNET, NonCommercialsNET)%>% mutate(Date = yearmonth(as.character(Date)))
# cot <- group_by(cot, Date) %>% summarise(Date=last(Date), COT_CrudeOil_Commercials=last(CommercialsNET),
#                                                            COT_CrudeOil_NonCommercials=last(NonCommercialsNET))
# Oil <- full_join(Oil, cot, by="Date")%>% arrange(Date) %>% mutate(YearMonth=yearmonth(Date))
}




# Spread random stuff  
{  
  # Backadjusted spread, these are prediction targets!
  cl_cb <- backadjust_spread(Futures[["CL"]], Futures[["CB"]], 10, 10, mult=c(1, 1))
  rb_cb <- backadjust_spread(Futures[["RB"]], Futures[["CB"]], 10, 10, mult=c(42, 1))
  ho_cb <- backadjust_spread(Futures[["HO"]], Futures[["CB"]], 10, 10, mult=c(42, 1))
  lo_cb <- backadjust_spread(Futures[["LO"]], Futures[["CB"]], 10, 10, mult=c(42, 1))
  lf_cb <- backadjust_spread(Futures[["LF"]], Futures[["CB"]], 10, 10, mult=c(1, 7.45))
  rb_ho <- backadjust_spread(Futures[["RB"]], Futures[["HO"]], 10, 10, mult=c(1, 1))
  rb_lf <- backadjust_spread(Futures[["RB"]], Futures[["LF"]], 10, 10, mult=c(312.9, 1))
  ho_lf <- backadjust_spread(Futures[["HO"]], Futures[["LF"]], 10, 10, mult=c(312.9, 1))
  lo_lf <- backadjust_spread(Futures[["LO"]], Futures[["LF"]], 2, 2, mult=c(312.9, 1))
  lo_ho <- backadjust_spread(Futures[["LO"]], Futures[["HO"]], 10, 10, mult=c(1, 1))
  Spreads <- list(cl_cb, rb_cb, ho_cb, lo_cb, lf_cb, rb_ho, rb_lf, ho_lf, lo_lf, lo_ho)
  names(Spreads) <-  c("cl_cb", "rb_cb", "ho_cb", "lo_cb", "lf_cb", "rb_ho", "rb_lf", "ho_lf", "lo_lf", "lo_ho")
  backadjs <- Reduce(function(...) full_join(..., by="Date", all=T), Spreads) %>% arrange(Date)
  backadjs <- select(backadjs, grep("Date|Backadjusted", colnames(backadjs)))
  colnames(backadjs) <- c("Date", names(Spreads))
  backadjs_monthly <- mutate(backadjs %>% na.omit, YearMonth=yearmonth(Date)) %>% group_by(YearMonth) %>% summarise(across(everything(),last)) 

  Oil_futures <- 
    merge(Oil, Curves[["CL"]] %>% mutate(Date=YearMonth) %>% select(-YearMonth) %>%  group_by(Date) %>% 
            summarise(across(everything(), last)), by="Date") %>% 
    merge(., Curves[["RB"]] %>% mutate(Date=YearMonth)%>% select(-YearMonth)%>%  group_by(Date) %>% 
            summarise(across(everything(), last)), by="Date") %>% 
    merge(., Curves[["HO"]] %>% mutate(Date=YearMonth)%>% select(-YearMonth)%>% group_by(Date) %>% 
            summarise(across(everything(), last)), by="Date") %>% 
    merge(., Curves[["LF"]] %>% mutate(Date=YearMonth)%>% select(-YearMonth) %>% group_by(Date) %>% 
            summarise(across(everything(), last)),by="Date") %>% 
    merge(., backadjs_monthly %>% mutate(Date=YearMonth)%>% select(-YearMonth) %>% group_by(Date) %>% 
            summarise(across(everything(), last)),by="Date")
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
N <- 2000
hl <- 20
ar_v <- exp(-log(2) / hl)
err_v <-  rnorm(N, 0, sigma_v)
err_x <-  rnorm(N, 0, sigma_x)
err_y <-  rnorm(N, 0, sigma_y)
sigma_x <- 1
sigma_y <- 0.2
sigma_v <- 0.05   # 0 to remove velocity
H <- matrix(c(1,0), ncol=2, byrow=T)
x <- matrix(NA, nrow=N, ncol=2)
x[1,] <- c(0, 0) # the 2nd process starting level determines the initial amplitude
y <- vector()
y[1] <- 0
for(t in 2:N) {
  Fx <-  matrix(c(1, 1, 0, ar_v), ncol=2, byrow=T)
  x[t,] <- Fx %*% x[t-1, ] + c(rnorm(1, 0, sigma_x),rnorm(1, 0, sigma_v)  )  
  y[t] <- H %*% x[t,]  + rnorm(1, 0, sigma_y)
}
plot(x[,1], type="l", col="red"); grid()
points(y, pch=16, col="blue")
if(sigma_v > 0)
  plot(x[,2], type="l", col="black"); abline(h=0); grid()
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
{
  par(mfrow=c(2,1), mar=c(2,2,1,0.5))
  N <- 307
  sigma_y <- 0.1
  sigma_x <- 0.25
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
  err_x <- rnorm(N, 0, sigma_x)
  err_y <- rnorm(N, 0, sigma_y)
  x <- vector()
  x[1:3] <- sigma_x[1] + 70
  y <- vector()
  y[1:3] <- x[1:3] + sigma_y[1]
  for(t in 3:N) {
    x[t] <- x[t-1] + cc[t,] %*% CC*0 + err_x[t] + X[t,] %*% B
    y[t] <- x[t] + 0.1 * y[t-1]  + err_y[t]
  }
  plot(x, col="red", type="l");  points(y, col="blue", pch=16);
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

# m3.5 sseasonality as process, not covariate
{
  par(mfrow=c(3,1), mar=c(2,2,1,0.5))
  N <- 307
  sigma_y <- 0.1
  sigma_x <- 1
  sigma_z <- 1
  CC <- c(-1, 1)*5
  cc <- matrix(0, ncol=length(CC), nrow=N)
  # generates fourier coefficients 
  for(t in 3:N) {
    for(i in seq(1, ncol(cc), 2))
      cc[t,i:(i+1)] <- c(cos(2 * pi * i * t / 12), sin(2 * pi * i * t / 12))
  }
  H <- matrix(c(1,1), ncol=2, byrow=T)
  x <- matrix(NA, nrow=N, ncol=2)
  x[1:3,] <- c(0, 0) 
  y <- vector()
  y[1:3] <- x[1]
  for(t in 4:N) {
    Fx <-  matrix(c(1, 0, 0, 0), ncol=2, byrow=T)
    x[t,] <- Fx %*% x[t-1, ] + c(0, -sum(x[(t-3):(t-1), 2])) + c(rnorm(1, 0, sigma_x),rnorm(1, 0, sigma_z)  )  
    y[t] <- H %*% x[t,]  + rnorm(1, 0, sigma_y)
  }
  plot(x[,1], col="red", type="l"); 
  plot(x[,2], col="green", pch=16, type="l");
  plot(y, col="blue", pch=16, type="o"); 
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

  
# m5.1 simple univariate seasonal model on returns
{
  par(mfrow=c(2,1), mar=c(2,2,1,0.5))
  N <- 200
  ar <- 0.7
  theta <- 0.9
  sigma_y <- 0.2
  sigma_x <- 2
  CC <- c(-1.37, 1.79, 0.46, 0.53)
  cc <- matrix(0, ncol=length(CC), nrow=N)
  # generates fourier coefficients
  for(t in 3:N) {
    for(i in seq(1, ncol(cc), 2))
      cc[t,i:(i+1)] <- c(cos(2 * pi * i * t / 12), sin(2 * pi * i * t / 12))
  }
  x <- vector()
  x[1] <- err_x[1]
  y <- vector()
  y[1] <- x[1] +  err_y[1]
  err_x <- rnorm(N, 0, sigma_x) 
  err_y <- rnorm(N, 0, sigma_y) 
  for(t in 2:N) {
    x[t] <- ar * x[t-1] + theta * err_y[t-1] + cc[t,] %*% CC * 0 + err_x[t]
    y[t] <- x[t]  + err_y[t]
  }
  plot(x, col="red", type="l"); points(y, col="blue", pch=16);
}




# Execute backtest univariate
{
period <- 252
on_returns <- FALSE  
fit <- fit_m3.0_
daily_df <- rb_cb
trading_df <- monthly_df_rb_cb ## this is the actual data
y <- trading_df$Backadjusted;  y[307] <- 50
d <- trading_df$Date
if(on_returns)
  y <- c(0,diff(y))

par(mfrow=c(3,1), mar=c(2,2,1,0.5))
# discrete positions
y_hat <-  fit$draws(variables = "y_hat")  %>% merge_chains %>% colMeans() %>% as.vector(); 

S <-  1#fit$draws(variables = "S")  %>% merge_chains %>% colMeans() %>% as.vector; 
df <- data.frame(Date=d, y=y, pred=lead(y_hat))
if(on_returns) {
  df$trade <- (with(df, ifelse(pred>0, 1, ifelse(pred<0, -1, 0)))) * 1 / S;  
} else {
  df$trade <- (with(df, ifelse(pred>y, 1, ifelse(pred<y, -1, 0)))) * 1 / S;
}
# continuons positions
# y_hat <-  fit$draws(variables = "y_hat")  %>% merge_chains %>% matrix(nrow=2000);
# if(!on_returns) {
#   p <- (lead(y_hat) - y) %>% apply(., 2, function(x) table(x>0)/length(x))
# } else {
#   p <- -lead(y_hat) %>% apply(., 2, function(x) table(x>0)/length(x))
# }
# p <- p[1,] - p[2,]
# df <- data.frame(Date=d, y=y, trade = p )
df <- merge(df, daily_df, by="Date", all=T)
df$trade <- na.locf(df$trade, na.rm = FALSE)
df$pnl <- 0.5 * lag(df$trade) * (df$Return1 - df$Return2)
df$pnl[is.na(df$pnl)] <- 0
df <- df[!is.na(df$trade),]
# plot(df$Date,cumsum(df$pnl))
# colors <- df$pnl; colors[colors>0] <- "blue"; colors[colors<0] <- "red"
# plot(df$Date, df$y, pch=16, type="o", col=colors, cex=2);#lines(df$Date, df$y); #abline(v=seq(tf$Date[1], tf$Date[nrow(tf)] , by=365), lty=2)
# plot.ts(SMA(df$pnl^2,3))
get_returns_statistics(data.frame(df$pnl), dates = df$Date,period = period, plot=T) %>% unlist
}


# Execute backtest multivariate
{
  # data
  elems <- c("rb_cb", "ho_cb")
  trading_df <- backadjs_monthly[,c("Date", elems)]
  y <- trading_df[,elems]
  d <- trading_df$Date
  fit <- fit_rb_cb_rb_ho
  # go
  y_hat <-  fit$draws(variables = "y_hat")  %>% merge_chains %>% colMeans()  %>% as.vector(); 
  S <-  fit$draws(variables = "S")  %>% merge_chains %>% colMeans() %>% sqrt  %>% unlist(); 
  dfs <- list()
  pnls <- list()
  for(i in 1:length(elems)) {
    spread <- Spreads[[elems[i]]]
    pred <-  y_hat[ ((i-1)*nrow(y)+1) : (i*nrow(y)) ] # leading because m_pred[t] is the prediction we make at time t-1
    df <- data.frame(Date=d, y=y, pred=lead(pred))
    df$trade <- as.vector(with(df, ifelse(pred>y[,i], 1, ifelse(pred<y[,i], -1, 0)))) ;
    df <- merge(df, spread, by="Date", all=T)
    df$trade <- na.locf(df$trade, na.rm = FALSE)
    df$pnl <- lag(df$trade) * (df$Return1 - df$Return2)
    df$pnl[is.na(df$pnl)] <- 0
    dfs[[i]] <- df
  }
  portfolio <- Reduce(function(...) full_join(..., by="Date", all=T), dfs) %>% arrange(Date) %>% select(contains("pnl"))
  portfolio[is.na(portfolio)] <- 0
  get_returns_statistics(portfolio * 1/ncol(portfolio), period = 252, plot=TRUE) %>% unlist
}




# Execute backtest for arima/ets data
{
  far <- function(x, h){forecast(Arima(x, order=c(2,0,2)), h=h)}
  e <- tsCV(y, far, h=1)
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
y <- Oil_futures$rb_cb %>% tail(307)
X0 <- array(0, dim=c(length(y), 0))
bb <- apply(Oil_futures[,-1], 2, function(x) {y <- c(0, diff(x)); y[is.na(y)] <- 0; (y-runMean(y, cumulative = TRUE))/runSD(y, cumulative = TRUE)  }); bb[is.na(bb)] <- 0
X2 <- tail(bb, 307)

fit_m3.0 <- m3.0$sample(data=list(N=length(y), y=y, m0=y[1], P0=sd(diff(y)), month=1:length(y), period=12, dft=2, 
                                  Jx=ncol(X0), Xx=X0,  Jy=ncol(X0), Xy=X0,  Jl=ncol(X0), Xl=X0, h=0, level=0, ma=1, garch=1, sigma1=1, trainset=length(y)),     
                        parallel_chains = 4, iter_warmup = 250, iter_sampling = 250)
fit2_ <- m3.2$sample(data=list(N=length(y), y=y, m0=c(y[1],0), P0=diag(c(1,1)), month=1:length(y), period=12, dft=2, J=ncol(X), X=X, h=h,sigma1=1),     parallel_chains = 4, iter_warmup = 250, iter_sampling = 250)

fit_ <- m3.5$sample(data=list(N=length(y), y=y, m0=c(y[1], 0) , P0=diag(c(1,1)) , month=1:length(y), period=12, dft=2, h=0),      
                    chains  =1, iter_warmup = 500, iter_sampling = 500)



