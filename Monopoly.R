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

# create a backadjusted future contract
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
  rollover <- rep(FALSE, nrow(m))
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
    nearest[i] <-  m[i,j] 
    # rollback contract if:
    # - we have not reached the last contract yet
    # - we have reached at least the number of days from the end of current contract 
    # - there is price on the next contract
    # - it is not the last day
    if(j < ncol(m) && i >= ec[j]-N+1 && !(is.na(m[i,j+1]) | is.nan(m[i,j+1])) && i < nrow(m)-1  ) {
      j <- j + 1
      adjs[i] <- adjs[i] + jump # update the adjustment now that we rollback the contract
      rollover[i] <- TRUE
    } 
    if(j == ncol(m)+1)
      break
    i <- i + 1
  }
  adjs <- c(diff(adjs) %>% rev %>% cumsum %>% rev,0)
  continous <- nearest + adjs
  ret <- c(0, diff(continous))
  return(data.frame(Date=df[,1], Nearest=nearest, Backadjusted=continous, Return=ret, Adjs=adjs, Rollover=rollover))
}

# create a backadjusted future contract, another way
backadjust_future2 <- function(df, N=1, log=FALSE) {
  m <- as.matrix(df[,-1]) # the first column is supposed to be the Date
  if(log)
    m <- log(m)
  sc <- rep(NA, ncol(m))
  ec <- rep(NA, ncol(m))
  for(j in 1:ncol(m)) {
    a <- which(is.nan(m[,j]))
    sc[j] <- a[1]+1
    ec[j] <- a[2]-1
  }
  j <- 1; 
  i <- 1; 
  continous <- rep(NA, nrow(m)); 
  nearest <- rep(NA, nrow(m)); 
  rollover <- rep(FALSE, nrow(m)); 
  ret <- rep(NA, nrow(m)); 
  first <- NA
  last <- 0
  for(i in 2:nrow(m)) { 
    if(log)
      ret[i] <- m[i,j] - m[i-1,j]  
    else
      ret[i] <- log(m[i,j] / m[i-1,j])
    if(j < ncol(m) && i >= ec[j]-N+2) {
      j <- j + 1;
      rollover[i] <- TRUE
      if(log)
        ret[i] <- m[i,j] - m[i-1,j]
      else
        ret[i] <- log(m[i,j] / m[i-1,j])
    }
    nearest[i] <- m[i,j]
    continous[i] <- m[i,j] - m[i-1,j]
    if(!(is.na(m[i,j]) | is.nan(m[i,j]))) {
      last <- m[i,j]
      if(is.na(first))
        first <- m[i,j]
    }  
    
  } 
  continous[is.na(continous)] <- 0
  continous <- first + cumsum(continous)
  continous <- continous + (last - continous[length(continous)])
  return(data.frame(Date=df[,1], Nearest=nearest, Backadjusted=continous, Return=ret, Adjs=continous-nearest, Rollover=rollover))
}


# create a backadjusted future spread contract
backadjust_spread <- function(df1, df2, N=c(1,1), mult=c(1,1), func=backadjust_future2, log=FALSE) {
  c1 <- func(df1, N[1], log=log)
  c2 <- func(df2, N[2], log=log)
  z <- merge(c1, c2, by="Date", all=TRUE)
  z$Adjs.x <- zoo::na.locf.default(z$Adjs.x, na.rm = FALSE, fromLast = TRUE)
  z$Adjs.y <- zoo::na.locf.default(z$Adjs.y, na.rm = FALSE, fromLast = TRUE)
  if(log){
    nearest <- z$Nearest.x+log(mult[1]) - z$Nearest.y+log(mult[2])
    backadj <- nearest - (z$Adjs.x+log(mult[1]) - z$Adjs.y+log(mult[2]))
  }
  else {
    nearest <- z$Nearest.x*mult[1] - z$Nearest.y*mult[2]
    backadj <- nearest - (z$Adjs.x*mult[1] - z$Adjs.y*mult[2])
  }

  return(data.frame(Date=z[,1], Near1=z$Nearest.x, Near2=z$Nearest.y, 
                                Backadj1=z$Backadjusted.x, Backadj2=z$Backadjusted.y, 
                    Rollover1=z$Rollover.x, Rollover2=z$Rollover.y, 
                                Return1=z$Return.x, Return2=z$Return.y,
                                Nearest=nearest, Backadjusted=backadj))
}

# Load futures data and calculate rollover curves
{
  to_load <- list(c("CL", "/home/marco/trading/Historical Data/Barchart/CrudeOil/"),
                  c("CB", "/home/marco/trading/Historical Data/Barchart/Brent//"),
                  c("RB", "/home/marco/trading/Historical Data/Barchart/Gasoline/"),
                  c("HO", "/home/marco/trading/Historical Data/Barchart/ULSDNYHArbor/"),
                  c("LF", "/home/marco/trading/Historical Data/Barchart/Gasoil/"))
  # load the full futures contracts
  Futures <- list()
  for(a in to_load) 
    Futures[[a[1]]] <- load_future_contracts(a[1], a[2])
  # calculate the rollover curves
  Curves <- list()
  for(a in  names(Futures)) {
    b <- as.data.frame(rollover_curve(as.matrix(Futures[[a]][,-1])))
    colnames(b) <- c(paste0(a[1], "_lm"), paste0(a[1], "_diff"))
    Curves[[a]] <- data.frame(Futures[[a]][,1], b)
  }
  # Backadjusted spread
  cl_cb <- backadjust_spread(Futures[["CL"]], Futures[["CB"]], N=c(10, 10), mult=c(1, 1))
  rb_cb <- backadjust_spread(Futures[["RB"]], Futures[["CB"]], N=c(10, 10), mult=c(42, 1))
  ho_cb <- backadjust_spread(Futures[["HO"]], Futures[["CB"]], N=c(10, 10), mult=c(42, 1))
  lf_cb <- backadjust_spread(Futures[["LF"]], Futures[["CB"]], N=c(10, 10), mult=c(1/7.45, 1))
  rb_ho <- backadjust_spread(Futures[["RB"]], Futures[["HO"]], N=c(10, 10), mult=c(1, 1)*10)
  rb_lf <- backadjust_spread(Futures[["RB"]], Futures[["LF"]], N=c(10, 10), mult=c(1, 1/312.9)*10)
  ho_lf <- backadjust_spread(Futures[["HO"]], Futures[["LF"]], N=c(10, 10), mult=c(1, 1/312.9)*10)
  Spreads <- list(cl_cb, rb_cb, ho_cb, lf_cb, rb_ho, rb_lf, ho_lf)
  names(Spreads) <-  c("cl_cb", "rb_cb", "ho_cb", "lf_cb", "rb_ho", "rb_lf", "ho_lf")
  backadjs <- Reduce(function(...) full_join(..., by="Date", all=T), list(cl_cb, rb_cb, ho_cb, lf_cb, rb_ho, rb_lf, ho_lf)) %>% arrange(Date)
  backadjs <- select(backadjs, grep("Date|Backadjusted", colnames(backadjs)))
  colnames(backadjs) <- c("Date", "cl_cb", "rb_cb", "ho_cb", "lf_cb", "rb_ho", "rb_lf", "ho_lf")
  backadjs_monthly <- mutate(backadjs %>% na.omit, m=yearmonth(Date)) %>% group_by(m) %>% summarise(across(everything(),last)) %>% select(-m) %>% tail(307)

  
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
  N <- 300
  sigma_y <- 0.1
  sigma_x <- 3
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
  x[1:3] <- sigma_x[1]
  y <- vector()
  y[1:3] <- sigma_x[1] + sigma_y[1]
  for(t in 3:N) {
    x[t] <- x[t-1] + cc[t,] %*% CC  * 0 + err_x[t] + X[t,] %*% B
    y[t] <- x[t] + err_y[t]
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
fit <- fit_m3.0_rb_cb
daily_df <- rb_cb
trading_df <- monthly_df_rb_cb 
y <- trading_df$Backadjusted 
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
# plot(df$Date,cumsum(df$pnl))
# colors <- df$pnl; colors[colors>0] <- "blue"; colors[colors<0] <- "red"
# plot(df$Date, df$y, pch=16, type="o", col=colors, cex=2);#lines(df$Date, df$y); #abline(v=seq(tf$Date[1], tf$Date[nrow(tf)] , by=365), lty=2)
# plot.ts(SMA(df$pnl^2,3))
get_returns_statistics(data.frame(df$pnl), period = period, plot=T) %>% unlist
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
bb <- apply(Oil_extended2[,-1], 2, function(x) {y <- c(0, diff(x)); y[is.na(y)] <- 0; (y-runMean(y, cumulative = TRUE))/runSD(y, cumulative = TRUE)  }); bb[is.na(bb)] <- 0
X2 <- tail(bb, 307)

fit_m3.0 <- m3.0$sample(data=list(N=length(y), y=y, m0=y[1], P0=sd(diff(y)), month=1:length(y), period=12, dft=2, J=ncol(X0), X=X0, h=0, level=1, ma=2,garch=1, sigma1=1, trainset=length(y)),     parallel_chains = 4, iter_warmup = 500, iter_sampling = 500)
fit2_ <- m3.2$sample(data=list(N=length(y), y=y, m0=c(y[1],0), P0=diag(c(1,1)), month=1:length(y), period=12, dft=2, J=ncol(X), X=X, h=h,sigma1=1),     parallel_chains = 4, iter_warmup = 250, iter_sampling = 250)

fit_ <- m3.5$sample(data=list(N=length(y), y=y, m0=c(y[1], 0) , P0=diag(c(1,1)) , month=1:length(y), period=12, dft=2, h=0),      
                    chains  =1, iter_warmup = 500, iter_sampling = 500)


aa <- apply(outlook[,-1] %>% tail(324) %>% head(307), 2, function(x) {y <- c(0, diff(x)); y[is.na(y)] <- 0; (y-runMean(y, cumulative = TRUE))/runSD(y, cumulative = TRUE)  }); aa[is.na(aa)] <- 0
pca <- prcomp(aa)
X1 <- (pca$x[,1:30])

stanfit <- rstan::read_stan_csv(fit_ouch$output_files());plot(stanfit,pars=names(stanfit)[1:25])


