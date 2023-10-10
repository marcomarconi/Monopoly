{
    library(ggthemes)

}

### Trading rules:
# Ultra 10y (1.0): Short first 5 days of the month, Long last 5 days of the month
# Brent (1.1): Short on monday and tuesday in contango, Long rest of the week in backwardation
# Lumber (1.3): Short on monday and tuesday when EMA1 < EMA4, Long rest of the week when EMA1 > EMA4
# Live cattle (1.1): Long last 2 weeks of the month when EMA1 > EMA4
# Lean hogs (1.1): Short on monday in contango when EMA1 < EMA4, Long rest of the week in backwardation when EMA1 > EMA4
# Natural Gas (0.7): Short first, third and fourth week of the month when contango, long the second when backwardation
# OJ (0.8): Long first week of the month when backwardation
# Soymeal (0.7): Long second week of the month when backwardation
# Oats (1.0): Long first/second week of the month when Backwardation and EMA1 > EMA4, Short third/fourth week of the month when contango and EMA1 < EMA4
# GBPCAD (0.7): Short first week of the month


# Bonds
{
Bonds <- list()
for(symbol in names(Futures)) {
    if(!(symbol %in% c("ZN",  "ZF", "ZT", "UD", "ZB")))
        next
    print(symbol)
    Bonds[[symbol]] <- backadjust_future(Futures[[symbol]], N=5)
    Bonds[[symbol]]$Symbol <- symbol
}
Bonds <- BackAdj # Ignore the above
df <- Bonds[c("ZN", "ZF", "ZT", "UD", "ZB")] %>% do.call(rbind, .) %>% filter(year(Date) > 2000)
a <-  mutate(df, dom=mday(Date), date=yearweek(Date), Symbol=factor(Symbol)) %>% 
    mutate(Trend = lag(EMA(AdjClose, 1) - EMA(AdjClose, 4))) %>% 
    mutate(df, Trade = case_when(dom >= 25    ~ 1, dom <= 5  ~ -1, TRUE ~ 0)) %>% 
    mutate(Cost = case_when(Symbol == "ZN" ~ 0.00008, Symbol == "ZF" ~ 0.00005, Symbol == "ZT" ~ 0.00003, Symbol == "UD" ~ 0.00007,  Symbol == "ZB" ~ 0.00012,TRUE ~ 0)) %>% 
    mutate(Position = 0.25 / calculate_volatility(Return)) %>% 
    mutate(Excess = ifelse(is.na(Return), 0, Return*Trade)) %>% 
    group_by(date, Symbol) %>% 
    summarise(Excess=sum(Excess, na.rm=TRUE), Trades=first(length(rle(Trade[Trade!=0]))), Cost=first(Cost*Trades)) %>% group_by(Symbol) %>% 
    summarise(date=as.Date(date),PnL=cumsum(Excess-Cost),Excess=Excess-Cost, Cost=Cost) %>% 
    mutate(Symbol = case_when(Symbol == "ZN" ~ "10y", Symbol == "ZF" ~ "5y", Symbol == "ZT" ~ "2y", 
                              Symbol == "ZB" ~ "30y",  Symbol == "UD" ~ "Ultra",TRUE ~ Symbol))
ggplot(a) + geom_line(aes(date, PnL, color=Symbol), linewidth=2) + scale_color_colorblind() + ylim(0, 1.5)+
    theme(text= element_text(size=40), legend.title = element_blank(), legend.text = element_text(size=48), legend.key.width = unit(5, "line"), axis.title.x = element_blank()) 
a %>% summarise(mean(Excess)/sd(Excess)*sqrt(52))
}

# Oils
{
Oils <- list()
for(symbol in names(Futures)) {
    if(!(symbol %in% c("CB", "CL", "RB", "HO", "LF")))
        next
    print(symbol)
    Oils[[symbol]] <- backadjust_future(Futures[[symbol]], N=5)
    Oils[[symbol]]$Symbol <- symbol
}
Oils <- BackAdj # Ignore the above
df <- Oils[c("CB", "CL", "RB", "HO", "LF")] %>% do.call(rbind, .) %>% filter(year(Date) > 2000)
a <- mutate(df, dom=wday(Date), date=yearweek(Date), Symbol=factor(Symbol)) %>% 
    mutate(Cost = case_when(Symbol == "CB" ~ 0.0003, Symbol == "CL" ~ 0.0003, Symbol == "RB" ~ 0.001, 
                            Symbol == "HO" ~ 0.001,  Symbol == "LF" ~ 0.0007,TRUE ~ 0)) %>% 
    mutate(Trade = case_when(dom <= 3 & Basis < 0 ~ -1 , dom > 3 & Basis > 0   ~ 1, TRUE ~ 0)) %>% 
    mutate(Position = 0.25 / calculate_volatility(Return)) %>% 
    mutate(Excess = ifelse(is.na(Return), 0, Return*Trade*Position)) %>% 
    group_by(date, Symbol) %>% summarise(Excess=sum(Excess, na.rm=TRUE), Trades=first(length(rle(Trade[Trade!=0]))), Cost=first(Cost*Trades)) %>% group_by(Symbol) %>% 
    summarise(date=as.Date(date),PnL=cumsum(Excess-Cost),Excess=Excess-Cost, Cost=Cost, Symbol=as.character(Symbol)) %>% 
    mutate(Symbol = case_when(Symbol == "CB" ~ "Brent", Symbol == "CL" ~ "WTI", Symbol == "RB" ~ "Gasoline", 
                              Symbol == "HO" ~ "Heating Oil",  Symbol == "LF" ~ "Gasoil",TRUE ~ Symbol))
ggplot(a) + geom_line(aes(date, PnL, color=Symbol), linewidth=2) + scale_color_colorblind() + ylim(0, 6)+
    theme(text= element_text(size=48), legend.title = element_blank(), legend.key.width = unit(2, "line"), axis.title.x = element_blank()) 
a %>% summarise(mean(Excess)/sd(Excess)*sqrt(52))
# > strategy_performance(filter(a, Symbol=="Brent") %>% pull(Excess), filter(a, Symbol=="Brent") %>% pull(date), period=52) %>% unlist
# Mean annual return      Annualized SD       Sharpe ratio               Skew         Lower tail         Upper tail 
#             17.89              16.50               1.12              -0.27               1.63               1.10 
#             Max DD             Avg DD         Adj Avg DD                GSR                 R2 
#             -36.90              -6.26              -0.38               0.99               0.99 
}

# Lumber
{
df <- BackAdj[["LS"]] %>% filter(year(Date) > 2000)
a <- mutate(df, dom=wday(Date), Date=yearweek(Date)) %>% 
    mutate(Spread = 0.003) %>% 
    mutate(Position = 1 / calculate_volatility(df$Return)) %>% mutate(Position = ifelse(!is.na(Position), Position, 0)) %>% 
    mutate(Trend = lag(EMA(AdjClose, 1) - EMA(AdjClose, 4))) %>% 
    mutate(Trade = case_when(dom <= 3 &  Trend <= 0 ~ -1 , dom > 3  & Trend >= 0  ~ 1, TRUE ~ 0)) %>% 
    mutate(Excess = Return * Position * Trade, Cost = Spread * Position) %>% 
    group_by(Date) %>% summarise(Excess=sum(Excess, na.rm=TRUE), Trades=first(sum(unique(Trade)!=0)), Cost=first(Cost*Trades)) %>% ungroup %>% 
    summarise(Date=Date,PnL=cumsum(Excess-Cost),Excess=Excess-Cost, Cost=Cost)
ggplot(a) + geom_line(aes(Date, PnL), linewidth=2)
with(a, mean(Excess)/sd(Excess)*sqrt(52))
# 1.19806
}

# Live cattle
{
df <- BackAdj[["LE"]] %>% filter(year(Date) > 2000)
a <- 
  mutate(df, dom = mday(Date)) %>% mutate(dom = case_when(dom <= 7 ~ 1, dom > 7 & dom <= 14 ~ 2, dom > 14 & dom <= 21 ~ 3, dom > 21 & dom <= 31 ~ 4, TRUE ~ 0)) %>% 
  mutate(df, date=yearweek(Date)) %>% 
  mutate( Cost = 0.0006) %>% 
  mutate(Trend = lag(EMA(AdjClose, 1) - EMA(AdjClose, 4))) %>% 
  mutate(Trade = case_when((dom == 3  | dom == 4 ) & Trend > 0  ~ 1  ,TRUE ~ 0)) %>% 
  mutate(Excess = Return * Trade) %>% 
  group_by(date) %>% summarise(Excess=sum(Excess, na.rm=TRUE), Trades=first(sum(unique(Trade)!=0)), Cost=first(Cost*Trades)) %>% ungroup %>% 
  summarise(date=date,PnL=cumsum(Excess-Cost),Excess=Excess-Cost, Cost=Cost)
ggplot(a) + geom_line(aes(date, PnL), linewidth=2)
with(a, mean(Excess)/sd(Excess)*sqrt(52))
# Mean annual return      Annualized SD       Sharpe ratio               Skew         Lower tail         Upper tail 
# 9.96               8.68               1.17               1.87               2.87              -5.29 
# Max DD             Avg DD         Adj Avg DD                GSR                 R2 
# -14.88              -2.64              -0.30               1.11               0.99 
# 
}

# Hogs
{
  df <- BackAdj[["HE"]] %>% filter(year(Date) > 2000)
  a <- mutate(df, dom=wday(Date), Date=yearweek(Date)) %>% 
    mutate(Spread = 0.0011) %>% 
    mutate(Position = 1 / calculate_volatility(df$Return)) %>% mutate(Position = ifelse(!is.na(Position), Position, 0)) %>% 
    mutate(Trend = lag(EMA(AdjClose, 1) - EMA(AdjClose, 4))) %>% 
    mutate(Trade = case_when(dom == 2 & Trend < 0 & Basis < 0 ~ -1, dom > 2 & Trend > 0 & Basis > 0 ~ 1  , TRUE ~ 0)) %>% 
    mutate(Excess = Return * Position * Trade, Cost = Spread * Position) %>% 
    group_by(Date) %>% summarise(Excess=sum(Excess, na.rm=TRUE), Trades=first(sum(unique(Trade)!=0)), Cost=first(Cost*Trades)) %>% ungroup %>% 
    summarise(Date=Date,PnL=cumsum(Excess-Cost),Excess=Excess-Cost, Cost=Cost)
  ggplot(a) + geom_line(aes(Date, PnL), linewidth=2)
  with(a, mean(Excess)/sd(Excess)*sqrt(52))
  # 1.05631
  
}


# GAS
{
df <- BackAdj[["NG"]] %>% filter(year(Date) > 2000)
a <- 
  mutate(df, dom = mday(Date)) %>% mutate(dom = case_when(dom <= 7 ~ 1, dom > 7 & dom <= 14 ~ 2, dom > 14 & dom <= 21 ~ 3, dom > 21 & dom <= 31 ~ 4, TRUE ~ 0)) %>% 
  mutate(df, date=yearweek(Date), dow=wday(Date)) %>% 
  mutate(Spread = 0.001) %>% 
  mutate(Position = 1 / calculate_volatility(df$Return)) %>% mutate(Position = ifelse(!is.na(Position), Position, 0)) %>% 
  mutate(Trade = case_when((dom %in% c(1,3,4)) & Basis < 0 ~ -1 , (dom %in% c(2)) & Basis > 0 ~ 1 ,TRUE ~ 0)) %>% 
  mutate(Excess = Return * Position * Trade, Cost = Spread * Position) %>% 
  group_by(date) %>% summarise(Excess=sum(Excess, na.rm=TRUE), Trades=first(sum(unique(Trade)!=0)), Cost=first(Cost*Trades)) %>% ungroup %>% 
  summarise(date=date,PnL=cumsum(Excess-Cost),Excess=Excess-Cost, Cost=Cost)
ggplot(a) + geom_line(aes(date, PnL), linewidth=2)
with(a, mean(Excess)/sd(Excess)*sqrt(52))
# 0.7323059
}

# OJ
{
df <- BackAdj[["OJ"]] %>% filter(year(Date) > 2000)
a <- 
  mutate(df, dom = mday(Date)) %>% mutate(dom = case_when(dom <= 7 ~ 1, dom > 7 & dom <= 14 ~ 2, dom > 14 & dom <= 21 ~ 3, dom > 21 & dom <= 31 ~ 4, TRUE ~ 0)) %>% 
  mutate(df, date=yearweek(Date), dow=wday(Date)) %>% 
  mutate(Spread = 0.002) %>% 
  mutate(Position = 1 / calculate_volatility(df$Return)) %>% mutate(Position = ifelse(!is.na(Position), Position, 0)) %>% 
  mutate(Trade = case_when(dom == 1  & Basis > 0 ~ 1,TRUE ~ 0)) %>% 
  mutate(Excess = Return * Position * Trade, Cost = Spread * Position) %>% 
  group_by(date) %>% summarise(Excess=sum(Excess, na.rm=TRUE), Trades=first(sum(unique(Trade)!=0)), Cost=first(Cost*Trades)) %>% ungroup %>% 
  summarise(date=date,PnL=cumsum(Excess-Cost),Excess=Excess-Cost, Cost=Cost)
ggplot(a) + geom_line(aes(date, PnL), linewidth=2)
with(a, mean(Excess)/sd(Excess)*sqrt(52))
# 0.8098324
}

# Soy meal
{
  df <- BackAdj[["ZM"]] %>% filter(year(Date) > 2000)
  a <- 
    mutate(df, dom = mday(Date)) %>% mutate(dom = case_when(dom <= 7 ~ 1, dom > 7 & dom <= 14 ~ 2, dom > 14 & dom <= 21 ~ 3, dom > 21 & dom <= 31 ~ 4, TRUE ~ 0)) %>% 
    mutate(df, date=yearweek(Date), dow=wday(Date)) %>% 
    mutate(Spread = 0.002) %>% 
    mutate(Position = 1 / calculate_volatility(df$Return)) %>% mutate(Position = ifelse(!is.na(Position), Position, 0)) %>% 
    mutate(Trend = lag(EMA(AdjClose, 1) - EMA(AdjClose, 4))) %>% 
    mutate(Trade = case_when(dom == 2  & Basis > 0  ~ 1   , TRUE ~ 0)) %>% 
    mutate(Excess = Return * Position * Trade, Cost = Spread * Position) %>% 
    group_by(date) %>% summarise(Excess=sum(Excess, na.rm=TRUE), Trades=first(sum(unique(Trade)!=0)), Cost=first(Cost*Trades)) %>% ungroup %>% 
    summarise(date=date,PnL=cumsum(Excess-Cost),Excess=Excess-Cost, Cost=Cost)
  ggplot(a) + geom_line(aes(date, PnL), linewidth=2)
  with(a, mean(Excess)/sd(Excess)*sqrt(52))
  # 0.7258006
}

# Oats
{
  df <- BackAdj[["ZO"]] %>% filter(year(Date) > 2000)
  a <- 
    mutate(df, dom = mday(Date)) %>% mutate(dom = case_when(dom <= 7 ~ 1, dom > 7 & dom <= 14 ~ 2, dom > 14 & dom <= 21 ~ 3, dom > 21 & dom <= 31 ~ 4, TRUE ~ 0)) %>% 
    mutate(df, date=yearweek(Date), dow=wday(Date)) %>% 
    mutate(Spread = 0.003) %>% 
    mutate(Position = 1 / calculate_volatility(df$Return)) %>% mutate(Position = ifelse(!is.na(Position), Position, 0)) %>% 
    mutate(Trend = lag(EMA(AdjClose, 1) - EMA(AdjClose, 4))) %>% 
    mutate(Trade = case_when(dom %in% c(1,2)  & Basis > 0 & Trend > 0 ~ 1      , TRUE ~ 0)) %>% 
    mutate(Excess = Return * Position * Trade, Cost = Spread * Position) %>% 
    group_by(date) %>% summarise(Excess=sum(Excess, na.rm=TRUE), Trades=first(sum(unique(Trade)!=0)), Cost=first(Cost*Trades)) %>% ungroup %>% 
    summarise(date=date,PnL=cumsum(Excess-Cost),Excess=Excess-Cost, Cost=Cost)
  ggplot(a) + geom_line(aes(date, PnL), linewidth=2)
  with(a, mean(Excess)/sd(Excess)*sqrt(52))
  # 0.9251473
}


# GBPAUD (PV), GBPCAD (PW), GBPNZD (GN), GBPUSD (MB) etc...
# GBPCAD is the chosen one
{
df <- BackAdj[c("B6", "PV", "PW", "GN", "MB", "GB")] %>% do.call(rbind, .) %>% filter(year(Date) > 2000) 
df <- BackAdj[["PW"]] %>% filter(year(Date) > 2000)
a <- 
  mutate(df, dom = mday(Date)) %>% mutate(dom = case_when(dom <= 7 ~ 1, dom > 7 & dom <= 14 ~ 2, dom > 14 & dom <= 21 ~ 3, dom > 21 & dom <= 31 ~ 4, TRUE ~ 0)) %>% 
  mutate(df, date=yearweek(Date), dow=wday(Date)) %>% 
  mutate(Cost = case_when(Symbol == "B6" ~ 0.00029, Symbol == "PV" ~ 0.00014, Symbol == "PW" ~ 0.00013, 
                          Symbol == "GN" ~ 0.00017,  Symbol == "MB" ~ 0.00007, Symbol == "GB" ~ 0.00009, TRUE ~ 0)) %>% mutate(Return=ifelse(Symbol=="GB", -Return, Return)) %>% 
  mutate(Trend = lag(EMA(AdjClose, 1) - EMA(AdjClose, 4))) %>% 
  mutate(Trade = case_when(dom == 1  ~ -1 ,TRUE ~ 0)) %>% 
  mutate(Excess = Return * Trade) %>% 
  group_by(date, Symbol) %>% summarise(Symbol=first(Symbol), Excess=sum(Excess, na.rm=TRUE), Trades=first(sum(unique(Trade)!=0)), Cost=first(Cost*Trades)) %>% group_by(Symbol) %>% 
  summarise(Symbol=Symbol,date=date,PnL=cumsum(Excess-Cost),Excess=Excess-Cost, Cost=Cost)
ggplot(a) + geom_line(aes(date, PnL, color=Symbol), linewidth=2) + scale_color_colorblind()
group_by(a, Symbol) %>% summarise(mean(Excess)/sd(Excess)*sqrt(52))
with(a, mean(Excess)/sd(Excess)*sqrt(52))
# Mean annual return      Annualized SD       Sharpe ratio               Skew         Lower tail         Upper tail             Max DD 
# 2.85               4.27               0.71               0.13               1.11               1.26              -7.08 
# Avg DD         Adj Avg DD                GSR                 R2 
# -1.61              -0.38               0.71               0.99 
#            
}









  # Coptemplate for week day
  df <- BackAdj[["CA"]] %>% filter(year(Date) > 2000)
  a <- mutate(df, dom=wday(Date), date=yearweek(Date)) %>% 
    mutate( Cost = 0.0005)%>% 
    mutate(Trend = lag(EMA(AdjClose, 1) - EMA(AdjClose, 4))) %>% 
    mutate(Trade = case_when( dom == 2 & Basis < 0  ~ -1  ,TRUE ~ 0)) %>% 
    mutate(Excess = Return * Trade) %>% 
    group_by(date) %>% summarise(Excess=sum(Excess, na.rm=TRUE), Trades=first(sum(unique(Trade)!=0)), Cost=first(Cost*Trades)) %>% ungroup %>% 
    summarise(date=date,PnL=cumsum(Excess-Cost),Excess=Excess-Cost, Cost=Cost)
  ggplot(a) + geom_line(aes(date, PnL), linewidth=2)
  with(a, mean(Excess)/sd(Excess)*sqrt(52))
                 
  # GBPAUD (PV), GBPCAD (PW), GBPNZD (GN), GBPUSD (MB)
  df <- BackAdj[["SW"]] %>% filter(year(Date) > 2000)
  a <- 
    mutate(df, dom = mday(Date)) %>% mutate(dom = case_when(dom <= 7 ~ 1, dom > 7 & dom <= 14 ~ 2, dom > 14 & dom <= 21 ~ 3, dom > 21 & dom <= 31 ~ 4, TRUE ~ 0)) %>% 
    mutate(df, date=yearweek(Date), dow=wday(Date)) %>% 
    mutate( Cost = 0) %>% 
    mutate(Trend = lag(EMA(AdjClose, 1) - EMA(AdjClose, 4))) %>% 
    mutate(Trade = case_when(dom == 4  & Basis > 0 ~ 1   , TRUE ~ 0)) %>% 
    mutate(Excess = Return * Trade) %>% 
    group_by(date) %>% summarise(Excess=sum(Excess, na.rm=TRUE), Trades=first(sum(unique(Trade)!=0)), Cost=first(Cost*Trades)) %>% ungroup %>% 
    summarise(date=date,PnL=cumsum(Excess-Cost),Excess=Excess-Cost, Cost=Cost)
  ggplot(a) + geom_line(aes(date, PnL), linewidth=2)
  with(a, mean(Excess)/sd(Excess)*sqrt(52))
             