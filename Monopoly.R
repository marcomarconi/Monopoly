{
  library(tidyverse)
  library(rstan)
  library(loo)
  library(mvtnorm)
  library(cmdstanr)
  library(quantmod)
  library(mvtnorm)
  library(posterior)
  library(bayesplot)
  library(reshape2)
  source("/home/marco/trading/Systems//Common/Common.R")
  setwd("/home/marco/trading/Systems/Monopoly/")
  Sys.setlocale("LC_TIME", "en_US.UTF-8")
  theme_set(theme_bw(base_size = 32))
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
ar_x <- exp(-log(2)/hl)^0 # ^0 to remove ar
ar_v <- 0.99
err_x <- rep(0, N)
err_v <- rep(0, N)
err_y <- rep(N)
sigma_x <- 0.25
sigma_y <- 0.01*0
sigma_v <- 0.00001*0   # 0 to remove velocity
u <- 0
for(i in 2:N) {
  err_v[i] <-  0.95 * err_v[i-1] + rnorm(1, 0, sigma_v)
  err_x[i] <-  rnorm(1, 0, sigma_x)
  err_y[i] <-  rnorm(1, 0, sigma_y)
  v[i] <- ar_v * v[i-1] + err_v[i]
  x[i] <- ar_x * x[i-1] + u + v[i] + err_x[i]
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
  


# Load all CPI and IR data
{
CPI <- read_csv("Data/CPI.csv") %>% mutate(Indicator="CPI") %>% rename(Country=LOCATION, Date=TIME) %>% select(Indicator, Country, Date, Value) 
IR <- read_csv("Data/InterestRate.csv") %>% mutate(Indicator="IR") %>% rename(Country=LOCATION, Date=TIME) %>% select(Indicator, Country, Date, Value) 
GDP <- read_csv("Data/GDP.csv") %>% mutate(Indicator="GDP") %>% rename(Country=LOCATION, Date=TIME) %>% select(Indicator, Country, Date, Value) 
GDP$Date <- sub("Q1", "03", GDP$Date) %>% sub("Q2", "06", .) %>% sub("Q3", "09", .) %>% sub("Q4", "12", .) 
SP <- read_csv("Data/SharePrice.csv") %>% mutate(Indicator="SP") %>% rename(Country=LOCATION, Date=TIME) %>% select(Indicator, Country, Date, Value) 
UR <- read_csv("Data/UnemploymentRate.csv") %>% mutate(Indicator="UR") %>% rename(Country=LOCATION, Date=TIME) %>% select(Indicator, Country, Date, Value) 
Df <- rbind(CPI, IR, GDP, SP, UR)
Df$Indicator <- factor(Df$Indicator)
Df$Country <- factor(Df$Country)
Df$Date <- as.Date(paste0(Df$Date, "-01"), format="%Y-%m-%d")
}
          
# Plotting some data
{
filter(Df, Country %in% c("USA") & Indicator %in% c("IR", "CPI")) %>% #mutate(ValueL=c(0,diff(log(Value))*100)) %>% mutate(Value=ifelse(Indicator=="SP", ValueL, Value)) %>% 
    ggplot() + geom_line(aes(Date, Value , col=Indicator), lwd=1) + geom_hline(yintercept = 0) + facet_wrap(.~Country) + theme(text = element_text(size=32))
filter(Df, (Indicator=="IR" | Indicator=="CPI") & Country %in% c("USA")) %>% dcast(Date~Indicator) %>% na.omit %>% 
ggplot() + geom_point(aes(CPI, IR))
filter(Df, (Indicator=="IR" | Indicator=="CPI")  & Country %in% c("USA")) %>% dcast(Date~Indicator) %>%  
  ggplot() + geom_point(aes(c(0, diff(CPI)), c(0, diff(IR))))
filter(Df, (Indicator=="IR" | Indicator=="CPI") & Country %in% c("USA")) %>%
ggplot() + geom_histogram(aes(Value)) + facet_wrap(~Indicator)
usa <- filter(Df, Country %in% c("USA") & Indicator %in% c("IR", "CPI")) %>%  pivot_wider(names_from = Indicator, values_from = Value) %>% select(-Country, -Date) 
}
set.seed(1)
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


