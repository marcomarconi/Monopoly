// The final crack model
// you might want to remove the level

data {
  int<lower=0> N;
  int h;
  array[N] real y;
  // initial values
  real P0;
  real m0;
  // seasonal 
  array[N+h] int month;
  int period;
  int dft;
  // covariates
  int Jx;
  matrix[N, Jx] Xx;
  int Jy;
  matrix[N, Jy] Xy;
  int Jl;
  matrix[N, Jl] Xl;
  // moving average
  int ma;
  // with level
  int level;
  // with garch
  int garch;
  int sigma1;
  // trainset
  int trainset;
  
}

transformed data {
    array[N+h] row_vector[dft*2] c;
     // Build up the matrix of seasonality vectors (see prophet's paper, section 3.2)
    for(t in 1:N+h) {
      for(i in 1:(dft*2)){
        if(i % 2 == 0)
          continue;
        c[t,i] = cos(2 * pi() * i * month[t] / period);
        c[t,(i+1)] =  sin(2 * pi() * i * month[t] / period);
      }
    }

}

parameters {
  real m1;
  real<lower=0>  P1;
  real<lower=0> sigma_y;
  vector<lower=-1, upper=1>[ma==1] theta; 
  vector[dft*2] C;
  vector<lower=0, upper=1>[level==1] ar;
  vector [level==1]pl_a;
  real<lower=0> sigma_x_q0;
  vector<lower=0, upper=1>[garch==1] sigma_x_q1;
  vector<lower=0, upper=(1-sigma_x_q1)>[garch==1] sigma_x_q2;
  vector[Jx] Bx;
  vector[Jy] By;
  vector[Jl] Bl;
}
transformed parameters {
  vector[N+h] m_pred; 
  vector[N+h] S; 
  vector[N+h] v; 
  vector[N] Dx;
  vector[N] Dy;
  vector[N] Dl;
  {
    vector[N+h] m; 
    vector[N+h] P_pred; 
    vector[N+h] sigma_x;
    vector[N+h] P; int start;
    real R; 
    real K;  
    
    Dx = Xx * Bx;
    Dy = Xy * By;
    Dl = Xl * Bl;
    
    v = rep_vector(0, N+h);
    R = sigma_y^2;
    
    start = max(garch+1, ma+1);
    for(t in 1:start) {
      m_pred[t] = m1;
      P_pred[t] = P1;
      sigma_x[t] = sigma1;
    }
    for (t in 1:N+h) {
        if (t > start) {
            if(!garch)
              sigma_x[t] = sigma_x_q0;
            else               
              sigma_x[t] = sqrt(
                              sigma_x_q0 +
                              sigma_x_q1[1] * (y[t - 1] - y[t - 2])^2 +
                              sigma_x_q2[1] * (sigma_x[t - 1])^2
                              );
            m_pred[t] = m[t-1];
            if(level) {
              real pl = pl_a[1] + Dl[t-1];
              m_pred[t] = ar[1] * m_pred[t] + pl;
            }
            m_pred[t] +=  c[t,] * C;   
            P_pred[t] = P[t-1] + sigma_x[t]^2;
        }
        if(t > start && t <= N) {
          m_pred[t] += Dx[t-1];
          v[t] = y[t] - (m_pred[t] + Dy[t-1] + (ma > 0 ? theta[1] * v[t-1] : 0));
        }
        S[t] = P_pred[t] + R;
        K = P_pred[t] / S[t];
        m[t] = m_pred[t] + K*v[t];  
        P[t] = P_pred[t] - K*S[t]*K;
    }        
  }
}

model {
  m1 ~ normal(m0, 1);
  P1 ~ exponential(P0);
  sigma_y ~ exponential(1);
  theta ~ normal(0, 1);
  C ~ normal(0, 1);
  pl_a ~ cauchy(0, 1);
  ar ~ beta(10, 2);
  sigma_x_q0 ~ exponential(1);
  Bx ~ normal(0, 1);
  By ~ normal(0, 1);
  Bl ~ normal(0, 1);
  y[2:trainset] ~ normal(m_pred[2:trainset] + Dy[1:(trainset-1)] + (ma > 0 ? theta[1] * v[1:(trainset-1)] : rep_vector(0, (trainset-1))), sqrt(S[2:trainset]));
}

generated quantities {
  array[N] real log_lik;
  array[N+h] real y_hat;
  log_lik[1] = normal_lpdf(y[1] | m_pred[1], sqrt(S[1]));
  y_hat[1] = normal_rng(m_pred[1], sqrt(S[1]));
  for(i in 2:N+h) {
      if(i <= N)
        log_lik[i] = normal_lpdf(y[i] | m_pred[i] + Dy[i-1] + (ma > 0 ? theta[1] * v[i-1] : 0), sqrt(S[i]));
      y_hat[i] = normal_rng(m_pred[i] + Dy[i-1] + (ma > 0 ? theta[1] * v[i-1] : 0), sqrt(S[i]));
  }  
}
