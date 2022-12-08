// The final crack model

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
  int J;
  matrix[N, J] X;
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
  row_vector[ma] theta; 
  vector[dft*2] C;
  vector<lower=0, upper=1>[level==1] ar;
  vector<lower=0> [level==1]pl;
  real<lower=0> sigma_x_q0;
  real<lower=0, upper=1> sigma_x_q1;
  real<lower=0, upper=(1-sigma_x_q1)> sigma_x_q2;
  vector[J] Bx;
  vector[J] By;
}
transformed parameters {
  vector[N+h] m_pred; 
  vector[N+h] m; 
  vector[N+h] P_pred; 
  vector[N+h] sigma_x;
  vector[N+h] P; 
  vector[N+h] S; 
  vector[N+h] v; 
  vector[N] Dx;
  vector[N] Dy;
  {
    int start;
    real R; 
    real K;  
    
    Dx = X * Bx;
    Dy = X * By;
    
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
              sigma_x[t] = sqrt(sigma_x_q0);
            else               
              sigma_x[t] = sqrt(
                              sigma_x_q0 +
                              sigma_x_q1 * (y[t - 1] - y[t - 2])^2 +
                              sigma_x_q2 * (sigma_x[t - 1])^2
                              );
            m_pred[t] = m[t-1] + theta * (m[(t-ma):(t-1)] - m[(t-ma-1):(t-2)]) + c[t,] * C;
            if(level)
              m_pred[t] = ar[1] * (m_pred[t] + pl[1]);
            P_pred[t] = P[t-1] + sigma_x[t]^2;
        }
        if(t > start && t <= N) {
          m_pred[t] += Dx[t];
          v[t] = y[t] - (m_pred[t] + Dy[t]);
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
  pl ~ exponential(1);
  ar ~ beta(10, 2);
  sigma_x_q0 ~ exponential(1);
  Bx ~ normal(0, 1);
  By ~ normal(0, 1);
  y[1:trainset] ~ normal(m_pred[1:trainset] + Dy[1:trainset] , sqrt(S[1:trainset]));
}

generated quantities {
  array[N] real log_lik;
  array[N+h] real y_hat;
  for(i in 1:N+h) {
      if(i <= N)
        log_lik[i] = normal_lpdf(y[i] | m_pred[i] + Dy[i], sqrt(S[i]));
      y_hat[i] = normal_rng(m_pred[i] + Dy[i], sqrt(S[i]));
  }  
}
