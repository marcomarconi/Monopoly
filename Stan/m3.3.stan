// Seasonal model, using discrete Fourier, with fixed level and ar, and GARCH on process

data {
  int<lower=0> N;
  int h;
  array[N] real y;
  real P0;
  real m0;
  real sigma1;
  // seasonal 
  array[N+h] int month;
  int period;
  int dft;
  // garchs
  int garch_x;
  int garch_y;
}
transformed data {
  int D = 2;
  matrix[D, D] I;
  matrix[N+h, dft*2] c;
  I = diag_matrix(rep_vector(1, D));
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
  vector[dft*2] C;
  real<lower=0, upper=1> ar;
  real<lower=0> level;
  real<lower=0> q0;
  real<lower=0, upper=1> q1;
  real<lower=0, upper=(1-q1)> q2;
  real<lower=0> r0;
  real<lower=0, upper=1> r1;
  real<lower=0, upper=(1-r1)> r2;
}
transformed parameters {
  vector[N+h] m_pred; 
  vector[N+h] m; 
  vector[N+h] P_pred; 
  vector[N+h] P; 
  vector[N+h] sigma_x; 
  vector[N+h] sigma_y; 
  vector[N+h] S; 
  vector[N+h] v; 
  {
    real K;  
    v = rep_vector(0, N+h);

    for(t in 1:2) {
      m_pred[t] = m0;
      P_pred[t] = P0;
      sigma_x[t] = sigma1;
      sigma_y[t] = sigma1;
    }
    for (t in 1:N+h) {
        if (t>2) {
          
            if(!garch_x)
              sigma_x[t] = sqrt(q0);
            else               
              sigma_x[t] = sqrt(
                              q0 +
                              q1 * (m[t - 1] - m[t - 2])^2 +
                              q2 * (sigma_x[t - 1])^2
                              );
              
            if(garch_y)
              sigma_y[t] = sqrt(r0 +
                                r1 * (y[t - 1] - y[t - 2])^2 +
                                r2 * (sigma_y[t - 1])^2);
            else
              sigma_y[t] = sqrt(r0);  
              
            m_pred[t] = ar * (m[t-1] + level) + c[t,] * C;
            P_pred[t] = P[t-1] + sigma_x[t]^2;
        }
        if(t <= N) {
          v[t] = y[t] - m_pred[t];
        }
        S[t] = P_pred[t] + sigma_y[t]^2;
        K = P_pred[t] / S[t];
        m[t] = m_pred[t] + K*v[t];  
        P[t] = P_pred[t] - K*S[t]*K;
    }        
  }
}

model {
  C ~ normal(0, 1);
  level ~ exponential(1);
  ar ~ beta(10, 2);
  q0 ~ exponential(1);
  r0 ~ exponential(1);
  y ~ normal(m_pred[1:N], sqrt(S[1:N]));
}

generated quantities {
  array[N] real log_lik;
  array[N+h] real y_hat;
  for(i in 1:N+h) {
      if(i <= N)
        log_lik[i] = normal_lpdf(y[i] | m_pred[i], sqrt(S[i]));
      y_hat[i] = normal_rng(m_pred[i], sqrt(S[i]));
  } 
}