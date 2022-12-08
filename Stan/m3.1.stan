// Simpel seasonal model, using discrete Fourier, with fixed level and ar

data {
  int<lower=0> N;
  int h;
  array[N] real y;
  real P0;
  real m0;
  // seasonal 
  array[N+h] int month;
  int period;
  int dft;
  

}
parameters {
  real<lower=0> sigma_y;
  real<lower=0> sigma_x; 
  vector[dft*2] C;
  real<lower=0, upper=1> ar;
  real<lower=0> level;
}
transformed parameters {
  vector[N+h] m_pred; 
  vector[N+h] m; 
  vector[N+h] P_pred; 
  vector[N+h] P; 
  vector[N+h] S; 
  vector[N+h] v; 
  {
    matrix[N+h, dft*2] c;
    real R; 
    real K;  
    
    v = rep_vector(0, N+h);
    R = sigma_y^2;
    
    // Build up the matrix of seasonality vectors (see prophet's paper, section 3.2)
    for(t in 1:N+h) {
      for(i in 1:(dft*2)){
        if(i % 2 == 0)
          continue;
        c[t,i] = cos(2 * pi() * i * month[t] / period);
        c[t,(i+1)] =  sin(2 * pi() * i * month[t] / period);
      }
    }

    m_pred[1] = m0;
    P_pred[1] = P0;
    for (t in 1:N+h) {
        if (t>1) {
            m_pred[t] = ar * (m[t-1] + level) + c[t,] * C; // ar multiplying level is prob unnecessary
            P_pred[t] = P[t-1] + sigma_x^2;
        }
        if(t <= N) {
          v[t] = y[t] - m_pred[t];
        }
        S[t] = P_pred[t] + R;
        K = P_pred[t] / S[t];
        m[t] = m_pred[t] + K*v[t];  
        P[t] = P_pred[t] - K*S[t]*K;
    }        
  }
}

model {
  sigma_x ~ exponential(1);
  sigma_y ~ exponential(1);
  C ~ normal(0, 1);
  level ~ exponential(1);
  ar ~ beta(10, 2);
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
