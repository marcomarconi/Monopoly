// Bivariate SSM with seasonal and variable trend (LLT style)

TO BE REMOVED

data {
  int<lower=0> N;
  array[N] real y;
  vector[2] m0;
  matrix[2, 2] P0;
  int h;
  // seasonal 
  array[N+h] int month;
  int period;
  int dft; 
  
}
transformed data {
  int D = 2;
  matrix[D, D] I;
  array[N+h] row_vector[dft*2] c;
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
  real<lower=0> sigma_proc_1;
  real<lower=0> sigma_proc_2;
  real<lower=0> sigma_obs;
  vector[dft*2] C;
}
transformed parameters {
  array[N+h] vector[D] m;
  array[N+h] matrix[D, D] P;
  array[N+h] vector[D] m_pred;
  array[N+h] matrix[D, D] P_pred;
  vector[N+h] S;
  vector[N+h]  mu;
  matrix[D, D] Q;
  real R;
  vector[N+h] v;
{
    matrix[D, D] Z;
    row_vector[D] H;
    vector[D] Ht;
    array[N+h] vector[D] K;
    v = rep_vector(0, N+h);
    
    Z = [[1, 1],
         [0, 1]];  
    H = [1, 0];    
    Ht = H';

    Q = diag_matrix([sigma_proc_1^2, sigma_proc_2^2]');
    R = sigma_obs^2;
    
    for(t in 1:1) {
      m_pred[t] = m0;
      P_pred[t] = P0;
    }
    for (t in 1 : N+h) {
      if (t > 1) {
        m_pred[t] = Z * m[t - 1] + [c[t,] * C, 0]';
        P_pred[t] = Z * P[t - 1] * Z' + Q;
      }
      v[t] = y[t] - H * m_pred[t];
      S[t] = (H * P_pred[t] * Ht + R);
      K[t] = (P_pred[t] * Ht) / S[t];
      m[t] = m_pred[t] + K[t] * v[t];
      P[t] = (I - K[t] * H) * P_pred[t];
      mu[t] = H * m_pred[t];
    }
    
  }
}
model {
  [sigma_proc_1, sigma_proc_2, sigma_obs] ~ exponential(1);
  C ~ normal(0, 1);
  y ~ normal(mu, sqrt(S));
}
generated quantities {
  array[N] real log_lik;
  vector[N+h] y_hat;
  for (i in 1 : N)
      log_lik[i] = normal_lpdf(y[i] | mu[i], sqrt(S[i]));
  for (i in 1 : N+h)
      y_hat[i] = normal_rng(mu[i], sqrt(S[i]));
}

