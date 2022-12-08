// Simple Bivariate SSM

data {
  int<lower=0> N;
  array[N] vector[2] y;
  vector[2] m0;
  matrix[2, 2] P0;
  int h;
}
transformed data {
  int D = 2;
  matrix[D, D] I;
  I = diag_matrix(rep_vector(1, D));
}
parameters {
  real<lower=0> sigma_proc_1;
  real<lower=0> sigma_proc_2;
  real<lower=0> sigma_obs_1;
  real<lower=0> sigma_obs_2;
  real<lower=0, upper=1> z11;
  real<lower=0, upper=1> z22;
  real z12;
  real z21;
  vector[D] u;

}
transformed parameters {
  array[N+h] vector[D] m;
  array[N+h] matrix[D, D] P;
  array[N+h] vector[D] m_pred;
  array[N+h] matrix[D, D] P_pred;
  array[N+h] matrix[D, D] S;
  array[N+h] vector[D] mu;
  matrix[D, D] Q;
  matrix[D, D] R;
  {
    matrix[D, D] Z;
    matrix[D, D] Zt;
    array[N+h] matrix[D, D] K;
    array[N+h] vector[D] v;
    v = rep_array(rep_vector(0, D), N+h);
    
    Z[1,1] = z11;
    Z[2,2] = z22;
    Z[1,2] = z12;
    Z[2,1] = z21;
    Zt = Z';


    Q = diag_matrix([sigma_proc_1^2, sigma_proc_2^2]');
    R = diag_matrix([sigma_obs_1^2, sigma_obs_2^2]');
    
    for(t in 1:2) {
      m_pred[t] = m0;
      P_pred[t] = P0;
    }
    profile("KF"){
    for (t in 1 : N+h) {
      if (t > 2) {
        m_pred[t] = Z * m[t - 1] + u;
        P_pred[t] = Z * P[t - 1] * Zt + Q;
      }
      S[t] = P_pred[t] + R;
      K[t] = P_pred[t] * diag_matrix(1.0 / diagonal(S[t])); // assume no correlation between errors
      if(t <= N) 
        v[t] = y[t,  : ] - m_pred[t];
      m[t] = m_pred[t] + K[t] * v[t];
      P[t] = (I - K[t]) * P_pred[t];
      mu[t] = m_pred[t];
    }}
    
  }
}
model {
  [sigma_proc_1, sigma_proc_2, sigma_obs_1, sigma_obs_2] ~ exponential(1);
  [z11, z22] ~ beta(2, 1);
  [z12, z21] ~ normal(0, 1);
  u ~ normal(0, 1);
  profile("LK"){
  for (i in 1 : N) 
        y[i] ~ multi_normal(mu[i], S[i]);}
}
generated quantities {
  array[N] real log_lik;
  array[N+h] vector[2] y_hat;
  for (i in 1 : N)
      log_lik[i] = multi_normal_lpdf(y[i] | mu[i], S[i]);
  for (i in 1 : N+h)
      y_hat[i] = multi_normal_rng(mu[i], S[i]);
}

