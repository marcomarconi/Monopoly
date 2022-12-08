// Bivariate SSM with time-varying seasonal amplitude and fixed level

data {
  int<lower=0> N;
  array[N] real y;
  vector[2] m0;
  matrix[2, 2] P0;
  int h;
  // seasonal 
  array[N+h] int month;
  int period;
  int dft; // only 1 works, otherwise it conflicts with ar
  // covariates
  int J;
  matrix[N, J] X;  
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
  real<lower=0> sigma_proc_1;
  real<lower=0> sigma_proc_2;
  real<lower=0> sigma_obs;
  vector[dft] C;
  real<lower=0, upper=1> ar;
  real<lower=0> level;
  vector[J] Bz;
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
  vector[N] Dz;
{
    matrix[D, D] Z;
    row_vector[D] H;
    vector[D] Ht;
    array[N+h] vector[D] K;
    vector[dft*2] Cb;
    v = rep_vector(0, N+h);
    
    for(i in 1:(dft*2)) {
      if(i % 2 == 0)
        Cb[i] = C[i %/% 2];
      else
        Cb[i] = 1;
    }
    Z = [[1, c[1,] * Cb],
         [0, 1]];  
    H = [1, 0];    
    Ht = H';

    Q = diag_matrix([sigma_proc_1^2, sigma_proc_2^2]');
    R = sigma_obs^2;
    Dz = X * Bz;
    for(t in 1:1) {
      m_pred[t] = m0;
      P_pred[t] = P0;
    }
    for (t in 1 : N+h) {
      if (t > 1) {
        Z[1,2] = c[t,] * Cb;
        m_pred[t] = Z * m[t - 1] + [ar*level*0,  Dz[t]]';  // I put a zero here?
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
  level ~ cauchy(0, 1);
  ar ~ beta(10, 2);
  Bz ~ normal(0, 1);
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

