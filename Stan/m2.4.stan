// Bivariate lag-D model, with a logistic effect

functions {
  real logit_f (real x, real m, real b) {
    return(1 / (1 + exp(-b*(x-m))));
  }
}
data {
  int<lower=0> N;
  array[N] vector[2] y;
  int D;
  vector[D] m0;
  matrix[D, D] P0;

  int h;
}
transformed data {
  matrix[D, D] I;
  I = diag_matrix(rep_vector(1, D));
}
parameters {
  vector<lower=0> [2] sigma_obs;
  vector<lower=0> [D] sigma_proc;
  real<lower=0, upper=1> z11;
  real<lower=0, upper=1> zdd;
  real z1d;
  real zd1;
  vector[D] u;
  real threshold;
  real<lower=0> rate;
}
transformed parameters {
  array[N+h] vector[D] m;
  array[N+h] vector[D] m_pred;
  array[N+h] matrix[D, D] P;
  array[N+h] matrix[D, D] P_pred;
  array[N+h] matrix[2, 2] S;
  array[N+h] vector[2] mu;
  matrix[D, D] Q;
  matrix[2, 2] R;
  {
    matrix[D, D] Z;
    matrix[2, D] H;
    matrix[D, 2] Ht;
    array[N+h] matrix[D, 2] K;
    array[N+h] vector[2] v;

    Z = rep_matrix(0, D, D);
    Z[1,1] = z11;
    Z[1,D] = z1d;
    Z[D,D] = zdd;
    if(2 < D)
        for(j in 2:(D-1))
          Z[j, j-1] = 1;
    H = rep_matrix(0, 2, D); H[1,1] = 1; H[2,D] = 1;      
    Ht = H';
    {      
      vector[D] q;
      vector[2] r;
      q = rep_vector(0, D);
      q[1] = sigma_proc[1]^2;
      q[D] = sigma_proc[2]^2;
      Q = diag_matrix(q);
      r[1] = sigma_obs[1]^2;
      r[2] = sigma_obs[2]^2;
      R = diag_matrix(r);
    }
    
    m_pred[1] = m0;
    P_pred[1] = P0;
    for (t in 1 : N+h) {
      if (t > 1) {
          Z[D, D-1] = zd1*(-logit_f(m[t-1, 1], threshold, rate)+1) ;
          m_pred[t] = Z * m[t - 1] + u;
          P_pred[t] = Z * P[t - 1] * Z' + Q;
      }
      if(t <= N) 
        v[t] = y[t,  : ] - H * m_pred[t];
      else
        v[t] = rep_vector(0, 2);
      S[t] = (H * P_pred[t] * Ht) + R;
      K[t] = (P_pred[t] * Ht) / S[t];
      m[t] = m_pred[t] + K[t] * v[t];
      P[t] = (I - K[t] * H) * P_pred[t];
      mu[t] = H * m_pred[t];
    }
  }
}
model {
  sigma_proc ~ exponential(1);
  sigma_obs ~ exponential(1);
  [z11, zdd] ~ beta(2, 1);
  [z1d, zd1] ~ normal(0, 1);
  u ~ normal(0, 1);
  threshold ~ normal(0, 1);
  rate ~ gamma(3, 1);
  for (i in 1 : N) 
        y[i] ~ multi_normal(mu[i], S[i]);
}
generated quantities {
  array[N] real log_lik;
  array[N+h] vector[2] y_hat;
  for (i in 1 : N)
      log_lik[i] = multi_normal_lpdf(y[i] | mu[i], S[i]);
  for (i in 1 : N+h)
      y_hat[i] = multi_normal_rng(mu[i], S[i]);
}

