

functions {
  real logit_f (real x, real m, real b) {
    return(1 / (1 + exp(-b*(x-m))));
  }
}
data {
  int<lower=0> N;
  array[N] vector[2] y;
  vector[2] m0;
  matrix[2, 2] P0;
  int delay;
}
transformed data {
  int D = 2;
  matrix[D, D] I;
  I = diag_matrix(rep_vector(1, D));
}
parameters {
  vector<lower=0> [D] sigma_obs;
  vector<lower=0> [D] sigma_proc;
  real<lower=0, upper=1> z11;
  real<lower=0, upper=1> z22;
  real z12;
  real z21;
  vector[D] u;
  real threshold;
  real<lower=0> rate;
}
transformed parameters {
  array[N] vector[D] m;
  array[N] vector[D] m_pred;
  array[N] matrix[D, D] P;
  array[N] matrix[D, D] P_pred;
  array[N] matrix[D, D] S;
  array[N] vector[D] mu;
  matrix[D, D] Q;
  matrix[D, D] R;
  {
    matrix[D, D] Z;
    matrix[D, D] C;
    array[N] matrix[D, D] K;
    array[N] vector[D] v;
    
    Z[1,1] = z11;
    Z[2,2] = z22;
    Z[1,2] = z12;
    Z[2,1] = z21;

    Q = diag_matrix([sigma_proc[1]^2, sigma_proc[2]^2]');
    R = diag_matrix([sigma_obs[1]^2, sigma_obs[2]^2]');
    
    for(t in 1:delay) {
      m_pred[t] = m0;
      P_pred[t] = P0;
    }
    for (t in 1 : N) {
      if (t > delay) {
        Z[2,1] = z21*(-logit_f(m[t-delay, 1], threshold, rate)+1) ;
        m_pred[t] = Z * m[t - 1] + u;
        P_pred[t] = Z * P[t - 1] * Z' + Q;
      }
      v[t] = y[t,  : ] - m_pred[t];
      S[t] = P_pred[t] + R);
      K[t] = P_pred[t]  / S[t];
      m[t] = m_pred[t] + K[t] * v[t];
      P[t] = P_pred[t] - P_pred[t] * K[t]';
      mu[t] = m_pred[t];
    }
  }
}
model {
  sigma_proc ~ exponential(1);
  sigma_obs ~ exponential(1);
  [z11, z22] ~ beta(2, 1);
  [z12, z21] ~ normal(0, 1);
  u ~ normal(0, 1);
  threshold ~ normal(0, 1);
  rate ~ gamma(3, 1);
  for (i in 1 : N) 
        y[i] ~ multi_normal(mu[i], S[i]);
}
generated quantities {
  array[N] real log_lik;
    for (i in 1 : N)
      log_lik[i] = multi_normal_lpdf(y[i] | mu[i], S[i]);
}

