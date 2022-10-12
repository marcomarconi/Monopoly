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
  real cpi_target;
  int covariates;
  int likelihood;
}
transformed data {
  int D = 2;
  matrix[D, D] I;
  I = diag_matrix(rep_vector(1, D));
}
parameters {
  vector<lower=0> [1] sigma_obs;
  vector<lower=0> [D] sigma_proc;
  real<lower=0, upper=1> z11;
  real<lower=0, upper=1> z22;
  real<upper=0> z12;
  real<lower=0> z21;
  real<lower=0> resp_rate;
  array[N] vector[covariates==1] c_op_diag;
  // vector[D] m1;
  // cov_matrix[D] P1;
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
    matrix[D, D] H;
    matrix[D, D] Ht;
    matrix[D, D] C;
    vector[D] u;
    vector[D] a;
    array[N] matrix[D, D] K;
    array[N] vector[D] v;
    

    H = diag_matrix(rep_vector(1, D));
    Ht = H';
    if(covariates) {
      C[1,1] = 0;
      C[2,2] = 0;
      C[1,2] = c_op_diag[1][1];
      C[2,1] = c_op_diag[1][2];
    } else
      C = rep_matrix(0, D, D);
    u = rep_vector(0, D);
    a = rep_vector(0, D);
    Q = diag_matrix([sigma_proc[1]^2, sigma_proc[2]^2]');
    R = diag_matrix([sigma_obs[1]^2, 0]');
    
    for(t in 1:3) {
      m_pred[t] = m0;
      P_pred[t] = P0;
    }
    for (t in 1 : N) {
      if (t > 3) {
        Z[1,1] = z11;
        Z[2,2] = z22;
        Z[1,2] = z12;
        Z[2,1] = z11*logit_f(m[t-1,1], cpi_target, resp_rate) / (m[t-1,1]+1e-6); //y[t-1,1] > cpi_target ? z21 : 0;// * (1-exp(-resp_rate*y[t-1,1])); //* 
        // u = (I - Z) * thresholds; // MARSS chapter 14
        m_pred[t] = Z * m[t - 1] + u + C * (y[t-1,  : ] - y[t-2,  : ]);
        P_pred[t] = Z * P[t - 1] * Z' + Q;
      }
      v[t] = y[t,  : ] - H * m_pred[t] - a;
      S[t] = (H * P_pred[t] * Ht + R);
      K[t] = (P_pred[t] * Ht) / S[t];
      m[t] = m_pred[t] + K[t] * v[t];
      P[t] = P_pred[t] - P_pred[t] * Ht * K[t]';
      mu[t] = H * m_pred[t] + a;
    }
  }
}
model {
  sigma_proc ~ exponential(1);
  sigma_obs ~ exponential(1);
  [z11, z22] ~ beta(2, 1);
  [z12, z21] ~ normal(0, 0.5);
  resp_rate ~ gamma(1, 1);
  c_op_diag[1] ~ normal(0, 0.1);
// m1 ~ normal(m0, 1);
  // P1 ~ wishart(D, P0);
  if(likelihood)
    for (i in 1 : N) 
        y[i] ~ multi_normal(mu[i], S[i]);
}
generated quantities {
  array[N] real log_lik;
    for (i in 1 : N)
      log_lik[i] = multi_normal_lpdf(y[i] | mu[i], S[i]);
}

