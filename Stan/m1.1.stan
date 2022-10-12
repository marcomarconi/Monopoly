data {
  int<lower=0> N;
  vector[N] cpi;
  vector[N] ir;
  real cpi_target;
  real m0;
  real P0;
  int likelihood;
}
transformed data {
  int D = 12;
}
parameters {
  real<lower=0> sigma_ir;
  real<lower=0> sigma_obs;
  real<lower=0> beta1;
  real<lower=0> beta2;
  real<lower=0, upper=1> ar_ir;
}
transformed parameters {
  vector[N] m;
  vector[N] m_pred;
  vector[N] P;
  vector[N] P_pred;
  vector[N] S;
  vector[N] mu;
  vector[D] lp;
  
  
  real Q;
  real R;
  {
    row_vector[2] C;
    real Z;
    real K;
    vector[N] v;
    int ss;
    Q = sigma_ir ^ 2;
    R = sigma_obs ^ 2;

    lp = rep_vector(log(1.0/D), D);

    for(d in 1:D) {
      ss =  d+2;
      m[1:ss] = rep_vector(m0, ss);
      P[1:ss] = rep_vector(P0, ss);
      m_pred[1:ss] = rep_vector(m0, ss);
      P_pred[1:ss] = rep_vector(P0, ss);
      for (t in ss : N) {
        C = (cpi[t] > cpi_target ? [beta1, beta2] : [0, 0]);
        Z = (cpi[t] > cpi_target ? 1 : ar_ir);
        if (t > ss) {
          m_pred[t] = Z * m[t - 1] + C * [cpi[t-d] - cpi_target, cpi[t-d] - cpi[t-d-1]]'; 
          P_pred[t] = Z * P[t - 1] * Z + Q;
        }
        v[t] = ir[t] - m_pred[t];
        S[t] = P_pred[t] + R;
        K = P_pred[t] / S[t];
        m[t] = m_pred[t] + K * v[t];
        P[t] = P_pred[t] - K*S[t]*K;
        mu[t] = m_pred[t];
        lp[d] = lp[d] + normal_lpdf(ir[t] | mu[t], sqrt(S[t]));
      }
    }
  }
}
model {
  sigma_ir ~ exponential(1);
  sigma_obs ~ exponential(1);
  beta1 ~ normal(0, 0.1);
  beta2 ~ normal(0, 1);
  ar_ir ~ beta(2, 1);
  if (likelihood) 
    target += log_sum_exp(lp);
    //ir ~ normal(mu, sqrt(S));
}
generated quantities {
  array[N] real log_lik;
  vector[D] pState;
  pState = exp(lp - log_sum_exp(lp));
  //for (i in 1 : N)
  //  log_lik[i] = normal_lpdf(ir[i] | mu[i], sqrt(S[i]));
      
}

