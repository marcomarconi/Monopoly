data {
  int<lower=0> N;
  array[N] vector[2] y;
  vector[2] m0;
  matrix[2, 2] P0;
  int family;
  int likelihood;
}
transformed data {
  matrix[2, 2] I;
  I = diag_matrix([1, 1]');
}
parameters {
  real<lower=0> sigma_y;
  real<lower=0, upper=sigma_y> sigma_x;
  real<lower=0> trend;
  real<lower=0> spread;
  array[family == 1] real<lower=0> nu;
}
transformed parameters {
  array[N] vector[2] m;
  array[N] vector[2] m_pred;
  array[N] matrix[2, 2] P;
  array[N] matrix[2, 2] P_pred;
  array[N] matrix[2, 2] S;
  array[N] vector[2] mu;
  matrix[2, 2] Q;
  matrix[2, 2] R;
  {
    matrix[2, 2] Z;
    matrix[2, 2] Zt;
    matrix[2, 2] H;
    matrix[2, 2] Ht;
    vector[2] U;
    vector[2] A;
    array[N] matrix[2, 2] K;
    array[N] vector[2] v;
    
    Z = [[1, 0], [1, 0]];
    Zt = Z';
    H = diag_matrix([1, 1]');
    Ht = H';
    U = [trend, spread]';
    A = [0, 0]';
    
    m_pred[1] = m0;
    P_pred[1] = P0;
    Q = diag_matrix([sigma_x ^ 2, 0]');
    R = diag_matrix([sigma_y ^ 2, sigma_y ^ 2]');
    for (t in 1 : N) {
      if (t > 1) {
        m_pred[t] = Z * m[t - 1] + U;
        P_pred[t] = Z * P[t - 1] * Zt + Q;
      }
      v[t] = y[t,  : ] - H * m_pred[t] - A;
      S[t] = (H * P_pred[t] * Ht + R);
      K[t] = (P_pred[t] * Ht) / S[t];
      m[t] = m_pred[t] + K[t] * v[t];
      P[t] = P_pred[t] - P_pred[t] * Ht * K[t]';
      mu[t] = H * m_pred[t] + A;
    }
  }
}
model {
  sigma_y ~ exponential(1);
  sigma_x ~ exponential(1);
  trend ~ exponential(100);
  spread ~ gamma(log(42), 1);
  nu ~ gamma(3, 2);
  if (likelihood) {
    if (family == 0) {
      for (i in 1 : N) 
        y[i] ~ multi_normal(mu[i], S[i]);
    } else if (family == 1) {
      for (i in 1 : N) 
        y[i] ~ multi_student_t(nu[1], mu[i], S[i]);
    }
  }
}
generated quantities {
  array[N] real log_lik;
  if (family == 0) {
    for (i in 1 : N) 
      log_lik[i] = multi_normal_lpdf(y[i] | mu[i], S[i]);
  } else if (family == 1) {
    for (i in 1 : N) 
      log_lik[i] = multi_student_t_lpdf(y[i] | nu[1], mu[i], S[i]);
  }
}

