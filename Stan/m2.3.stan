// Simple Bivariate SSM using gaussian_dlm_obs (for some reason it is slower than explicit KF)

data {
  int<lower=0> N;
  matrix[2, N] y;
  vector[2] m0;
  matrix[2, 2] P0;

}
transformed data {
  int D = 2;
  matrix[D, D] I;
  I = diag_matrix(rep_vector(1, D));
}
parameters {
  cov_matrix[D] Q;
  cov_matrix[D] R;
  matrix[D, D] Z;

}
transformed parameters {

}
model {
  Z[1,] ~ normal(0,1);
  Z[2,] ~ normal(0,1);
  Q ~ inv_wishart(2, diag_matrix(rep_vector(1, D)));
  R ~ inv_wishart(2, diag_matrix(rep_vector(1, D)));
  //for (i in 1 : N)
  y ~ gaussian_dlm_obs(I, Z, R, Q, m0, P0);
}
generated quantities {

}

