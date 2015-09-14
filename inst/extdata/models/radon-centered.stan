
data {
  int N;
  int P_X;
  int P_U;
  int J;

  real y[N];
  int county[N];
  row_vector[P_X] X[N];
  vector[P_U] U[J];
}

parameters {
  vector[P_X] Beta[J];
  real<lower=0> sigma;

  matrix[P_X, P_U] Gamma;
  vector<lower=0>[P_X] sigma_Beta;
  corr_matrix[P_X] Omega;
}

transformed parameters {
  cov_matrix[P_X] Sigma_Beta;
  vector[N] mu_y;

  Sigma_Beta <- quad_form_diag(Omega, sigma_Beta);

  for (n in 1:N) {
    mu_y[n] <- X[n] * Beta[county[n]];
  }
}

model {
  y ~ normal(mu_y, sigma);

  for (j in 1:J) {
    Beta[j] ~ multi_normal(Gamma * U[j], Sigma_Beta);
  }

  sigma ~ cauchy(0, 2.5);
  sigma_Beta ~ cauchy(0, 2.5);
  Omega ~ lkj_corr(2);
}

generated quantities {
  vector[N] y_rep;
  for (n in 1:N)
    y_rep[n] <- normal_rng(mu_y[n], sigma);
}
