
data {
  int<lower=1> N; 
  int<lower=1> J; 
  int<lower=1, upper=J> county[N];
  vector[J] u;
  vector[N] x;
  vector[N] y;
} 

parameters {
  vector[2] Beta[J];

  vector[2] alpha;
  vector[2] beta;

  real<lower=0,upper=100> sigma_a;
  real<lower=0,upper=100> sigma_b;
  real<lower=0,upper=100> sigma_y;

  real<lower=-1,upper=1> rho;
} 

transformed parameters {
  vector[N] y_hat;
  vector[2] Beta_hat[J];
  cov_matrix[2] Sigma_county;
  vector[J] a;
  vector[J] b;

  for (j in 1:J) {
    Beta_hat[j, 1] <- alpha[1] + alpha[2] * u[j];
    Beta_hat[j, 2] <- beta[1] + beta[2] * u[j];
    a[j] <- Beta[j, 1];
    b[j] <- Beta[j, 2];
  }

  Sigma_county[1, 1] <- sigma_a;
  Sigma_county[2, 2] <- sigma_b;
  Sigma_county[1, 2] <- sigma_a * sigma_b * rho;
  Sigma_county[2, 1] <- Sigma_county[1, 2];


  for (n in 1:N)
    y_hat[n] <- a[county[n]] + b[county[n]] * x[n];
}

model {
  for (j in 1:J)
    Beta[j] ~ multi_normal(Beta_hat[j], Sigma_county);
  
  y ~ normal(y_hat, sigma_y);

  // The only purpose of correlated intercepts and slopes is to get a
  // matrix parameter for tests (Sigma). But need tight prior on rho for
  // convergence
  rho ~ normal(0, 0.1);
}

generated quantities {
  vector[N] y_rep;
  for (n in 1:N)
    y_rep[n] <- normal_rng(y_hat[n], sigma_y);
}
