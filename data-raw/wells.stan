
data {
  int<lower=1> N; 
  int y[N];
  vector[N] c_dist100;
  vector[N] c_arsenic;
} 

parameters {
  vector[4] beta;
} 

transformed parameters {
  vector[N] y_hat;

  for (n in 1:N)
    y_hat[n] <- beta[1] +
      beta[2] * c_dist100[n] +
      beta[3] * c_arsenic[n] +
      beta[4] * c_arsenic[n] * c_dist100[n];
}

model {
  y ~ bernoulli_logit(y_hat);
}
