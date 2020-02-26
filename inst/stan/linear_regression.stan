data {
  int<lower=1> N;
  vector[N] x;
  vector[N] y;
  int<lower = 1> num_pred;
}
parameters {
  real intercept;
  real beta;
  real<lower=0> sigma;
}
model {
  // ... priors, etc.

  y ~ normal(intercept + beta * x, sigma);
}

generated quantities {
  vector[num_pred] y_pred;
  vector[num_pred] x_pred;
  
  for (i in 1:num_pred) {
    x_pred[i] = N + i;
    y_pred[i] = normal_rng(intercept + beta * x_pred[i], sigma);
  } 
  
}

