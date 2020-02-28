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
  vector[N] y_fit;

  for (i in 1:N) {
    y_fit[i] = normal_rng(intercept + beta * i, sigma);
  }   
  
  for (i in 1:num_pred) {
    y_pred[i] = normal_rng(intercept + beta * (N + i), sigma);
  } 
  
}

