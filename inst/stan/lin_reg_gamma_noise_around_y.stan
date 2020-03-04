/*//attempt failed

functions {
  vector my_generate_gamma_rng(vector alpha, vector beta) {
    return to_vector(gamma_rng(alpha, beta));
  }
}


data {
  int<lower=1> N;
  vector[N] x;
  vector[N] alpha_gamma;
  vector[N] beta_gamma;
  int<lower = 0> num_pred;
}
parameters {
  real intercept;
  real beta;
  real<lower=0> sigma;
}

transformed parameters {
    vector[N] y_gamma = my_generate_gamma_rng(alpha_gamma, beta_gamma);
}

model {
  y_gamma ~ normal(intercept + beta * x, sigma);
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

*/

