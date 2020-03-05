/*


data {
  int N;
  int<lower=1> n_samples;
  matrix[N, n_samples] y;
  int n_pred;
}

transformed data{
  real r[N];
  for (i in 1:N){
    r[i] = log(y[i] / (15 - y[i]));
  }
}


parameters{
  real <lower = 0> sigma_epsilon;
  real <lower = 0> sigma_eta;
  real <lower = -1, upper = 1> phi;
  real delta[N];
  real D;

}

/*transformed parameters{
}

model {

  for (j in 1:n_samples) {
    delta[1, j] ~ normal(D, sigma_eta);
    for (s in 1:(N-1)){
    // vectorize
    // see https://mc-stan.org/docs/2_20/stan-users-guide/autoregressive-section.html
    y[s+1, j] ~ normal(y[s, j] + delta[s], sigma_epsilon);
    delta[s+1, j] ~ normal(D + phi * (delta[s] - D), sigma_eta);
  }

  }
  
  D ~ normal(0,1);
  phi ~ normal(0, 0.1);
  sigma_eta ~ inv_gamma(1, 1); // random values I chose
  sigma_epsilon ~ inv_gamma(1, 1); // random values I chose
}



generated quantities{
  real y_pred[n_pred];
  real delta_pred[n_pred];
  real y_post[N];
 /*  real inc_post[N];
  real inc_pred[N];

  // ========= posterior samples ========== //

  y_post[1] = normal_rng(y[1], sigma_epsilon);
  for (s in 1:(N-1)){
    y_post[s+1] = normal_rng(y_post[s] + delta[s], sigma_epsilon);
  }


  // ========= predictions ========== //
  y_pred[1] = normal_rng(y[N] + delta[N], sigma_epsilon);
  delta_pred[1] = normal_rng(D + phi * (delta[N] - D), sigma_epsilon);

  for (s in 1:(n_pred - 1)){
    y_pred[s + 1] = normal_rng(y_pred[s] + delta_pred[s], sigma_epsilon);
    delta_pred[s + 1] = normal_rng(D + phi * (delta_pred[s] - D), sigma_epsilon);
  }
}


*/

