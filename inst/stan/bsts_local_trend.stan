data {
  int N;
  real y[N];
  int n_pred;
  real prior_var_phi;
  int <lower=1> length_local_trend;
}

transformed data{
  real r[N];
  vector [length_local_trend] x;
  for (i in 1:N){
    r[i] = log(y[i] / (15 - y[i]));
  }
  for (i in 1:length_local_trend) {
    x[i] = i;
  }
}


parameters{
  real <lower = 0> sigma_epsilon;
  real <lower = 0> sigma_eta;
  real <lower = -1, upper = 1> phi;
  real delta[N];
  real D[N];
  real intercept[N];
  real sd_trend;

}

/*transformed parameters{
}
*/
model {
  for (s in 1:(N-1)){
    // vectorize
    // see https://mc-stan.org/docs/2_20/stan-users-guide/autoregressive-section.html
    y[s+1] ~ normal(y[s] + delta[s], sigma_epsilon);
    delta[s+1] ~ normal(D[s] + phi * (delta[s] - D[s]), sigma_eta);
  }
  delta[1] ~ normal(D, sigma_eta);
  
  for (s in 1:(length_local_trend - 1)) {
    D[s] ~ normal(0,1);
    intercept[s] ~ normal(0,1);
  }

  for (s in length_local_trend:N) {
    y[(s - length_local_trend + 1):s] ~ normal(intercept[s] + x * D[s], sd_trend);
  }


  phi ~ normal(0, prior_var_phi);
  sigma_eta ~ exponential(3); // random values I chose
  sigma_epsilon ~ exponential(3); // random values I chose
}



generated quantities{
  real y_pred[n_pred];
  real delta_pred[n_pred];
  real y_post[N];
/*  real inc_post[N];
  real inc_pred[N];*/

  real phi_prior;
  real sigma_eta_prior;
  real sigma_epsilon_prior;


  // ========= prior samples ========== //
  phi_prior = normal_rng(0, prior_var_phi);
  sigma_eta_prior = exponential_rng(3); // random values I chose
  sigma_epsilon_prior = exponential_rng(3); // random values I chose

  // ========= posterior samples ========== //

  y_post[1] = normal_rng(y[1], sigma_epsilon);
  for (s in 1:(N-1)){
    y_post[s+1] = normal_rng(y_post[s] + delta[s], sigma_epsilon);
  }


  // ========= predictions ========== //
  y_pred[1] = normal_rng(y[N] + delta[N], sigma_epsilon);
  delta_pred[1] = normal_rng(D[N] + phi * (delta[N] - D[N]), sigma_epsilon);

  for (s in 1:(n_pred - 1)){
    y_pred[s + 1] = normal_rng(y_pred[s] + delta_pred[s], sigma_epsilon);
    delta_pred[s + 1] = normal_rng(D[N] + phi * (delta_pred[s] - D[N]), sigma_epsilon);
  }
}

