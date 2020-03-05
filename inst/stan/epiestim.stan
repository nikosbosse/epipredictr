data {
  int t;
  int <lower = 0> obs_inc[t];
  int tau;
  int num_pred;
  int <lower =0, upper =1> neg_binom_model;
}

transformed data{
  // here in this case, infectiousness[t] means all values including the value in t-1 summed up and weighted
  real infectiousness[t];
  real w[t + num_pred - 1];
  infectiousness[1] = 0;

  for (i in 1:t + num_pred -1){
    w[i] = gamma_cdf(i + 0.5, 2.706556, 0.1768991) - gamma_cdf(i  - 0.5, 2.706556, 0.1768991); 
  }

  for (s in 2:t){
    infectiousness[s] = 0;
    for (i in 1:(s - 1)){
      infectiousness[s] += obs_inc[i] * w[s - i];
    }
  }
}

parameters{
  real <lower = 0> R[t];
  real <lower = 0> phi[neg_binom_model];
}

model {

  if (neg_binom_model == 1) {
    for (s in (tau + 1):t){
      for (i in (s-tau + 1):s){
        target += neg_binomial_2_lpmf(obs_inc[i] | R[s] * infectiousness[i], phi[1]); 
        //past_incidences[i] ~ neg_binomial_2(R[s] * infectiousness[i], phi);
      }
    }  
  } else {
    for (s in (tau + 1):t){
      for (i in (s-tau + 1):s){
        target += poisson_lpmf(obs_inc[i] | R[s] * infectiousness[i]); 
        //past_incidences[i] ~ neg_binomial_2(R[s] * infectiousness[i], phi);
      }
    } 

  }
  
  for (i in 1:t){
    R[i] ~ gamma(0.15, 0.1);    
  }
  phi ~ gamma(0.05, 0.05);  
  

}

