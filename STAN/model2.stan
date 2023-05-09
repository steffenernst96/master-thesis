
data {
  int<lower=0> N;                 
  real<lower=0> rt[N];    
  int<lower=0, upper=1> correct[N];
  int<lower=0, upper=3> difficulty[N];
  int<lower=0, upper=1> condition[N];
  int<lower=0, upper=4> condition2[N];
  
}

parameters {
  real<lower=0> v[8]; // array mit 4 drift rates
  real<lower=0.5> a[2]; // array mit 2 boundaries
  real<lower=0.1> ndt;
}

  
model {
    // Priors
    v ~ normal(0, 2);
    a ~ normal(0.5, 1.5);
    ndt ~ normal(0.1, 0.2);

    for (n in 1:N) {
      
      if (correct[n] == 1) {
        rt[n] ~ wiener(a[condition[n] + 1], ndt, 0.5, v[difficulty[n] + condition2[n] + 1]); // difficulty bestimmt, welches v 
      } 
      else {
        rt[n] ~ wiener(a[condition[n] + 1], ndt, 0.5, -v[difficulty[n] + condition2[n] + 1]); // negativ, weil v immer "positiv" ist
      }   
    }
  }


generated quantities {
  vector[N] log_lik;
  for (n in 1:N) {
    if (correct[n] == 1) {
      log_lik[n] = wiener_lpdf(rt[n] | a[condition[n] + 1],
                               ndt,
                               0.5,
                               v[difficulty[n] + condition2[n] + 1]);
    }
    else {
      log_lik[n] = wiener_lpdf(rt[n] | a[condition[n] + 1],
                         ndt,
                         0.5,
                         -v[difficulty[n] + condition2[n] + 1]);

    }
  }
}
