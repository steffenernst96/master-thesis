
// Learn more about model development with Stan at:
//
//    http://mc-stan.org/users/interfaces/rstan.html
//    https://github.com/stan-dev/rstan/wiki/RStan-Getting-Started
//

data {
  int<lower=0> N;                 
  real<lower=0> rt[N];    
  int<lower=0, upper=1> correct[N];
  int<lower=0, upper=3> difficulty[N];
  int<lower=0, upper=1> condition[N];

}

parameters {
  real<lower=0> v[4]; // array mit 4 drift rates
  real<lower=0> a[2]; // array mit 2 drift rates
  real<lower=0> ndt;
}

model {
  // Priors
  v ~ gamma(2.5, 2.0); //gleicher Prior für alle 4 drift rates (?)
  a ~ gamma(4.0, 3.0); //gleicher Prior für 2 boundaries (?)
  ndt ~ gamma(1.5, 5.0);
  
  for (n in 1:N) {
    
    int d = difficulty[n];
    int c = condition[n];
    
    if (correct[n] == 1) {
      rt[n] ~ wiener(a[c+1], ndt, 0.5, v[d+1]); // difficulty bestimmt, welches v 
    } 
    else {
      rt[n] ~ wiener(a[c+1], ndt, 0.5, -v[d+1]); // negativ, weil v immer "positiv" ist
    }      
  }
}


