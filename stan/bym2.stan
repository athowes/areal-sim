data {
  real<lower=0, upper=1> var_pi;
  real scaling_factor;
  int<lower=0> n;
  int<lower=1> n_edges;
  int<lower=1, upper=n> node1[n_edges];
  int<lower=1, upper=n> node2[n_edges];
}

parameters {
  vector[n] u; 
  vector[n] v;
}

transformed parameters {
  vector[n] phi = sqrt(1 - var_pi) * v + sqrt(var_pi / scaling_factor) * u;   
}

model {
  target += -0.5 * dot_self(u[node1] - u[node2]);
  sum(u) ~ normal(0, 0.001 * n);
  
  v ~ normal(0, 1);
}

