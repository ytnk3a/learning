//
// This Stan program defines a simple model, with a
// vector of values 'y' modeled as normally distributed
// with mean 'mu' and standard deviation 'sigma'.
//
// Learn more about model development with Stan at:
//
//    http://mc-stan.org/users/interfaces/rstan.html
//    https://github.com/stan-dev/rstan/wiki/RStan-Getting-Started
//

functions {
  vector sho(real t,
             vector y,
             real theta) {
    vector[2] dydt;
    dydt[1] = y[2];
    dydt[2] = -y[1] - theta * y[2];
    return dydt;
  }
}
data {
  int<lower=1> T;
  array[T] vector[2] y;  // 各時点での2次元ベクトル
  real t0;
  array[T] real ts;      // 各時点の時刻
}
parameters {
  vector[2] y0;
  vector<lower=0>[2] sigma;
  real theta;
}
model {
  array[T] vector[2] mu = ode_rk45(sho, y0, t0, ts, theta);
  
  sigma ~ normal(0, 2.5);
  theta ~ std_normal();
  y0 ~ std_normal();
  
  for (t in 1:T) {
    y[t] ~ normal(mu[t], sigma);
  }
}