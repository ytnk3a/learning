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

  real[] virus_dynamics(
    real t,
    real[] y,
    real[] theta,
    real[] x_r,
    int[] x_i
  ) {
    
    real d = theta[1];
    real beta  = theta[2];
    real delta = theta[3];
    real p = theta[4];
    real c = theta[5];

    real dydt[3];
    dydt[1] = -d * y[1] - beta * y[1] * y[3];
    dydt[2] = beta * y[1] * y[3] - delta * y[2];
    dydt[3] = p * y[2] - c * y[3];
    return dydt;
  }
  
}

data {
  int<lower=1> num_data_points;
  int<lower=1> num_model_parameters;
  int<lower=1> num_initial_states;
  
  real time_start;
  
  real time_observed[num_data_points];
  real TC_observed[num_data_points];
  real IC_observed[num_data_points];
  real VL_observed[num_data_points];
}

transformed data {
  real x_r[0];
  int x_i[0];
}

parameters {
   // model parameters
   real<lower=log(1e-11), upper=log(1e-10)> beta;
   real<lower=log(1e+4), upper=log(1e+5)> p;
   real<lower=log(1e+4), upper=log(1e+5)> V0L;

   // standard deviation of error model
   real<lower=0, upper=100> sigma;
}

transformed parameters {
  real d = 0.057;
  real delta = 0.057 + 1.75;
  real c = 1.93 + 0.039;
  real T0C = log(6.46 * (10.0^6.0));
  real I0C = log(10.0^-1.0);
  
  real theta[5];
  real Y0[num_initial_states];
  real<lower=0, upper=1e+20> y_hat[num_data_points, 3];
  
  theta[1] = d;
  theta[2] = exp(beta);
  theta[3] = delta;
  theta[4] = exp(p);
  theta[5] = c;
  Y0[1] = exp(T0C);
  Y0[2] = exp(I0C);
  Y0[3] = exp(V0L);

  y_hat = integrate_ode_rk45(
    virus_dynamics, 
    Y0, 
    time_start, 
    time_observed, 
    theta, 
    x_r, 
    x_i
    );
  
}

model {
  
  // beta ~ uniform(log(1e-11), log(1e-10)); //8.61*(10.0^-11.0)
  // p ~ uniform(log(1e+4), log(1e+5)); //3.26*(10.0^4.0)
  // V0L ~ uniform(log(1e+4), log(1e+5)); //5.0*(10.0^4.0)
  
   beta ~ normal(log(5e-11), 1); //8.61*(10.0^-11.0)
   p ~ normal(log(5e+4), 1); //3.26*(10.0^4.0)
   V0L ~ normal(log(1e+5), 1); //5.0*(10.0^4.0)
   
  
  for (i in 1:num_data_points) {
    TC_observed[i] ~ lognormal(log(y_hat[i, 1]), sigma);
    IC_observed[i] ~ lognormal(log(y_hat[i, 2]), sigma);
    VL_observed[i] ~ lognormal(log(y_hat[i, 3]), sigma);
    
  }
  
  sigma ~ normal(0.5,0.5);
}

