


functions {
  
  real[] ky(
    real t,
    real[] y,      // state
    real[] theta,  // parameter
    real[] x_r,   // 
    int[]  x_i    // 
    ) {
      
      real k = theta[1];
      real dy_dt = k * y[1];
      return { dy_dt };
      
    }
    
}


data {
  int<lower=1> n;
  real t0;
  real ts[n];
  real obs[n];
}


transformed data {
  real x_r[0];
  int x_i[0];
}

// transformed data {
  //   array[0] real x_r;
  //   array[0] int x_i;
  //   array[1] real y0;      // 配列として初期値を設定
  //   y0[1] = y0_val;
  // }
  // 
  
  
  parameters {
    real<lower=0> y0;
    real<lower=0> k;
    real<lower=0> sigma; // obs-err
  }
  
  // parameters {
    //   real<lower=0> k;
    //   real<lower=0> sigma;   // 観測誤差
    // }
    // 
    // 
    transformed parameters {
      real y_hat[n, 1];
      {
        real theta[1];
        real initial_state[1];
        
        theta[1] = k;
        initial_state[1] = y0;
        
        y_hat = integrate_ode_rk45(ky, initial_state, t0, ts, theta, x_r, x_i);
        
        
      }
      
    }
    
    // transformed parameters {
      //   array[n, 1] real y_hat;  // 2次元配列として宣言
      //   {
        //     array[1] real theta;
        //     theta[1] = k;
        //     y_hat = integrate_ode_rk45(ky, y0, t0, ts, theta, x_r, x_i);
        //   }
        // }
        
        
        
        // Memo functions argument
        // array[,] real integrate_ode_rk45(
          //   function ode, 
          //   array[] real initial_state, 
          //   real initial_time, 
          //   array[] real times, 
          //   array[] real theta, 
          //   array[] real x_r, 
          //   array[] int x_i
          //   )
          
          model {
            
            y0 ~ normal(2, 2);  // 0.1付近だが柔軟
            k ~ normal(2.5, 2);
            sigma ~ exponential(0.5);
            
            for (i in 1:n) {
              
              obs ~ normal(y_hat[i, 1], sigma);
              
            }
            
          }
          
          // model {
            //   // 事前分布
            //   k ~ normal(1, 1);      // より適切な事前分布
            //   sigma ~ exponential(1);
            //   
            //   // 尤度
            //   for (i in 1:n) {
              //     obs[i] ~ normal(y_hat[i, 1], sigma);
              //   }
              // }
              