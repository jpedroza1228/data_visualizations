data {
  int<lower=1> J; // # of respondents/students j
  int<lower=1> I; // # of items i
  int<lower=1> K; // # of attributes k
  matrix[J, I] Y; // response matrix x
  matrix[I, K] Q; //Q matrix Q
}
parameters {
  array[I] real<lower=0, upper=1> guess;
  array[I] real<lower=0, upper=1> no_guess;
  real<lower=0, upper=1> lambda1;
  real<lower=0, upper=1> lambda20;
  real<lower=0, upper=1> lambda21;
  real<lower=0, upper=1> lambda30;
  real<lower=0, upper=1> lambda31;
  real<lower=0, upper=1> lambda40;
  real<lower=0, upper=1> lambda41;
  real<lower=0, upper=1> lambda50;
  real<lower=0, upper=1> lambda51;
}
transformed parameters {
  array[J] real theta1;
  array[J] real theta2;
  array[J] real theta3;
  array[J] real theta4;
  array[J] real theta5;
  matrix[J, I] delta;
  array[I] real pi;
  matrix[J, I] log_lik;
  vector[I] ps_i = rep_vector(0, I); // Initialize ps_i with zeros
  array[J] real ps_j;
  
  for (j in 1 : J) {
    theta1[j] = lambda1 * 1 + (1 - lambda1) * (1 - 1);
    theta2[j] = theta1[j] * lambda21 + (1 - theta1[j]) * lambda20;
    theta3[j] = theta2[j] * lambda31 + (1 - theta2[j]) * lambda30;
    theta4[j] = theta3[j] * lambda41 + (1 - theta3[j]) * lambda40;
    theta5[j] = theta4[j] * lambda51 + (1 - theta4[j]) * lambda50;
    
    for (i in 1 : I) {
      delta[j, i] = pow(theta1[j], Q[i, 1]) * pow(theta2[j], Q[i, 2])
                    * pow(theta3[j], Q[i, 3]) * pow(theta4[j], Q[i, 4])
                    * pow(theta5[j], Q[i, 5]);
      
      pi[i] = pow(guess[i], delta[j, i]) * pow(no_guess[i], 1 - delta[j, i]);
      
      log_lik[j, i] = X[j, i] * log(pi[i]) + (1 - X[j, i]) * log(1 - pi[i]);
      
      // Accumulate the values of log_lik over items
      ps_i[i] = ps_i[i] + log_lik[j, i];
    }
    // Sum up the log_lik values for each student
    ps_j[j] = sum(log_lik[j]);
  }
}