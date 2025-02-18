data {
  // Define variables in data

  int<lower=1> N; // Number of observations (an integer)
  int K; // Possible Responses (Mainly for Multinomial Regressions)
  int I; // Number of Columns/Items (dependent on type of model)
  matrix[N, I] X; // Matrix of all the predictors
  vector<lower=0, upper=1>[N] Y; // Outcome variable
}
parameters {
  real A; //Intercept for model
  vector [I] B; // Coefficients for predictors
}
transformed parameters {
  //logistic reg formula
  real eta;
  real pie;
  vector[N] log_lik;

  for (n in 1:N){
  eta = A + X[n] * B;
  pie = inv_logit(eta);
  log_lik[n] = Y[n] * log(pie) + (1 - Y[n]) * log(1 - pie);
  }
}
model {
  //priors 
  A ~ normal(0, 100);
  B ~ normal(0, 10);

  target += sum(log_lik);
}
generated quantities {
  vector[N] log_lik_rep; 
  
  for (n in 1:N){
    log_lik_rep[n] = Y[n] * log(pie) + (1 - Y[n]) * log(1 - pie);
  }
}




//data {
//  int<lower=1> N;                     // Number of observations
//  int<lower=1> I;                     // Number of predictor variables (after dummy coding)
//  int<lower=1, upper=4> Y[N]; // Outcome variable with 4 levels (1, 2, 3, 4)
//  matrix[N, I] X;                     // Predictor matrix with dummy-coded variables
//}
//parameters {
//  matrix[4, I] beta;                   // Coefficients for each level of favorite_abcd
//}
//model {
//  // Priors
//  for (k in 1:4)
//    beta[k] ~ normal(0, 1);            // Prior for coefficients
//  // Likelihood
//  for (n in 1:N)
//    Y[n] ~ categorical_logit(beta * X[n]');  // Multinomial logit model
//}
//generated quantities {
//  matrix[N, 4] log_lik;  // Log-likelihood for each data point and category
//
//  for (n in 1:N) {
//    vector[4] logits = beta * X[n]';
//    log_lik[n] = categorical_logit_lpmf(favorite_abcd[n] | logits);
//  }
//}