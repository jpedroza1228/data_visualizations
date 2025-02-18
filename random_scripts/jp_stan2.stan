data {
  int<lower=1> N; // Number of observations
  int I; // Number of Columns/Items (dependent on type of model)
  int K; // Possible Responses (Mainly for Multinomial Regressions)
  vector[N] Y; // Outcome variable
  matrix[N, I] X; // 
}
parameters {

  real a_; //intercept for 
  real a_; //intercept for 
  real a_; //intercept for 
  real a_; //intercept for 
  real a_; //intercept for 
  real a_; //intercept for 
  real a_; //intercept for 
  real<lower=0> ; //main effect of
  real<lower=0> ; //main effect of
  real<lower=0> ; //main effect of
  real<lower=0> ; //main effect of
  real<lower=0> ; //main effect of
  real<lower=0> ; //main effect of
  real<lower=0> ; //main effect of
  real<lower=0> ; //main effect of
  real<lower=0> ; //main effect of
  real<lower=0> ; //main effect of
  real<lower=0> ; //main effect of
  real<lower=0> ; //main effect of
  real<lower=0> ; //main effect of
  real<lower=0> ; //main effect of
  real<lower=0> ; //main effect of
  real<lower=0> ; //main effect of
  real<lower=0> ; //main effect of
  real<lower=0> ; //main effect of
  real<lower=0> ; //main effect of
  real<lower=0> ; //main effect of
  real<lower=0> ; //main effect of
  real<lower=0> ; //main effect of
  real<lower=0> ; //main effect of
  real<lower=0> ; //main effect of
  real<lower=0> ; //main effect of
  real<lower=0> ; //main effect of


  real ;
  real b_female_latte;
  real b_gen_other_latte;
  real b_thirty544_latte;
  real b_over44_latte;
  real b_under24_latte;

  real ;
  real b_female_drip;
  real b_gen_other_drip;
  real b_thirty544_drip;
  real b_over44_drip;
  real b_under24_drip;

  real a_other;
  real b_female_other;
  real b_gen_other_other;
  real b_thirty544_other;
  real b_over44_other;
  real b_under24_other;

  real a_cappuccino;
  real b_female_cappuccino;
  real b_gen_other_cappuccino;
  real b_thirty544_cappuccino;
  real b_over44_cappuccino;
  real b_under24_cappuccino;

  real a_espresso;
  real b_female_espresso;
  real b_gen_other_espresso;
  real b_thirty544_espresso;
  real b_over44_espresso;
  real b_under24_espresso;

  real a_cortado;
  real b_female_cortado;
  real b_gen_other_cortado;
  real b_thirty544_cortado;
  real b_over44_cortado;
  real b_under24_cortado;

  real a_americano;
  real b_female_americano;
  real b_gen_other_americano;
  real b_thirty544_americano;
  real b_over44_americano;
  real b_under24_americano;


  real a_cup_1orless;
  real b_female_cup_1orless;
  real b_gen_other_cup_1orless;
  real b_thirty544_cup_1orless;
  real b_over44_cup_1orless;
  real b_under24_cup_1orless;

  real a_cup_3ormore;
  real b_female_cup_3ormore;
  real b_gen_other_cup_3ormore;
  real b_thirty544_cup_3ormore;
  real b_over44_cup_3ormore;
  real b_under24_cup_3ormore;


  real a_expert7;
  real b_cup_1orless_expert7;
  real b_cup_3ormore_expert7;
  real b_americano_expert7;
  real b_cortado_expert7;
  real b_espresso_expert7;
  real b_cappuccino_expert7;
  real b_drip_expert7;
  real b_latte_expert7;
  real b_other_expert7;

  real a_expert6;
  real b_cup_1orless_expert6;
  real b_cup_3ormore_expert6;
  real b_americano_expert6;
  real b_cortado_expert6;
  real b_espresso_expert6;
  real b_cappuccino_expert6;
  real b_drip_expert6;
  real b_latte_expert6;
  real b_other_expert6;

  real a_expert8;
  real b_cup_1orless_expert8;
  real b_cup_3ormore_expert8;
  real b_americano_expert8;
  real b_cortado_expert8;
  real b_espresso_expert8;
  real b_cappuccino_expert8;
  real b_drip_expert8;
  real b_latte_expert8;
  real b_other_expert8;

  real a_expert4;
  real b_cup_1orless_expert4;
  real b_cup_3ormore_expert4;
  real b_americano_expert4;
  real b_cortado_expert4;
  real b_espresso_expert4;
  real b_cappuccino_expert4;
  real b_drip_expert4;
  real b_latte_expert4;
  real b_other_expert4;
  
  real a_expert3;
  real b_cup_1orless_expert3;
  real b_cup_3ormore_expert3;
  real b_americano_expert3;
  real b_cortado_expert3;
  real b_espresso_expert3;
  real b_cappuccino_expert3;
  real b_drip_expert3;
  real b_latte_expert3;
  real b_other_expert3;

  real a_expert2;
  real b_cup_1orless_expert2;
  real b_cup_3ormore_expert2;
  real b_americano_expert2;
  real b_cortado_expert2;
  real b_espresso_expert2;
  real b_cappuccino_expert2;
  real b_drip_expert2;
  real b_latte_expert2;
  real b_other_expert2;

  real a_expert1;
  real b_cup_1orless_expert1;
  real b_cup_3ormore_expert1;
  real b_americano_expert1;
  real b_cortado_expert1;
  real b_espresso_expert1;
  real b_cappuccino_expert1;
  real b_drip_expert1;
  real b_latte_expert1;
  real b_other_expert1;

  real a_expert9;
  real b_cup_1orless_expert9;
  real b_cup_3ormore_expert9;
  real b_americano_expert9;
  real b_cortado_expert9;
  real b_espresso_expert9;
  real b_cappuccino_expert9;
  real b_drip_expert9;
  real b_latte_expert9;
  real b_other_expert9;

  real a_expert10;
  real b_cup_1orless_expert10;
  real b_cup_3ormore_expert10;
  real b_americano_expert10;
  real b_cortado_expert10;
  real b_espresso_expert10;
  real b_cappuccino_expert10;
  real b_drip_expert10;
  real b_latte_expert10;
  real b_other_expert10;


  real a_roast_medium;
  real b_expert1_roast_medium;
  real b_expert2_roast_medium;
  real b_expert3_roast_medium;
  real b_expert4_roast_medium;
  real b_expert6_roast_medium;
  real b_expert7_roast_medium;
  real b_expert8_roast_medium;
  real b_expert9_roast_medium;
  real b_expert10_roast_medium;

  real a_roast_dark;
  real b_expert1_roast_dark;
  real b_expert2_roast_dark;
  real b_expert3_roast_dark;
  real b_expert4_roast_dark;
  real b_expert6_roast_dark;
  real b_expert7_roast_dark;
  real b_expert8_roast_dark;
  real b_expert9_roast_dark;
  real b_expert10_roast_dark;
}
transformed parameters {
  
  vector[N] theta_latte;
  vector[N] theta_drip;
  vector[N] theta_other;
  vector[N] theta_cappuccino;
  vector[N] theta_espresso;
  vector[N] theta_cortado;
  vector[N] theta_americano;

  vector[N] theta_cup_1orless;
  vector[N] theta_cup_3ormore;

  vector[N] theta_expert1;
  vector[N] theta_expert2;
  vector[N] theta_expert3;
  vector[N] theta_expert4;
  vector[N] theta_expert6;
  vector[N] theta_expert7;
  vector[N] theta_expert8;
  vector[N] theta_expert9;
  vector[N] theta_expert10;

  vector[N] theta_roast_medium;
  vector[N] theta_roast_dark;

  // Need to remove reference group for all analyses for quicker optimization and more accurate posterior
  for(n in 1:N){
    theta_latte[n] =  inv_logit(
      a_latte + 
      b_female_latte * female[n] + 
      b_gen_other_latte * gen_other[n] + 
      b_thirty544_latte * thirty544[n] + 
      b_over44_latte * over44[n] + 
      b_under24_latte * under24[n] 
    );

    theta_drip[n] = inv_logit(
      a_drip + 
      b_female_drip * female[n] +
      b_gen_other_drip * gen_other[n] +
      b_thirty544_drip * thirty544[n] +
      b_over44_drip * over44[n] +
      b_under24_drip * under24[n]
    );

    theta_other[n] = inv_logit(
      a_other +
      b_female_other * female[n] +
      b_gen_other_other * gen_other[n] +
      b_thirty544_other * thirty544[n] +
      b_over44_other * over44[n] +
      b_under24_other * under24[n]
    );

    theta_cappuccino[n] = inv_logit(
      a_cappuccino +
      b_female_cappuccino * female[n] +
      b_gen_other_cappuccino * gen_other[n] +
      b_thirty544_cappuccino * thirty544[n] +
      b_over44_cappuccino * over44[n] +
      b_under24_cappuccino * under24[n]
    );

    theta_espresso[n] = inv_logit(
      a_espresso +
      b_female_espresso * female[n] +
      b_gen_other_espresso * gen_other[n] +
      b_thirty544_espresso * thirty544[n] +
      b_over44_espresso * over44[n] +
      b_under24_espresso * under24[n]
    );

    theta_cortado[n] = inv_logit(
      a_cortado +
      b_female_cortado * female[n] +
      b_gen_other_cortado * gen_other[n] +
      b_thirty544_cortado * thirty544[n] +
      b_over44_cortado * over44[n] +
      b_under24_cortado * under24[n]
    );

    theta_americano[n] = inv_logit(
      a_americano +
      b_female_americano * female[n] +
      b_gen_other_americano * gen_other[n] +
      b_thirty544_americano * thirty544[n] +
      b_over44_americano * over44[n] +
      b_under24_americano * under24[n]
    );

    theta_cup_1orless[n] = inv_logit(
      a_cup_1orless +
      b_female_cup_1orless * female[n] +
      b_gen_other_cup_1orless * gen_other[n] +
      b_thirty544_cup_1orless * thirty544[n] +
      b_over44_cup_1orless * under24[n] +
      b_under24_cup_1orless * over44[n]
    );

    theta_cup_3ormore[n] = inv_logit(
      a_cup_3ormore +
      b_female_cup_3ormore * female[n] +
      b_gen_other_cup_3ormore * gen_other[n] +
      b_thirty544_cup_3ormore * thirty544[n] +
      b_over44_cup_3ormore * over44[n] +
      b_under24_cup_3ormore * under24[n]
    );
  }
  
  for(n in 1:N){
    theta_expert7[n] = inv_logit(
      a_expert7 +
      b_cup_1orless_expert7 * theta_latte[n] +
      b_cup_3ormore_expert7 * theta_drip[n] +
      b_americano_expert7 * theta_other[n] +
      b_cortado_expert7 * theta_cappuccino[n] +
      b_espresso_expert7 * theta_espresso[n] +
      b_cappuccino_expert7 * theta_cortado[n] +
      b_drip_expert7 * theta_americano[n] +
      b_latte_expert7 * theta_cup_1orless[n] +
      b_other_expert7 * theta_cup_3ormore[n]
    );
    
    theta_expert6[n] = inv_logit(
      a_expert6 +
      b_cup_1orless_expert6 * theta_latte[n] +
      b_cup_3ormore_expert6 * theta_drip[n] +
      b_americano_expert6 * theta_other[n] +
      b_cortado_expert6 * theta_cappuccino[n] +
      b_espresso_expert6 * theta_espresso[n] +
      b_cappuccino_expert6 * theta_cortado[n] +
      b_drip_expert6 * theta_americano[n] +
      b_latte_expert6 * theta_cup_1orless[n] +
      b_other_expert6 * theta_cup_3ormore[n]
    );
    
    theta_expert8[n] = inv_logit(
      a_expert8 +
      b_cup_1orless_expert8 * theta_latte[n] +
      b_cup_3ormore_expert8 * theta_drip[n] +
      b_americano_expert8 * theta_other[n] +
      b_cortado_expert8 * theta_cappuccino[n] +
      b_espresso_expert8 * theta_espresso[n] +
      b_cappuccino_expert8 * theta_cortado[n] +
      b_drip_expert8 * theta_americano[n] +
      b_latte_expert8 * theta_cup_1orless[n] +
      b_other_expert8 * theta_cup_3ormore[n]
    );
    
    theta_expert4[n] = inv_logit(
      a_expert4 +
      b_cup_1orless_expert4 * theta_latte[n] +
      b_cup_3ormore_expert4 * theta_drip[n] +
      b_americano_expert4 * theta_other[n] +
      b_cortado_expert4 * theta_cappuccino[n] +
      b_espresso_expert4 * theta_espresso[n] +
      b_cappuccino_expert4 * theta_cortado[n] +
      b_drip_expert4 * theta_americano[n] +
      b_latte_expert4 * theta_cup_1orless[n] +
      b_other_expert4 * theta_cup_3ormore[n]
    );
    
    theta_expert3[n] = inv_logit(
      a_expert3 +
      b_cup_1orless_expert3 * theta_latte[n] +
      b_cup_3ormore_expert3 * theta_drip[n] +
      b_americano_expert3 * theta_other[n] +
      b_cortado_expert3 * theta_cappuccino[n] +
      b_espresso_expert3 * theta_espresso[n] +
      b_cappuccino_expert3 * theta_cortado[n] +
      b_drip_expert3 * theta_americano[n] +
      b_latte_expert3 * theta_cup_1orless[n] +
      b_other_expert3 * theta_cup_3ormore[n]
    );
    
    theta_expert2[n] = inv_logit(
      a_expert2 +
      b_cup_1orless_expert2 * theta_latte[n] +
      b_cup_3ormore_expert2 * theta_drip[n] +
      b_americano_expert2 * theta_other[n] +
      b_cortado_expert2 * theta_cappuccino[n] +
      b_espresso_expert2 * theta_espresso[n] +
      b_cappuccino_expert2 * theta_cortado[n] +
      b_drip_expert2 * theta_americano[n] +
      b_latte_expert2 * theta_cup_1orless[n] +
      b_other_expert2 * theta_cup_3ormore[n]
    );
    
    theta_expert1[n] = inv_logit(
      a_expert1 +
      b_cup_1orless_expert1 * theta_latte[n] +
      b_cup_3ormore_expert1 * theta_drip[n] +
      b_americano_expert1 * theta_other[n] +
      b_cortado_expert1 * theta_cappuccino[n] +
      b_espresso_expert1 * theta_espresso[n] +
      b_cappuccino_expert1 * theta_cortado[n] +
      b_drip_expert1 * theta_americano[n] +
      b_latte_expert1 * theta_cup_1orless[n] +
      b_other_expert1 * theta_cup_3ormore[n]
    );
    
    theta_expert9[n] = inv_logit(
      a_expert9 +
      b_cup_1orless_expert9 * theta_latte[n] +
      b_cup_3ormore_expert9 * theta_drip[n] +
      b_americano_expert9 * theta_other[n] +
      b_cortado_expert9 * theta_cappuccino[n] +
      b_espresso_expert9 * theta_espresso[n] +
      b_cappuccino_expert9 * theta_cortado[n] +
      b_drip_expert9 * theta_americano[n] +
      b_latte_expert9 * theta_cup_1orless[n] +
      b_other_expert9 * theta_cup_3ormore[n]
    );
    
    theta_expert10[n] = inv_logit(
      a_expert10 +
      b_cup_1orless_expert10 * theta_latte[n] +
      b_cup_3ormore_expert10 * theta_drip[n] +
      b_americano_expert10 * theta_other[n] +
      b_cortado_expert10 * theta_cappuccino[n] +
      b_espresso_expert10 * theta_espresso[n] +
      b_cappuccino_expert10 * theta_cortado[n] +
      b_drip_expert10 * theta_americano[n] +
      b_latte_expert10 * theta_cup_1orless[n] +
      b_other_expert10 * theta_cup_3ormore[n]
    );
  }

  for(n in 1:N){
    theta_roast_medium[n] = inv_logit(
      a_roast_medium +
      b_expert1_roast_medium * theta_expert1[n] +
      b_expert2_roast_medium * theta_expert2[n] +
      b_expert3_roast_medium * theta_expert3[n] +
      b_expert4_roast_medium * theta_expert4[n] +
      b_expert6_roast_medium * theta_expert6[n] +
      b_expert7_roast_medium * theta_expert7[n] +
      b_expert8_roast_medium * theta_expert8[n] +
      b_expert9_roast_medium * theta_expert9[n] +
      b_expert10_roast_medium * theta_expert10[n]
    );

    theta_roast_dark[n] = inv_logit(
      a_roast_dark +
      b_expert1_roast_dark * theta_expert1[n] +
      b_expert2_roast_dark * theta_expert2[n] +
      b_expert3_roast_dark * theta_expert3[n] +
      b_expert4_roast_dark * theta_expert4[n] +
      b_expert6_roast_dark * theta_expert6[n] +
      b_expert7_roast_dark * theta_expert7[n] +
      b_expert8_roast_dark * theta_expert8[n] +
      b_expert9_roast_dark * theta_expert9[n] +
      b_expert10_roast_dark * theta_expert10[n]
    );
  }
}
model {
  //priors 
  a_latte ~ normal(0, 10);
  b_female_latte ~ normal(0, 10);
  b_gen_other_latte ~ normal(0, 10);
  b_thirty544_latte ~ normal(0, 10);
  b_over44_latte ~ normal(0, 10);
  b_under24_latte ~ normal(0, 10);
  a_drip ~ normal(0, 10);
  b_female_drip ~ normal(0, 10);
  b_gen_other_drip ~ normal(0, 10);
  b_thirty544_drip ~ normal(0, 10);
  b_over44_drip ~ normal(0, 10);
  b_under24_drip ~ normal(0, 10);
  a_other ~ normal(0, 10);
  b_female_other ~ normal(0, 10);
  b_gen_other_other ~ normal(0, 10);
  b_thirty544_other ~ normal(0, 10);
  b_over44_other ~ normal(0, 10);
  b_under24_other ~ normal(0, 10);
  a_cappuccino ~ normal(0, 10);
  b_female_cappuccino ~ normal(0, 10);
  b_gen_other_cappuccino ~ normal(0, 10);
  b_thirty544_cappuccino ~ normal(0, 10);
  b_over44_cappuccino ~ normal(0, 10);
  b_under24_cappuccino ~ normal(0, 10);
  a_espresso ~ normal(0, 10);
  b_female_espresso ~ normal(0, 10);
  b_gen_other_espresso ~ normal(0, 10);
  b_thirty544_espresso ~ normal(0, 10);
  b_over44_espresso ~ normal(0, 10);
  b_under24_espresso ~ normal(0, 10);
  a_cortado ~ normal(0, 10);
  b_female_cortado ~ normal(0, 10);
  b_gen_other_cortado ~ normal(0, 10);
  b_thirty544_cortado ~ normal(0, 10);
  b_over44_cortado ~ normal(0, 10);
  b_under24_cortado ~ normal(0, 10);
  a_americano ~ normal(0, 10);
  b_female_americano ~ normal(0, 10);
  b_gen_other_americano ~ normal(0, 10);
  b_thirty544_americano ~ normal(0, 10);
  b_over44_americano ~ normal(0, 10);
  b_under24_americano ~ normal(0, 10);
  a_cup_1orless ~ normal(0, 10);
  b_female_cup_1orless ~ normal(0, 10);
  b_gen_other_cup_1orless ~ normal(0, 10);
  b_thirty544_cup_1orless ~ normal(0, 10);
  b_over44_cup_1orless ~ normal(0, 10);
  b_under24_cup_1orless ~ normal(0, 10);
  a_cup_3ormore ~ normal(0, 10);
  b_female_cup_3ormore ~ normal(0, 10);
  b_gen_other_cup_3ormore ~ normal(0, 10);
  b_thirty544_cup_3ormore ~ normal(0, 10);
  b_over44_cup_3ormore ~ normal(0, 10);
  b_under24_cup_3ormore ~ normal(0, 10);
  a_expert7 ~ normal(0, 10);
  b_cup_1orless_expert7 ~ normal(0, 10);
  b_cup_3ormore_expert7 ~ normal(0, 10);
  b_americano_expert7 ~ normal(0, 10);
  b_cortado_expert7 ~ normal(0, 10);
  b_espresso_expert7 ~ normal(0, 10);
  b_cappuccino_expert7 ~ normal(0, 10);
  b_drip_expert7 ~ normal(0, 10);
  b_latte_expert7 ~ normal(0, 10);
  b_other_expert7 ~ normal(0, 10);
  a_expert6 ~ normal(0, 10);
  b_cup_1orless_expert6 ~ normal(0, 10);
  b_cup_3ormore_expert6 ~ normal(0, 10);
  b_americano_expert6 ~ normal(0, 10);
  b_cortado_expert6 ~ normal(0, 10);
  b_espresso_expert6 ~ normal(0, 10);
  b_cappuccino_expert6 ~ normal(0, 10);
  b_drip_expert6 ~ normal(0, 10);
  b_latte_expert6 ~ normal(0, 10);
  b_other_expert6 ~ normal(0, 10);
  a_expert8 ~ normal(0, 10);
  b_cup_1orless_expert8 ~ normal(0, 10);
  b_cup_3ormore_expert8 ~ normal(0, 10);
  b_americano_expert8 ~ normal(0, 10);
  b_cortado_expert8 ~ normal(0, 10);
  b_espresso_expert8 ~ normal(0, 10);
  b_cappuccino_expert8 ~ normal(0, 10);
  b_drip_expert8 ~ normal(0, 10);
  b_latte_expert8 ~ normal(0, 10);
  b_other_expert8 ~ normal(0, 10);
  a_expert4 ~ normal(0, 10);
  b_cup_1orless_expert4 ~ normal(0, 10);
  b_cup_3ormore_expert4 ~ normal(0, 10);
  b_americano_expert4 ~ normal(0, 10);
  b_cortado_expert4 ~ normal(0, 10);
  b_espresso_expert4 ~ normal(0, 10);
  b_cappuccino_expert4 ~ normal(0, 10);
  b_drip_expert4 ~ normal(0, 10);
  b_latte_expert4 ~ normal(0, 10);
  b_other_expert4 ~ normal(0, 10);
  a_expert3 ~ normal(0, 10);
  b_cup_1orless_expert3 ~ normal(0, 10);
  b_cup_3ormore_expert3 ~ normal(0, 10);
  b_americano_expert3 ~ normal(0, 10);
  b_cortado_expert3 ~ normal(0, 10);
  b_espresso_expert3 ~ normal(0, 10);
  b_cappuccino_expert3 ~ normal(0, 10);
  b_drip_expert3 ~ normal(0, 10);
  b_latte_expert3 ~ normal(0, 10);
  b_other_expert3 ~ normal(0, 10);
  a_expert2 ~ normal(0, 10);
  b_cup_1orless_expert2 ~ normal(0, 10);
  b_cup_3ormore_expert2 ~ normal(0, 10);
  b_americano_expert2 ~ normal(0, 10);
  b_cortado_expert2 ~ normal(0, 10);
  b_espresso_expert2 ~ normal(0, 10);
  b_cappuccino_expert2 ~ normal(0, 10);
  b_drip_expert2 ~ normal(0, 10);
  b_latte_expert2 ~ normal(0, 10);
  b_other_expert2 ~ normal(0, 10);
  a_expert1 ~ normal(0, 10);
  b_cup_1orless_expert1 ~ normal(0, 10);
  b_cup_3ormore_expert1 ~ normal(0, 10);
  b_americano_expert1 ~ normal(0, 10);
  b_cortado_expert1 ~ normal(0, 10);
  b_espresso_expert1 ~ normal(0, 10);
  b_cappuccino_expert1 ~ normal(0, 10);
  b_drip_expert1 ~ normal(0, 10);
  b_latte_expert1 ~ normal(0, 10);
  b_other_expert1 ~ normal(0, 10);
  a_expert9 ~ normal(0, 10);
  b_cup_1orless_expert9 ~ normal(0, 10);
  b_cup_3ormore_expert9 ~ normal(0, 10);
  b_americano_expert9 ~ normal(0, 10);
  b_cortado_expert9 ~ normal(0, 10);
  b_espresso_expert9 ~ normal(0, 10);
  b_cappuccino_expert9 ~ normal(0, 10);
  b_drip_expert9 ~ normal(0, 10);
  b_latte_expert9 ~ normal(0, 10);
  b_other_expert9 ~ normal(0, 10);
  a_expert1 ~ normal(0, 10);
  b_cup_1orless_expert10 ~ normal(0, 10);
  b_cup_3ormore_expert10 ~ normal(0, 10);
  b_americano_expert10 ~ normal(0, 10);
  b_cortado_expert10 ~ normal(0, 10);
  b_espresso_expert10 ~ normal(0, 10);
  b_cappuccino_expert10 ~ normal(0, 10);
  b_drip_expert10 ~ normal(0, 10);
  b_latte_expert10 ~ normal(0, 10);
  b_other_expert10 ~ normal(0, 10);
  a_roast_medium ~ normal(0, 10);
  b_expert1_roast_medium ~ normal(0, 10);
  b_expert2_roast_medium ~ normal(0, 10);
  b_expert3_roast_medium ~ normal(0, 10);
  b_expert4_roast_medium ~ normal(0, 10);
  b_expert6_roast_medium ~ normal(0, 10);
  b_expert7_roast_medium ~ normal(0, 10);
  b_expert8_roast_medium ~ normal(0, 10);
  b_expert9_roast_medium ~ normal(0, 10);
  b_expert10_roast_medium ~ normal(0, 10);
  a_roast_dark ~ normal(0, 10);
  b_expert1_roast_dark ~ normal(0, 10);
  b_expert2_roast_dark ~ normal(0, 10);
  b_expert3_roast_dark ~ normal(0, 10);
  b_expert4_roast_dark ~ normal(0, 10);
  b_expert6_roast_dark ~ normal(0, 10);
  b_expert7_roast_dark ~ normal(0, 10);
  b_expert8_roast_dark ~ normal(0, 10);
  b_expert9_roast_dark ~ normal(0, 10);
  b_expert10_roast_dark ~ normal(0, 10);

  //vector[N] log_lik;

  //log_lik[n] = Y[n] * log(pie) + (1 - Y[n]) * log(1 - pie);

  //target += sum(log_lik);
}
generated quantities {
  //vector[N] log_lik_rep; 
  
  //for (n in 1:N){
  //  log_lik_rep[n] = Y[n] * log(pie) + (1 - Y[n]) * log(1 - pie);
  //}
}





//data {
//  int<lower=1> N;              // Number of observations
//  int<lower=1> K;              // Number of response types (e.g., latte, drip)
//  int<lower=1> P;              // Number of predictors (e.g., female, gen_other)
//  matrix[N, P] X;              // Predictor matrix
//}
//parameters {
//  vector[K] a;                 // Intercepts for each category
//  matrix[K, P] b;              // Coefficients for predictors
//}
//transformed parameters {
//  matrix[N, K] theta;          // Transformed parameters matrix for all categories
//  theta = inv_logit(rep_matrix(a, N) + X * b'); // Vectorized logit
//
//  vector[N] theta_shared = inv_logit(a + shared_terms); // Shared logit calculation
//}