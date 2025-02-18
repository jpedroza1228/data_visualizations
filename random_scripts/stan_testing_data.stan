// functions{
//      int numLevels(int[] m) {
//         int sorted[num_elements(m)];
//         int count = 1;
//         sorted = sort_asc(m);
//         for (i in 2:num_elements(sorted)) {
//           if (sorted[i] != sorted[i-1])
//              count = count + 1;
//         }
//         return(count);
//      }
// }
data {
  // Define variables in data
  int<lower=1> N; // Number of observations (an integer)
  vector<lower=0, upper=1>[N] drink_at_home;
  vector<lower=0, upper=1>[N] drink_at_office;
  vector<lower=0, upper=1>[N] drink_on_go;
  vector<lower=0, upper=1>[N] drink_at_cafe;
  vector<lower=0, upper=1>[N] drink_none_of_these;
  vector<lower=0, upper=1>[N] home_brew_pour_over;
  vector<lower=0, upper=1>[N] home_brew_french_press;
  vector<lower=0, upper=1>[N] home_brew_espresso;
  vector<lower=0, upper=1>[N] home_brew_mr_coffee;
  vector<lower=0, upper=1>[N] home_brew_pods;
  vector<lower=0, upper=1>[N] home_brew_instant;
  vector<lower=0, upper=1>[N] home_brew_bean2cup;
  vector<lower=0, upper=1>[N] home_brew_cold_brew;
  vector<lower=0, upper=1>[N] home_brew_cometeer;
  vector<lower=0, upper=1>[N] home_brew_other;
  vector<lower=0, upper=1>[N] coffee_black;
  vector<lower=0, upper=1>[N] coffee_milk_alt_creamer;
  vector<lower=0, upper=1>[N] coffee_sugar;
  vector<lower=0, upper=1>[N] coffee_syrup;
  vector<lower=0, upper=1>[N] coffee_other;
  vector<lower=0, upper=1>[N] why_drink_taste_good; // outcome variable
}
transformed data {
  // Define transformed data
}
parameters {
  // Define parameters to estimate

  //real a_home_brew_pour_over;

  //real b_drink_at_home_home_brew_pour_over;

  //real b_drink_at_office_home_brew_pour_over;

  //real b_drink_on_go_home_brew_pour_over;

  //real b_drink_at_cafe_home_brew_pour_over;

  //real b_drink_none_of_these_home_brew_pour_over;

  //

  //real a_home_brew_french_press;

  //real b_drink_at_home_home_brew_french_press;

  //real b_drink_at_office_home_brew_french_press;

  //real b_drink_on_go_home_brew_french_press;

  //real b_drink_at_cafe_home_brew_french_press;

  //real b_drink_none_of_these_home_brew_french_press;

  //

  //real a_home_brew_espresso;

  //real b_drink_at_home_home_brew_espresso;

  //real b_drink_at_office_home_brew_espresso;

  //real b_drink_on_go_home_brew_espresso;

  //real b_drink_at_cafe_home_brew_espresso;

  //real b_drink_none_of_these_home_brew_espresso;

  //

  //real a_home_brew_mr_coffee;

  //real b_drink_at_home_home_brew_mr_coffee;

  //real b_drink_at_office_home_brew_mr_coffee;

  //real b_drink_on_go_home_brew_mr_coffee;

  //real b_drink_at_cafe_home_brew_mr_coffee;

  //real b_drink_none_of_these_home_brew_mr_coffee;

  //

  //real a_home_brew_pods;

  //real b_drink_at_home_home_brew_pods;

  //real b_drink_at_office_home_brew_pods;

  //real b_drink_on_go_home_brew_pods;

  //real b_drink_at_cafe_home_brew_pods;

  //real b_drink_none_of_these_home_brew_pods;

  //

  //real a_home_brew_instant;

  //real b_drink_at_home_home_brew_instant;

  //real b_drink_at_office_home_brew_instant;

  //real b_drink_on_go_home_brew_instant;

  //real b_drink_at_cafe_home_brew_instant;

  //real b_drink_none_of_these_home_brew_instant;

  //

  //real a_home_brew_bean2cup;

  //real b_drink_at_home_home_brew_bean2cup;

  //real b_drink_at_office_home_brew_bean2cup;

  //real b_drink_on_go_home_brew_bean2cup;

  //real b_drink_at_cafe_home_brew_bean2cup;

  //real b_drink_none_of_these_home_brew_bean2cup;

  //

  //real a_home_brew_cold_brew;

  //real b_drink_at_home_home_brew_cold_brew;

  //real b_drink_at_office_home_brew_cold_brew;

  //real b_drink_on_go_home_brew_cold_brew;

  //real b_drink_at_cafe_home_brew_cold_brew;

  //real b_drink_none_of_these_home_brew_cold_brew;

  //

  //real a_home_brew_cometeer;

  //real b_drink_at_home_home_brew_cometeer;

  //real b_drink_at_office_home_brew_cometeer;

  //real b_drink_on_go_home_brew_cometeer;

  //real b_drink_at_cafe_home_brew_cometeer;

  //real b_drink_none_of_these_home_brew_cometeer;

  //

  //real a_home_brew_other;

  //real b_drink_at_home_home_brew_other;

  //real b_drink_at_office_home_brew_other;

  //real b_drink_on_go_home_brew_other;

  //real b_drink_at_cafe_home_brew_other;

  //real b_drink_none_of_these_home_brew_other;

  //

  //real a_coffee_black;

  //real b_home_brew_pour_over_coffee_black;

  //real b_home_brew_french_press_coffee_black;

  //real b_home_brew_espresso_coffee_black;

  //real b_home_brew_mr_coffee_coffee_black;

  //real b_home_brew_pods_coffee_black;

  //real b_home_brew_instant_coffee_black;

  //real b_home_brew_bean2cup_coffee_black;

  //real b_home_brew_cold_brew_coffee_black;

  //real b_home_brew_cometeer_coffee_black;

  //real b_home_brew_other_coffee_black;

  //

  //real a_coffee_milk_alt_creamer;

  //real b_home_brew_pour_over_coffee_milk_alt_creamer;

  //real b_home_brew_french_press_coffee_milk_alt_creamer;

  //real b_home_brew_espresso_coffee_milk_alt_creamer;

  //real b_home_brew_mr_coffee_coffee_milk_alt_creamer;

  //real b_home_brew_pods_coffee_milk_alt_creamer;

  //real b_home_brew_instant_coffee_milk_alt_creamer;

  //real b_home_brew_bean2cup_coffee_milk_alt_creamer;

  //real b_home_brew_cold_brew_coffee_milk_alt_creamer;

  //real b_home_brew_cometeer_coffee_milk_alt_creamer;

  //real b_home_brew_other_coffee_milk_alt_creamer;

  //

  //real a_coffee_sugar;

  //real b_home_brew_pour_over_coffee_sugar;

  //real b_home_brew_french_press_coffee_sugar;

  //real b_home_brew_espresso_coffee_sugar;

  //real b_home_brew_mr_coffee_coffee_sugar;

  //real b_home_brew_pods_coffee_sugar;

  //real b_home_brew_instant_coffee_sugar;

  //real b_home_brew_bean2cup_coffee_sugar;

  //real b_home_brew_cold_brew_coffee_sugar;

  //real b_home_brew_cometeer_coffee_sugar;

  //real b_home_brew_other_coffee_sugar;

  //

  //real a_coffee_syrup;

  //real b_home_brew_pour_over_coffee_syrup;

  //real b_home_brew_french_press_coffee_syrup;

  //real b_home_brew_espresso_coffee_syrup;

  //real b_home_brew_mr_coffee_coffee_syrup;

  //real b_home_brew_pods_coffee_syrup;

  //real b_home_brew_instant_coffee_syrup;

  //real b_home_brew_bean2cup_coffee_syrup;

  //real b_home_brew_cold_brew_coffee_syrup;

  //real b_home_brew_cometeer_coffee_syrup;

  //real b_home_brew_other_coffee_syrup;

  //

  //real a_coffee_other;

  //real b_home_brew_pour_over_coffee_other;

  //real b_home_brew_french_press_coffee_other;

  //real b_home_brew_espresso_coffee_other;

  //real b_home_brew_mr_coffee_coffee_other;

  //real b_home_brew_pods_coffee_other;

  //real b_home_brew_instant_coffee_other;

  //real b_home_brew_bean2cup_coffee_other;

  //real b_home_brew_cold_brew_coffee_other;

  //real b_home_brew_cometeer_coffee_other;

  //real b_home_brew_other_coffee_other;

  //

  real a_why_drink_taste_good;
  real b_coffee_black_why_drink_taste_good;
  real b_coffee_milk_alt_creamer_why_drink_taste_good;
  real b_coffee_sugar_why_drink_taste_good;
  real b_coffee_syrup_why_drink_taste_good;
  real b_coffee_other_why_drink_taste_good;
}

transformed parameters {
  // Transform parameters

  vector[N] theta_why_drink_taste_good;

  //for(k in 1:N) {

  //   home_brew_other[k] = a_home_brew_other + b_drink_at_home_home_brew_other*drink_at_home[k] + b_drink_at_office_home_brew_other*drink_at_office[k] + b_drink_on_go_home_brew_other*drink_on_go[k] + b_drink_at_cafe_home_brew_other*drink_at_cafe[k] + b_drink_none_of_these_home_brew_other*drink_none_of_these[k];

  //}

  //for(k in 1:N) {

  //   home_brew_cometeer[k] = a_home_brew_cometeer + b_drink_at_home_home_brew_cometeer*drink_at_home[k] + b_drink_at_office_home_brew_cometeer*drink_at_office[k] + b_drink_on_go_home_brew_cometeer*drink_on_go[k] + b_drink_at_cafe_home_brew_cometeer*drink_at_cafe[k] + b_drink_none_of_these_home_brew_cometeer*drink_none_of_these[k];

  //}

  //for(k in 1:N) {

  //   home_brew_cold_brew[k] = a_home_brew_cold_brew + b_drink_at_home_home_brew_cold_brew*drink_at_home[k] + b_drink_at_office_home_brew_cold_brew*drink_at_office[k] + b_drink_on_go_home_brew_cold_brew*drink_on_go[k] + b_drink_at_cafe_home_brew_cold_brew*drink_at_cafe[k] + b_drink_none_of_these_home_brew_cold_brew*drink_none_of_these[k];

  //}

  //for(k in 1:N) {

  //   home_brew_bean2cup[k] = a_home_brew_bean2cup + b_drink_at_home_home_brew_bean2cup*drink_at_home[k] + b_drink_at_office_home_brew_bean2cup*drink_at_office[k] + b_drink_on_go_home_brew_bean2cup*drink_on_go[k] + b_drink_at_cafe_home_brew_bean2cup*drink_at_cafe[k] + b_drink_none_of_these_home_brew_bean2cup*drink_none_of_these[k];

  //}

  //for(k in 1:N) {

  //   home_brew_instant[k] = a_home_brew_instant + b_drink_at_home_home_brew_instant*drink_at_home[k] + b_drink_at_office_home_brew_instant*drink_at_office[k] + b_drink_on_go_home_brew_instant*drink_on_go[k] + b_drink_at_cafe_home_brew_instant*drink_at_cafe[k] + b_drink_none_of_these_home_brew_instant*drink_none_of_these[k];

  //}

  //for(k in 1:N) {

  //   home_brew_pods[k] = a_home_brew_pods + b_drink_at_home_home_brew_pods*drink_at_home[k] + b_drink_at_office_home_brew_pods*drink_at_office[k] + b_drink_on_go_home_brew_pods*drink_on_go[k] + b_drink_at_cafe_home_brew_pods*drink_at_cafe[k] + b_drink_none_of_these_home_brew_pods*drink_none_of_these[k];

  //}

  //for(k in 1:N) {

  //   home_brew_mr_coffee[k] = a_home_brew_mr_coffee + b_drink_at_home_home_brew_mr_coffee*drink_at_home[k] + b_drink_at_office_home_brew_mr_coffee*drink_at_office[k] + b_drink_on_go_home_brew_mr_coffee*drink_on_go[k] + b_drink_at_cafe_home_brew_mr_coffee*drink_at_cafe[k] + b_drink_none_of_these_home_brew_mr_coffee*drink_none_of_these[k];

  //}

  //for(k in 1:N) {

  //   home_brew_espresso[k] = a_home_brew_espresso + b_drink_at_home_home_brew_espresso*drink_at_home[k] + b_drink_at_office_home_brew_espresso*drink_at_office[k] + b_drink_on_go_home_brew_espresso*drink_on_go[k] + b_drink_at_cafe_home_brew_espresso*drink_at_cafe[k] + b_drink_none_of_these_home_brew_espresso*drink_none_of_these[k];

  //}

  //for(k in 1:N) {

  //   home_brew_french_press[k] = a_home_brew_french_press + b_drink_at_home_home_brew_french_press*drink_at_home[k] + b_drink_at_office_home_brew_french_press*drink_at_office[k] + b_drink_on_go_home_brew_french_press*drink_on_go[k] + b_drink_at_cafe_home_brew_french_press*drink_at_cafe[k] + b_drink_none_of_these_home_brew_french_press*drink_none_of_these[k];

  //}

  //for(k in 1:N) {

  //   home_brew_pour_over[k] = a_home_brew_pour_over + b_drink_at_home_home_brew_pour_over*drink_at_home[k] + b_drink_at_office_home_brew_pour_over*drink_at_office[k] + b_drink_on_go_home_brew_pour_over*drink_on_go[k] + b_drink_at_cafe_home_brew_pour_over*drink_at_cafe[k] + b_drink_none_of_these_home_brew_pour_over*drink_none_of_these[k];

  //}

  //for(k in 1:N) {

  //   coffee_other[k] = a_coffee_other + b_home_brew_pour_over_coffee_other*home_brew_pour_over[k] + b_home_brew_french_press_coffee_other*home_brew_french_press[k] + b_home_brew_espresso_coffee_other*home_brew_espresso[k] + b_home_brew_mr_coffee_coffee_other*home_brew_mr_coffee[k] + b_home_brew_pods_coffee_other*home_brew_pods[k] + b_home_brew_instant_coffee_other*home_brew_instant[k] + b_home_brew_bean2cup_coffee_other*home_brew_bean2cup[k] + b_home_brew_cold_brew_coffee_other*home_brew_cold_brew[k] + b_home_brew_cometeer_coffee_other*home_brew_cometeer[k] + b_home_brew_other_coffee_other*home_brew_other[k];

  //}

  //for(k in 1:N) {

  //   coffee_syrup[k] = a_coffee_syrup + b_home_brew_pour_over_coffee_syrup*home_brew_pour_over[k] + b_home_brew_french_press_coffee_syrup*home_brew_french_press[k] + b_home_brew_espresso_coffee_syrup*home_brew_espresso[k] + b_home_brew_mr_coffee_coffee_syrup*home_brew_mr_coffee[k] + b_home_brew_pods_coffee_syrup*home_brew_pods[k] + b_home_brew_instant_coffee_syrup*home_brew_instant[k] + b_home_brew_bean2cup_coffee_syrup*home_brew_bean2cup[k] + b_home_brew_cold_brew_coffee_syrup*home_brew_cold_brew[k] + b_home_brew_cometeer_coffee_syrup*home_brew_cometeer[k] + b_home_brew_other_coffee_syrup*home_brew_other[k];

  //}

  //for(k in 1:N) {

  //   coffee_sugar[k] = a_coffee_sugar + b_home_brew_pour_over_coffee_sugar*home_brew_pour_over[k] + b_home_brew_french_press_coffee_sugar*home_brew_french_press[k] + b_home_brew_espresso_coffee_sugar*home_brew_espresso[k] + b_home_brew_mr_coffee_coffee_sugar*home_brew_mr_coffee[k] + b_home_brew_pods_coffee_sugar*home_brew_pods[k] + b_home_brew_instant_coffee_sugar*home_brew_instant[k] + b_home_brew_bean2cup_coffee_sugar*home_brew_bean2cup[k] + b_home_brew_cold_brew_coffee_sugar*home_brew_cold_brew[k] + b_home_brew_cometeer_coffee_sugar*home_brew_cometeer[k] + b_home_brew_other_coffee_sugar*home_brew_other[k];

  //}

  //for(k in 1:N) {

  //   coffee_milk_alt_creamer[k] = a_coffee_milk_alt_creamer + b_home_brew_pour_over_coffee_milk_alt_creamer*home_brew_pour_over[k] + b_home_brew_french_press_coffee_milk_alt_creamer*home_brew_french_press[k] + b_home_brew_espresso_coffee_milk_alt_creamer*home_brew_espresso[k] + b_home_brew_mr_coffee_coffee_milk_alt_creamer*home_brew_mr_coffee[k] + b_home_brew_pods_coffee_milk_alt_creamer*home_brew_pods[k] + b_home_brew_instant_coffee_milk_alt_creamer*home_brew_instant[k] + b_home_brew_bean2cup_coffee_milk_alt_creamer*home_brew_bean2cup[k] + b_home_brew_cold_brew_coffee_milk_alt_creamer*home_brew_cold_brew[k] + b_home_brew_cometeer_coffee_milk_alt_creamer*home_brew_cometeer[k] + b_home_brew_other_coffee_milk_alt_creamer*home_brew_other[k];

  //}

  //for(k in 1:N) {

  //   coffee_black[k] = a_coffee_black + b_home_brew_pour_over_coffee_black*home_brew_pour_over[k] + b_home_brew_french_press_coffee_black*home_brew_french_press[k] + b_home_brew_espresso_coffee_black*home_brew_espresso[k] + b_home_brew_mr_coffee_coffee_black*home_brew_mr_coffee[k] + b_home_brew_pods_coffee_black*home_brew_pods[k] + b_home_brew_instant_coffee_black*home_brew_instant[k] + b_home_brew_bean2cup_coffee_black*home_brew_bean2cup[k] + b_home_brew_cold_brew_coffee_black*home_brew_cold_brew[k] + b_home_brew_cometeer_coffee_black*home_brew_cometeer[k] + b_home_brew_other_coffee_black*home_brew_other[k];

  //}

  for (i in 1 : N) {
    theta_why_drink_taste_good[i] = a_why_drink_taste_good
                                    + b_coffee_black_why_drink_taste_good * coffee_black[i]
                                    + b_coffee_milk_alt_creamer_why_drink_taste_good * coffee_milk_alt_creamer[i]
                                    + b_coffee_sugar_why_drink_taste_good * coffee_sugar[i]
                                    + b_coffee_syrup_why_drink_taste_good * coffee_syrup[i]
                                    + b_coffee_other_why_drink_taste_good * coffee_other[i];
  }
}

model{

// Priors

// a_home_brew_pour_over ~ normal( 0, 10 );

// b_drink_at_home_home_brew_pour_over ~ normal( 0, 10 );

// b_drink_at_office_home_brew_pour_over ~ normal( 0, 10 );

// b_drink_on_go_home_brew_pour_over ~ normal( 0, 10 );

// b_drink_at_cafe_home_brew_pour_over ~ normal( 0, 10 );

// b_drink_none_of_these_home_brew_pour_over ~ normal( 0, 10 );



// a_home_brew_french_press ~ normal( 0, 10 );

// b_drink_at_home_home_brew_french_press ~ normal( 0, 10 );

// b_drink_at_office_home_brew_french_press ~ normal( 0, 10 );

// b_drink_on_go_home_brew_french_press ~ normal( 0, 10 );

// b_drink_at_cafe_home_brew_french_press ~ normal( 0, 10 );

// b_drink_none_of_these_home_brew_french_press ~ normal( 0, 10 );



// a_home_brew_espresso ~ normal( 0, 10 );

// b_drink_at_home_home_brew_espresso ~ normal( 0, 10 );

// b_drink_at_office_home_brew_espresso ~ normal( 0, 10 );

// b_drink_on_go_home_brew_espresso ~ normal( 0, 10 );

// b_drink_at_cafe_home_brew_espresso ~ normal( 0, 10 );

// b_drink_none_of_these_home_brew_espresso ~ normal( 0, 10 );



// a_home_brew_mr_coffee ~ normal( 0, 10 );

// b_drink_at_home_home_brew_mr_coffee ~ normal( 0, 10 );

// b_drink_at_office_home_brew_mr_coffee ~ normal( 0, 10 );

// b_drink_on_go_home_brew_mr_coffee ~ normal( 0, 10 );

// b_drink_at_cafe_home_brew_mr_coffee ~ normal( 0, 10 );

// b_drink_none_of_these_home_brew_mr_coffee ~ normal( 0, 10 );



// a_home_brew_pods ~ normal( 0, 10 );

// b_drink_at_home_home_brew_pods ~ normal( 0, 10 );

// b_drink_at_office_home_brew_pods ~ normal( 0, 10 );

// b_drink_on_go_home_brew_pods ~ normal( 0, 10 );

// b_drink_at_cafe_home_brew_pods ~ normal( 0, 10 );

// b_drink_none_of_these_home_brew_pods ~ normal( 0, 10 );



// a_home_brew_instant ~ normal( 0, 10 );

// b_drink_at_home_home_brew_instant ~ normal( 0, 10 );

// b_drink_at_office_home_brew_instant ~ normal( 0, 10 );

// b_drink_on_go_home_brew_instant ~ normal( 0, 10 );

// b_drink_at_cafe_home_brew_instant ~ normal( 0, 10 );

// b_drink_none_of_these_home_brew_instant ~ normal( 0, 10 );



// a_home_brew_bean2cup ~ normal( 0, 10 );

// b_drink_at_home_home_brew_bean2cup ~ normal( 0, 10 );

// b_drink_at_office_home_brew_bean2cup ~ normal( 0, 10 );

// b_drink_on_go_home_brew_bean2cup ~ normal( 0, 10 );

// b_drink_at_cafe_home_brew_bean2cup ~ normal( 0, 10 );

// b_drink_none_of_these_home_brew_bean2cup ~ normal( 0, 10 );



// a_home_brew_cold_brew ~ normal( 0, 10 );

// b_drink_at_home_home_brew_cold_brew ~ normal( 0, 10 );

// b_drink_at_office_home_brew_cold_brew ~ normal( 0, 10 );

// b_drink_on_go_home_brew_cold_brew ~ normal( 0, 10 );

// b_drink_at_cafe_home_brew_cold_brew ~ normal( 0, 10 );

// b_drink_none_of_these_home_brew_cold_brew ~ normal( 0, 10 );



// a_home_brew_cometeer ~ normal( 0, 10 );

// b_drink_at_home_home_brew_cometeer ~ normal( 0, 10 );

// b_drink_at_office_home_brew_cometeer ~ normal( 0, 10 );

// b_drink_on_go_home_brew_cometeer ~ normal( 0, 10 );

// b_drink_at_cafe_home_brew_cometeer ~ normal( 0, 10 );

// b_drink_none_of_these_home_brew_cometeer ~ normal( 0, 10 );



// a_home_brew_other ~ normal( 0, 10 );

// b_drink_at_home_home_brew_other ~ normal( 0, 10 );

// b_drink_at_office_home_brew_other ~ normal( 0, 10 );

// b_drink_on_go_home_brew_other ~ normal( 0, 10 );

// b_drink_at_cafe_home_brew_other ~ normal( 0, 10 );

// b_drink_none_of_these_home_brew_other ~ normal( 0, 10 );



// a_coffee_black ~ normal( 0, 10 );

// b_home_brew_pour_over_coffee_black ~ normal( 0, 10 );

// b_home_brew_french_press_coffee_black ~ normal( 0, 10 );

// b_home_brew_espresso_coffee_black ~ normal( 0, 10 );

// b_home_brew_mr_coffee_coffee_black ~ normal( 0, 10 );

// b_home_brew_pods_coffee_black ~ normal( 0, 10 );

// b_home_brew_instant_coffee_black ~ normal( 0, 10 );

// b_home_brew_bean2cup_coffee_black ~ normal( 0, 10 );

// b_home_brew_cold_brew_coffee_black ~ normal( 0, 10 );

// b_home_brew_cometeer_coffee_black ~ normal( 0, 10 );

// b_home_brew_other_coffee_black ~ normal( 0, 10 );



// a_coffee_milk_alt_creamer ~ normal( 0, 10 );

// b_home_brew_pour_over_coffee_milk_alt_creamer ~ normal( 0, 10 );

// b_home_brew_french_press_coffee_milk_alt_creamer ~ normal( 0, 10 );

// b_home_brew_espresso_coffee_milk_alt_creamer ~ normal( 0, 10 );

// b_home_brew_mr_coffee_coffee_milk_alt_creamer ~ normal( 0, 10 );

// b_home_brew_pods_coffee_milk_alt_creamer ~ normal( 0, 10 );

// b_home_brew_instant_coffee_milk_alt_creamer ~ normal( 0, 10 );

// b_home_brew_bean2cup_coffee_milk_alt_creamer ~ normal( 0, 10 );

// b_home_brew_cold_brew_coffee_milk_alt_creamer ~ normal( 0, 10 );

// b_home_brew_cometeer_coffee_milk_alt_creamer ~ normal( 0, 10 );

// b_home_brew_other_coffee_milk_alt_creamer ~ normal( 0, 10 );



// a_coffee_sugar ~ normal( 0, 10 );

// b_home_brew_pour_over_coffee_sugar ~ normal( 0, 10 );

// b_home_brew_french_press_coffee_sugar ~ normal( 0, 10 );

// b_home_brew_espresso_coffee_sugar ~ normal( 0, 10 );

// b_home_brew_mr_coffee_coffee_sugar ~ normal( 0, 10 );

// b_home_brew_pods_coffee_sugar ~ normal( 0, 10 );

// b_home_brew_instant_coffee_sugar ~ normal( 0, 10 );

// b_home_brew_bean2cup_coffee_sugar ~ normal( 0, 10 );

// b_home_brew_cold_brew_coffee_sugar ~ normal( 0, 10 );

// b_home_brew_cometeer_coffee_sugar ~ normal( 0, 10 );

// b_home_brew_other_coffee_sugar ~ normal( 0, 10 );



// a_coffee_syrup ~ normal( 0, 10 );

// b_home_brew_pour_over_coffee_syrup ~ normal( 0, 10 );

// b_home_brew_french_press_coffee_syrup ~ normal( 0, 10 );

// b_home_brew_espresso_coffee_syrup ~ normal( 0, 10 );

// b_home_brew_mr_coffee_coffee_syrup ~ normal( 0, 10 );

// b_home_brew_pods_coffee_syrup ~ normal( 0, 10 );

// b_home_brew_instant_coffee_syrup ~ normal( 0, 10 );

// b_home_brew_bean2cup_coffee_syrup ~ normal( 0, 10 );

// b_home_brew_cold_brew_coffee_syrup ~ normal( 0, 10 );

// b_home_brew_cometeer_coffee_syrup ~ normal( 0, 10 );

// b_home_brew_other_coffee_syrup ~ normal( 0, 10 );



// a_coffee_other ~ normal( 0, 10 );

// b_home_brew_pour_over_coffee_other ~ normal( 0, 10 );

// b_home_brew_french_press_coffee_other ~ normal( 0, 10 );

// b_home_brew_espresso_coffee_other ~ normal( 0, 10 );

// b_home_brew_mr_coffee_coffee_other ~ normal( 0, 10 );

// b_home_brew_pods_coffee_other ~ normal( 0, 10 );

// b_home_brew_instant_coffee_other ~ normal( 0, 10 );

// b_home_brew_bean2cup_coffee_other ~ normal( 0, 10 );

// b_home_brew_cold_brew_coffee_other ~ normal( 0, 10 );

// b_home_brew_cometeer_coffee_other ~ normal( 0, 10 );

// b_home_brew_other_coffee_other ~ normal( 0, 10 );



  a_why_drink_taste_good ~ normal(0,100);
  b_coffee_black_why_drink_taste_good ~ normal( 0, 10 );
  b_coffee_milk_alt_creamer_why_drink_taste_good ~ normal( 0, 10 );
  b_coffee_sugar_why_drink_taste_good ~ normal( 0, 10 );
  b_coffee_syrup_why_drink_taste_good ~ normal( 0, 10 );
  b_coffee_other_why_drink_taste_good ~ normal( 0, 10 );
  
  // Likelihoods
  why_drink_taste_good ~ binomial_logit(1, theta_why_drink_taste_good);
}

// generated quantities {

//      // simulate data from the posterior

//      int<lower=0,upper=1> yrep_why_drink_taste_good[N];

//      // log-likelihood posterior

//      vector[N] log_lik_why_drink_taste_good;

//      for (i in 1:num_elements(yrep_why_drink_taste_good)) {

//        yrep_why_drink_taste_good[i] = binomial_rng(why_drink_taste_good[i], inv_logit(theta_why_drink_taste_good[i]));

//      }

//      for (i in 1:N) {

//        log_lik_why_drink_taste_good[i] = binomial_logit_lpmf(why_drink_taste_good[i] | 1, theta_why_drink_taste_good[i]);

//      }

// }


