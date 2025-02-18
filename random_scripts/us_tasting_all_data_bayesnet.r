library(tidyverse)
library(bnlearn)
library(cmdstanr)
library(posterior)
library(bayesplot)
library(psych)
library(Rgraphviz)
library(reactable)

coffee <- read_csv(here::here("random_data", "only_coffee_data.csv"))

glimpse(coffee)

dummy.code(coffee$gender) |> head()

coffee$gen_male <- dummy.code(coffee$gender)[, 1]
coffee$gen_female <- dummy.code(coffee$gender)[, 2]
coffee$gen_nonbi <- dummy.code(coffee$gender)[, 3]
coffee$gen_nottosay <- dummy.code(coffee$gender)[, 4]
coffee$gen_other <- dummy.code(coffee$gender)[, 5]

dummy.code(coffee$education_level) |> head()

coffee$ed_bachelor <- dummy.code(coffee$education_level)[, 1]
coffee$ed_master <- dummy.code(coffee$education_level)[, 2]
coffee$ed_associate <- dummy.code(coffee$education_level)[, 3]
coffee$ed_doc <- dummy.code(coffee$education_level)[, 4]
coffee$ed_hsgrad <- dummy.code(coffee$education_level)[, 5]
coffee$ed_lessthanhs <- dummy.code(coffee$education_level)[, 6]

dummy.code(coffee$ethnicity_race) |> head()

coffee$race_white <- dummy.code(coffee$ethnicity_race)[, 1]
coffee$race_asian <- dummy.code(coffee$ethnicity_race)[, 2]
coffee$race_latino <- dummy.code(coffee$ethnicity_race)[, 3]
coffee$race_other <- dummy.code(coffee$ethnicity_race)[, 4]
coffee$race_black <- dummy.code(coffee$ethnicity_race)[, 5]
coffee$race_alaska <- dummy.code(coffee$ethnicity_race)[, 6]

dummy.code(coffee$employment_status) |> head()

coffee$emp_fulltime <- dummy.code(coffee$employment_status)[, 1]
coffee$emp_student <- dummy.code(coffee$employment_status)[, 2]
coffee$emp_parttime <- dummy.code(coffee$employment_status)[, 3]
coffee$emp_unemployed <- dummy.code(coffee$employment_status)[, 4]
coffee$emp_homemaker <- dummy.code(coffee$employment_status)[, 5]
coffee$emp_retired <- dummy.code(coffee$employment_status)[, 6]

dummy.code(coffee$number_of_children) |> head()

coffee$child_none <- dummy.code(coffee$number_of_children)[, 1]
coffee$child_2 <- dummy.code(coffee$number_of_children)[, 2]
coffee$child_1 <- dummy.code(coffee$number_of_children)[, 3]
coffee$child_3 <- dummy.code(coffee$number_of_children)[, 4]
coffee$child_more3 <- dummy.code(coffee$number_of_children)[, 5]

dummy.code(coffee$political_affiliation) |> head()

coffee$political_dem <- dummy.code(coffee$political_affiliation)[, 1]
coffee$political_noaff <- dummy.code(coffee$political_affiliation)[, 2]
coffee$political_ind <- dummy.code(coffee$political_affiliation)[, 3]
coffee$political_rep <- dummy.code(coffee$political_affiliation)[, 4]

dummy.code(coffee$remote_work) |> head()

coffee$remote_home <- dummy.code(coffee$remote_work)[, 1]
coffee$remote_inperson <- dummy.code(coffee$remote_work)[, 2]
coffee$remote_both <- dummy.code(coffee$remote_work)[, 3]

dummy.code(coffee$money_spend_a_month) |> head()

coffee$spentmonth_2040 <- dummy.code(coffee$money_spend_a_month)[, 1]
coffee$spentmonth_4060 <- dummy.code(coffee$money_spend_a_month)[, 2]
coffee$spentmonth_6080 <- dummy.code(coffee$money_spend_a_month)[, 3]
coffee$spentmonth_less20 <- dummy.code(coffee$money_spend_a_month)[, 4]
coffee$spentmonth_80100 <- dummy.code(coffee$money_spend_a_month)[, 5]
coffee$spentmonth_more100 <- dummy.code(coffee$money_spend_a_month)[, 6]

dummy.code(coffee$equipment_spent_5years) |> head()

coffee$equip5yr_more1000 <- dummy.code(coffee$equipment_spent_5years)[, 1]
coffee$equip5yr_100300 <- dummy.code(coffee$equipment_spent_5years)[, 2]
coffee$equip5yr_5001000 <- dummy.code(coffee$equipment_spent_5years)[, 3]
coffee$equip5yr_300500 <- dummy.code(coffee$equipment_spent_5years)[, 4]
coffee$equip5yr_50100 <- dummy.code(coffee$equipment_spent_5years)[, 5]
coffee$equip5yr_2050 <- dummy.code(coffee$equipment_spent_5years)[, 6]
coffee$equip5yr_less20 <- dummy.code(coffee$equipment_spent_5years)[, 7]


coffee |> glimpse()

coffee_bi <- coffee |>
  select(
    -c(
      remote_work,
      money_spend_a_month,
      equipment_spent_5years,
      gender:political_affiliation
    )
  )

glimpse(coffee_bi)

coffee_bi <- coffee_bi |>
  mutate(
    across(
      c(
        good_value_equipment,
        good_value_cafe,
        know_where_coffee_comes_from,
        like_taste
      ),
      ~case_when(
        .x == "Yes" ~ 1,
        .x == "No" ~ 0
      )
    )
  )

id <- coffee$submission_id

coffee_bi <- coffee_bi |>
  select(-submission_id)



# BAYESIAN STATISTICS
stan_list <- list(
  N = nrow(coffee_bi[,-1]),
  K = count(coffee_bi, favorite_abcd) |> nrow(),
  I = ncol(coffee_bi[,-1]),
  #Y = coffee_bi$favorite_abcd,

  drink_at_home = coffee_bi$drink_at_home,
  drink_at_office = coffee_bi$drink_at_office,
  drink_on_go = coffee_bi$drink_on_go,
  drink_at_cafe = coffee_bi$drink_at_cafe,
  drink_none_of_these = coffee_bi$drink_none_of_these,

  home_brew_pour_over = coffee_bi$home_brew_pour_over,
  home_brew_french_press = coffee_bi$home_brew_french_press,
  home_brew_espresso = coffee_bi$home_brew_espresso,
  home_brew_mr_coffee = coffee_bi$home_brew_mr_coffee,
  home_brew_pods = coffee_bi$home_brew_pods,
  home_brew_instant = coffee_bi$home_brew_instant,
  home_brew_bean2cup = coffee_bi$home_brew_bean2cup,
  home_brew_cold_brew = coffee_bi$home_brew_cold_brew,
  home_brew_cometeer = coffee_bi$home_brew_cometeer,
  home_brew_other = coffee_bi$home_brew_other,

  coffee_black = coffee_bi$coffee_black,
  coffee_milk_alt_creamer = coffee_bi$coffee_milk_alt_creamer,
  coffee_sugar = coffee_bi$coffee_sugar,
  coffee_syrup = coffee_bi$coffee_syrup,
  coffee_other = coffee_bi$coffee_other,

  why_drink_taste_good = coffee_bi$why_drink_taste_good,
  why_drink_caffeine = coffee_bi$why_drink_caffeine,
  why_drink_ritual = coffee_bi$why_drink_ritual,
  why_drink_makes_bathroom = coffee_bi$why_drink_makes_bathroom,
  why_drink_other = coffee_bi$why_drink_other,

  like_taste = coffee_bi$like_taste,
  know_where_coffee_comes_from = coffee_bi$know_where_coffee_comes_from,
  good_value_cafe = coffee_bi$good_value_cafe,
  good_value_equipment = coffee_bi$good_value_equipment,

  cup_per_day1 = coffee_bi$cup_per_day1,
  cup_per_day2 = coffee_bi$cup_per_day2,
  cup_per_day3 = coffee_bi$cup_per_day3,
  cup_per_dayless1 = coffee_bi$cup_per_dayless1,
  cup_per_day4 = coffee_bi$cup_per_day4,
  cup_per_daymore4 = coffee_bi$cup_per_daymore4,

  favorite_coffee_drip = coffee_bi$favorite_coffee_drip,
  favorite_coffee_pourover = coffee_bi$favorite_coffee_pourover,
  favorite_coffee_latte = coffee_bi$favorite_coffee_latte,
  favorite_coffee_espresso = coffee_bi$favorite_coffee_espresso,
  favorite_coffee_cappuccino = coffee_bi$favorite_coffee_cappuccino,
  favorite_coffee_cortado = coffee_bi$favorite_coffee_cortado,
  favorite_coffee_americano = coffee_bi$favorite_coffee_americano,
  favorite_coffee_icedcoffee = coffee_bi$favorite_coffee_icedcoffee,
  favorite_coffee_mocha = coffee_bi$favorite_coffee_mocha,
  favorite_coffee_coldbrew = coffee_bi$favorite_coffee_coldbrew,
  favorite_coffee_other = coffee_bi$favorite_coffee_other,
  favorite_coffee_blended = coffee_bi$favorite_coffee_blended,

  coffee_char_pref_chocolatey = coffee_bi$coffee_char_pref_chocolatey,
  coffee_char_pref_fruity = coffee_bi$coffee_char_pref_fruity,    
  coffee_char_pref_fullbody = coffee_bi$coffee_char_pref_fullbody,
  coffee_char_pref_bright = coffee_bi$coffee_char_pref_bright,
  coffee_char_pref_juicy = coffee_bi$coffee_char_pref_juicy,
  coffee_char_pref_nutty = coffee_bi$coffee_char_pref_nutty,
  coffee_char_pref_sweet = coffee_bi$coffee_char_pref_sweet,
  coffee_char_pref_caramalized = coffee_bi$coffee_char_pref_caramalized,
  coffee_char_pref_floral = coffee_bi$coffee_char_pref_floral,
  coffee_char_pref_bold = coffee_bi$coffee_char_pref_bold,

  coffee_strength_medium = coffee_bi$coffee_strength_medium,
  coffee_strength_somestrong = coffee_bi$coffee_strength_somestrong,
  coffee_strength_verystrong = coffee_bi$coffee_strength_verystrong,
  coffee_strength_somelight = coffee_bi$coffee_strength_somelight,
  coffee_strength_weak = coffee_bi$coffee_strength_weak,

  roast_pref_medium = coffee_bi$roast_pref_medium,
  roast_pref_light = coffee_bi$roast_pref_light,
  roast_pref_dark = coffee_bi$roast_pref_dark,
  roast_pref_nordic = coffee_bi$roast_pref_nordic,
  roast_pref_blonde = coffee_bi$roast_pref_blonde,
  roast_pref_french = coffee_bi$roast_pref_french,
  roast_pref_italian = coffee_bi$roast_pref_italian,

  caffeine_pref_full = coffee_bi$caffeine_pref_full,
  caffeine_pref_half = coffee_bi$caffeine_pref_half,
  caffeine_pref_decaf = coffee_bi$caffeine_pref_decaf,

  expert5 = coffee_bi$expert5,
  expert7 = coffee_bi$expert7,
  expert6 = coffee_bi$expert6,
  expert8 = coffee_bi$expert8,
  expert4 = coffee_bi$expert4,
  expert3 = coffee_bi$expert3,
  expert2 = coffee_bi$expert2,
  expert1 = coffee_bi$expert1,
  expert9 = coffee_bi$expert9,
  expert10 = coffee_bi$expert10,

  coffee_a_bitter1 = coffee_bi$coffee_a_bitter1,
  coffee_a_bitter2 = coffee_bi$coffee_a_bitter2,          
  coffee_a_bitter3 = coffee_bi$coffee_a_bitter3,
  coffee_a_bitter4 = coffee_bi$coffee_a_bitter4,
  coffee_a_bitter5 = coffee_bi$coffee_a_bitter5,

  coffee_a_acid1 = coffee_bi$coffee_a_acid1,
  coffee_a_acid4 = coffee_bi$coffee_a_acid4,
  coffee_a_acid3 = coffee_bi$coffee_a_acid3,
  coffee_a_acid5 = coffee_bi$coffee_a_acid5,
  coffee_a_acid2 = coffee_bi$coffee_a_acid2,

  coffee_a_pref1 = coffee_bi$coffee_a_pref1,
  coffee_a_pref4 = coffee_bi$coffee_a_pref4,
  coffee_a_pref3 = coffee_bi$coffee_a_pref3,
  coffee_a_pref2 = coffee_bi$coffee_a_pref2,
  coffee_a_pref5 = coffee_bi$coffee_a_pref5,

  coffee_b_bitter1 = coffee_bi$coffee_b_bitter1,
  coffee_b_bitter3 = coffee_bi$coffee_b_bitter3,
  coffee_b_bitter4 = coffee_bi$coffee_b_bitter4,
  coffee_b_bitter2 = coffee_bi$coffee_b_bitter2,
  coffee_b_bitter5 = coffee_bi$coffee_b_bitter5,

  coffee_b_acid1 = coffee_bi$coffee_b_acid1,
  coffee_b_acid2 = coffee_bi$coffee_b_acid2,
  coffee_b_acid3 = coffee_bi$coffee_b_acid3,
  coffee_b_acid4 = coffee_bi$coffee_b_acid4,
  coffee_b_acid5 = coffee_bi$coffee_b_acid5,

  coffee_b_pref1 = coffee_bi$coffee_b_pref1,
  coffee_b_pref3 = coffee_bi$coffee_b_pref3,
  coffee_b_pref2 = coffee_bi$coffee_b_pref2,
  coffee_b_pref4 = coffee_bi$coffee_b_pref4,
  coffee_b_pref5 = coffee_bi$coffee_b_pref5,

  coffee_c_bitter1 = coffee_bi$coffee_c_bitter1,
  coffee_c_bitter3 = coffee_bi$coffee_c_bitter3,
  coffee_c_bitter4 = coffee_bi$coffee_c_bitter4,
  coffee_c_bitter2 = coffee_bi$coffee_c_bitter2,
  coffee_c_bitter5 = coffee_bi$coffee_c_bitter5,

  coffee_c_acid1 = coffee_bi$coffee_c_acid1,
  coffee_c_acid2 = coffee_bi$coffee_c_acid2,
  coffee_c_acid3 = coffee_bi$coffee_c_acid3,
  coffee_c_acid4 = coffee_bi$coffee_c_acid4,
  coffee_c_acid5 = coffee_bi$coffee_c_acid5,

  coffee_c_pref1 = coffee_bi$coffee_c_pref1,
  coffee_c_pref3 = coffee_bi$coffee_c_pref3,
  coffee_c_pref4 = coffee_bi$coffee_c_pref4,
  coffee_c_pref2 = coffee_bi$coffee_c_pref2,
  coffee_c_pref5 = coffee_bi$coffee_c_pref5,

  coffee_d_bitter1 = coffee_bi$coffee_d_bitter1,
  coffee_d_bitter2 = coffee_bi$coffee_d_bitter2,
  coffee_d_bitter3 = coffee_bi$coffee_d_bitter3,
  coffee_d_bitter4 = coffee_bi$coffee_d_bitter4,
  coffee_d_bitter5 = coffee_bi$coffee_d_bitter5,

  coffee_d_acid1 = coffee_bi$coffee_d_acid1,
  coffee_d_acid4 = coffee_bi$coffee_d_acid4,
  coffee_d_acid5 = coffee_bi$coffee_d_acid5,
  coffee_d_acid3 = coffee_bi$coffee_d_acid3,
  coffee_d_acid2 = coffee_bi$coffee_d_acid2,

  coffee_d_pref1 = coffee_bi$coffee_d_pref1,
  coffee_d_pref5 = coffee_bi$coffee_d_pref5,
  coffee_d_pref4 = coffee_bi$coffee_d_pref4,
  coffee_d_pref3 = coffee_bi$coffee_d_pref3,
  coffee_d_pref2 = coffee_bi$coffee_d_pref2,

  most_spent_68 = coffee_bi$most_spent_68,
  most_spent_810 = coffee_bi$most_spent_810,
  most_spent_1015 = coffee_bi$most_spent_1015,
  most_spent_46 = coffee_bi$most_spent_46,
  most_spent_1520 = coffee_bi$most_spent_1520,
  most_spent_more20 = coffee_bi$most_spent_more20,
  most_spent_24 = coffee_bi$most_spent_24,
  most_spent_less2 = coffee_bi$most_spent_less2,
  
  willing_spend_810 = coffee_bi$willing_spend_810,
  willing_spend_1015 = coffee_bi$willing_spend_1015,
  willing_spend_68 = coffee_bi$willing_spend_68,
  willing_spend_more20 = coffee_bi$willing_spend_more20,       
  willing_spend_1520 = coffee_bi$willing_spend_1520,
  willing_spend_46 = coffee_bi$willing_spend_46,
  willing_spend_24 = coffee_bi$willing_spend_24,
  willing_spend_less2 = coffee_bi$willing_spend_less2,

  
  age_2534 = coffee_bi$age_2534,
  age_3544 = coffee_bi$age_3544,
  age_1824 = coffee_bi$age_1824,
  age_4554 = coffee_bi$age_4554,
  age_5564 = coffee_bi$age_5564,
  age_over65 = coffee_bi$age_over65,
  age_under18 = coffee_bi$age_under18,

  gen_male = coffee_bi$gen_male,
  gen_female = coffee_bi$gen_female,
  gen_nonbi = coffee_bi$gen_nonbi,
  gen_nottosay = coffee_bi$gen_nottosay,
  gen_other = coffee_bi$gen_other,

  ed_bachelor = coffee_bi$ed_bachelor,
  ed_master = coffee_bi$ed_master,
  ed_associate = coffee_bi$ed_associate,
  ed_doc = coffee_bi$ed_doc,
  ed_hsgrad = coffee_bi$ed_hsgrad,
  ed_lessthanhs = coffee_bi$ed_lessthanhs,

  race_white = coffee_bi$race_white,
  race_asian = coffee_bi$race_asian,
  race_latino = coffee_bi$race_latino,
  race_other = coffee_bi$race_other,
  race_black = coffee_bi$race_black,
  race_alaska = coffee_bi$race_alaska,

  emp_fulltime = coffee_bi$emp_fulltime,
  emp_student = coffee_bi$emp_student,
  emp_parttime = coffee_bi$emp_parttime,
  emp_unemployed = coffee_bi$emp_unemployed,
  emp_homemaker = coffee_bi$emp_homemaker,
  emp_retired = coffee_bi$emp_retired,

  child_none = coffee_bi$child_none,
  child_2 = coffee_bi$child_2,
  child_1 = coffee_bi$child_1,
  child_3 = coffee_bi$child_3,
  child_more3 = coffee_bi$child_more3,

  political_dem = coffee_bi$political_dem,
  political_noaff = coffee_bi$political_noaff,
  political_ind = coffee_bi$political_ind,
  political_rep = coffee_bi$political_rep,

  remote_home = coffee_bi$remote_home,
  remote_inperson = coffee_bi$remote_inperson,
  remote_both = coffee_bi$remote_both,

  spentmonth_2040 = coffee_bi$spentmonth_2040,
  spentmonth_4060 = coffee_bi$spentmonth_4060,
  spentmonth_6080 = coffee_bi$spentmonth_6080,
  spentmonth_less20 = coffee_bi$spentmonth_less20,
  spentmonth_80100 = coffee_bi$spentmonth_80100,
  spentmonth_more100 = coffee_bi$spentmonth_more100,

  equip5yr_more1000 = coffee_bi$equip5yr_more1000,
  equip5yr_100300 = coffee_bi$equip5yr_100300,
  equip5yr_5001000 = coffee_bi$equip5yr_5001000,
  equip5yr_300500 = coffee_bi$equip5yr_300500,
  equip5yr_50100 = coffee_bi$equip5yr_50100,
  equip5yr_2050 = coffee_bi$equip5yr_2050,
  equip5yr_less20 = coffee_bi$equip5yr_less20
)

glimpse(stan_list)

rm(mod, fit)

set.seed(12345)
mod <- cmdstan_model(here::here("random_scripts", "jp_stan2.stan"))

# mod$format(
#   canonicalize = list("deprecations"),
#   overwrite_file = TRUE,
#   backup = FALSE
# )

fit <- mod$sample(
  data = stan_list,
  seed = 12345,
  iter_warmup = 2000,
  iter_sampling = 2000,
  # adapt_delta = .90,
  chains = 4,
  # step_size = .01,
  parallel_chains = 8
)

# fit$output()[[1]]

fit$diagnostic_summary()

rm(bn_measure, bn_converge)

bn_measure <- summarize_draws(fit$draws(), default_summary_measures())
bn_converge <- summarize_draws(fit$draws(), default_convergence_measures())

bn_converge |>
  arrange(rhat) |>
  mutate(
    across(
      -variable,
      ~round(.x, 2)
    )
  ) |>
  reactable()

bn_measure |>
  filter(
    str_detect(
      variable,
      "theta"
    )
  ) |> 
  mutate(
    across(
      -variable,
      ~round(.x, 2)
    )
  ) |>
  select(
    variable,
    mean,
    sd
  ) |>
  reactable()

alpha_cafe <- fit$draws("alpha_good_value_cafe") |> as_draws_matrix()
beta_home_cafe <- fit$draws("remote_home_beta_good_value_cafe") |> as_draws_matrix()
liketaste <- fit$draws("theta_like_taste") |> as_draws_matrix()

mcmc_trace(alpha_cafe)
mcmc_trace(beta_home_cafe)
mcmc_areas(liketaste[, 1:30])