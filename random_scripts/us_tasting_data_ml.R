library(tidyverse)
# library(mlr3verse)
# library(mlr3extralearners)
library(inspectdf)
library(bnlearn)
library(rstan)
library(cmdstanr)
library(posterior)
library(bayesplot)

coffee <- read_csv(here::here("random_data", "hoffmann_america_taste_data.csv")) |>
  janitor::clean_names()

coffee |>
  inspect_na() |>
  show_plot()

coffee_drop <- coffee[, which(colMeans(!is.na(coffee)) > 0.5)]

inspect_na(coffee_drop)

coffee_drop <- coffee_drop |>
  select(
    -c(
      where_do_you_typically_drink_coffee,
      how_do_you_brew_coffee_at_home,
      do_you_usually_add_anything_to_your_coffee,
      why_do_you_drink_coffee
    )
  ) |>
  rename(
    age = what_is_your_age,
    cup_per_day = how_many_cups_of_coffee_do_you_typically_drink_per_day,
    drink_at_home = where_do_you_typically_drink_coffee_at_home,
    drink_at_office = where_do_you_typically_drink_coffee_at_the_office,
    drink_on_go = where_do_you_typically_drink_coffee_on_the_go,
    drink_at_cafe = where_do_you_typically_drink_coffee_at_a_cafe,
    drink_none_of_these = where_do_you_typically_drink_coffee_none_of_these,
    home_brew_pour_over = how_do_you_brew_coffee_at_home_pour_over,
    home_brew_french_press = how_do_you_brew_coffee_at_home_french_press,
    home_brew_espresso = how_do_you_brew_coffee_at_home_espresso,
    home_brew_mr_coffee = how_do_you_brew_coffee_at_home_coffee_brewing_machine_e_g_mr_coffee,
    home_brew_pods = how_do_you_brew_coffee_at_home_pod_capsule_machine_e_g_keurig_nespresso,
    home_brew_instant = how_do_you_brew_coffee_at_home_instant_coffee,
    home_brew_bean2cup = how_do_you_brew_coffee_at_home_bean_to_cup_machine,
    home_brew_cold_brew = how_do_you_brew_coffee_at_home_cold_brew,
    home_brew_cometeer = how_do_you_brew_coffee_at_home_coffee_extract_e_g_cometeer,
    home_brew_other = how_do_you_brew_coffee_at_home_other,
    favorite_coffee_drink = what_is_your_favorite_coffee_drink,
    coffee_black = do_you_usually_add_anything_to_your_coffee_no_just_black,
    coffee_milk_alt_creamer = do_you_usually_add_anything_to_your_coffee_milk_dairy_alternative_or_coffee_creamer,
    coffee_sugar = do_you_usually_add_anything_to_your_coffee_sugar_or_sweetener,
    coffee_syrup = do_you_usually_add_anything_to_your_coffee_flavor_syrup,
    coffee_other = do_you_usually_add_anything_to_your_coffee_other,
    coffee_characteristic_preference = before_todays_tasting_which_of_the_following_best_described_what_kind_of_coffee_you_like,
    coffee_strength = how_strong_do_you_like_your_coffee,
    roast_preference = what_roast_level_of_coffee_do_you_prefer,
    caffeine_preference = how_much_caffeine_do_you_like_in_your_coffee,
    expertise = lastly_how_would_you_rate_your_own_coffee_expertise,
    preference_a_to_b = between_coffee_a_coffee_b_and_coffee_c_which_did_you_prefer,
    preference_a_to_d = between_coffee_a_and_coffee_d_which_did_you_prefer,
    favorite_abcd = lastly_what_was_your_favorite_overall_coffee,
    remote_work = do_you_work_from_home_or_in_person,
    money_spend_a_month = in_total_much_money_do_you_typically_spend_on_coffee_in_a_month,
    why_drink_taste_good = why_do_you_drink_coffee_it_tastes_good,
    why_drink_caffeine = why_do_you_drink_coffee_i_need_the_caffeine,
    why_drink_ritual = why_do_you_drink_coffee_i_need_the_ritual,
    why_drink_makes_bathroom = why_do_you_drink_coffee_it_makes_me_go_to_the_bathroom,
    why_drink_other = why_do_you_drink_coffee_other,
    like_taste = do_you_like_the_taste_of_coffee,
    know_where_coffee_comes_from = do_you_know_where_your_coffee_comes_from,
    most_spent_on_cup_coffee = what_is_the_most_youve_ever_paid_for_a_cup_of_coffee,
    willing_to_spend_cup_coffee = what_is_the_most_youd_ever_be_willing_to_pay_for_a_cup_of_coffee,
    good_value_cafe = do_you_feel_like_you_re_getting_good_value_for_your_money_when_you_buy_coffee_at_a_cafe,
    equipment_spent_5years = approximately_how_much_have_you_spent_on_coffee_equipment_in_the_past_5_years,
    good_value_equipment = do_you_feel_like_you_re_getting_good_value_for_your_money_with_regards_to_your_coffee_equipment
  )

count(coffee_drop, expertise)

coffee_logical <- coffee_drop |>
  select_if(is.logical)

coffee_drop <- coffee_drop |>
  drop_na(
    colnames(coffee_logical)
  )

coffee_drop <- coffee_drop |>
  mutate(
    across(
      where(
        is.logical
      ),
      ~case_when(
        .x == TRUE ~ 1,
        .x == FALSE ~ 0
      )
    ),
    across(
      where(
        is.character
      ),
      ~as.factor(.x)
    )
  )

coffee_drop <- coffee_drop |>
  select(
    -matches(
      "_notes"
    )
  )

glimpse(coffee_drop)

coffee_drop |>
  inspect_na() |>
  show_plot()

count(coffee_drop, home_brew_pour_over)

lgr::get_logger("mlr3")$set_threshold("trace")
lgr::get_logger("bbotk")$set_threshold("trace")

glimpse((data))

set.seed(12345)
task <- as_task_regr(
  x = data,
  target = "expertise"
)

set.seed(12345)
task$col_roles$stratum <- "expertise"

set.seed(12345)
graph <- po(
  "missind",
  affect_columns = selector_type("factor")
  ) %>>%
  po(
    "imputehist", 
    affect_columns = selector_type("numeric")
  ) %>>%
  po(
     "scale",
     affect_columns = selector_type(c("numeric", "integer"))
   ) %>>%
  po(
    "encode",
    affect_columns = selector_type("factor")
  )

set.seed(12345)
grapher <- graph$clone()$train(task)[[1]]

grapher$data() |> glimpse()


# mlr_learners
set.seed(12345)
glmnet_learn <- lrn(
  "regr.glmnet",
  alpha = 0,
  lambda.min.ratio = .5
  )

less_learn <- lrn(
  "regr.featureless"
)

set.seed(12345)
splits <- partition(task, ratio = 0.8)

set.seed(12345)
cv <- rsmp("cv", folds = 5)$instantiate(task)

set.seed(12345)
less_learn$train(grapher, splits$train)
glmnet_learn$train(grapher, splits$train)

less_learn$model
less_learn$state

glmnet_learn$model
glmnet_learn$state

set.seed(12345)
less_train_pred <- less_learn$predict(grapher, splits$train)
less_train_pred$score()
sqrt(3.80)

set.seed(12345)
glmnet_train_pred <- glmnet_learn$predict(grapher, splits$train)
glmnet_train_pred$score()
sqrt(3.76)

set.seed(12345)
glmnet_rr <- resample(
  task = grapher,
  learner = glmnet_learn,
  resampling = cv,
  store_models = TRUE
)
glmnet_rr

set.seed(12345)
glmnet_rr$score()
glmnet_rr$aggregate()
sqrt(3.76)

set.seed(12345)
glmnet_tune <- lrn(
  "regr.cv_glmnet",
  alpha = to_tune(0, 1),
  lambda.min.ratio = to_tune(0, .99)
)

arnold <- trm("run_time", secs = 60)

future::plan("multisession", workers = 4)

set.seed(12345)
instance <- ti(
  task = grapher,
  learner = glmnet_tune,
  resampling = cv,
  terminator = arnold,
  store_models = TRUE,
  search_space = NULL
)

set.seed(12345)
tuner <- tnr("grid_search", resolution = 10, batch_size = 5)

instance

tuner$param_set

set.seed(12345)
tuner$optimize(instance)

set.seed(12345)
as.data.table(instance$archive) |>
  arrange(regr.mse) |>
  head()
sqrt(2.83)

glmnet_tuned <- lrn("regr.glmnet")
glmnet_tuned$param_set$values <- instance$result_learner_param_vals

set.seed(12345)
glmnet_tuned$train(grapher, splits$train)

set.seed(12345)
glmnet_tuned_train_pred <- glmnet_tuned$predict(grapher, splits$train)
glmnet_tuned_train_pred$score()
sqrt(2.70)

set.seed(12345)
glmnet_tuned_rr <- resample(
  task = grapher,
  learner = glmnet_tuned,
  resampling = cv,
  store_models = TRUE
)
glmnet_tuned_rr

set.seed(12345)
glmnet_tuned_rr$score()
glmnet_tuned_rr$aggregate()
sqrt(2.79)

set.seed(12345)
rf_learn <- lrn(
  "regr.ranger",
  num.trees = 1000,
  importance = "permutation",
  num.threads = 4
)

set.seed(12345)
rf_learn$train(grapher, splits$train)

rf_learn$model
rf_learn$state

imp_tbl <- tibble(
  names = names(rf_learn$clone()$importance()),
  values = rf_learn$clone()$importance() |> as.vector()
) |>
  mutate(
    names = as.factor(names)
  )

imp_tbl |>
  ggplot(
    aes(
      fct_reorder(
        names,
        values
      ),
      values
    )
  ) +
  geom_col(
    color = "black",
    fill = "dodgerblue"
  ) +
  coord_flip() +
  theme_light()

set.seed(12345)
rf_learn_train_pred <- rf_learn$predict(grapher, splits$train)
rf_learn_train_pred$score()
sqrt(.85)

set.seed(12345)
rf_tune <- lrn(
  "regr.ranger",
  num.trees = 1000,
  importance = "permutation",
  num.threads = 4,
  max.depth = to_tune(0, 20),
  min.node.size = to_tune(2, 20)
)

set.seed(12345)
rf_instance <- ti(
  task = grapher,
  learner = rf_tune,
  resampling = cv,
  terminator = arnold,
  store_models = TRUE
)

set.seed(12345)
tuner$optimize(rf_instance)

set.seed(12345)
as.data.table(rf_instance$archive) |>
  arrange(regr.mse) |>
  head()
sqrt(2.78)

rf_tuned <- lrn("regr.ranger")
rf_tuned$param_set$values <- rf_instance$result_learner_param_vals


set.seed(12345)
rf_tuned$train(grapher, splits$train)

rf_tuned$model
# rf_tuned$state

imp_tbl2 <- tibble(
  names = names(rf_tuned$clone()$importance()),
  values = rf_tuned$clone()$importance() |> as.vector()
) |>
  mutate(
    names = as.factor(names)
  )

imp_tbl2 |>
  ggplot(
    aes(
      fct_reorder(
        names,
        values
      ),
      values
    )
  ) +
  geom_col(
    color = "black",
    fill = "dodgerblue"
  ) +
  coord_flip() +
  theme_light()


set.seed(12345)
rf_tuned_train_pred <- rf_tuned$predict(grapher, splits$train)
rf_tuned_train_pred$score()
sqrt(.90)

set.seed(12345)
rf_tuned_rr <- resample(
  task = grapher,
  learner = rf_tuned,
  resampling = cv,
  store_models = TRUE
)
rf_tuned_rr

set.seed(12345)
rf_tuned_rr$score()
rf_tuned_rr$aggregate()
sqrt(2.77)








# subset
data_sub <- data |>
  select(
    home_brew_espresso,
    home_brew_pour_over,
    coffee_d_personal_preference,
    coffee_a_personal_preference,
    coffee_black,
    why_drink_taste_good,
    drink_at_home,
    coffee_milk_alt_creamer,
    expertise
  )
data_sub

set.seed(12345)
task_sub <- as_task_regr(
  x = data_sub,
  target = "expertise"
)

set.seed(12345)
task_sub$col_roles$stratum <- "expertise"

set.seed(12345)
grapher_sub <- graph$clone()$train(task_sub)[[1]]

grapher_sub$data() |> glimpse()

set.seed(12345)
splits_sub <- partition(task_sub, ratio = 0.8)

set.seed(12345)
cv_sub <- rsmp("cv", folds = 5)$instantiate(task_sub)

set.seed(12345)
rf_learn_sub <- lrn(
  "regr.ranger",
  num.trees = 1000,
  importance = "permutation",
  num.threads = 4
)

set.seed(12345)
rf_learn_sub$train(grapher_sub, splits_sub$train)

rf_learn_sub$model
rf_learn_sub$state

imp_tbl_sub <- tibble(
  names = names(rf_learn_sub$clone()$importance()),
  values = rf_learn_sub$clone()$importance() |> as.vector()
) |>
  mutate(
    names = as.factor(names)
  )

imp_tbl_sub |>
  ggplot(
    aes(
      fct_reorder(
        names,
        values
      ),
      values
    )
  ) +
  geom_col(
    color = "black",
    fill = "dodgerblue"
  ) +
  coord_flip() +
  theme_light()

set.seed(12345)
rf_learn_sub_train_pred <- rf_learn_sub$predict(grapher_sub, splits_sub$train)
rf_learn_sub_train_pred$score()
sqrt(2.28)

set.seed(12345)
rf_tune_sub <- lrn(
  "regr.ranger",
  num.trees = 1000,
  importance = "permutation",
  num.threads = 4,
  max.depth = to_tune(0, 20),
  min.node.size = to_tune(2, 20)
)

set.seed(12345)
rf_instance_sub <- ti(
  task = grapher_sub,
  learner = rf_tune_sub,
  resampling = cv_sub,
  terminator = arnold,
  store_models = TRUE
)

set.seed(12345)
tuner$optimize(rf_instance_sub)

set.seed(12345)
as.data.table(rf_instance_sub$archive) |>
  arrange(regr.mse) |>
  head()
sqrt(2.83)

rf_tuned_sub <- lrn("regr.ranger")
rf_tuned_sub$param_set$values <- rf_instance_sub$result_learner_param_vals


set.seed(12345)
rf_tuned_sub$train(grapher_sub, splits_sub$train)

rf_tuned_sub$model
# rf_tuned_sub$state

imp_tbl2_sub <- tibble(
  names = names(rf_tuned_sub$clone()$importance()),
  values = rf_tuned_sub$clone()$importance() |> as.vector()
) |>
  mutate(
    names = as.factor(names)
  )

imp_tbl2_sub |>
  ggplot(
    aes(
      fct_reorder(
        names,
        values
      ),
      values
    )
  ) +
  geom_col(
    color = "black",
    fill = "dodgerblue"
  ) +
  coord_flip() +
  theme_light()


set.seed(12345)
rf_tuned_sub_train_pred <- rf_tuned_sub$predict(grapher_sub, splits_sub$train)
rf_tuned_sub_train_pred$score()
sqrt(2.47)

set.seed(12345)
rf_tuned_sub_rr <- resample(
  task = grapher_sub,
  learner = rf_tuned_sub,
  resampling = cv_sub,
  store_models = TRUE
)
rf_tuned_sub_rr

set.seed(12345)
rf_tuned_sub_rr$score()
rf_tuned_sub_rr$aggregate()
sqrt(2.83)


# predictions on test split
set.seed(12345)
rf_tuned_test_pred <- rf_tuned$predict(grapher, splits$test)
rf_tuned_test_pred$score()
sqrt(2.78)

set.seed(12345)
rf_tuned_sub_test_pred <- rf_tuned_sub$predict(grapher_sub, splits_sub$test)
rf_tuned_sub_test_pred$score()
sqrt(2.85)
