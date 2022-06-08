library(tidyverse)
library(tidymodels)
options(scipen = 999)
theme_set(theme_light())


hybrid <- read_csv("tidy_tuesday/hybrid.csv")

hybrid <- hybrid %>% 
  filter(year >= 2015) %>% 
  mutate(perm_drive_number = recode(perm_drive_number, "\\N" = NA_character_),
         q1 = recode(q1, "\\N" = NA_character_),
         final_driver_pos = recode(final_driver_pos, "\\N" = NA_character_),
         gp_finish_time = recode(gp_finish_time, "\\N" = NA_character_),
         gp_finish_time_ms = recode(gp_finish_time_ms, "\\N" = NA_character_)) %>% 
  mutate(race_id = as.factor(race_id),
         constructor_id = as.factor(constructor_id),
         constructor_season_pos = as.factor(constructor_season_pos),
         constructor_season_wins = as.factor(constructor_season_wins),
         round = as.factor(round),
         circuit_id = as.factor(circuit_id),
         constructor_nationality = as.factor(constructor_nationality),
         constructor_ref = as.factor(constructor_ref),
         driver_id = as.factor(driver_id),
         drive_season_position = as.factor(drive_season_position),
         drive_season_pos_text = as.character(drive_season_pos_text),
         drive_season_wins = as.factor(drive_season_wins),
         perm_drive_number = as.factor(perm_drive_number),
         driver_initial_id = as.factor(driver_initial_id),
         nationality = as.factor(nationality),
         race_pos = as.factor(race_pos),
         stop_number = as.factor(stop_number),
         driver_number = as.factor(driver_number),
         quali_pos = as.factor(quali_pos),
         grid = as.factor(grid),
         final_driver_pos = as.factor(final_driver_pos),
         position_order = as.factor(position_order),
         gp_finish_time_ms = as.numeric(gp_finish_time_ms),
         fast_lap_rank = as.factor(fast_lap_rank),
         circuit_ref = as.factor(circuit_ref),
         location = as.factor(location),
         country = as.factor(country),
         champion = as.factor(champion),
         constructor_champ = as.factor(constructor_champ),
         location = recode(location,
                           "S<e3>o Paulo" = "Sao Paulo",
                           "Portim<e3>o" = "Portimao",
                           "N<fc>rburg" = "Nurburg",
                           "Montmel<f3>" = "Montmelo"))

model_data <- hybrid %>% 
  dplyr::select(-X1, -X1_1, -rowid,
                -constructor_standings_id,
                -constructor_id,
                -constructor_season_pos_text,
                -constructor_results_id,
                -constructor_status,
                -gp_name,
                -gp_date,
                -gp_start_time,
                -gp_url,
                -constructor_name,
                -con_url,
                -driver_standings_id,
                -drive_season_pos_text,
                -driver_id,
                -perm_drive_number,
                -forename,
                -surname,
                -dob,
                -driver_url,
                -lap_time,
                -time_of_pit,
                -pit_duration,
                -qualify_id,
                -driver_number,
                -season_url,
                -q1,
                -q2,
                -q3,
                -result_id,
                -final_driver_pos_text,
                -gp_finish_time,
                -fastest_lap_time,
                -driver_status,
                -name,
                -circuit_url,
                -engine_disp,
                -unique_driver_id,
                -champion) %>% 
  mutate(driver_initial_id = fct_relevel(driver_initial_id, "HAM")) %>% 
  filter(driver_initial_id != "DIR" &
           driver_initial_id != "MER" &
           driver_initial_id != "RSS" &
           driver_initial_id != "STE")

# glimpse(hybrid)
# glimpse(model_data)

# look into getting counts of getting into q1, q2, and q3 - NOTE

set.seed(123021)

# model_data_sub <- model_data %>%
#   sample_frac(.5)

set.seed(123021)

hybrid_split <- initial_split(model_data, strata = drive_season_points)
# hybrid_split <- initial_split(model_data_sub, strata = champion)

hybrid_train <- training(hybrid_split)
hybrid_test <- testing(hybrid_split)

set.seed(123021)

lm_model <- linear_reg() %>% 
  set_engine("lm") %>% 
  set_mode("regression") #%>% 
  # set_args(penalty = tune(),
  #          mixture = tune())

glimpse(hybrid_train)

set.seed(123021)

lm_fit <- lm_model %>% 
  fit(drive_season_points ~ lat*long*alt +
        driver_initial_id +
        drive_season_wins +
        lap_time_number +
        race_pos +
        round +
        year +
        quali_pos + 
        lap_time_ms +
        stop_number +
        pit_lap_number +
        pit_dur_ms +
        completed_laps +
        gp_finish_time_ms +
        circuit_ref,
      data = hybrid_train)

# lm_fit %>%
#   tidy() %>% 
#   View()

lm_pred <- predict(lm_fit, hybrid_train) %>% 
  bind_cols(hybrid_train %>% 
              select(drive_season_points))

lm_pred %>% 
  metrics(truth = drive_season_points, estimate = .pred)


set.seed(123021)

lm_recipe <- recipe(drive_season_points ~ lat + long + alt +
                      driver_initial_id +
                      drive_season_wins +
                      lap_time_number +
                      race_pos +
                      round +
                      year +
                      quali_pos + 
                      lap_time_ms +
                      stop_number +
                      pit_lap_number +
                      pit_dur_ms +
                      completed_laps +
                      gp_finish_time_ms +
                      circuit_ref,
                    data = hybrid_train) %>%
  step_dummy(all_nominal_predictors()) %>%
  step_interact(terms =~ lat:long:alt) %>% 
  step_zv(all_predictors()) %>%
  step_nzv(all_predictors())

f1_fold <- vfold_cv(hybrid_train, v = 5)

set.seed(123021)

lm_flow <- workflow() %>%
  add_recipe(lm_recipe) %>%
  add_model(lm_model)

lm_fit <- tune_grid(lm_flow,
                    resamples = f1_fold,
                    control = control_resamples(verbose = TRUE,
                                                save_pred = TRUE))

lm_fit$.notes %>%
  pluck(1) %>%
  View()

# lm_fit %>%
#   show_fit()

lm_fit %>%
  collect_metrics()

# lm_fit %>%
#   select_best(metric = "rmse")



library(ranger)

get_model <- function(x) {
  pull_workflow_fit(x) %>% tidy()
}

set.seed(123021)

# model_recipe <- recipe(champion ~ .,
#                        data = hybrid_train) %>% 
#   update_role(constructor_results_id, qualify_id,
#               constructor_standings_id, driver_standings_id,
#               result_id, status_id,
#               new_role = "id") %>% 
#   update_role(q1, q2, q3,
#               new_role = "min_sec_ms") %>%
#   step_dummy(all_nominal_predictors(), -has_role("id")) %>% 
#   step_zv(all_predictors()) %>% 
#   step_nzv(all_predictors())

set.seed(123021)

# cores <- parallel::detectCores()

# model <- rand_forest() %>% 
#   set_engine("ranger",
#              num.threads = cores,
#              importance = "permutation",
#              verbose = TRUE) %>% 
#   set_mode("classification") %>% 
#   set_args(trees = 1000)

# random_gump <- rand_forest() %>% 
#   set_engine("ranger",
#              num.threads = cores,
#              importance = "permutation",
#              verbose = TRUE) %>% 
#   set_mode("regression") %>% 
#   set_args(mtry = tune(),
#            trees = 1000,
#            min_n = tune())

set.seed(123021)

# f1_fold <- vfold_cv(hybrid_train, v = 5)

set.seed(123021)

# f1_flow <- workflow() %>% 
  # add_recipe(model_recipe) %>% 
  # add_model(model)

set.seed(123021)

one_model_fit <- model %>% 
  fit(champion ~ .,
      data = hybrid_train)

# re_fit <- tune_grid(f1_flow,
#                     resamples = f1_fold,
#                     control = control_resamples(verbose = TRUE,
#                                                 save_pred = TRUE,
#                                                 extract = get_model))

# one_model_pred <- 
  predict(one_model_fit, hybrid_train) %>% 
  bind_cols(predict(one_model_fit, hybrid_train, type = "prob"))




# re_fit$.notes %>% 
#   pluck(1)

# re_fit %>% 
#   show_fit()

# re_fit %>% 
#   collect_metrics()

# re_fit %>% 
#   select_best()


