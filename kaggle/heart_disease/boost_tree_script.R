library(tidyverse)
library(tidymodels)

heart <- read_csv("C:/Users/cpppe/Downloads/Heart_Disease_Prediction.csv") |>
  janitor::clean_names()

theme_set(theme_light())




heart <- heart %>% 
  mutate(
    sex = recode(sex,
                 '1' = 'male',
                 '0' = 'female'),
    sex = as.factor(sex),
    chest_pain = as.factor(cp),
    fbs = recode(fbs,
                 '1' = 'true',
                 '0' = 'false'),
    fbs = as.factor(fbs),
    restecg = as.factor(restecg),
    exang = recode(exang,
                   '1' = 'yes',
                   '0' = 'no'),
    exang = as.factor(exang),
    slope = as.factor(slope),
    ca = as.factor(ca),
    thal = recode(thal,
                  '0' = NA_character_,
                  '1' = 'normal',
                  '2' = 'fixed_defect',
                  '3' = 'reversible_defect'),
    thal = as.factor(thal),
    target = as.factor(target)
    ) %>% 
  select(-cp) %>% 
  drop_na(thal)

map2(
  heart,
  names(heart),
  ~ggplot(data = heart,
          aes(.x, target)) + 
    geom_point(alpha = .5) + 
    geom_smooth(method = 'glm',
                se = FALSE,
                method.args = list('binomial')) +
    labs(x = glue::glue('{.y}'))
)

set.seed(12345)

heart_split <- initial_split(heart,
                             prop = .8,
                             strata = 'target')

heart_train <- training(heart_split)
heart_test <- testing(heart_split)

heart_fold <- vfold_cv(heart_train, v = 10)

library(ranger)

set.seed(12345)

boost_model <- boost_tree() %>% 
  set_mode('classification') %>% 
  set_engine('xgboost') %>% 
  set_args(
    trees = 1000,
    tree_depth = tune(),
    min_n = tune(),
    loss_reduction = tune(),
    sample_size = tune(),
    mtry = tune(),
    learn_rate = tune()
  )

set.seed(12345)

glimpse(heart_train)

rec <- recipe(target ~ .,
              data = heart_train) %>%
  step_unknown(all_nominal_predictors()) %>%
  step_novel(all_nominal_predictors()) %>%
  step_dummy(all_nominal_predictors()) %>%
  step_nzv(all_predictors())

set.seed(12345)

flow <- workflow() %>% 
  add_model(boost_model) %>% 
  add_recipe(rec)

set.seed(12345)

boost_grid <- grid_latin_hypercube(
  tree_depth(),
  min_n(),
  loss_reduction(),
  sample_size = sample_prop(),
  finalize(mtry(),
           heart_train),
  learn_rate(),
  size = 30
)

set.seed(12345)

xgb_fit <- tune_grid(
  flow,
  resamples = heart_fold,
  grid = boost_grid,
  control = control_grid(verbose = TRUE,
                              save_pred = TRUE)
)

collect_metrics(xgb_fit)
show_best(xgb_fit, metric = "accuracy", n = 10)
# show_best(xgb_fit, metric = "roc_auc", n = 10)

# best accuracy is .972

set.seed(12345)

# put this in visual
best_fit <- xgb_fit %>%
  select_best(metric = 'accuracy')
best_fit

final_flow <- finalize_workflow(flow, best_fit)
final_flow

final_fit <- last_fit(final_flow,
                      split = heart_split)

final_fit %>%
  collect_metrics()

final_fit %>%
  collect_predictions()

library(vip)

final_model <- fit(final_flow, heart)
  
final_model %>% 
  extract_fit_parsnip() %>% 
  vi() %>% 
  janitor::clean_names() %>% 
  mutate(importance = abs(importance)) %>% 
  ggplot(aes(fct_reorder(variable, importance), importance)) + 
  geom_col(color = 'white',
           fill = 'darkgreen') + 
  coord_flip() +
  scale_y_continuous(expand = c(0, 0))

# having a 

# try out survival analysis too
# library(survival)

