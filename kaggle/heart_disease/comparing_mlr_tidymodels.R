library(tidyverse)
library(tidymodels)
library(mlr3verse)

heart <- read_csv("C:/Users/cpppe/Downloads/Heart_Disease_Prediction.csv") |>
  janitor::clean_names()

theme_set(theme_light())


heart <- heart |>
  mutate(
    across(
      c(
        sex,
        chest_pain_type,
        fbs_over_120,
        ekg_results,
        slope_of_st,
        number_of_vessels_fluro,
        thallium,
        exercise_angina,
        heart_disease
      ),
      ~as.factor(.x)
    )
  )

glimpse(heart)

count(heart, heart_disease)

set.seed(12345)
tidy_splits <- initial_split(
  heart,
  prop = .8
  )

set.seed(12345)
tidy_train <- training(heart_split)
tidy_test <- testing(heart_split)

set.seed(12345)
tidy_folds <- vfold_cv(heart_train, v = 10)

tidy_model <- rand_forest(trees = 1000) |>
  set_mode("classification") |>
  set_engine("ranger") |>
  set_args(
    importance = "permutation"
  )

set.seed(12345)
mlr_task <- as_task_classif(
  heart,
  target = "heart_disease",
  positive = "Presence"
  )

set.seed(12345)
mlr_splits <- partition(
  mlr_task,
  ratio = .8
)

set.seed(12345)
mlr_learn <- lrn(
  "classif.ranger",
  predict_type = "prob",
  importance = "permutation"
)

mlr_folds <- rsmp("cv", folds = 10)

mlr_graph <- 

mlr_splits$train
mlr_splits$test
