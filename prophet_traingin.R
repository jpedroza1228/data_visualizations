library(tidyverse)
library(tidymodels)
library(timetk)
library(prophet)
library(modeltime)
library(lubridate)
library(googlesheets4)

american <- read_sheet('https://docs.google.com/spreadsheets/d/1Ld9E_5Da9xaZ31VxLCQ41UG26FWjNXHBuU6YHqq1kaY/edit#gid=0') %>% 
  janitor::clean_names() %>% 
  mutate(date = as_date(date)) %>% 
  rename(ds = date,
         y = close)

# ----------------------------------------
american %>% 
  plot_time_series(ds, y, .interactive = FALSE)


#  splits
set.seed(5282022)
amer_splits <- initial_time_split(american)
amer_train <- training(amer_splits)
amer_test <- testing(amer_splits)
# prophet train model

train_prop <- prophet(amer_train)

future_prop <- make_future_dataframe(train_prop,
                                     periods = 6,
                                     freq = 'month',
                                     include_history = FALSE)

prop_cast <- predict(train_prop,
                     future_prop)

plot(train_prop,
     prop_cast) + 
  add_changepoints_to_plot(train_prop)

# train model with tuning parameters
# change points c(seq(.001, .5, by = .05), .5)

amer_train_mod <- prophet_reg() %>% 
  set_engine('prophet') %>% 
  set_args(season = tune(),
           prior_scale_changepoints = tune(),
           prior_scale_holidays = 10,
           prior_scale_seasonality = 10) %>% 
  fit(y ~ ds,
      data = amer_train)

amer_train_rec <- 


# c('additive', 'multiplicative')
# c(seq(.001, .5, by = .05), .5)

train_maps %>% 
  mutate(train_cals = map(amer_splits,
                          ~train_add_models %>%
                            modeltime_calibrate(new_data = training(.x))))



  
train_cal <- train_model %>% 
  modeltime_calibrate(new_data = training(amer_splits) %>% 
                        arrange(ds))

train_cal %>% 
  modeltime_forecast(h = '6 months',
                     actual_data = american) %>% 
  plot_modeltime_forecast(.interactive = FALSE)

train_cal %>%
  modeltime_accuracy()

# train model with 

tune_prophet <- function(splits){
  train_data <- analysis(splits)
  test_data <- assessment(splits)
  
  m1 <- prophet(train_data,
                seasonality.mode = 'additive')
  m2 <- prophet(train_data,
                seasaonlity.mode = 'multiplicative')
  
  future <- make_future_dataframe(m1,
                                  periods = nrow(test_data),
                                  freq = 'month',
                                  include_history = FALSE)
  
  bind_rows(
    predict(m1, future) %>% 
      select(ds, yhat) %>% 
      mutate(type = 'additive'),
    predict(m2, future) %>% 
      select(ds, yhat) %>% 
      mutate(type = 'multiplicative')) %>% 
    left_join(test_data, by = 'ds')
}

tune_prophet()