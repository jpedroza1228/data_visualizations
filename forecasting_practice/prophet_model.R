library(tidyverse)
library(tidymodels)
# library(forecast)
library(prophet)
library(lubridate)
library(modeltime)
library(timetk)

theme_set(theme_light())

googlesheets4::gs4_deauth()

jetblue <- googlesheets4::read_sheet('https://docs.google.com/spreadsheets/d/1SpRXsC3kXDaQLUfC6cPIOvsqxDF6updhgHRJeT8PTog/edit#gid=0') %>% 
  janitor::clean_names() %>% 
  mutate(ds = as_date(date)) %>% 
  select(-date)

honest <- googlesheets4::read_sheet('https://docs.google.com/spreadsheets/d/1SBLfoU3AhAQF6IJTn4TNTXKmopVk44S0JeifK3-DMoU/edit#gid=0') %>% 
  janitor::clean_names() %>% 
  mutate(ds = as_date(date)) %>% 
  select(-date)

alibaba <- googlesheets4::read_sheet('https://docs.google.com/spreadsheets/d/1CHh6zEzdWST2U-AsjBBhFNaOyTKJceuFdwsiIV5YCFs/edit#gid=0') %>% 
  janitor::clean_names() %>% 
  mutate(ds = as_date(date)) %>% 
  select(-date)


jetblue %>% 
  glimpse()

us_holi <-
  generated_holidays %>% 
  filter(country == 'US') %>% 
  mutate(date = as_date(ds))

set.seed(05262022)

baba_splits <-
  initial_time_split(alibaba)

# training dataset for alibaba

set.seed(05262022)

model_baba <- prophet_reg() %>% 
  set_engine(engine = 'prophet',
             num.threads = parallel::detectCores(),
             verbose = TRUE) %>% 
  set_args(prior_scale_changepoints = 0.05,
           prior_scale_seasonality = 10,
           prior_scale_holidays = 10,
           season = 'additive') %>% 
  fit(close ~ ds,
      data = training(baba_splits))

model_baba %>% 
  modeltime_calibrate(new_data = training(baba_splits)) %>% 
  modeltime_forecast(new_data = training(baba_splits),
                     actual_data = alibaba) %>% 
  plot_modeltime_forecast(.interactive = FALSE) 

model_baba %>% 
  modeltime_calibrate(new_data = training(baba_splits)) %>% 
  modeltime_accuracy() %>% 
  pluck('rmse')

# including testing data
model_baba %>% 
  modeltime_calibrate(new_data = testing(baba_splits)) %>% 
  modeltime_forecast(new_data = testing(baba_splits),
                     actual_data = alibaba) %>% 
  plot_modeltime_forecast(.interactive = FALSE) 

model_baba %>% 
  modeltime_calibrate(new_data = testing(baba_splits)) %>% 
  modeltime_accuracy() %>% 
  pluck('rmse')

# whole dataset
future_baba <- alibaba %>% 
  future_frame(.length_out = '12 months', .bind_data = TRUE)

model_baba %>% 
  modeltime_calibrate(new_data = alibaba) %>% 
  modeltime_forecast(new_data = future_baba,
                     actual_data = alibaba) %>% 
  plot_modeltime_forecast(.interactive = FALSE)




# using modeltime and all data - ALIBABA

set.seed(05262022)

model_baba <- prophet_reg() %>% 
  set_engine(engine = 'prophet',
             num.threads = parallel::detectCores(),
             verbose = TRUE,
             holidays = china_holi) %>% 
  set_args(prior_scale_changepoints = 0.05,
           prior_scale_seasonality = 10,
           prior_scale_holidays = 10,
           season = 'additive') %>% 
  fit(close ~ ds,
      data = alibaba)





set.seed(05262022)

model_baba %>% 
  modeltime_calibrate(new_data = alibaba) %>% 
  modeltime_accuracy() %>% 
  pluck('rmse')


model_baba_cal <- model_baba %>%
  modeltime_calibrate(new_data = alibaba)
model_baba_cal

# including testing data

model_forecast_baba <- model_baba_cal %>% 
  modeltime_forecast(new_data = future_baba,
                     actual_data = alibaba)

model_forecast_baba

plot_modeltime_forecast(model_forecast_baba,
                        .interactive = FALSE) 



future_baba <- alibaba %>% 
  future_frame(.length_out = '3 months', .bind_data = TRUE)

model_baba_cal %>% 
  modeltime_forecast(new_data = future_baba,
                     actual_data = alibaba) %>% 
  plot_modeltime_forecast(.interactive = FALSE)


model_forecast_baba %>% 
  filter(.index > '2022-05-26') %>% 
  plot_modeltime_forecast(.interactive = FALSE)

model_baba_cal %>% 
  modeltime_accuracy()

# using modeltime and all data

set.seed(05262022)

future_jet <- jetblue %>% 
  future_frame(.length_out = '12 months', .bind_data = TRUE)

model_mt <- prophet_reg() %>% 
  set_engine(engine = 'prophet',
             num.threads = parallel::detectCores(),
             verbose = TRUE,
             holidays = us_holi) %>% 
  fit(close ~ ds,
    data = jetblue)

set.seed(05262022)

model_mt %>% 
  modeltime_calibrate(new_data = jetblue) %>% 
  modeltime_accuracy() %>% 
  pluck('rmse')


model_mt_cal <- model_mt %>%
  modeltime_calibrate(new_data = jetblue)
model_mt_cal

model_forecast <- model_mt_cal %>% 
  modeltime_forecast(new_data = future_jet,
                     actual_data = jetblue)

model_forecast

plot_modeltime_forecast(model_forecast,
                        .interactive = FALSE) 

model_mt_cal %>% 
  modeltime_forecast(new_data = future_jet,
                     actual_data = jetblue) %>% 
  plot_modeltime_forecast(.interactive = FALSE)


model_forecast %>% 
  filter(.index > '2022-05-26') %>% 
  plot_modeltime_forecast(.interactive = FALSE)

model_mt_cal %>% 
  modeltime_accuracy()

xgb_model <- boost_tree() %>%
  set_engine('xgboost',
             num.threads = parallel::detectCores(),
             verbose = TRUE) %>%
  set_mode('regression') %>%
  set_args(
    learn_rate = .03,
    trees = 1000,
    tree_depth = 6,
    min_n = 1,
    loss_reduction = 0,
    sample_size = 1
  ) %>%
  fit(close ~ ds, data = jetblue)

str(xgb_model$fit)

xgb_mod_df <- data.frame(x = xgb_model$fit$evaluation_log$iter,
                         y = xgb_model$fit$evaluation_log$training_rmse)

xgb_plot <- ggplot(data = xgb_mod_df,
       aes(x, y)) + 
  geom_point(alpha = .3) +
  geom_line(color = 'dodgerblue')

ggplotly(xgb_plot)

mean(rmse_xgb)

# on forecasted data
xbg_cast <- model_forecast %>% 
  filter(.index > '2022-05-26') %>% 
  rename(ds = .index,
         close = .value)
  
xgb_model_cast <- boost_tree() %>%
  set_engine('xgboost',
             num.threads = parallel::detectCores(),
             verbose = TRUE) %>%
  set_mode('regression') %>%
  set_args(
    learn_rate = .03,
    trees = 1000,
    tree_depth = 6,
    min_n = 1,
    loss_reduction = 0,
    sample_size = 1
  ) %>%
  fit(close ~ ds, data = xbg_cast)

str(xgb_model_cast$fit)

xgb_mod_df_cast <- data.frame(iteration = xgb_model_cast$fit$evaluation_log$iter,
                         rmse_value = xgb_model_cast$fit$evaluation_log$training_rmse)

xgb_plot_cast <- ggplot(data = xgb_mod_df_cast,
                   aes(iteration, rmse_value)) + 
  geom_point(alpha = .3) +
  geom_line(color = 'dodgerblue')

ggplotly(xgb_plot_cast)

mean(xgb_mod_df_cast$rmse_value)

#modeltime, adding predictor

set.seed(05262022)

future_cases <- jet_data %>% 
  future_frame(.length_out = '90 days', .bind_data = TRUE)

model_cases <- prophet_reg() %>% 
  set_engine(engine = 'prophet',
             num.threads = parallel::detectCores(),
             verbose = TRUE,
             holidays = holidays) %>% 
  fit(new_cases ~ ds,
      data = jet_data)

set.seed(05262022)

model_cases_cal <- model_cases %>%
  modeltime_calibrate(new_data = jet_data)

model_cases_cal$.calibration_data
tail(future_cases, 20)

model_cases_forecast <- model_cases_cal %>% 
  modeltime_forecast(new_data = future_cases,
                     actual_data = jet_data)

plot_modeltime_forecast(model_cases_forecast,
                        .interactive = TRUE) 

model_cases_forecast %>% 
  filter(.index > '2022-05-26') %>% 
  plot_modeltime_forecast(.interactive = FALSE)

model_cases_cal %>% 
  modeltime_accuracy()

# Now include future predictions for cases to include them in model

set.seed(05262022)

case_pred <- model_cases_forecast %>% 
  filter(.index > '2022-05-26') %>% 
  rename(ds = .index,
         new_cases = .value,
         case_conf_lo = .conf_lo,
         case_conf_hi = .conf_hi) %>% 
  select(ds:case_conf_hi)

y_pred <- model_forecast %>% 
  filter(.index > '2022-05-26') %>% 
  rename(ds = .index,
         y = .value,
         y_conf_lo = .conf_lo,
         y_conf_hi = .conf_hi) %>% 
  select(ds:y_conf_hi)

join_future <- full_join(case_pred, y_pred,
          by = 'ds')

model_plus <- prophet_reg() %>% 
  set_engine(engine = 'prophet',
             num.threads = parallel::detectCores(),
             verbose = TRUE,
             holidays = holidays) %>% 
  fit(y ~ ds + new_cases,
      data = jet_data)

set.seed(05262022)

model_plus_cal <- model_plus %>%
  modeltime_calibrate(new_data = jet_data)

model_plus_cal$.calibration_data
tail(join_future, 20)

model_plus_forecast <- model_plus_cal %>% 
  modeltime_forecast(new_data = join_future,
                     actual_data = jet_data)

plot_modeltime_forecast(model_plus_forecast,
                        .interactive = FALSE) 

model_plus_forecast %>% 
  filter(.index > '2022-05-26') %>% 
  plot_modeltime_forecast(.interactive = FALSE)

model_plus_cal %>% 
  modeltime_accuracy()


# using prophet

set.seed(05262022)

time_mod <- prophet(jetblue,
                    holidays = holidays)

future <- make_future_dataframe(time_mod,
                                periods = 365)

prof_forecast <- predict(time_mod,
                         future) 

plot(time_mod,
     prof_forecast)

# prophet_plot_components(time_mod,
                        # prof_forecast)

glimpse(prof_forecast)

tail(prof_forecast[c('ds', 'trend', 'yhat', 'trend_lower', 'yhat_lower', 'trend_upper', 'yhat_upper')], 30)

prof_forecast %>% 
  select(ds, yhat) %>% 
  filter(ds > '2022-05-26') %>% 
  ggplot(aes(ds, yhat)) + 
  geom_point(alpha = .5) +
  geom_line()


# http://rstudio-pubs-static.s3.amazonaws.com/454439_6d057c9b587946a2abcbbda10b181627.html

set.seed(05262022)

time_mod <- prophet(holidays = holidays)
time_mod <- add_regressor(time_mod,
                          'new_cases')
time_mod <- fit.prophet(time_mod, jet_data)


future <- make_future_dataframe(time_mod,
                                periods = 365)

# need to know how to include multiple predictors
# future$new_cases <- jet_data$new_cases

prof_forecast <- predict(time_mod,
                         future) 

plot(time_mod,
     prof_forecast)

prophet_plot_components(time_mod,
                        prof_forecast)

glimpse(prof_forecast)

tail(prof_forecast[c('ds', 'trend', 'yhat', 'trend_lower', 'yhat_lower', 'trend_upper', 'yhat_upper')], 30)

prof_forecast %>% 
  select(ds, yhat) %>% 
  filter(ds > '2022-05-26') %>% 
  ggplot(aes(ds, yhat)) + 
  geom_point(alpha = .5) +
  geom_line()

