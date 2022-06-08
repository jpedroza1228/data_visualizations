# jetblue practice

library(tidyverse)
library(tidymodels)
library(prophet)
library(lubridate)
library(modeltime)
library(timetk)
library(workflowsets)

theme_set(theme_light())

googlesheets4::gs4_deauth()

jetblue <- 
  googlesheets4::read_sheet('https://docs.google.com/spreadsheets/d/1SpRXsC3kXDaQLUfC6cPIOvsqxDF6updhgHRJeT8PTog/edit#gid=0') %>% 
  janitor::clean_names() %>% 
  mutate(ds = as_date(date)) %>% 
  select(-date)

set.seed(05262022)

jetblue %>% 
  plot_time_series(ds,
                   close,
                   .interactive = FALSE)

parallel::detectCores(logical = FALSE)
parallel_start(4,
               .method = 'parallel')

jet_split <- initial_time_split(jetblue)



set.seed(05262022)

base_model <- 
  prophet_reg() %>% 
  set_engine(engine = 'prophet',
             verbose = TRUE) %>% 
  set_args(seasonality_daily = FALSE,
           seasonality_weekly = FALSE,
           seasonality_yearly = TRUE,
           season = 'additive') %>% 
  fit(close ~ ds,
      data = training(jet_split))

base_model %>% 
  modeltime_calibrate(new_data = training(jet_split)) %>% 
  modeltime_forecast(new_data = training(jet_split),
                     actual_data = jetblue) %>% 
  plot_modeltime_forecast(.interactive = FALSE) 

base_model %>% 
  modeltime_calibrate(new_data = training(jet_split)) %>% 
  modeltime_accuracy() %>% 
  pluck('rmse')





# don't really need a transformation at all for the outcome
# this would be before the fold
rec <-
  recipe(close ~ ds,
         data = training(jet_split))
#   step_box_cox(close)

# rec

# rec_df <- prep(rec) %>%
#   juice()
# rec_df

train_fold <-
  rolling_origin(training(jet_split),
                 initial = 90,
                 assess = 30,
                 cumulative = TRUE)
  

train_slide_fold <-
  sliding_period(training(jet_split),
                 ds,
                 period = 'month',
                 lookback = 3,
                 assess_stop = 1)

train_fold$splits[[1]]
train_slide_fold$splits[[1]]

set.seed(05262022)


prophet_mod <- function(splits,
                        changepoints = .05,
                        seasonality = 10,
                        holiday = 10,
                        season_type = 'additive',
                        day_seaon = FALSE,
                        week_season = FALSE,
                        year_season = TRUE){
  
  analy_data <- analysis(splits)
  assess_data <- assessment(splits)
  
  model <- prophet_reg() %>% 
    set_engine(engine = 'prophet',
               verbose = TRUE) %>% 
    set_args(prior_scale_changepoints = changepoints,
             prior_scale_seasonality = seasonality,
             prior_scale_holidays = holiday,
             season = season_type,
             seasonality_daily = day_seaon,
             seasonality_weekly = week_season,
             seasonality_yearly = year_season) %>% 
    fit(close ~ ds,
        data = analy_data)

  model %>% 
    modeltime_calibrate(new_data = assess_data) %>% 
    modeltime_accuracy()
}

prophet_vis <- function(splits,
                        changepoints = .05,
                        seasonality = 10,
                        holiday = 10,
                        season_type = 'additive',
                        day_seaon = FALSE,
                        week_season = FALSE,
                        year_season = TRUE){
  
  analy_data <- analysis(splits)
  assess_data <- assessment(splits)
  
  model <- prophet_reg() %>% 
    set_engine(engine = 'prophet',
               verbose = TRUE) %>% 
    set_args(prior_scale_changepoints = changepoints,
             prior_scale_seasonality = seasonality,
             prior_scale_holidays = holiday,
             season = season_type,
             seasonality_daily = day_seaon,
             seasonality_weekly = week_season,
             seasonality_yearly = year_season) %>% 
    fit(close ~ ds,
        data = analy_data)
  
  model %>% 
    modeltime_calibrate(new_data = assess_data) %>% 
    modeltime_forecast(new_data = assess_data,
                       actual_data = jetblue) %>% 
    plot_modeltime_forecast(.interactive = FALSE)
}

set.seed(05262022)

par_mod <-
  train_fold %>% 
    sample_frac(.1) %>% 
  mutate(models = map(splits, ~prophet_mod(.x)),
         mod_vis = map(splits,
                       ~prophet_vis(.x)),
         rmse_value = map(models,
                          ~pluck(.x$rmse)))

par_mod %>% 
  unnest(rmse_value) %>% 
  summarize(rmse_mean = mean(rmse_value))


# much worse fit with the sliding period instead of the rolling origin
# use rolling origin moving forward

# slide_changepoints <- seq(.001, .5, length.out = 36)


# par_slide_mod <-
#   train_slide_fold %>%
#   # sample_frac(.2) %>%
#   mutate(models = map2(splits, 
#                        slide_changepoints,
#                        ~prophet_mod(.x,
#                                     changepoints = .y)),
#          rmse_value = map(models,
#                           ~pluck(.x$rmse)))

# map(par_slide_mod$rmse_value, ~pluck(.x))

# now change to multiplicative
set.seed(05262022)

par_mod_multi <- train_fold %>%
  sample_frac(.1) %>% 
  mutate(models = map(splits, 
                      ~prophet_mod(.x, season_type = 'multiplicative')),
         mod_vis = map(splits,
                       ~prophet_vis(.x)),
         rmse_value = map(models,
                          ~pluck(.x$rmse)))

par_mod_multi %>% 
  unnest(rmse_value) %>% 
  summarize(rmse_mean = mean(rmse_value))


# we'll stick with additive for the final model to use with testing data



grid_values <- grid_regular(prior_scale_changepoints(),
           prior_scale_seasonality(),
           prior_scale_holidays(),
           levels = c(prior_scale_changepoints = 20,
                      prior_scale_seasonality = 5,
                      prior_scale_holidays = 5))

# changepoints <-
#   rep(seq(.001, .5, by = .01), length.out = 711)
# season_values <- 
#   rep(seq(.01, 10, by = .1), length.out = 711)
# holi_values <- 
#   rep(seq(.01, 10, by = .1), length.out = 711)


set.seed(05262022)

par_mod2 <-
  train_fold %>% 
  mutate(models = pmap(list(splits,
                  changepoints,
                  season_values,
                  holi_values),
                  ~prophet_mod(..1,
                               changepoints = ..2,
                               seasonality = ..3,
                               holiday = ..4)),
         mod_vis = map(splits,
                       ~prophet_vis(.x)),
         rmse_value = map(models,
                          ~pluck(.x$rmse)))

map(par_mod2$models, ~pluck(.x))

par_mod2 %>% 
  unnest(rmse_value) %>% 
  summarize(rmse_mean = mean(rmse_value))

par_mod2 %>% 
  unnest(rmse_value) %>% 
  arrange(rmse_value)

changepoints[123]
season_values[123]
holi_values[123]

changepoints[226]
season_values[226]
holi_values[226]









# diagnostics

jet_train %>% 
  plot_acf_diagnostics(ds,
                       close,
                       .lags = '6 months',
                       .interactive = FALSE)

jet_train %>% 
  plot_seasonal_diagnostics(ds,
                            close,
                            .interactive = FALSE)

jet_train %>% 
  plot_stl_diagnostics(ds,
                       close,
                       .frequency = 'auto',
                       .trend = 'auto',
                       .feature_set = c('observed',
                                        'season',
                                        'trend',
                                        'remainder'),
                       .interactive = FALSE)

jet_train %>% 
  plot_time_series(ds,
                   close,
                   .interactive = TRUE)

jet_train %>% 
  plot_anomaly_diagnostics(ds,
                           close,
                           .facet_ncol = 1,
                           .interactive = FALSE)

# jet_train %>% 
#   tk_anomaly_diagnostics(ds,
#                          close) %>% 
#   View()

# look into how to deal with these anamolies
jet_train %>% 
  tk_anomaly_diagnostics(ds,
                         close) %>% 
  filter(anomaly == 'Yes')

# base model to be trained






# diagnostics

jetblue %>% 
  plot_acf_diagnostics(ds,
                       close,
                       .lags = '6 months',
                       .interactive = FALSE)

jetblue %>% 
  plot_seasonal_diagnostics(ds,
                            close,
                            .interactive = FALSE)

jetblue %>% 
  plot_stl_diagnostics(ds,
                       close,
                       .frequency = 'auto',
                       .trend = 'auto',
                       .feature_set = c('observed',
                                        'season',
                                        'trend',
                                        'remainder'),
                       .interactive = FALSE)

jetblue %>% 
  plot_time_series(ds,
                   close,
                   .interactive = TRUE)

jetblue %>% 
  plot_anomaly_diagnostics(ds,
                           close,
                           .facet_ncol = 1,
                           .interactive = FALSE)

# jetblue %>% 
#   tk_anomaly_diagnostics(ds,
#                          close) %>% 
#   View()

# look into how to deal with these anamolies
jetblue %>% 
  tk_anomaly_diagnostics(ds,
                         close) %>% 
  filter(anomaly == 'Yes')



# tune some parameters using rolling origin resamples
set.seed(05262022)

jet_folds <-
  sliding_period(jetblue,
               ds,
               period = 'month',
               lookback = Inf,
               assess_stop = 1)


sliding_period(jetblue,
                 ds,
                 period = 'month',
                 lookback = Inf,
                 assess_stop = 1) %>% 
    mutate(train_data = map(splits, analysis),
           test_data = map(splits, assessment)) %>% 
    select(-splits) %>% 
    pivot_longer(-id) %>% 
    # View()
    filter(id %in% c("Slice01", "Slice02", "Slice03")) %>%
    unnest(value) %>% 
    ggplot(aes(x = ds,
               y = close,
               color = name,
               group = NA)) + 
    geom_line() + 
    facet_wrap(vars(id),
               scales = "fixed")

# resamples using rolling origin

tune_mod_fun <- function(splits, season_type){
  set.seed(05262022)
  
  train_data <- analysis(splits)
  test_data <- assessment(splits)
  
  jet_model <- 
    prophet_reg() %>% 
    set_engine(engine = 'prophet',
               num.threads = parallel::detectCores(),
               verbose = TRUE) %>% 
    set_args(prior_scale_changepoints = tune(),
             prior_scale_seasonality = tune(),
             prior_scale_holidays = tune(),
             season = season_type,
             seasonality_daily = FALSE,
             seasonality_weekly = FALSE,
             seasonality_yearly = TRUE) %>% 
    fit(close ~ ds,
        data = train_data)
  
  jet_grid <- grid_latin_hypercube(prior_scale_changepoints(),
                                   prior_scale_seasonality(),
                                   prior_scale_holidays(),
                                   size = 20)
 
}



jet_flow <- workflow() %>% 
  add_model(jet_model) %>% 
  add_recipe(rec)
  



jet_model %>% 
  modeltime_calibrate(new_data = jet_train) %>% 
  modeltime_forecast(new_data = jet_train,
                     actual_data = jetblue) %>% 
  plot_modeltime_forecast(.interactive = FALSE) 

jet_model %>% 
  modeltime_calibrate(new_data = jet_train) %>% 
  modeltime_accuracy() %>% 
  pluck('rmse')





# including testing data
jet_model %>% 
  modeltime_calibrate(new_data = jet_test) %>% 
  modeltime_forecast(new_data = jet_test,
                     actual_data = jetblue) %>% 
  plot_modeltime_forecast(.interactive = FALSE) 

model_baba %>% 
  modeltime_calibrate(new_data = jet_test) %>% 
  modeltime_accuracy() %>% 
  pluck('rmse')
