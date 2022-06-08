# prophet_diff

library(tidyverse)
library(tidymodels)
library(prophet)
library(lubridate)
library(modeltime)
library(timetk)
library(padr)

theme_set(theme_light())

googlesheets4::gs4_deauth()

jet <- 
  googlesheets4::read_sheet('https://docs.google.com/spreadsheets/d/1SpRXsC3kXDaQLUfC6cPIOvsqxDF6updhgHRJeT8PTog/edit#gid=0') %>% 
  janitor::clean_names() %>% 
  mutate(ds = as_date(date))

jetblue <- jet %>% 
  mutate(actual_day = wday(ds,
                           label = TRUE),
         lambda = box_cox_vec(close,
                              lambda = 'auto'),
         clean = ts_clean_vec(close),
         clean_sqrt = sqrt(clean),
         lambda_clean = ts_clean_vec(lambda),
         clean_z = (clean - mean(clean)/sd(clean)),
         lambda_z = (lambda_clean - mean(lambda_clean)/sd(lambda_clean)),
         clean_diff1 = diff_vec(clean,
                                lag = 1),
         clean_diff1z = diff_vec(clean_z,
                                 lag = 1),
         roll_avg = slidify_vec(clean,
                                   ~mean(.),
                                   .period = 30,
                                   .partial = TRUE),
         roll_avg_z = slidify_vec(clean_z,
                                   ~mean(.),
                                   .period = 30,
                                   .partial = TRUE),
         loess = smooth_vec(clean,
                                   period = 30,
                                   # span = .70,
                                   degree = 1),
         loess_z = smooth_vec(clean_z,
                              period = 30,
                              degree = 1)) %>% 
  separate(col = date,
           into = c('year', 'month', 'day_num'),
           sep = '-') %>% 
  drop_na(clean_diff1, clean_diff1z)

glimpse(jetblue)

only_numeric <- jetblue %>% 
  select(close, lambda:loess_z)

map2(only_numeric,
     names(only_numeric),
     ~ggplot(data = only_numeric,
             aes(.x)) + 
       geom_histogram(color = 'white',
                      fill = 'dodgerblue') +
       geom_vline(xintercept = mean(.x) +
                    sd(.x) +
                    sd(.x) +
                    sd(.x),
                  color = 'red',
                  size = 1.25,
                  linetype = 2) + 
       geom_vline(xintercept = mean(.x) -
                    sd(.x) -
                    sd(.x) -
                    sd(.x),
                  color = 'red',
                  size = 1.25,
                  linetype = 2) + 
       labs(title = .y))

set.seed(05262022)

parallel::detectCores()
parallel_start(10,
               .method = 'parallel')

map2(only_numeric,
     names(only_numeric),
     ~only_numeric %>% 
       plot_time_series(jetblue$ds,
                        .x,
                        .interactive = FALSE) + 
       labs(title = .y))

# look into how to deal with these anamolies
jetblue %>% 
  plot_anomaly_diagnostics(ds,
                           roll_avg_z,
                           .facet_ncol = 1,
                           .interactive = FALSE)



# if removing anomalies
anomaly <- jetblue %>%
  tk_anomaly_diagnostics(ds,
                         clean_z)

jetblue <- left_join(jetblue, anomaly) %>%
  filter(anomaly != 'Yes')

jetblue %>%
  plot_time_series(ds,
                   clean_z,
                   .interactive = FALSE)

jetblue %>% 
  ggplot(aes(clean_z)) + 
  geom_histogram(color = 'white',
                 fill = 'dodgerblue') +
  geom_vline(xintercept = mean(no_anomaly$clean_z) +
               sd(no_anomaly$clean_z) +
               sd(no_anomaly$clean_z) +
               sd(no_anomaly$clean_z),
             color = 'red',
             size = 1.25,
             linetype = 2) + 
  geom_vline(xintercept = mean(no_anomaly$clean_z) -
               sd(no_anomaly$clean_z) -
               sd(no_anomaly$clean_z) -
               sd(no_anomaly$clean_z),
             color = 'red',
             size = 1.25,
             linetype = 2)

jetblue %>% 
  ggplot(aes(clean_diff1z)) + 
  geom_histogram(color = 'white',
                 fill = 'dodgerblue') +
  geom_vline(xintercept = mean(no_anomaly$clean_diff1z) +
               sd(no_anomaly$clean_diff1z) +
               sd(no_anomaly$clean_diff1z) +
               sd(no_anomaly$clean_diff1z),
             color = 'red',
             size = 1.25,
             linetype = 2) + 
  geom_vline(xintercept = mean(no_anomaly$clean_diff1z) -
               sd(no_anomaly$clean_diff1z) -
               sd(no_anomaly$clean_diff1z) -
               sd(no_anomaly$clean_diff1z),
             color = 'red',
             size = 1.25,
             linetype = 2) +
  annotate(geom = 'text',
           x = 2,
           y = 150,
           label = '1.26 & -1.28')

no_anomaly <- jetblue %>% 
  filter(clean_diff1z < 1.26,
         clean_diff1z > -1.28)

no_anomaly %>% 
  ggplot(aes(clean_diff1z)) + 
  geom_histogram(color = 'white',
                 fill = 'dodgerblue') +
  geom_vline(xintercept = mean(no_anomaly$clean_diff1z) +
               sd(no_anomaly$clean_diff1z) +
               sd(no_anomaly$clean_diff1z) +
               sd(no_anomaly$clean_diff1z),
             color = 'red',
             size = 1.25,
             linetype = 2) + 
  geom_vline(xintercept = mean(no_anomaly$clean_diff1z) -
               sd(no_anomaly$clean_diff1z) -
               sd(no_anomaly$clean_diff1z) -
               sd(no_anomaly$clean_diff1z),
             color = 'red',
             size = 1.25,
             linetype = 2)

# more diagnostics
jetblue %>% 
  plot_acf_diagnostics(ds,
                       clean_z,
                       .interactive = TRUE)

# ACF lag of ~143, PACF lag of ~2

jetblue %>% 
  plot_seasonal_diagnostics(ds,
                            clean_z,
                            .interactive = FALSE)
# seems like 2020 and 2022 had some seasonality, might want to keep that value = TRUE

jetblue %>% 
  plot_stl_diagnostics(ds,
                       clean_z,
                       .frequency = 'auto',
                       .trend = 'auto',
                       .feature_set = c('observed',
                                        'season',
                                        'trend',
                                        'remainder'),
                       .interactive = FALSE)



set.seed(05262022)
jet_split <- initial_time_split(no_anomaly) #whatever outcome should be stratified strata = clean_sqrt

set.seed(05262022)

# make sure to change y variable from close, clean, lambda_clean, rolling_avg
prophet_mod <- function(splits,
                        changepoints = .05,
                        seasonality = .01,
                        holiday = .01,
                        season_type = 'additive',
                        day_season = 'auto',
                        week_season = 'auto',
                        year_season = 'auto',
                        train = TRUE){
  
  analy_data <- analysis(splits)
  assess_data <- assessment(splits)
  
  model <- prophet_reg() %>% 
    set_engine(engine = 'prophet',
               verbose = TRUE) %>% 
    set_args(prior_scale_changepoints = changepoints,
             prior_scale_seasonality = seasonality,
             prior_scale_holidays = holiday,
             season = season_type,
             seasonality_daily = day_season,
             seasonality_weekly = week_season,
             seasonality_yearly = year_season) %>% 
    fit(clean_z ~ ds, 
        data = analy_data)
  
  if(train == TRUE){
    train_cali <- model %>% 
      modeltime_calibrate(new_data = analy_data)
    
    train_acc <- train_cali %>% 
      modeltime_accuracy()
    
    return(list(train_cali, train_acc))
  }
  
  else{
    test_cali <- model %>% 
      modeltime_calibrate(new_data = assess_data)
    
    test_acc <- test_cali %>% 
      modeltime_accuracy()
    
    return(list(test_cali, test_acc))
  }
  
  
}

prophet_mod(jet_split,
            train = TRUE) %>% 
  pluck(2)

prophet_mod(jet_split,
            train = TRUE) %>% #stop here to see how the model fit to the training data
  pluck(1) %>% #plucking the first part of the list to plot
  modeltime_forecast(new_data = training(jet_split), #change to testing(<split>) when train = FALSE
                     actual_data = jetblue) %>% 
  plot_modeltime_forecast(.interactive = FALSE)



prophet_mod(jet_split,
            train = TRUE) %>% 
  pluck(1) %>% 
  modeltime_residuals() %>% 
  plot_modeltime_residuals(.interactive = FALSE)

resid_plot_mod <- prophet_mod(jet_split,
                              train = TRUE) %>% 
  pluck(1) %>% 
  modeltime_residuals()

modeltime_residuals_test(resid_plot_mod)

forecast::checkresiduals(resid_plot_mod$.residuals)

map(seq(1, 30, by = 1),
    ~modeltime_residuals_test(resid_plot_mod,
                              lag = .x,
                              fitdf = 0))

map(seq(1, 30, by = 1),
    ~Box.test(resid_plot_mod$.residuals,
              lag = .x,
              fitdf = 0,
              type = 'Ljung-Box'))


# additive is the better fitting model
# tune prior scales

# for tuning

# proph_model <- prophet_reg() %>% 
#   set_engine(engine = 'prophet',
#              verbose = TRUE) %>% 
#   set_args(prior_scale_changepoints = tune(),
#            prior_scale_seasonality = tune(),
#            prior_scale_holidays = tune(),
#            season = 'additive',
#            seasonality_daily = FALSE,
#            seasonality_weekly = FALSE,
#            seasonality_yearly = TRUE)

# proph_rec <-
#   recipe(close ~ ds,
#          data = training(jet_split))


# use rolling origin
# using 180 days for initial time period, or ~82%
# 40 days for the assessment, or ~18%

set.seed(05262022)
train_fold <-
  rolling_origin(training(jet_split),
                 initial = 180,
                 assess = 40,
                 cumulative = TRUE)

set.seed(05262022)

#don't need this because I'm only going to tune the changepoints

# grid_values <-
#   grid_latin_hypercube(prior_scale_changepoints(),
#                        prior_scale_seasonality(),
#                        prior_scale_holidays(),
#                        size = 5)

set.seed(05262022)
# proph_fit <- tune_grid(object = proph_model,
#           preprocessor = proph_rec,
#           resamples = train_fold,
#           grid = grid_values,
#           control = control_grid(verbose = TRUE,
#                                  save_pred = TRUE,
#                                  allow_par = TRUE))

# collect_metrics(proph_fit)
# show_best(proph_fit,
#           metric = "rmse",
#           n = 1)

# let's see how these parameters look forecasted

prophet_mod(jet_split,
            changepoints = .529,
            seasonality = .00243,
            holiday = .230) %>%  
  pluck(1) %>% 
  modeltime_forecast(new_data = training(jet_split),
                     actual_data = jetblue) %>% 
  plot_modeltime_forecast(.interactive = FALSE)

prophet_mod(jet_split,
            changepoints = .529,
            seasonality = .00243,
            holiday = .230) %>%  
  pluck(1) %>% 
  modeltime_residuals() %>% 
  plot_modeltime_residuals(.interactive = FALSE)

tuned_resid <- prophet_mod(jet_split,
                           changepoints = .529,
                           seasonality = .00243,
                           holiday = .230) %>%  
  pluck(1) %>% 
  modeltime_residuals()

forecast::checkresiduals(tuned_resid$.residuals)

# going back to the drawing board
set.seed(05262022)

changepoints <-
  rep(seq(.001, .05, by = .001), length.out = 603)

proph_mod_change <-
  train_fold %>%
  mutate(models = map2(splits, 
                       changepoints,
                       ~prophet_mod(.x,
                                    changepoints = .y,
                                    seasonality = .00243,
                                    holiday = .23)),
         calibrate = map(models,
                         ~pluck(.x[[1]])),
         forecasts = map(calibrate,
                         ~modeltime_forecast(.x,
                                             new_data = training(jet_split),
                                             actual_data = jetblue) %>% 
                           plot_modeltime_forecast(.interactive = FALSE)),
         metric_value = map(models,
                            ~pluck(.x[[2]])),
         rmse_value = map(models,
                          ~pluck(.x[[2]]$rmse)))

proph_mod_change %>% 
  unnest(metric_value) %>% 
  select(id, mase, rmse) %>% 
  arrange(mase)

proph_mod_change %>% 
  unnest(rmse_value) %>% 
  summarize(rmse_mean = mean(rmse_value))

proph_mod_change %>% 
  select(id, rmse_value) %>% 
  unnest(rmse_value) %>% 
  arrange(rmse_value)



changepoints[013]
# .013


# let's check some other models
# first an auto arima
set.seed(05262022)

arima_fit <- arima_reg() %>% 
  set_engine(engine = 'auto_arima') %>% 
  fit(clean_z ~ ds,
      data = training(jet_split))

arima_fit

arima_resid <- arima_fit %>% 
  modeltime_calibrate(new_data = training(jet_split)) %>% 
  modeltime_residuals()

forecast::checkresiduals(arima_resid$.residuals)

arima_fit %>% 
  modeltime_calibrate(new_data = training(jet_split)) %>% 
  modeltime_forecast(new_data = training(jet_split),
                     actual_data = jetblue) %>% 
  plot_modeltime_forecast(.interactive = FALSE)

arima_fit %>% 
  modeltime_calibrate(new_data = training(jet_split)) %>% 
  modeltime_accuracy()
# mase = 1, rmse = .43, rsq = .99

# let's also try an ETS model
set.seed(05262022)

ets_fit <- exp_smoothing() %>%
  set_engine(engine = "ets") %>%
  fit(clean_z ~ ds,
      data = training(jet_split))

ets_fit

ets_resid <- ets_fit %>% 
  modeltime_calibrate(new_data = training(jet_split)) %>% 
  modeltime_residuals()

forecast::checkresiduals(ets_resid$.residuals)

ets_fit %>% 
  modeltime_calibrate(new_data = training(jet_split)) %>% 
  modeltime_forecast(new_data = training(jet_split),
                     actual_data = jetblue) %>% 
  plot_modeltime_forecast(.interactive = FALSE)

ets_fit %>% 
  modeltime_calibrate(new_data = training(jet_split)) %>% 
  modeltime_accuracy()
# masae = 1, rmse = .43, rsq = .985


# final training model
prophet_mod(jet_split,
            changepoints = .013,
            seasonality = .00243,
            holiday = .23,
            season_type = 'additive',
            day_season = 'auto',
            week_season = 'auto',
            year_season = 'auto',
            train = TRUE) %>%  
  pluck(2)
# mase = 6.62, rmse = 2.52, rsq = .497

prophet_mod(jet_split,
            changepoints = .013,
            seasonality = .00243,
            holiday = .23,
            season_type = 'additive',
            day_season = 'auto',
            week_season = 'auto',
            year_season = 'auto',
            train = TRUE) %>%  
  pluck(1) %>% 
  modeltime_forecast(new_data = training(jet_split),
                     actual_data = jetblue) %>% 
  plot_modeltime_forecast(.interactive = FALSE)

prophet_mod(jet_split,
            changepoints = .013,
            seasonality = .00243,
            holiday = .23,
            season_type = 'additive',
            day_season = 'auto',
            week_season = 'auto',
            year_season = 'auto',
            train = TRUE) %>% 
  pluck(1) %>% 
  modeltime_residuals() %>% 
  plot_modeltime_residuals(.interactive = FALSE)

resid_plot_mod <- prophet_mod(jet_split,
                              changepoints = .013,
                              seasonality = .00243,
                              holiday = .23,
                              season_type = 'additive',
                              day_season = 'auto',
                              week_season = 'auto',
                              year_season = 'auto',
                              train = TRUE) %>% 
  pluck(1) %>% 
  modeltime_residuals()

forecast::checkresiduals(resid_plot_mod$.residuals)

# testing set - prophet model
prophet_mod(jet_split,
            changepoints = .013,
            seasonality = .00243,
            holiday = .23,
            season_type = 'additive',
            day_season = 'auto',
            week_season = 'auto',
            year_season = 'auto',
            train = FALSE) %>%
  pluck(1) %>% 
  modeltime_forecast(new_data = testing(jet_split),
                     actual_data = jetblue) %>% 
  plot_modeltime_forecast(.interactive = FALSE)

prophet_mod(jet_split,
            changepoints = .013,
            seasonality = .00243,
            holiday = .23,
            season_type = 'additive',
            day_season = 'auto',
            week_season = 'auto',
            year_season = 'auto',
            train = FALSE) %>%
  pluck(2)
# mase = 10.3, rmse = 3.83, rsq = .8677


# testing set - arima model
arima_fit %>% 
  modeltime_calibrate(new_data = testing(jet_split)) %>% 
  modeltime_forecast(new_data = testing(jet_split),
                     actual_data = jetblue) %>% 
  plot_modeltime_forecast(.interactive = FALSE)

arima_fit %>% 
  modeltime_calibrate(new_data = testing(jet_split)) %>% 
  modeltime_accuracy()
# mase = 15.7, rmse = 5.44, rsq = NA


# testing set - ETS model
ets_fit %>% 
  modeltime_calibrate(new_data = testing(jet_split)) %>% 
  modeltime_forecast(new_data = testing(jet_split),
                     actual_data = jetblue) %>% 
  plot_modeltime_forecast(.interactive = FALSE)

ets_fit %>% 
  modeltime_calibrate(new_data = testing(jet_split)) %>% 
  modeltime_accuracy()
# mase = 15.7, rmse = 5.44, rsq = NA

# forecasting
prophet_mod(jet_split,
            changepoints = .013,
            seasonality = .00243,
            holiday = .23,
            season_type = 'additive',
            day_season = 'auto',
            week_season = 'auto',
            year_season = 'auto',
            train = FALSE) %>%
  pluck(1) %>% 
  modeltime_refit(data = jetblue) %>% 
  modeltime_forecast(h = '3 months',
                     actual_data = jetblue) %>% 
  plot_modeltime_forecast(.interactive = TRUE)

arima_fit %>% 
  modeltime_calibrate(new_data = testing(jet_split)) %>% 
  modeltime_refit(data = jetblue) %>% 
  modeltime_forecast(h = '3 months',
                     actual_data = jetblue) %>% 
  plot_modeltime_forecast(.interactive = TRUE)
# Arima model had the best fit when using testing data

ets_fit %>% 
  modeltime_calibrate(new_data = testing(jet_split)) %>% 
  modeltime_refit(data = jetblue) %>% 
  modeltime_forecast(h = '3 months',
                     actual_data = jetblue) %>% 
  plot_modeltime_forecast(.interactive = TRUE)
