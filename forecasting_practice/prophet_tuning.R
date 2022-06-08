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
         clean = ts_clean_vec(close),
         clean_z = (clean - mean(clean)/sd(clean)),
         clean_diff1 = diff_vec(clean,
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
                            degree = 1),
         loess_z = smooth_vec(clean_z,
                              period = 30,
                              degree = 1)) %>% 
  separate(col = date,
           into = c('year_num', 'month_num', 'day_num'),
           sep = '-') %>% 
  mutate(year_num = as.factor(year_num),
         year_num = relevel(year_num, ref = '2018')) %>% 
  separate(col = day_num,
           into = c('day_num', 'drop'),
           sep = ' ') %>%
  mutate(day_num = as.numeric(day_num),
         month_num = as.factor(month_num)) %>% 
  select(-drop) %>% 
  drop_na(clean_diff1)  %>% 
  arrange(ds)

glimpse(jetblue)

jetblue %>% 
  group_by(year_num, month_num) %>% 
  summarize(sd = sd(close)) %>% 
  ungroup() %>% 
  ggplot(aes(month_num, sd)) + 
  geom_point() + 
  facet_wrap(vars(year_num))

only_numeric <- jetblue %>% 
  select(close, clean:loess_z)

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


jetblue %>% 
  plot_anomaly_diagnostics(ds,
                           clean,
                           .facet_ncol = 1,
                           .interactive = FALSE)

# jetblue %>% 
#   plot_anomaly_diagnostics(ds,
#                            clean_diff1,
#                            .facet_ncol = 1,
#                            .interactive = FALSE)

jetblue %>% 
  tk_anomaly_diagnostics(ds,
                         clean) %>% 
  ggplot(aes(ds, observed)) + 
  geom_line() + 
  geom_point(aes(color = anomaly))

# jetblue %>% 
#   tk_anomaly_diagnostics(ds,
#                          clean_diff1) %>% 
#   ggplot(aes(ds, observed)) + 
#   geom_line(aes(color = anomaly)) +
#   geom_point(aes(color = anomaly))
  
# if removing anomalies
# anomaly_diff <-
#   jetblue %>%
#   tk_anomaly_diagnostics(ds,
#                          clean_diff1)

# jetblue_diff <- left_join(jetblue, anomaly_diff) %>%
#   filter(anomaly != 'Yes')

anomaly_roll <- jetblue %>%
  tk_anomaly_diagnostics(ds,
                         clean)

jetblue_roll <- left_join(jetblue, anomaly_roll) %>%
  filter(anomaly != 'Yes')

# jetblue_diff %>%
#   plot_time_series(ds,
#                    clean_diff1,
#                    .interactive = FALSE)


jetblue_roll %>%
  plot_time_series(ds,
                   clean,
                   .interactive = FALSE)

# jetblue_diff %>% 
#   ggplot(aes(clean_diff1)) + 
#   geom_histogram(color = 'white',
#                  fill = 'dodgerblue') +
#   geom_vline(xintercept = mean(jetblue_diff$clean_diff1) +
#                sd(jetblue_diff$clean_diff1) +
#                sd(jetblue_diff$clean_diff1) +
#                sd(jetblue_diff$clean_diff1),
#              color = 'red',
#              size = 1.25,
#              linetype = 2) + 
#   geom_vline(xintercept = mean(jetblue_diff$clean_diff1) -
#                sd(jetblue_diff$clean_diff1) -
#                sd(jetblue_diff$clean_diff1) -
#                sd(jetblue_diff$clean_diff1),
#              color = 'red',
#              size = 1.25,
#              linetype = 2) +
#   annotate(geom = 'text',
#            x = 2,
#            y = 150,
#            label = '1.14 & -1.15')

# jetblue_diff <- jetblue_diff %>% 
#   filter(clean_diff1z < 1.14,
#          clean_diff1z > -1.15)

# jetblue_diff %>% 
#   ggplot(aes(clean_diff1)) + 
#   geom_histogram(color = 'white',
#                  fill = 'dodgerblue') +
#   geom_vline(xintercept = mean(jetblue_diff$clean_diff1) +
#                sd(jetblue_diff$clean_diff1) +
#                sd(jetblue_diff$clean_diff1) +
#                sd(jetblue_diff$clean_diff1),
#              color = 'red',
#              size = 1.25,
#              linetype = 2) + 
#   geom_vline(xintercept = mean(jetblue_diff$clean_diff1) -
#                sd(jetblue_diff$clean_diff1) -
#                sd(jetblue_diff$clean_diff1) -
#                sd(jetblue_diff$clean_diff1),
#              color = 'red',
#              size = 1.25,
#              linetype = 2)

jetblue_roll %>% 
  ggplot(aes(clean)) + 
  geom_histogram(color = 'white',
                 fill = 'dodgerblue') +
  geom_vline(xintercept = mean(jetblue_roll$clean) +
               sd(jetblue_roll$clean) +
               sd(jetblue_roll$clean) +
               sd(jetblue_roll$clean),
             color = 'red',
             size = 1.25,
             linetype = 2) + 
  geom_vline(xintercept = mean(jetblue_roll$clean) -
               sd(jetblue_roll$clean) -
               sd(jetblue_roll$clean) -
               sd(jetblue_roll$clean),
             color = 'red',
             size = 1.25,
             linetype = 2)

# more diagnostics
# jetblue_diff %>% 
#        plot_acf_diagnostics(ds,
#                         clean_diff1,
#                         .interactive = TRUE)

jetblue_roll %>% 
  plot_acf_diagnostics(ds,
                       clean,
                       .interactive = TRUE)

# jetblue_diff %>% 
#   plot_seasonal_diagnostics(ds,
#                             clean_diff1,
#                             .interactive = FALSE)

jetblue_roll %>% 
  plot_seasonal_diagnostics(ds,
                            clean,
                            .interactive = FALSE)

# jetblue_diff %>% 
#   plot_stl_diagnostics(ds,
#                        clean_diff1,
#                        .frequency = 'auto',
#                        .trend = 'auto',
#                        .feature_set = c('observed',
#                                         'season',
#                                         'trend',
#                                         'remainder'),
#                        .interactive = FALSE)

jetblue_roll %>% 
  plot_stl_diagnostics(ds,
                       clean,
                       .frequency = 'auto',
                       .trend = 'auto',
                       .feature_set = c('observed',
                                        'season',
                                        'trend',
                                        'remainder'),
                       .interactive = FALSE)



# set.seed(05262022)
# jet_split_diff <- initial_time_split(jetblue_diff) #whatever outcome should be stratified strata = clean_sqrt

# set.seed(05262022)

# prophet_mod_diff <- function(splits,
#                         changepoints = .05,
#                         seasonality = .01,
#                         holiday = .01,
#                         season_type = 'additive',
#                         day_season = 'auto',
#                         week_season = 'auto',
#                         year_season = 'auto',
#                         train = TRUE){
#   
#   analy_data <- analysis(splits)
#   assess_data <- assessment(splits)
#   
#   model <- prophet_reg() %>% 
#     set_engine(engine = 'prophet',
#                verbose = TRUE) %>% 
#     set_args(prior_scale_changepoints = changepoints,
#              prior_scale_seasonality = seasonality,
#              prior_scale_holidays = holiday,
#              season = season_type,
#              seasonality_daily = day_season,
#              seasonality_weekly = week_season,
#              seasonality_yearly = year_season) %>% 
#     fit(clean_diff1 ~ ds, 
#         data = analy_data)
#   
#   if(train == TRUE){
#     train_cali <- model %>% 
#                    modeltime_calibrate(new_data = analy_data)
#     
#     train_acc <- train_cali %>% 
#                   modeltime_accuracy()
#     
#     return(list(train_cali, train_acc))
#   }
#   
#   else{
#    test_cali <- model %>% 
#       modeltime_calibrate(new_data = assess_data)
#    
#    test_acc <- test_cali %>% 
#      modeltime_accuracy()
#    
#    return(list(test_cali, test_acc))
#   }
#   
#   
# }

# prophet_mod_diff(jet_split_diff,
#             train = TRUE) %>% 
#   pluck(2)

# prophet_mod_diff(jet_split_diff,
#             train = TRUE) %>% #stop here to see how the model fit to the training data
#   pluck(1) %>% #plucking the first part of the list to plot
#   modeltime_forecast(new_data = training(jet_split_diff), #change to testing(<split>) when train = FALSE
#                      actual_data = jetblue_diff) %>% 
#   plot_modeltime_forecast(.interactive = FALSE)



# prophet_mod_diff(jet_split_diff,
#             train = TRUE) %>% 
#   pluck(1) %>% 
#   modeltime_residuals() %>% 
#   plot_modeltime_residuals(.interactive = FALSE)

# resid_plot_mod <- prophet_mod_diff(jet_split_diff,
#                               train = TRUE) %>% 
#   pluck(1) %>% 
#   modeltime_residuals()

# modeltime_residuals_test(resid_plot_mod)

# forecast::checkresiduals(resid_plot_mod$.residuals)

# map(seq(1, 30, by = 1),
#     ~modeltime_residuals_test(resid_plot_mod,
#               lag = .x,
#               fitdf = 0))

# map(seq(1, 30, by = 1),
#     ~Box.test(resid_plot_mod$.residuals,
#               lag = .x,
#               fitdf = 0,
#               type = 'Ljung-Box'))


# rolling average stuff
set.seed(05262022)
jet_split_roll <- initial_time_split(jetblue_roll) #whatever outcome should be stratified strata = clean_sqrt

set.seed(05262022)

prophet_mod_roll <- function(splits,
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
    fit(clean ~ ds + month_num, 
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

prophet_mod_roll(jet_split_roll,
                 train = TRUE) %>% 
  pluck(2)
# mase = 2.59
# rmse = .99
# rsq = .92

prophet_mod_roll(jet_split_roll,
                 train = TRUE) %>% #stop here to see how the model fit to the training data
  pluck(1) %>% #plucking the first part of the list to plot
  modeltime_forecast(new_data = training(jet_split_roll), #change to testing(<split>) when train = FALSE
                     actual_data = jetblue_roll) %>% 
  plot_modeltime_forecast(.interactive = FALSE)



prophet_mod_roll(jet_split_roll,
                 train = TRUE) %>% 
  pluck(1) %>% 
  modeltime_residuals() %>% 
  plot_modeltime_residuals(.interactive = FALSE)

resid_plot_mod <- prophet_mod_roll(jet_split_roll,
                                   train = TRUE) %>% 
  pluck(1) %>% 
  modeltime_residuals()

# modeltime_residuals_test(resid_plot_mod)

forecast::checkresiduals(resid_plot_mod$.residuals)
mean(resid_plot_mod$.residuals)
acf(resid_plot_mod$.residuals,
    plot = FALSE)

# map(seq(1, 30, by = 1),
#     ~modeltime_residuals_test(resid_plot_mod,
#                               lag = .x,
#                               fitdf = 0))

map(seq(1, 30, by = 1),
    ~Box.test(resid_plot_mod$.residuals,
              lag = .x,
              fitdf = 0,
              type = 'Ljung-Box'))

# for tuning

# proph_model <- prophet_reg() %>%
#   set_engine(engine = 'prophet',
#              verbose = TRUE) %>%
#   set_args(prior_scale_changepoints = tune(),
#            prior_scale_seasonality = tune(),
#            prior_scale_holidays = tune(),
#            season = 'additive',
#            seasonality_daily = 'auto',
#            seasonality_weekly = 'auto',
#            seasonality_yearly = 'auto')

# proph_rec_diff <-
#   recipe(clean_diff1 ~ ds,
#          data = training(jet_split_diff))

# proph_rec_roll <-
#   recipe(clean ~ ds ,
#          data = training(jet_split_roll))


# use rolling origin
# using 180 days for initial time period, or ~82%
# 40 days for the assessment, or ~18%

# set.seed(05262022)
# train_fold_diff <-
#   rolling_origin(training(jet_split_diff),
#                  initial = 180,
#                  assess = 40,
#                  cumulative = TRUE)

set.seed(05262022)
train_fold_roll <-
  rolling_origin(training(jet_split),
                 initial = 180,
                 assess = 40,
                 skip = 6,
                 cumulative = TRUE)
# 
# set.seed(05262022)

# grid_values <-
#   grid_latin_hypercube(prior_scale_changepoints(),
#                        prior_scale_seasonality(),
#                        prior_scale_holidays(),
#                        size = 5)

set.seed(05262022)
# proph_fit_diff <- tune_grid(object = proph_model,
#                        preprocessor = proph_rec_diff, 
#                        resamples = train_fold_diff,
#                        grid = grid_values,
#                        control = control_grid(verbose = TRUE,
#                                               save_pred = TRUE,
#                                               allow_par = TRUE))

# collect_metrics(proph_fit_diff)
# show_best(proph_fit_diff,
#           metric = "rmse",
#           n = 1)

set.seed(05262022)
# proph_fit_roll <- tune_grid(object = proph_model,
#                             preprocessor = proph_rec_roll, 
#                             resamples = train_fold_roll,
#                             grid = grid_values,
#                             control = control_grid(verbose = TRUE,
#                                                    save_pred = TRUE,
#                                                    allow_par = TRUE))

# collect_metrics(proph_fit_roll) %>% 
# show_best(proph_fit_roll,
          # metric = "rmse",
          # n = 1)
# 2.66, .005, .001

# let's see how these parameters look forecasted
# whatever points are provided from the model before, use those in the following models

# prophet_mod_diff(jet_split_diff,
#             changepoints = 2.12,
#             seasonality = .002,
#             holiday = .25) %>%  
#   pluck(1) %>% 
#   modeltime_forecast(new_data = training(jet_split_diff),
#                      actual_data = jetblue_diff) %>% 
#   plot_modeltime_forecast(.interactive = FALSE)

# prophet_mod_diff(jet_split_diff,
#             changepoints = 2.12,
#             seasonality = .002,
#             holiday = .25) %>%  
#   pluck(1) %>% 
#   modeltime_residuals() %>% 
#   plot_modeltime_residuals(.interactive = FALSE)
  
# tuned_resid_diff <- prophet_mod_diff(jet_split_diff,
#                                 changepoints = 2.12,
#                                 seasonality = .002,
#                                 holiday = .25) %>%  
#   pluck(1) %>% 
#   modeltime_residuals()

# forecast::checkresiduals(tuned_resid_diff$.residuals)


# not going to run this because first model was better
set.seed(05262022)
prophet_mod_roll(jet_split_roll,
                 changepoints = 2.66,
                 seasonality = .005,
                 holiday = .001) %>% 
  pluck(2)
# mase = 1.61
# rmse = .61
# rsq = .97

prophet_mod_roll(jet_split_roll,
                 changepoints = 2.66,
                 seasonality = .005,
                 holiday = .001) %>%  
  pluck(1) %>% 
  modeltime_forecast(new_data = training(jet_split_roll),
                     actual_data = jetblue_roll) %>% 
  plot_modeltime_forecast(.interactive = FALSE)

prophet_mod_roll(jet_split_roll,
                 changepoints = 2.66,
                 seasonality = .005,
                 holiday = .001) %>%  
  pluck(1) %>% 
  modeltime_residuals() %>% 
  plot_modeltime_residuals(.interactive = FALSE)

tuned_resid_roll <- prophet_mod_roll(jet_split_roll,
                                     changepoints = 2.66,
                                     seasonality = .005,
                                     holiday = .001) %>%  
  pluck(1) %>% 
  modeltime_residuals()

forecast::checkresiduals(tuned_resid_roll$.residuals)

# going back to the drawing board
set.seed(05262022)

# changepoints_diff <-
#   rep(seq(.001, 100, by = .2), length.out = 591)

changepoints_roll <-
  rep(seq(.001, 100, by = .2), length.out = 588)


# proph_mod_change_diff <-
#   train_fold_diff %>%
#   mutate(models = map2(splits, 
#                        changepoints_diff,
#                        ~prophet_mod_diff(.x,
#                                     changepoints = .y,
#                                     seasonality = .002,
#                                     holiday = .25)),
#          calibrate = map(models,
#                          ~pluck(.x[[1]])),
#          forecasts = map(calibrate,
#                          ~modeltime_forecast(.x,
#                                              new_data = training(jet_split_diff),
#                                              actual_data = jetblue_diff) %>% 
#                            plot_modeltime_forecast(.interactive = FALSE)),
#          metric_value = map(models,
#                             ~pluck(.x[[2]])),
#          rmse_value = map(models,
#                           ~pluck(.x[[2]]$rmse)))

# proph_mod_change_diff %>% 
#   unnest(metric_value) %>% 
#   select(id, mase, rmse) %>% 
#   arrange(mase)

# proph_mod_change_diff %>% 
#   unnest(rmse_value) %>% 
#   summarize(rmse_mean = mean(rmse_value))

# proph_mod_change_diff %>% 
#   select(id, rmse_value) %>% 
#   unnest(rmse_value) %>% 
#   arrange(rmse_value)

# changepoints[012]

# changepoints[145]


proph_mod_change_roll <-
  train_fold_roll %>%
  mutate(models = map2(splits, 
                       changepoints_roll,
                       ~prophet_mod_roll(.x,
                                         changepoints = .y,
                                         seasonality = .005,
                                         holiday = .001)),
         calibrate = map(models,
                         ~pluck(.x[[1]])),
         forecasts = map(calibrate,
                         ~modeltime_forecast(.x,
                                             new_data = training(jet_split_roll),
                                             actual_data = jetblue_roll) %>% 
                           plot_modeltime_forecast(.interactive = FALSE)),
         metric_value = map(models,
                            ~pluck(.x[[2]])),
         rmse_value = map(models,
                          ~pluck(.x[[2]]$rmse)))


proph_mod_change_roll %>% 
  unnest(metric_value) %>% 
  select(id, mase, rmse) %>% 
  arrange(mase)

proph_mod_change_roll %>% 
  unnest(rmse_value) %>% 
  summarize(rmse_mean = mean(rmse_value))

proph_mod_change_roll %>% 
  select(id, rmse_value) %>% 
  unnest(rmse_value) %>% 
  arrange(rmse_value)

changepoints_roll[002]
# 2

# final parameters 2, .005, .001


# let's check some other models
# first an auto arima
set.seed(05262022)

# arima_fit_diff <- arima_reg() %>% 
#   set_engine(engine = 'auto_arima') %>% 
#   fit(clean_diff1 ~ ds,
#       data = training(jet_split_diff))

# arima_fit_diff

# arima_resid_diff <- arima_fit_diff %>% 
#   modeltime_calibrate(new_data = training(jet_split_diff)) %>% 
#   modeltime_residuals()

# forecast::checkresiduals(arima_resid_diff$.residuals)

# arima_fit_diff %>% 
  # modeltime_calibrate(new_data = training(jet_split_diff)) %>% 
  # modeltime_forecast(new_data = training(jet_split_diff),
  #                     actual_data = jetblue_diff) %>% 
  # plot_modeltime_forecast(.interactive = FALSE)

# arima_fit_diff %>% 
#   modeltime_calibrate(new_data = training(jet_split_diff)) %>% 
#   modeltime_accuracy()



set.seed(05262022)

arima_fit_roll <- arima_reg() %>% 
  set_engine(engine = 'auto_arima') %>% 
  fit(clean ~ ds + month_num,
      data = training(jet_split_roll))
arima_fit_roll

arima_resid_roll <- arima_fit_roll %>% 
  modeltime_calibrate(new_data = training(jet_split_roll)) %>% 
  modeltime_residuals()

forecast::checkresiduals(arima_resid_roll$.residuals)

arima_fit_roll %>% 
  modeltime_calibrate(new_data = training(jet_split_roll)) %>% 
  modeltime_forecast(new_data = training(jet_split_roll),
                     actual_data = jetblue_roll) %>% 
  plot_modeltime_forecast(.interactive = FALSE)

arima_fit_roll %>% 
  modeltime_calibrate(new_data = training(jet_split_roll)) %>% 
  modeltime_accuracy()
# mase = 1
# rmse = .43
# rsq = .99





# let's also try an ETS model
set.seed(05262022)

# ets_fit_diff <- exp_smoothing() %>%
#   set_engine(engine = "ets") %>%
#   fit(clean_diff1 ~ ds,
#       data = training(jet_split_diff))

# ets_fit_diff

# ets_resid_diff <- ets_fit_diff %>% 
#   modeltime_calibrate(new_data = training(jet_split_diff)) %>% 
#   modeltime_residuals()

# forecast::checkresiduals(ets_resid_diff$.residuals)

# ets_fit_diff %>% 
#   modeltime_calibrate(new_data = training(jet_split_diff)) %>% 
#   modeltime_forecast(new_data = training(jet_split_diff),
#                      actual_data = jetblue_diff) %>% 
#   plot_modeltime_forecast(.interactive = FALSE)

# ets_fit_diff %>% 
#   modeltime_calibrate(new_data = training(jet_split_diff)) %>% 
#   modeltime_accuracy()



set.seed(05262022)

ets_fit_roll <- exp_smoothing() %>%
  set_engine(engine = "ets") %>%
  fit(clean ~ ds,
      data = training(jet_split_roll))

ets_fit_roll

ets_resid_roll <- ets_fit_roll %>% 
  modeltime_calibrate(new_data = training(jet_split_roll)) %>% 
  modeltime_residuals()

forecast::checkresiduals(ets_resid_roll$.residuals)

ets_fit_roll %>% 
  modeltime_calibrate(new_data = training(jet_split_roll)) %>% 
  modeltime_forecast(new_data = training(jet_split_roll),
                     actual_data = jetblue_roll) %>% 
  plot_modeltime_forecast(.interactive = FALSE)

ets_fit_roll %>% 
  modeltime_calibrate(new_data = training(jet_split_roll)) %>% 
  modeltime_accuracy()
# mase = 1, rmse = .43, rsq = .99



# final training model
# prophet_mod_diff(jet_split_diff,
#             changepoints = 2.20,
#             seasonality = .002,
#             holiday = .25,
#             train = TRUE) %>%  
#   pluck(2)


# prophet_mod_diff(jet_split_diff,
#             changepoints = 2.20,
#             seasonality = .002,
#             holiday = .25,
#             train = TRUE) %>%  
#   pluck(1) %>% 
#   modeltime_forecast(new_data = training(jet_split_diff),
#                      actual_data = jetblue_diff) %>% 
#   plot_modeltime_forecast(.interactive = FALSE)

# prophet_mod_diff(jet_split_diff,
#             changepoints = 2.20,
#             seasonality = .002,
#             holiday = .25,
#             train = TRUE) %>% 
#   pluck(1) %>% 
#   modeltime_residuals() %>% 
#   plot_modeltime_residuals(.interactive = FALSE)

# resid_plot_mod_diff <- prophet_mod_diff(jet_split_diff,
#                               changepoints = 2.20,
#                               seasonality = .002,
#                               holiday = .25,
#                               train = TRUE) %>% 
#   pluck(1) %>% 
#   modeltime_residuals()

# forecast::checkresiduals(resid_plot_mod_diff$.residuals)


# final training model
prophet_mod_roll(jet_split_roll,
                 changepoints = 2.66,
                 seasonality = .005,
                 holiday = .001,
                 train = TRUE) %>%  
  pluck(2)
# mase = 1.62
# rmse = .61
# rsq = .97

prophet_mod_roll(jet_split_roll,
                 changepoints = 2.66,
                 seasonality = .005,
                 holiday = .001,
                 train = TRUE) %>%  
  pluck(1) %>% 
  modeltime_forecast(new_data = training(jet_split_roll),
                     actual_data = jetblue_roll) %>% 
  plot_modeltime_forecast(.interactive = FALSE)

prophet_mod_roll(jet_split_roll,
                 changepoints = 2.66,
                 seasonality = .005,
                 holiday = .001,
                 train = TRUE) %>% 
  pluck(1) %>% 
  modeltime_residuals() %>% 
  plot_modeltime_residuals(.interactive = FALSE)

resid_plot_mod_roll <- prophet_mod_roll(jet_split_roll,
                                        changepoints = 2.66,
                                        seasonality = .005,
                                        holiday = .001,
                                        train = TRUE) %>% 
  pluck(1) %>% 
  modeltime_residuals()

forecast::checkresiduals(resid_plot_mod_roll$.residuals)


# testing set - prophet model
# prophet_mod_diff(jet_split_diff,
#             changepoints = 2.20,
#             seasonality = .002,
#             holiday = .25,
#             train = FALSE) %>%
#   pluck(1) %>% 
#   modeltime_forecast(new_data = testing(jet_split_diff),
#                      actual_data = jetblue_diff) %>% 
#   plot_modeltime_forecast(.interactive = FALSE)

# prophet_mod_diff(jet_split_diff,
#             changepoints = 2.20,
#             seasonality = .002,
#             holiday = .25,
#             train = FALSE) %>%
#   pluck(2)



# testing set - prophet model
prophet_mod_roll(jet_split_roll,
                 changepoints = 2.66,
                 seasonality = .005,
                 holiday = .001,
                 train = FALSE) %>%
  pluck(1) %>% 
  modeltime_forecast(new_data = testing(jet_split_roll),
                     actual_data = jetblue_roll) %>% 
  plot_modeltime_forecast(.interactive = FALSE)

prophet_mod_roll(jet_split_roll,
                 changepoints = 2.66,
                 seasonality = .005,
                 holiday = .001,
                 train = FALSE) %>%
  pluck(2)
# mase = 45.4
# rmse = 15.9
# rsq = .73


# testing set - arima model
# arima_fit_diff %>% 
#   modeltime_calibrate(new_data = testing(jet_split_diff)) %>% 
#   modeltime_forecast(new_data = testing(jet_split_diff),
#                      actual_data = jetblue_diff) %>% 
#   plot_modeltime_forecast(.interactive = FALSE)

# arima_fit_diff %>% 
#   modeltime_calibrate(new_data = testing(jet_split_diff)) %>% 
#   modeltime_accuracy()



# testing set - arima model
arima_fit_roll %>% 
  modeltime_calibrate(new_data = testing(jet_split_roll)) %>% 
  modeltime_forecast(new_data = testing(jet_split_roll),
                     actual_data = jetblue) %>% 
  plot_modeltime_forecast(.interactive = FALSE)

arima_fit_roll %>% 
  modeltime_calibrate(new_data = testing(jet_split_roll)) %>% 
  modeltime_accuracy()
# mase = 15.7
# rmse = 5.48
# rsq = .01


# testing set - ETS model
# ets_fit_diff %>% 
#   modeltime_calibrate(new_data = testing(jet_split_diff)) %>% 
#   modeltime_forecast(new_data = testing(jet_split_diff),
#                      actual_data = jetblue) %>% 
#   plot_modeltime_forecast(.interactive = FALSE)

# ets_fit_diff %>% 
#   modeltime_calibrate(new_data = testing(jet_split_diff)) %>% 
#   modeltime_accuracy()



# testing set - ETS model
ets_fit_roll %>% 
  modeltime_calibrate(new_data = testing(jet_split_roll)) %>% 
  modeltime_forecast(new_data = testing(jet_split_roll),
                     actual_data = jetblue_roll) %>% 
  plot_modeltime_forecast(.interactive = FALSE)

ets_fit_roll %>% 
  modeltime_calibrate(new_data = testing(jet_split_roll)) %>% 
  modeltime_accuracy()
# mase = 15.7, rmse = 5.46


# forecasting
future <- jetblue_roll %>% 
  future_frame(.length_out = '1 year', .bind_data = TRUE)

future <-
  future %>%
  select(-year_num, -month_num, -day_num) %>%
  mutate(date2 = ds) %>%
  separate(col = date2,
           into = c('year_num', 'month_num', 'day_num'),
           sep = '-') %>%
  mutate(year_num = as.factor(year_num),
         year_num = relevel(year_num, ref = '2018'),
         month_num = as.factor(month_num),
         day_num = as.numeric(day_num))


# prophet_mod_diff(jet_split_diff,
#             changepoints = 2.20,
#             seasonality = .002,
#             holiday = .25,
#             train = FALSE) %>%
#   pluck(1) %>% 
#   modeltime_refit(data = jetblue_diff) %>% 
#   modeltime_forecast(h = '1 year',
#                      actual_data = jetblue_diff) %>% 
#   plot_modeltime_forecast(.interactive = TRUE)


prophet_mod_roll(jet_split_roll,
                 changepoints = 2.66,
                 seasonality = .005,
                 holiday = .001,
                 train = FALSE) %>%
  pluck(1) %>% 
  modeltime_refit(data = future) %>% 
  modeltime_forecast(new_data = future,
                     actual_data = jetblue_roll) %>% 
  plot_modeltime_forecast(.interactive = TRUE)


# arima_fit_diff %>% 
#   modeltime_calibrate(new_data = testing(jet_split_diff)) %>% 
#   modeltime_refit(data = jetblue_diff) %>% 
#   modeltime_forecast(h = '1 year',
#                      actual_data = jetblue_diff) %>% 
#   plot_modeltime_forecast(.interactive = TRUE)


arima_fit_roll %>% 
  modeltime_calibrate(new_data = testing(jet_split_roll)) %>% 
  modeltime_refit(data = future) %>% 
  modeltime_forecast(new_data = future,
                     actual_data = jetblue_roll) %>% 
  plot_modeltime_forecast(.interactive = TRUE)


# ets_fit_diff %>% 
#   modeltime_calibrate(new_data = testing(jet_split_diff)) %>% 
#   modeltime_refit(data = jetblue_diff) %>% 
#   modeltime_forecast(h = '1 year',
#                      actual_data = jetblue_diff) %>% 
#   plot_modeltime_forecast(.interactive = TRUE)


ets_fit_roll %>% 
  modeltime_calibrate(new_data = testing(jet_split_roll)) %>% 
  modeltime_refit(data = jetblue_roll) %>% 
  modeltime_forecast(h = '1 year',
                     actual_data = jetblue_roll) %>% 
  plot_modeltime_forecast(.interactive = TRUE)
