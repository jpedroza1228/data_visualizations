# using models to see if we can forecast other airline stock data

link <- 'https://docs.google.com/spreadsheets/d/1Ld9E_5Da9xaZ31VxLCQ41UG26FWjNXHBuU6YHqq1kaY/edit#gid=0'

theme_set(theme_light())

googlesheets4::gs4_deauth()

amer <- 
  googlesheets4::read_sheet(link) %>% 
  janitor::clean_names() %>% 
  mutate(ds = as_date(date))

american <-
  amer %>% 
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
         year_num = relevel(year_num, ref = '2013')) %>% 
  separate(col = day_num,
           into = c('day_num', 'drop'),
           sep = ' ') %>%
  mutate(day_num = as.numeric(day_num),
         month_num = as.factor(month_num)) %>% 
  select(-drop) %>% 
  drop_na(clean_diff1)  %>% 
  arrange(ds)

glimpse(american)

only_numeric <- american %>% 
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

american %>% 
  tk_anomaly_diagnostics(ds,
                         clean) %>% 
  ggplot(aes(ds, observed)) + 
  geom_line() + 
  geom_point(aes(color = anomaly))

anomaly_roll <- american %>%
  tk_anomaly_diagnostics(ds,
                         clean)

american <- left_join(american, anomaly_roll) %>%
  filter(anomaly != 'Yes')

american %>%
  plot_time_series(ds,
                   clean,
                   .interactive = TRUE)

american %>% 
  plot_acf_diagnostics(ds,
                       clean,
                       .interactive = TRUE)

american %>% 
  plot_seasonal_diagnostics(ds,
                            clean,
                            .interactive = FALSE)

american %>% 
  plot_stl_diagnostics(ds,
                       clean,
                       .frequency = 'auto',
                       .trend = 'auto',
                       .feature_set = c('observed',
                                        'season',
                                        'trend',
                                        'remainder'),
                       .interactive = FALSE)


set.seed(05262022)
amer_split <- initial_time_split(american) 

set.seed(05262022)

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
    fit(clean ~ ds + year_num, 
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

prophet_mod(amer_split,
                 train = TRUE) %>% 
  pluck(2)
# mase = 3.46
# rmse = 2.94
# rsq = .88

prophet_mod(amer_split,
                 train = TRUE) %>% 
  pluck(1) %>% 
  modeltime_forecast(new_data = training(amer_split), 
                     actual_data = american) %>% 
  plot_modeltime_forecast(.interactive = FALSE)

prophet_mod(amer_split,
                 train = TRUE) %>% 
  pluck(1) %>% 
  modeltime_residuals() %>% 
  plot_modeltime_residuals(.interactive = FALSE)

resid_plot_mod <- prophet_mod(amer_split,
                                   train = TRUE) %>% 
  pluck(1) %>% 
  modeltime_residuals()

# modeltime_residuals_test(resid_plot_mod)

forecast::checkresiduals(resid_plot_mod$.residuals)
mean(resid_plot_mod$.residuals)
acf(resid_plot_mod$.residuals,
    plot = FALSE)


# with the final training model of jetblue
prophet_mod(amer_split,
            changepoints = 2,
            seasonality = .01,
            holiday = .01,
            train = TRUE) %>%
  pluck(2)
# mase = 2.73
# rmse = 2.38
# rsq = .92

prophet_mod(amer_split,
            changepoints = 2,
            seasonality = .01,
            holiday = .01,
            train = TRUE) %>% 
  pluck(1) %>% 
  modeltime_forecast(new_data = training(amer_split), 
                     actual_data = american) %>% 
  plot_modeltime_forecast(.interactive = FALSE)

resid_plot_mod <- prophet_mod(amer_split,
                              changepoints = 2,
                              seasonality = .01,
                              holiday = .01,
                              train = TRUE) %>% 
  pluck(1) %>% 
  modeltime_residuals()

forecast::checkresiduals(resid_plot_mod$.residuals)



# arima model
set.seed(05262022)

arima_fit_roll <- arima_reg() %>% 
  set_engine(engine = 'auto_arima') %>% 
  fit(clean ~ ds + year_num,
      data = training(amer_split))
arima_fit_roll

arima_resid_roll <- arima_fit_roll %>% 
  modeltime_calibrate(new_data = training(amer_split)) %>% 
  modeltime_residuals()

forecast::checkresiduals(arima_resid_roll$.residuals)

arima_fit_roll %>% 
  modeltime_calibrate(new_data = training(amer_split)) %>% 
  modeltime_forecast(new_data = training(amer_split),
                     actual_data = american) %>% 
  plot_modeltime_forecast(.interactive = FALSE)

arima_fit_roll %>% 
  modeltime_calibrate(new_data = training(amer_split)) %>% 
  modeltime_accuracy()
# mase = 1.22
# rmse = 1.11
# rsq = .99


# testing

prophet_mod(amer_split,
            changepoints = 2,
            seasonality = .01,
            holiday = .01,
            train = FALSE) %>%
  pluck(2)
# mase = 6.8
# rmse = 4.20
# rsq = .16

prophet_mod(amer_split,
            changepoints = 2,
            seasonality = .01,
            holiday = .01,
            train = FALSE) %>% 
  pluck(1) %>% 
  modeltime_forecast(new_data = testing(amer_split), 
                     actual_data = american) %>% 
  plot_modeltime_forecast(.interactive = FALSE)


arima_fit_roll %>% 
  modeltime_calibrate(new_data = testing(amer_split)) %>% 
  modeltime_forecast(new_data = testing(amer_split),
                     actual_data = american) %>% 
  plot_modeltime_forecast(.interactive = FALSE)

arima_fit_roll %>% 
  modeltime_calibrate(new_data = testing(amer_split)) %>% 
  unnest(.calibration_data)
  modeltime_accuracy()
# mase = 35.1
# rmse = 17
# rsq = .62


# forecasting
future <- american %>% 
  future_frame(.length_out = '1 year', .bind_data = TRUE)

future <-
  future %>%
    select(-year_num, -month_num, -day_num) %>%
    mutate(date2 = ds) %>%
    separate(col = date2,
             into = c('year_num', 'month_num', 'day_num'),
             sep = '-') %>%
    mutate(year_num = as.factor(year_num),
           year_num = relevel(year_num, ref = '2013'),
           month_num = as.factor(month_num),
           day_num = as.numeric(day_num))


prophet_mod(amer_split,
                 changepoints = 2,
                 seasonality = .01,
                 holiday = .01,
                 train = FALSE) %>%
  pluck(1) %>% 
  modeltime_refit(data = future) %>% #only runs with data that is present, which is the same as the american df
  modeltime_forecast(new_data = future,
                     actual_data = american) %>% 
  plot_modeltime_forecast(.interactive = TRUE)


arima_fit_roll %>% 
  modeltime_calibrate(new_data = testing(amer_split)) %>% 
  modeltime_refit(data = future) %>% 
  modeltime_forecast(new_data = future,
                     actual_data = american) %>% 
  plot_modeltime_forecast(.interactive = TRUE)
