library(tidyverse)
library(tictoc)

final <- read_csv('kaggle/predict_future_sales/data/final_train.csv') 

set.seed(31022)
library(prophet)

glimpse(final)

final %>% 
  count(date_block_num) %>% 
  View()

final %>% 
  group_by(mnt_yr_extra) %>% 
  count(ymd_date) %>% 
  View()


final %>% 
  filter(date_block_num > 23) %>% 
  group_by(mnt_yr_extra) %>% 
  count(ymd_date) %>% 
  View()

# create a new column that is not a list and
# use that in map2 as the second argument to change parameters for 
# cross validation


final <- 
final %>% 
  select(-date_approx) %>% 
  mutate(mnt_yr_extra = month_yr) %>% 
  unite(col = 'ymd_date',
        c('month_yr', 'day')) %>% 
  mutate(ymd_date = lubridate::ymd(ymd_date))

glimpse(final)

# trying to run it with all data to make predictions
# all_in <- 
#   final %>% 
#   separate(col = mnt_yr_extra,
#            into = c('yr', 'mnt'))

# all_modeling <- 
#   all_in %>% 
#   select(ymd_date,
#          item_cnt_month_sqrt) %>% 
#   rename(ds = ymd_date,
#          y = item_cnt_month_sqrt)

# prophet modeling for all the data
# model <- prophet(all_modeling)  %>% 
# add_regressor('variable name') %>% 
# add_regressor('variable name')

# model with cross validation
# does not work with these dates because seasonality is based on years
# cutoffs <- as.Date(c('2013-12-31', '2014-12-31'))
# df_cv <- cross_validation(model,
#   cutoffs = cutoffs,
#   horizon = 31,
#   units = 'days')

# plot_cross_validation_metric(df_cv,
#                              metric = 'rmse')

# model alone
# future <- make_future_dataframe(model,
#                                 periods = 31)

# fc_predict <- predict(model,
#                       future)

# plot(model,
#      fc_predict)


nested <- final %>% 
  separate(col = mnt_yr_extra,
           into = c('yr', 'mnt')) %>% 
  group_by(yr) %>% 
  nest()

tictoc::tic()
nested <-
  nested %>% 
  mutate(
    y_n_dates = map(
    data,
    ~.x %>% 
      select(ymd_date,
             item_cnt_month_sqrt) %>% 
      rename(ds = ymd_date,
             y = item_cnt_month_sqrt)
  ),
  sales_model = map(
    y_n_dates,
    ~prophet(.x)
  )
  )
tictoc::toc()


# 1034 individual days
# 34 months, starting with 0 to 33
# 0-11 = 2013, 12-23 = 2014, 24-33 = 2015

# 365 for 2013
# 365 for 2014
# 304 for 2015

# nested$initial_values <- c(219, 219, 182)

# nested <-
#   nested %>% 
#   mutate(
#     cv_values = map2(
#       sales_model,
#       initial_values,
#       ~cross_validation(
#         .x,
#         initial = .y,
#         horizon = 30,
#         units = 'days'
#       )
#       )
#   )

    
tic()
nested <- 
  nested %>% 
  mutate(
    future = map(
      sales_model,
      ~make_future_dataframe(
        .x,
        periods = 31,
        freq = 'day'
      )
    ),
    fc_pred = map2(
      sales_model,
      future,
      ~predict(
        .x,
        .y
      )
    ),
    components = map2(
      sales_model,
      fc_pred,
      ~prophet_plot_components(
        .x,
        .y
      )
    )
  )
toc()

nested %>% 
  unnest(fc_pred) %>% 
  select(ds, yr, yhat)

ex # n = 1127


# nested %>% 
#   mutate(
#     model_fit = map(
#       sales_model,
#       ~fit.prophet(
#         .x
#       )
#     )
#   )








model <- prophet(time_data)

future <- make_future_dataframe(model, periods = 31, freq = 'day')

forecast <- predict(model, future)

head(forecast)

prophet_plot_components(model, forecast)

# plot(model, forecast)

glimpse(model)
glimpse(forecast)

# performance_metrics(model, metrics = 'rmse')

m <- tibble(model$history)

m <- m %>% 
  left_join(forecast, by = 'ds')

m %>% 
  sample_frac(.2) %>% 
  ggplot(aes(ds, y_scaled)) + 
  geom_point(color = 'gray70', alpha = .3)
