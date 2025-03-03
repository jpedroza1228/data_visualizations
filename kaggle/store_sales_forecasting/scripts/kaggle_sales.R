library(tidyverse)
library(forecast)
library(prophet)
library(tidymodels)
library(lubridate)
library(data.table)
library(tictoc)

theme_set(theme_light())

tic()
train <- fread('C:/Users/cpppe/Desktop/github_projects/data_visualizations/kaggle/store_sales_forecasting/data/train.csv') %>% 
  janitor::clean_names()
toc()

tic()
stores <- fread('C:/Users/cpppe/Desktop/github_projects/data_visualizations/kaggle/store_sales_forecasting/data/stores.csv') %>% 
  janitor::clean_names()

oil <- fread('C:/Users/cpppe/Desktop/github_projects/data_visualizations/kaggle/store_sales_forecasting/data/oil.csv') %>% 
  janitor::clean_names()

holidays <- fread('C:/Users/cpppe/Desktop/github_projects/data_visualizations/kaggle/store_sales_forecasting/data/holidays_events.csv') %>% 
  janitor::clean_names()

transactions <- fread('C:/Users/cpppe/Desktop/github_projects/data_visualizations/kaggle/store_sales_forecasting/data/transactions.csv') %>% 
  janitor::clean_names()
toc()

glimpse(train)
glimpse(stores)
glimpse(transactions)
glimpse(oil)
glimpse(holidays)


# summarized data 
train[,
      .(sum_sales = sum(sales)),
      by = store_nbr] 

train_store <- 
  merge(
  x = train,
  y = stores,
  all.x = TRUE,
  by = 'store_nbr'
)

glimpse(train_store)

train_store_trans <- 
  merge(
    x = train_store,
    y = transactions,
    all.x = TRUE,
    by = c('store_nbr',
           'date')
  )

tr_st_tran_oil <- 
  merge(
    x = train_store_trans,
    y = oil,
    all.x = TRUE,
    by = 'date'
  )

tr_st_tran_oil[,
               .N,
               by = date]

holidays[,
         .N,
         by = date]

# filter holidays to only have data from 2013 to 2017
# because those are the dates we are using

holidays <-
  holidays[,
         date2 := date
         ]

holidays <- 
  holidays[,
         c('year', 'month', 'day') :=
           tstrsplit(
           date2,
           split = '-'
         )
         ]

holidays <-
  holidays[year >= 2013 &
           year <= 2017 &
             month <= 08 &
             day <= 15]

holidays <- 
  unique(holidays,
       by = 'date')

tic()
data <-
  merge(
    x = tr_st_tran_oil,
    y = holidays,
    all.x = TRUE,
    by = 'date'
  )
toc()

rm(holidays,
   oil,
   stores,
   tr_st_tran_oil,
   train,
   train_store,
   train_store_trans,
   transactions)

data[,
     .N,
     by = 'transferred']

# visualizations

glimpse(data)

data[,
     .(sum_sales = sum(sales),
       sqrt_sales = sqrt(sum(sales)),
       log_sales = log(sum(sales)),
       diff_sales = diff(sales)),
     by = 'date'] %>% 
  ggplot(
    aes(
      date,
      sqrt_sales
    )
  ) +
  geom_line()

data[,
     .(sum_sales = sum(sales),
       sqrt_sales = sqrt(sum(sales)),
       log_sales = log(sum(sales)),
       diff_sales = diff(sales)),
     by = 'date'] %>% 
  ggplot(
    aes(
      date,
      log_sales
    )
  ) +
  geom_line()

data[,
     .(sum_sales = sum(sales),
       sqrt_sales = sqrt(sum(sales)),
       log_sales = log(sum(sales)),
       diff_sales = diff(sales)),
     by = 'date'] %>% 
  ggplot(
    aes(
      date,
      diff_sales
    )
  ) +
  geom_line()


data[,
     .(sum_sales = sum(sales),
       sqrt_sales = sqrt(sum(sales))),
     by = c('date', 'cluster')] %>% 
  ggplot(
    aes(
      date,
      sqrt_sales
    )
  ) +
  geom_line(
    aes(color = as.factor(cluster)
    )
  ) +
  facet_wrap(
    vars(
      cluster
    )
  )


data[,
     .(sum_sales = sum(sales),
       sqrt_sales = sqrt(sum(sales))),
     by = c('date', 'family', 'cluster')] %>% 
  ggplot(
    aes(
      date,
      sqrt_sales
    )
  ) +
  geom_line(
    aes(color = as.factor(cluster)
    )
  ) +
  facet_wrap(
    vars(
      family
    )
  )


# summarized data and filtered together
data[,
     .(sum_sales = sum(sales),
       sqrt_sales = sqrt(sum(sales))),
     by = date][
       sum_sales < 20000
     ] %>% 
  ggplot(
    aes(
      date,
      sum_sales
    )
  ) +
  geom_point() + 
  geom_line()

# there are trends of every january having the lowest sales


# before working on the actual data,
# I'll have to do some data manipulation
# grouped data by store, 
data[,
       c('sqrt_sum_sales',
         'sum_sales') := .(sqrt(sum(sales)),
                         sum(sales)),
       by = c('store_nbr', 'family', 'cluster')][]

# split into training and testing datasets
set.seed(5112022)
data_split <- initial_time_split(data, strata = sqrt_sum_sales)

data_train <- training(data_split)
data_test <- testing(data_split)


# sum_sales_ts <- data_train$sum_sales %>% 
#   ts()
# 
sqrt_sales_ts <- data_train$sqrt_sum_sales %>%
  ts()

# differences will want to use the raw data, not the calculated sum scores
# sales_ts <- data_train$sales %>% 
#   ts()

# examination of differences in points
# df_y <- diff(sum_sales_ts) 

# autoplot(df_y)

# ggseasonplot(df_y)
# apparently data is not seasonal

# ggsubseriesplot(df_y)
# apparently data is not seasonal

# seasonality visualizations
data_train[,
           .(sum_sales = sum(sales),
             sqrt_sales = sqrt(sum(sales)),
             log_sales = log(sum(sales)),
             diff_sales = diff(sales)),
     by = c('date', 'year')] %>% 
  ggplot(
    aes(
      date,
      sqrt_sales
    )
  ) +
  geom_line(
    aes(
      color = year 
        )
  ) +
  facet_wrap(
    vars(
      year
    ),
    scales = 'free'
  )

# all years have a huge drop between january
# appears to have seasonality in january 

# Method Types

# start with different methods

# seasonal naive method
# this uses the detrended data, or the difference values that was created
# df_y

set.seed(5112022)
seasonal_fit <- 
  snaive(
    df_y,
    h = 365
      )

summary(seasonal_fit) #RMSE = 4094472
# checkresiduals(seasonal_fit) #this will take a while with the current dataset
# this is a bad model, there is a lot of autocorrelation

# autoplot(
#   df_y
# ) +
#   autolayer(
#     snaive(
#     df_y,
#     h = 365
#     ),
#     series = 'Seasonsal Naive',
#     PI = FALSE
#     )


# exponential smoothing model
set.seed(5112022)
ets_fit <- 
  ets(
    sqrt_sales_ts
    )

summary(ets_fit) #RMSE = 616.96
# checkresiduals(ets_fit) #this has the worse distribution of residuals, not normal at all
# ets is a better fitting model than the seasonal naive model
# still a lot of autocorrelation in the model though

set.seed(5112022)
arima_fit <- 
  auto.arima(
    sqrt_sales_ts,
    # d = 1, #order of first-differencing (difference scores)
    # D = 1, # order of seasonal-differencing (seasonal difference scores)
    stepwise = FALSE, #does not try out ALL the models in a stepwise fashion
    approximation = FALSE, #approximates the best fitting model
    trace = TRUE #print out all the models that will be run
  )

summary(arima_fit) #RMSE = 588.61
sqrt(346460)
# tic()
# checkresiduals(arima_fit)
# toc()

set.seed(5112022)
arima_best <-
  arima(sqrt_sales_ts,
        order = c(0, 1, 4))

summary(arima_best) #RMSE = 612.99
sqrt(375753)
checkresiduals(arima_best)
# still a horrible fitting model
# may need to check the log values of the outcome

# attempt to fit a prophet model
set.seed(5112022)
change_only <- data_train[,
                          .(date,
                            sales)]

change_only[,
            sqrt_sales := sales][,
                                 sales := NULL]

setnames(
  change_only,
  old = c('date', 'sqrt_sales'),
  new = c('ds', 'y')
)


set.seed(5112022)
tic()
prop_model <- prophet(change_only,
                      fit = TRUE)


head(prop_model)
str(prop_model)
prop_model$params
# sigma = .15
prop_model$component.modes

future <- make_future_dataframe(prop_model,
                                periods = 365)

head(future)

fcast <- predict(prop_model,
                  future)
head(fcast)

plot(prop_model,
     fcast)

mean(fcast$yhat)

# cross validation
prop_cv <- cross_validation(prop_model,
                            initial = 730,
                            period = 180,
                            horizon = 365,
                            units = 'days')
prop_cv
# model looks absolutely horrible

# rmse(prop_model$history$y, fcast$yhat)



# make holidays dummy variables for specific dates
# learn how to do this in data.table





# load in the test data and submission file

submission <- fread('C:/Users/cpppe/Desktop/github_projects/data_visualizations/kaggle/store_sales_forecasting/data/sample_submission.csv') %>% 
  janitor::clean_names()

glimpse(submission)

