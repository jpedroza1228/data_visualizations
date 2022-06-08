# data manipulation

library(tidyverse)
library(forecast)
library(tidymodels)

theme_set(theme_light())

item <- read_csv("kaggle/predict_future_sales/data/items.csv") %>% 
  select(-item_name)
item_cat <- read_csv("kaggle/predict_future_sales/data/item_categories.csv") %>% 
  select(-item_category_name)
sales_train <- read_csv("kaggle/predict_future_sales/data/sales_train.csv")
shops <- read_csv("kaggle/predict_future_sales/data/shops.csv") %>% 
  select(-shop_name)

item_df <- left_join(item, item_cat)
shops_inc <- left_join(sales_train, shops)
data <- left_join(shops_inc, item_df)

data <- 
  data %>% 
  mutate(
    date2 = date
  ) %>% 
  separate(
    date,
    c('day', 'month', 'year')
    )

sum_data <- 
  data %>% 
  group_by(
    month, year, shop_id, item_id
    ) %>% 
  summarize(
    item_cnt_month = sum(item_cnt_day)
    ) %>% 
  ungroup()



final <- 
  left_join(
    data,
            sum_data,
            by = c('month', 'year', 
                   'shop_id', 'item_id')
    )

rm(item, item_cat,
   item_df, sales_train,
   shops, shops_inc,
   data, sum_data)

glimpse(final)

final <- 
  final %>% 
  mutate(
    day = as.numeric(day),
         month = as.factor(month),
         year = as.factor(year)
    )

# trying to use forecast package

final15 <- 
  final %>% 
  filter(
    year == 2015,
    shop_id == 6
    )

ex <-
  final15 %>%
  select(
    item_cnt_month
    ) %>% 
  ts() 
  
ets(ex) %>% 
  forecast() %>% 
  autoplot()






final <-
  final %>% 
  mutate(item_cnt_month_sqrt = sqrt(item_cnt_month),
         item_price_sqrt = sqrt(item_price),
         item_cnt_month_sqrt = if_else(is.na(item_cnt_month_sqrt),
                                       median(item_cnt_month_sqrt,
                                              na.rm = TRUE),
                                       item_cnt_month_sqrt)) %>% 
  select(-item_price,
         -item_cnt_month)

final <- 
  final %>% 
  mutate(item_cnt_month_sqrt = if_else(is.na(item_cnt_month_sqrt),
                                       median(item_cnt_month_sqrt,
                                              na.rm = TRUE),
                                       item_cnt_month_sqrt),
         item_price_sqrt = if_else(is.na(item_price_sqrt),
                                   median(item_price_sqrt,
                                          na.rm = TRUE),
                                   item_price_sqrt))

final %>% 
  inspectdf::inspect_na()

final %>% 
  inspectdf::inspect_cor(with = 'item_cnt_month_sqrt')

final %>% 
  sample_frac(.001) %>% 
  group_by(shop_id, year) %>% 
  summarize(sum_sales = sum(item_cnt_month_sqrt)) %>% 
  ungroup() %>% 
  ggplot(aes(as.factor(shop_id), sum_sales)) + 
  geom_col(color = 'white',
           fill = 'darkgreen') +
  coord_flip() +
  facet_wrap(vars(year))

final %>% 
  sample_frac(.001) %>% 
  ggplot(aes(item_price_sqrt)) + 
  geom_histogram(color = 'white',
                 fill = 'darkgreen',
                 bins = 20)

final %>% 
  sample_frac(.001) %>% 
  ggplot(aes(item_price_sqrt, item_cnt_month_sqrt)) + 
  geom_point(alpha = .5) + 
  geom_smooth(method = 'lm',
              se = FALSE) +
  facet_wrap(vars(year))

# write.csv(final, 'final_train.csv')

final <-
  final %>% 
    unite(col = 'month_yr',
          c('year', 'month'),
          sep = '-') %>% 
    mutate(date_approx = lubridate::ym(month_yr))

# forecasting
set.seed(31022)
library(prophet)

time_data <- 
  final %>% 
  rename(ds = date_approx,
         y = item_cnt_month_sqrt)

glimpse(time_data)

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


# model building
set.seed(125475)
sale_split <- initial_split(sub, strata = "item_cnt_month")
sale_train <- training(sale_split)
sale_test <- testing(sale_split)

set.seed(125475)

sale_fold <- vfold_cv(sale_train, strata = "item_cnt_month", v = 5)

get_model <- function(x) {
  pull_workflow_fit(x) %>% tidy()
}

set.seed(125475)

start_recipe <- recipe(item_cnt_month ~ moscow_store + date_block_num + item_price + item_id +
                         shop_id + item_category_id + month_temp, data = sale_train) %>% 
  step_zv(all_predictors()) %>% 
  step_unknown(all_nominal()) %>% 
  step_novel(all_nominal()) %>% 
  step_dummy(all_nominal()) %>% 
  step_nzv(all_predictors()) %>%
  step_medianimpute(all_numeric(), -all_outcomes()) %>% 
  step_normalize(all_numeric(), -all_outcomes()) %>% 
  step_BoxCox(item_price, item_cnt_month) %>% 
  step_zv(all_predictors()) %>% 
  step_nzv(all_predictors()) %>% 
  step_naomit(item_price) 

set.seed(125475)

cores <- parallel::detectCores()

random_gump <- rand_forest() %>% 
  set_engine("ranger",
             num.threads = cores,
             importance = "permutation",
             verbose = TRUE) %>% 
  set_mode("regression") %>% 
  set_args(mtry = tune(),
           trees = 1000,
           min_n = tune())

flow <- workflow() %>% 
  add_recipe(start_recipe) %>% 
  add_model(random_gump)

gump_fit <- tune_grid(flow,
                      resamples = sale_fold,
                      grid = 5,
                      metrics = metric_set(rmse),
                      control = control_resamples(verbose = TRUE,
                                                  save_pred = TRUE,
                                                  extract = get_model))

show_best(gump_fit, n = 5)

best_fit <- gump_fit %>% 
  select_best(metric = "rmse")

set.seed(125475)

final_flow <- finalize_workflow(flow, best_fit) %>% 
  fit(data = sale_train)

last_fit(final_flow, split = sale_split) %>% 
  show_best(metric = "rmse", n = 5)

# validate_results <- last_fit(final_flow,
#                              split = sale_split)

predictions <- predict(final_flow, testing(sale_split))

predictions


test <-read_csv("C:/Users/cpppe/Desktop/github_projects/data_visualizations/kaggle/predict_future_sales/data/test.csv") %>% 
  left_join(item_df) %>% 
  left_join(shops_inc) %>%
  group_by(date_block_num, shop_id) %>% 
  mutate(item_cnt_month = sum(item_cnt_day)) %>% 
  ungroup() %>%
  mutate(date2 = date) %>% 
  separate(date, c("day", "month", "year")) %>% 
  mutate(moscow_store = if_else(str_detect(shop_name, "^Москва"), "inside", "outside"),
         moscow_store = as.factor(moscow_store),
         month_temp = recode(month, "1" = "cold",
                             "2" = "cold",
                             "3" = "cold",
                             "4" = "warm",
                             "5" = "warm",
                             "6" = "warm",
                             "7" = "warm",
                             "8" = "warm",
                             "9" = "warm",
                             "10" = "warm",
                             "11" = "cold",
                             "12" = "cold"),
         month_temp = as.factor(month_temp))


test_predictions <- predict(final_flow, test)
test_predictions

