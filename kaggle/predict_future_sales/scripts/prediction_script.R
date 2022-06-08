getwd()

library(tidyverse)
library(tidymodels)

theme_set(theme_light())

item <- read_csv("kaggle/predict_future_sales/data/items.csv")
item_cat <- read_csv("kaggle/predict_future_sales/data/item_categories.csv")
sales <- read_csv("kaggle/predict_future_sales/data/sales_train.csv")
shops <-read_csv("kaggle/predict_future_sales/data/shops.csv")

names(item)
names(item_cat)
names(shops)

item_df <- left_join(item, item_cat)
shops_inc <- left_join(sales, shops)
data <- left_join(shops_inc, item_df)

rm(item, item_cat,
   item_df, sales, shops,
   shops_inc)

data %>% 
  glimpse()

sum_data <- 
  data %>% 
  separate(date, c("day", "month", "year")) %>% 
  group_by(shop_id, month, year, item_category_id) %>% 
  summarize(item_cnt_month = sum(item_cnt_day)) %>% 
  ungroup()

sum_data <- sum_data %>% 
  unite(col = 'month_yr',
        c('year', 'month'),
        sep = '-')

sum_data$date_approx <- lubridate::ym(sum_data$month_yr)

head(sum_data)

sum_data %>% 
  count(shop_id)

sum_data %>% 
  ggplot(aes(date_approx, item_cnt_month)) +
  geom_point(data = sum_data %>% 
               filter(item_cnt_month < 3000)) +
  geom_text(data = sum_data %>% 
              filter(item_cnt_month > 3000),
            aes(label = shop_id))

df13 <-
  sum_data %>% 
  filter(str_detect(month_yr, '^2013'))
  
df13 <- 
  df13 %>% 
  separate(col = month_yr,
           into = c('year', 'month'),
           sep = '-') %>% 
  mutate(month_temp = recode(month, "01" = "cold",
                             "02" = "cold",
                             "03" = "cold",
                             "04" = "warm",
                             "05" = "warm",
                             "06" = "warm",
                             "07" = "warm",
                             "08" = "warm",
                             "09" = "warm",
                             "10" = "warm",
                             "11" = "cold",
                             "12" = "cold"),
         month_temp = as.factor(month_temp),
         shop_id = as.factor(shop_id),
         month = as.factor(month),
         item_cnt_month_sqrt = sqrt(item_cnt_month)) %>% 
  select(-year)

df13 %>% 
  glimpse()

df14 <- 
  sum_data %>% 
  filter(str_detect(month_yr, '^2014'))

df14 <- 
  df14 %>% 
  separate(col = month_yr,
           into = c('year', 'month'),
           sep = '-') %>% 
  mutate(month_temp = recode(month, "01" = "cold",
                             "02" = "cold",
                             "03" = "cold",
                             "04" = "warm",
                             "05" = "warm",
                             "06" = "warm",
                             "07" = "warm",
                             "08" = "warm",
                             "09" = "warm",
                             "10" = "warm",
                             "11" = "cold",
                             "12" = "cold"),
         month_temp = as.factor(month_temp),
         shop_id = as.factor(shop_id),
         month = as.factor(month),
         item_cnt_month_sqrt = sqrt(item_cnt_month)) %>% 
  select(-year)

df15 <- 
  sum_data %>% 
  filter(str_detect(month_yr, '^2015'))

df15 <- 
  df15 %>% 
  separate(col = month_yr,
           into = c('year', 'month'),
           sep = '-') %>% 
  mutate(month_temp = recode(month, "01" = "cold",
                             "02" = "cold",
                             "03" = "cold",
                             "04" = "warm",
                             "05" = "warm",
                             "06" = "warm",
                             "07" = "warm",
                             "08" = "warm",
                             "09" = "warm",
                             "10" = "warm",
                             "11" = "cold",
                             "12" = "cold"),
         month_temp = as.factor(month_temp),
         shop_id = as.factor(shop_id),
         month = as.factor(month),
         item_cnt_month_sqrt = sqrt(item_cnt_month)) %>% 
  select(-year) %>% 
  drop_na(item_cnt_month_sqrt)

# rm(data)

df15 %>% 
  group_by(month) %>% 
  count()

# https://www.r-bloggers.com/2018/12/rolling-origins-and-fama-french/

set.seed(31022)
split15 <- initial_time_split(df15,
                              prop = .80,
                              strata = 'item_cnt_month')

set.seed(31022)
train15 <- training(split15)
test15 <- testing(split15)


psych::describe(train15$item_cnt_month_log, na.rm = TRUE)
psych::describe(train15$item_cnt_month, na.rm = TRUE)
psych::describe(sqrt(train15$item_cnt_month), na.rm = TRUE)

# train15 %>% 
#   ggplot(aes(item_cnt_month)) + 
#   geom_histogram(color = 'white',
#                  fill = 'darkgreen',
#                  bins = 20) +
#   scale_x_log10()

# map2(
#   train15, 
#   names(train15),
#   ~ggplot(data = train15,
#           aes(.x, item_cnt_month)) + 
#     geom_point(alpha = .5) + 
#     geom_smooth(method = "lm", se = FALSE, color = "black") +
#     geom_smooth(method = "loess", se = FALSE, color = "dodgerblue") +
#     stat_smooth(method = "lm", se = FALSE, formula = y ~ x + I(x^2), color = "red") +
#     scale_y_log10() + 
#     labs(x = glue::glue('{.y}'),
#          y = 'Items Sold Per Month')
# )

# random forest
library(ranger)
set.seed(31022)

# map(seq(1000, 2000, by = 100),
#     ~ranger(
#       item_cnt_month_sqrt ~ .,
#       importance = 'permutation',
#       data = train15,
#       num.trees = .x,
#       mtry = 6,
#       min.node.size = 2)
#     )

model <- ranger::ranger(item_cnt_month_sqrt ~ .,
                        importance = 'permutation',
                        data = train15,
                        num.trees = 1000,
                        mtry = 6,
                        min.node.size = 2
)
model

train15$rf_resid[!is.na(train15$item_cnt_month_sqrt)] <-
  model$predictions - sqrt(train15$item_cnt_month_sqrt[!is.na(train15$item_cnt_month_sqrt)])

train15 %>% 
  ggplot(aes(month,
             rf_resid)) + 
  geom_jitter(alpha = .5,
              aes(color = month_temp))

model$r.squared
model$prediction.error


model_test <- ranger::ranger(item_cnt_month_sqrt ~ .,
                             importance = 'permutation',
                             data = test15,
                             num.trees = 1000,
                             mtry = 6,
                             min.node.size = 2
)
model_test

test15$rf_resid_test[!is.na(test15$item_cnt_month_sqrt)] <-
  model_test$predictions - sqrt(test15$item_cnt_month_sqrt[!is.na(test15$item_cnt_month_sqrt)])

model_test$r.squared
model_test$prediction.error

test15 %>% 
  ggplot(aes(month,
             rf_resid_test)) + 
  geom_jitter(alpha = .5,
              aes(color = month_temp))

test <- read_csv('kaggle/predict_future_sales/data/test.csv')

item <- read_csv("kaggle/predict_future_sales/data/items.csv")
item_cat <- read_csv("kaggle/predict_future_sales/data/item_categories.csv")
shops <-read_csv("kaggle/predict_future_sales/data/shops.csv")

test <- test %>% 
  left_join(item) %>% 
  left_join(item_cat) %>% 
  left_join(shops)


test <- test %>% 
  mutate(
    month = 11,
    year = 2015,
    item_cnt_month_sqrt = NA,
    shop_id = as.factor(shop_id),
    month_temp = recode(month, "01" = "cold",
                        "02" = "cold",
                        "03" = "cold",
                        "04" = "warm",
                        "05" = "warm",
                        "06" = "warm",
                        "07" = "warm",
                        "08" = "warm",
                        "09" = "warm",
                        "10" = "warm",
                        "11" = "cold",
                        "12" = "cold")
  ) %>% 
  select(-item_name,
         -item_category_name,
         -shop_name)

glimpse(test)

test_pred <- ranger::predictions(model,
                                 newdata = test,
                                 type = 'response')
test_pred

jp_submission <- tibble(ID = test$ID,
                        item_cnt_month = test_pred)


