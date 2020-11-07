coffee <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-07-07/coffee_ratings.csv')

library(tidyverse)

theme_set(theme_minimal())

names(coffee)

coffee %>% 
  ggplot(aes(cupper_points, total_cup_points)) +
  geom_point(color = 'dodgerblue') +
  coord_flip() +
    facet_wrap(~country_of_origin)

characteristic <- function(x, y){
  coffee %>% 
    ggplot(aes({{x}}, {{y}})) +
    geom_point(alpha = .7, aes(color = species)) +
    geom_smooth(method = 'lm', se = FALSE)
}

coffee %>% 
  select(21:29) %>% 
  map(~characteristic(.x, total_cup_points))


cof_numeric <- coffee %>% 
  select(1, 21:29, 41:43)

inspectdf::inspect_na(cof_numeric)

cof_numeric <- na.omit(cof_numeric)

library(MASS)
library(tidymodels)

# install.packages(c('cluster', 'factoextra'))
set.seed(5464)

cof_split <- initial_split(cof_numeric)

cof_train <- training(cof_split)



set.seed(5464)
cof_train <- scale(cof_train)

# http://sia.webpopix.org/mixtureModels.html#mixture-model-versus-clustering





set.seed(5464)
fviz_nbclust(cof_train, kmeans, method = "wss")
fviz_nbclust(cof_train, kmeans, method = "silhouette")


set.seed(5464)
gap_stat <- clusGap(cof_train, FUN = kmeans, nstart = 25,
                    K.max = 10, B = 50)
fviz_gap_stat(gap_stat)

#testing
set.seed(5464)
cof_test <- testing(cof_split)

t1 <- kmeans(cof_test, centers = 1, nstart = 20)
t2 <- kmeans(cof_test, centers = 2, nstart = 20)
t3 <- kmeans(cof_test, centers = 3, nstart = 20)


fviz_cluster(t1, data = cof_test)
fviz_cluster(t2, data = cof_test)
fviz_cluster(t3, data = cof_test)

set.seed(5464)
fviz_nbclust(cof_test, kmeans, method = "wss")
fviz_nbclust(cof_test, kmeans, method = "silhouette")


set.seed(5464)
gap_stat <- clusGap(cof_test, FUN = kmeans, nstart = 25,
                    K.max = 10, B = 50)
fviz_gap_stat(gap_stat)


#two clusters is best fit of a good cup of coffee.
