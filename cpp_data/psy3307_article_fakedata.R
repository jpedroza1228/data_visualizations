
library(tidyverse)

set.seed(2222022)
data <- data.frame(milk_celeb1 = rnorm(n = 50,
                                       m = 3.39,
                                       sd = .82863),
                   milk_celeb2 = rnorm(n = 50,
                                       m = 2.70,
                                       sd = 1.12938),
                   fridge_celeb1 = rnorm(n = 50,
                                         m = 3.32,
                                         sd = .91339),
                   fridge_celeb2 = rnorm(n = 50,
                                         m = 3.36,
                                         sd = .96384))

data <- data %>% 
  pivot_longer(everything(),
               names_to = 'conditions',
               values_to = 'condition_values')

data_df <- data %>% 
  separate(conditions, into = c('product_type', 'celebrity'), sep = '_') %>% 
  mutate(product_type = case_when(product_type == 'milk' ~ 1,
                                  product_type == 'fridge' ~ 2),
         celebrity = case_when(celebrity == 'celeb1' ~ 1,
                               celebrity == 'celeb2' ~ 2))

data_df 

# H1: There will be a difference in DV (Attitude Toward Advertisement) by which product is shown (milk, fridge)
# H2: There will be a difference in DV (Attitude Toward Advertisement) by which celebrity type (celeb 1, celeb 2)
# H3: The relationship between product and attitude will depend on the celebrity promoting the ad

write.csv(data_df, 'example_article.csv')

example <- aov(condition_values ~ 1 + product_type*celebrity, data = data)
summary(example)
car::Anova(example, type = 'II')
car::Anova(example, type = 'III')

data %>% 
  group_by(product_type, celebrity) %>% 
  summarize(n = n(),
            mean_value = mean(condition_values),
            sd_value = sd(condition_values)) %>% 
  ungroup() %>% 
  ggplot(aes(product_type, mean_value,
             group = celebrity)) + 
  geom_line(aes(color = celebrity)) +
  geom_point(aes(color = celebrity))

TukeyHSD(example, which = 'product_type')
model.tables(example, type = 'means', se = TRUE)
