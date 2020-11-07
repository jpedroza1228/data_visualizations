library(tidyverse)
# install.packages(c('tidytext', 'wordcloud', 'reshape2'))
library(tidytext)

faculty <- rio::import(here::here('interesting_data', 'prev_faculty_survey.csv')) %>% 
  janitor::clean_names()

str(faculty)

separated <- faculty %>% 
  separate_rows(comment)
  
separated <- separated %>%  
  unnest_tokens(word, comment) %>%
  anti_join(stop_words)

separated %>% 
  count(word, sort = TRUE) %>% 
  with(wordcloud::wordcloud(word, n))
