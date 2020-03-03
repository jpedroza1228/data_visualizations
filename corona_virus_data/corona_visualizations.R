library(tidyverse)
library(ggmap)
library(maps)
library(googleway)

# devtools::install_github('thomasp85/gganimate')


beer <- read_csv('https://raw.githubusercontent.com/RamiKrispin/coronavirus-csv/master/coronavirus_dataset.csv')

beer <- beer %>% 
  janitor::clean_names()

world <- map_data(map = 'world', subregion = 'state')


world <- world %>% 
  rename(state = subregion)

world$state <- str_to_lower(world$state)
world$state <- str_replace_all(world$state, pattern = " ",
                               replacement = "_")

library(gganimate)

ggplot(data = beer) +
  geom_polygon(data = world, aes(x = long, y = lat, group = group), fill = NA, color = 'gray70', alpha = .3) +
  geom_point(data = beer, aes(x = long, y = lat, color = type, size = cases)) +
  coord_fixed(1.3) +
  scale_color_manual(values = c('#1A8CFE', '#FE2F1A', '#8E49FF')) +
  facet_grid(rows = vars(type)) +
  theme_classic()

death_animate <- beer %>% 
  filter(type == 'death') %>% 
  ggplot() +
  geom_polygon(data = world, aes(x = long, y = lat, group = group), fill = NA, color = 'gray70', alpha = .3) +
  geom_point(aes(x = long, y = lat, size = cases, color = type)) +
  coord_fixed(1.3) +
  scale_color_manual(values = c('#FE2F1A')) +
  transition_time(date) +
  ease_aes('linear') +
  labs(title = 'Date: {frame_time}') +
  theme_classic()

animate(death_animate, fps = 10)

us <- map_data(map = 'usa', subregion = 'county')

little_beer <- beer %>% 
  filter(country_region == 'US')

us_animate <- little_beer %>% 
  filter(province_state != 'Unassigned Location (From Diamond Princess)') %>% 
  ggplot() +
  geom_polygon(data = us, aes(x = long, y = lat, group = group), fill = NA, color = 'gray70', alpha = .3) +
  geom_point(aes(x = long, y = lat, color = type)) +
  coord_fixed(1.3) +
  scale_color_manual(values = c('#1A8CFE', '#FE2F1A', '#8E49FF')) +
  transition_time(date) +
  ease_aes('linear') +
  labs(title = 'Date: {frame_time}') +
  theme_classic()

animate(us_animate)
anim_save('us_cases.gif')

