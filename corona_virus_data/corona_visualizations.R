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

us <- map_data(map = 'usa')


little_beer <- beer %>% 
  filter(country_region == 'US')


us_animate <- little_beer %>% 
  filter(province_state != 'Unassigned Location (From Diamond Princess)') %>% 
  ggplot() +
  geom_polygon(data = us, aes(x = long, y = lat, group = group), fill = NA, color = 'gray70', alpha = .3) +
  geom_point(aes(x = long, y = lat, color = type), size = 2) +
  coord_fixed(1.3) +
  scale_color_manual(values = c('#1A8CFE', '#FE2F1A', '#8E49FF')) +
  # transition_time(date) +
  # ease_aes('linear') +
  # labs(title = 'Date: {frame_time}') +
  theme_nothing() +
  theme(legend.position = 'none')

us_animate

# animate(us_animate, fps = 100)
# anim_save('us_cases.gif')


county <- map_data('county')


west_coast <- county %>% 
  filter(region == 'california' |
           region == 'oregon' |
           region == 'washington')



west_coast_beer <- little_beer %>% 
  filter(province_state != 'Unassigned Location (From Diamond Princess)') %>% 
  separate(province_state, 
           into = c('county', 'state'),
           sep = ',') %>% 
  mutate(county = str_remove_all(county, "County")) %>% 
  filter(state == ' CA' |
           state == ' WA' |
           state == ' OR')


counts_by_state <- west_coast_beer %>% 
  group_by(type, state) %>% 
  summarize(total_state = sum(cases))

counts_all <- west_coast_beer %>% 
  group_by(type) %>% 
  summarize(total_state = sum(cases))



west_coast_beer %>% 
  ggplot() +
  geom_polygon(data = west_coast, aes(x = long, y = lat, group = group), fill = NA, color = 'gray70', alpha = .3) +
  geom_jitter(aes(x = long, y = lat, color = type), size = 2, width = 0.2) +
  coord_fixed(1.1) +
  colorblindr::scale_color_OkabeIto(labels = paste(counts_all$type, counts_all$total_state)) +
  theme_classic() 

west_coast_beer %>% 
  ggplot() +
  geom_polygon(data = west_coast, aes(x = long, y = lat, group = group), fill = NA, color = 'gray70', alpha = .3) +
  geom_jitter(aes(x = long, y = lat, color = type)) +
  coord_fixed(1.1) +
  colorblindr::scale_color_OkabeIto(labels = paste(counts_all$type, counts_all$total_state)) +
  theme_classic() +
  facet_wrap(~type)
