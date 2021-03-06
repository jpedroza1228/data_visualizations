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

# animate(death_animate, fps = 10)

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

west_coast_beer <- west_coast_beer %>% 
  mutate(county = case_when(county == "Portland" ~ 'Multnomah',
                            TRUE ~ county))

west_coast_beer %>% 
  filter(state == ' OR') %>% 
  group_by(county) %>% 
  count(cases)

west_coast %>% 
  filter(region == 'oregon' &
           county == 'multnomah') %>% 
  group_by(county) %>% 
  count(group)


counts_by_state <- west_coast_beer %>% 
  group_by(type, state) %>% 
  summarize(total_state = sum(cases))

counts_all <- west_coast_beer %>% 
  group_by(type) %>% 
  summarize(total_state = sum(cases))


west_coast_beer %>% 
  ggplot() +
  geom_polygon(data = west_coast, aes(x = long, y = lat, group = group), fill = NA, color = 'gray70', alpha = .3) +
  geom_jitter(aes(x = long, y = lat, color = type), size = 2, width = 0.1, height = 0.1) +
  coord_fixed(1.1) +
  viridis::scale_color_viridis(discrete = TRUE,
                               labels = paste(counts_all$type, counts_all$total_state)) +
  theme_classic() 

west_coast_beer %>% 
  ggplot() +
  geom_polygon(data = west_coast, aes(x = long, y = lat, group = group), fill = NA, color = 'gray70', alpha = .3) +
  geom_jitter(aes(x = long, y = lat, color = type), width = .1) +
  coord_fixed(1.1) +
  viridis::scale_color_viridis(discrete = TRUE,
                               labels = paste(counts_all$type, counts_all$total_state)) +
  theme_classic() +
  facet_wrap(~type)


west_coast_beer$county <- str_to_lower(west_coast_beer$county)
west_coast_beer$county <- str_replace(west_coast_beer$county, pattern = " ",
                               replacement = "_")


west_coast_beer$county <- sub("_$", "", 
                              west_coast_beer$county)


west_coast <- west_coast %>% 
  rename(county = subregion)

west_coast$county <- str_replace_all(west_coast$county, pattern = ' ',
                                     replacement = '_')

west_joined <- left_join(west_coast_beer, west_coast, by = 'county')


# issue is that there are several county longitude and latitude coordinates. Will have to choose one
missing_counties <- west_joined %>% 
  group_by(county, group, order, long.x, lat.x, long.y, lat.y, type) %>% 
  filter(row_number()==1)


# install.packages('transformr')

missing_counties %>% 
  ggplot() +
  geom_polygon(aes(x = long.y, y = lat.y, group = group, fill = type)) +
  geom_polygon(data = west_coast, aes(x = long, y = lat, group = group), color = 'white', fill = 'gray70', alpha = .3) +
  coord_fixed(1.1) +
  theme_classic() +
  labs(title = 'Confirmed Cases, Deaths, and Recoveries of COVID-19\non West Coast',
       subtitle = 'Date: {frame_time}',
       x = 'Longitude',
       y = 'Latitude') +
  viridis::scale_fill_viridis(discrete = TRUE,
                               labels = paste(counts_all$type, counts_all$total_state)) +
  facet_wrap(~type)
  