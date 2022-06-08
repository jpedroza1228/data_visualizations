library(tidyverse)
cat_map <- purrr::map
options(scipen = 999)
theme_set(theme_light())

constructor_results <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-09-07/constructor_results.csv') %>% 
  janitor::clean_names() %>% 
  rename(constructor_race_points = points,
         constructor_status = status)

constructor_standings <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-09-07/constructor_standings.csv') %>% 
  janitor::clean_names() %>% 
  rename(constructor_season_points = points,
         constructor_season_pos = position,
         constructor_season_pos_text = position_text,
         constructor_season_wins = wins)

races <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-09-07/races.csv') %>% 
  janitor::clean_names() %>% 
  rename(gp_name = name,
         gp_date = date,
         gp_url = url,
         gp_start_time = time)

constructors <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-09-07/constructors.csv') %>% 
  janitor::clean_names() %>% 
  rename(constructor_name = name,
         constructor_nationality = nationality,
         con_url = url)

driver_standings <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-09-07/driver_standings.csv') %>% 
  janitor::clean_names() %>% 
  rename(drive_season_position = position,
         drive_season_pos_text = position_text,
         drive_season_points = points,
         drive_season_wins = wins)
# some issues

drivers <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-09-07/drivers.csv') %>% 
  janitor::clean_names() %>% 
  rename(unique_driver_id = driver_ref,
         perm_drive_number = number,
         driver_initial_id = code,
         driver_url = url)

lap_times <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-09-07/lap_times.csv') %>% 
  janitor::clean_names() %>% 
  rename(race_pos = position,
         lap_time = time,
         lap_time_ms = milliseconds,
         lap_time_number = lap)

pit_stops <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-09-07/pit_stops.csv') %>% 
  janitor::clean_names() %>% 
  rename(stop_number = stop,
         pit_lap_number = lap,
         time_of_pit = time,
         pit_duration = duration,
         pit_dur_ms = milliseconds)

qualifying <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-09-07/qualifying.csv') %>% 
  janitor::clean_names() %>% 
  rename(driver_number = number,
         quali_pos = position)

seasons <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-09-07/seasons.csv') %>% 
  janitor::clean_names() %>% 
  rename(season_url = url)

results <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-09-07/results.csv') %>% 
  janitor::clean_names() %>% 
  rename(driver_number = number,
         final_driver_pos = position,
         final_driver_pos_text= position_text,
         race_result_points = points,
         completed_laps = laps,
         gp_finish_time = time,
         gp_finish_time_ms = milliseconds,
         fast_lap_rank = rank)
# some issues

status <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-09-07/status.csv') %>% 
  janitor::clean_names() %>% 
  rename(driver_status = status)

circuits <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-09-07/circuits.csv') %>% 
  janitor::clean_names() %>% 
  rename(circuit_url = url,
         long = lng)

# joins
df1 <- left_join(constructor_standings, constructor_results)
df2 <- left_join(df1, races)
df3 <- left_join(df2, constructors)
df4 <- left_join(df3, driver_standings)
df5 <- left_join(df4, drivers)
df6 <- left_join(df5, lap_times)
df7 <- left_join(df6, pit_stops)
df8 <- left_join(df7, qualifying)
df9 <- left_join(df8, seasons)
df10 <- left_join(df9, results)
df11 <- left_join(df10, status)

rm(df1)
rm(df2)
rm(df3)
rm(df4)
rm(df5)
rm(df6)
rm(df7)
rm(df8)
rm(df9)
rm(df10)
rm(constructor_standings)
rm(constructor_results)
rm(races)
rm(constructors)
rm(driver_standings)
rm(drivers)
rm(lap_times)
rm(pit_stops)
rm(qualifying)
rm(seasons)
rm(results)
rm(status)

f1_data <- left_join(df11, circuits) %>% 
  rowid_to_column()

rm(df11)
rm(circuits)

# write.csv(f1_data, "f1_data.csv")

set.seed(12282021)

f1_data1 <- f1_data %>% 
  sample_frac(.5)

f1_data2 <- anti_join(f1_data, f1_data1, by = 'rowid')

write.csv(f1_data1, "f1_data1.csv")
write.csv(f1_data2, "f1_data2.csv")
