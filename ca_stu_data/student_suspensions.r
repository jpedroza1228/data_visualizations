library(tidyverse)
library(reactable)

# data dictionary
# https://www.cde.ca.gov/ds/ad/fssd.asp
 
# reading in data
# sus_list <- map(
#   c(
#     "https://www3.cde.ca.gov/demo-downloads/discipline/suspension12.txt",
#     "https://www3.cde.ca.gov/demo-downloads/discipline/suspension13.txt",
#     "https://www3.cde.ca.gov/demo-downloads/discipline/suspension14.txt",
#     "https://www3.cde.ca.gov/demo-downloads/discipline/suspension15.txt",
#     "https://www3.cde.ca.gov/demo-downloads/discipline/suspension16.txt",
#     "https://www3.cde.ca.gov/demo-downloads/discipline/suspension17.txt",
#     "https://www3.cde.ca.gov/demo-downloads/discipline/suspension18.txt",
#     "https://www3.cde.ca.gov/demo-downloads/discipline/suspension19.txt",
#     "https://www3.cde.ca.gov/demo-downloads/discipline/suspension20.txt",
#     "https://www3.cde.ca.gov/demo-downloads/discipline/suspension21.txt",
#     "https://www3.cde.ca.gov/demo-downloads/discipline/suspension22-v2.txt",
#     "https://www3.cde.ca.gov/demo-downloads/discipline/suspension23.txt"
#   ),
#   ~read_delim(.x)
# ) 

# map2(
#   sus_list,
#   12:23,
#   ~write_csv(
#     .x,
#     glue::glue(
#       "ca_stu_data/suspension_data_20{.y}.csv"
#     )
#     )
# )

# combo <- map_dfr(sus_list, ~.x)


read_func <- function(year){
  readr::read_csv(
    glue::glue(
      "ca_stu_data/suspension_data_20{year}.csv"
    )
  )
}

combo <- map_dfr(
  12:23,
  ~read_func(.x)
)

combo <- combo |>
  janitor::clean_names() |>
  mutate(
    across(
      c(
        county_code,
        cumulative_enrollment,
        matches("suspen"),
        cumulative_enrollment_2
        ),
        ~as.numeric(.x)
    )
  )


combo |> glimpse()

county <- combo |>
  group_by(
    academic_year,
    county_name,
    reporting_category
    ) |>
    reframe(
      avg_suspend = mean(total_suspensions, na.rm = TRUE),
      avg_enroll = mean(cumulative_enrollment, na.rm = TRUE),
      suspend_per_total_enroll = total_suspensions/cumulative_enrollment
    )

county |>
  filter(
    reporting_category == "SD" &
    county_name != "State"
  ) |>
  group_by(academic_year, county_name) |>
  summarize(
    avg_sus_prop = mean(suspend_per_total_enroll, na.rm = TRUE)
  ) |>
  ungroup() |>
  drop_na() |>
  ggplot(
    aes(
      academic_year,
      avg_sus_prop,
      group = county_name,
      color = county_name
    )
  ) +
  geom_line() +
  theme_light() +
  theme(legend.position = "none")


county |>
  filter(
    reporting_category == "SD" &
    county_name != "State"
  ) |>
  group_by(academic_year, county_name) |>
  summarize(
    avg_county_sus = mean(avg_suspend, na.rm = TRUE)
  ) |>
  ungroup() |>
  drop_na() |>
  ggplot(
    aes(
      academic_year,
      avg_county_sus,
      group = county_name,
      color = county_name
    )
  ) +
  geom_line() +
  ggrepel::geom_text_repel(
    data = county |>
  filter(
    reporting_category == "SD" &
    county_name != "State"
  ) |>
  group_by(academic_year, county_name) |>
  summarize(
    avg_county_sus = mean(avg_suspend, na.rm = TRUE)
  ) |>
  ungroup() |>
  drop_na() |>
  filter(
    academic_year == "2012-13"
  ),
  aes(
    label = county_name
  )
  ) +
  theme_light() +
  theme(legend.position = "none")


county |>
  filter(
    reporting_category == "SD" &
    county_name != "State"
  ) |>
  pivot_longer(
    c(
      avg_suspend,
      avg_enroll
    )
  ) |>
  group_by(academic_year, county_name, name) |>
  summarize(
    county_avg = mean(value, na.rm = TRUE)
  ) |>
  ungroup() |>
  drop_na() |>
  ggplot(
    aes(
      academic_year,
      county_avg,
      group = county_name,
      color = county_name
    )
  ) +
  geom_line() +
  ggrepel::geom_text_repel(
    data = county |>
  filter(
    reporting_category == "SD" &
    county_name != "State"
  ) |>
  pivot_longer(
    c(
      avg_suspend,
      avg_enroll
    )
  ) |>
  group_by(academic_year, county_name, name) |>
  summarize(
    county_avg = mean(value, na.rm = TRUE)
  ) |>
  ungroup() |>
  drop_na() |>
  filter(
    academic_year == "2012-13"
  ),
  aes(
    label = county_name
  )
  ) +
  facet_wrap(
    vars(
      name
    ),
    scales = "free",
    ncol = 1
  ) +
  theme_light() +
  theme(legend.position = "none")
