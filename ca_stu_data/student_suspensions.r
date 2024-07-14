library(tidyverse)
library(reactable)
library(brms)
library(tidybayes)

theme_set(theme_light())

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

combo |> count(aggregate_level)

all <- combo |>
  filter(
    reporting_category %in% c(
      "RB", "RI", "RA", "RF", "RH", "RD", "RP", "RT", "RW"
    ) &
    charter_yn == "No" &
    county_name != "State" &
    aggregate_level == "C"
  )


all |> glimpse()

all |> count(academic_year)
all |> count(county_name)

all <- all |>
  mutate(
    year = case_when(
      academic_year == "2011-12" ~ 11,
      academic_year == "2012-13" ~ 12,
      academic_year == "2013-14" ~ 13,
      academic_year == "2014-15" ~ 14,
      academic_year == "2015-16" ~ 15,
      academic_year == "2016-17" ~ 16,
      academic_year == "2017-18" ~ 17,
      academic_year == "2018-19" ~ 18,
      academic_year == "2019-20" ~ 19,
      academic_year == "2020-21" ~ 20,
      academic_year == "2021-22" ~ 21,
      academic_year == "2022-23" ~ 22
    ),
    county_name = as.factor(county_name),
    county_name = relevel(county_name, ref = "Los Angeles")
  )

all |>
  filter(
    county_name == "Los Angeles" &
    year == 11
  ) |>
  select(aggregate_level, county_name, year, suspension_count_defiance_only, reporting_category) |>
  reactable::reactable(
    filterable = TRUE,
    sortable = TRUE,
    searchable = TRUE
  )

# this is only for using aggregate_level == "S" for school or "D" for district
# all <- all |>
#   group_by(
#     academic_year 
#   ) |>
#   mutate(
#     school_num = row_number()
#   ) |>
#   ungroup()

all <- all |> select(
  county_name,
  year,
  suspension_count_defiance_only,
  reporting_category
)

all_drop <- all |> drop_na()

all_drop |> group_by(year) |> count() |> ungroup() |> mutate(p = n/sum(n)*100)

all_train <- all_drop |> filter(year <= 19) 
all_test <- all_drop |> filter(year > 19)

summary(
  lme4::lmer(
  suspension_count_defiance_only ~ year*reporting_category + county_name + (1 | county_name),
  data = all_train
  )
)

library(cmdstanr)

fit <- brm(
  suspension_count_defiance_only ~ year*reporting_category + county_name + (1 | county_name),
  data = all_train,
  family = gaussian(),
  chains = 4,
  iter = 2000,
  warmup = 2000,
  cores = parallel::detectCores() - 1,
  seed = 12345,
  backend = "cmdstanr"
)

library(broom)
library(broom.mixed)
library(tidybayes)

summary(fit)
loo(fit)
