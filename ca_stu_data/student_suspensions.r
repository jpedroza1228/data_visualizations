# https://www.cde.ca.gov/ds/ad/fssd.asp

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
      "RB", "RI", "RA", "RH", "RD", "RP", "RT", "RW",
      "SD"
    ) &
    charter_yn == "No" &
    aggregate_level == "C"
  )

# s = School, D = District, C = County, T = state

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


# this is only for using aggregate_level == "S" for school or "D" for district
# all <- all |>
#   group_by(
#     academic_year 
#   ) |>
#   mutate(
#     school_num = row_number()
#   ) |>
#   ungroup()

all <- all |> 
  select(
  county_name,
  year,
  suspension_count_defiance_only,
  reporting_category
)

all_drop <- all |> 
  mutate(
    asian = case_when(
      reporting_category == "RA" ~ 1,
      TRUE ~ 0
    ),
    black = case_when(
      reporting_category == "RB" ~ 1,
      TRUE ~ 0
    ),
    not_reported = case_when(
      reporting_category == "RD" ~ 1,
      TRUE ~ 0
    ),
    latino = case_when(
      reporting_category == "RH" ~ 1,
      TRUE ~ 0
    ),
    american_indian = case_when(
      reporting_category == "RI" ~ 1,
      TRUE ~ 0
    ),
    pacific_islander = case_when(
      reporting_category == "RP" ~ 1,
      TRUE ~ 0
    ),
    two_or_more_races = case_when(
      reporting_category == "RT" ~ 1,
      TRUE ~ 0
    ),
    white = case_when(
      reporting_category == "RW" ~ 1,
      TRUE ~ 0
    ),
    stu_w_dis = case_when(
      reporting_category == "SD" ~ 1,
      TRUE ~ 0
    )
  ) |>
    select(
      -reporting_category
    )

all_drop <- all_drop |> filter(year >= 21)
all_drop_race <- all_drop |> drop_na(asian, black, latino, american_indian, pacific_islander, two_or_more_races, white, not_reported)
all_drop_sd <- all_drop |> drop_na(stu_w_dis)

# fit weights for size of district

freq_fit <- lme4::lmer(
  suspension_count_defiance_only ~ asian*year + black*year + latino*year + american_indian*year + pacific_islander*year + two_or_more_races*year + white*year  + (1 | county_name),
  data = all_drop_race,
  REML = FALSE
)

icc_lvl2 <- function(data){
  between <- data[1, 4]
  total <- data[1, 4] + data[2, 4]

  between/total
} 

summary(freq_fit)
lme4::ranef(freq_fit)

icc_df <- freq_fit |> lme4::VarCorr() |> as_tibble()
icc_lvl2(icc_df)
# 23.1% of variation in suspension counts due to defiance is from county differences

between_county <- lme4::ranef(freq_fit, condVar = TRUE)
between_county <- as_tibble(between_county)

between_county |>
  ggplot(
    aes(
      fct_reorder(
        grp,
        condval
      ),
      condval
    )
  ) +
  geom_errorbar(
    aes(
      ymin = condval + qnorm(0.025)*condsd,
      ymax = condval + qnorm(0.975)*condsd
      )
    ) +
  geom_point(
    aes(
      color = grp
    ),
    size = 4
  ) +
  coord_flip() +
  theme(
    legend.position = "none"
  )


library(cmdstanr)
library(brms)

fit <- brm(
  suspension_count_defiance_only ~ asian*year + black*year + latino*year + american_indian*year + pacific_islander*year + two_or_more_races*year + white*year  + (1 | county_name),
  data = all_drop_race,
  family = gaussian(),
  prior = c(
    set_prior("normal(0, 1)", class = "b"),
    set_prior("normal(0, 1)", class = "sd")
  ),
  cores = parallel::detectCores(),
  control = list(adapt_delta = .95),
  seed = 12345,
  backend = "cmdstanr"
)


library(broom)
library(broom.mixed)
library(tidybayes)

summary(fit, waic = TRUE)
tidy(fit)
ranef(fit)$county_name |>
  as_tibble(rownames = "county") |>
  ggplot(
    aes(
      fct_reorder(
        county,
        Estimate.Intercept
      ),
      Estimate.Intercept
    )
  ) +
  geom_errorbar(
    aes(
      ymin = Estimate.Intercept + Q2.5.Intercept,
      ymax = Estimate.Intercept + Q97.5.Intercept
      )
    ) +
  geom_point(
    aes(
      color = county
    ),
    size = 4
  ) +
  coord_flip() +
  theme(
    legend.position = "none"
  )



# students with disabilities
freq_fit_sd <- lme4::lmer(
  suspension_count_defiance_only ~ stu_w_dis*year  + (1 | county_name),
  data = all_drop_sd,
  REML = FALSE
)

summary(freq_fit_sd)
lme4::ranef(freq_fit_sd)

icc_df2 <- freq_fit_sd |> lme4::VarCorr() |> as_tibble()
icc_lvl2(icc_df2)
# 22.9% of variation in suspension counts due to defiance is from county differences

between_county_sd <- lme4::ranef(freq_fit_sd, condVar = TRUE)
between_county_sd <- as_tibble(between_county_sd)

between_county_sd |>
  ggplot(
    aes(
      fct_reorder(
        grp,
        condval
      ),
      condval
    )
  ) +
  geom_errorbar(
    aes(
      ymin = condval + qnorm(0.025)*condsd,
      ymax = condval + qnorm(0.975)*condsd
      )
    ) +
  geom_point(
    aes(
      color = grp
    ),
    size = 4
  ) +
  coord_flip() +
  theme(
    legend.position = "none"
  )


fit_sd <- brm(
  suspension_count_defiance_only ~ stu_w_dis*year  + (1 | county_name),
  data = all_drop_race,
  family = gaussian(),
  prior = c(
    set_prior("normal(0, 10)", class = "b"),
    set_prior("normal(0, 10)", class = "sd")
  ),
  cores = parallel::detectCores(),
  control = list(adapt_delta = .95),
  seed = 12345,
  backend = "cmdstanr"
)

summary(fit_sd, waic = TRUE)
tidy(fit_sd)
ranef(fit_sd)$county_name |>
  as_tibble(rownames = "county") |>
  ggplot(
    aes(
      fct_reorder(
        county,
        Estimate.Intercept
      ),
      Estimate.Intercept
    )
  ) +
  geom_errorbar(
    aes(
      ymin = Q2.5.Intercept,
      ymax = Q97.5.Intercept
      )
    ) +
  geom_point(
    aes(
      color = county
    ),
    size = 4
  ) +
  coord_flip() +
  theme(
    legend.position = "none"
  )
