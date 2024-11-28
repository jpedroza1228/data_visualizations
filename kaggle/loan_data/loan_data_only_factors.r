library(tidyverse)

data <- read_csv("https://raw.githubusercontent.com/jpedroza1228/JonathanAPedroza.com/refs/heads/main/posts/2024-11-15-bayes-net-loan-approval/loan_data.csv")

data |> glimpse()

data |>
  select(
    where(
      is.numeric
    )
  ) |>
  map(
    ~ggplot(
      data = data |>
  select(
    where(
      is.numeric
    )
  ),
  aes(
    .x
  )
    ) +
    geom_histogram()
  )

loan_cat <- data |> 
  mutate(
    credit_score_brack = case_when(
      credit_score > 800 ~ "excellent",
      credit_score < 800 & credit_score >= 740 ~ "very_good",
      credit_score < 740 & credit_score >= 670 ~ "good",
      credit_score < 670 & credit_score >= 580 ~ "fair",
      credit_score < 580 ~ "poor"
    ),
    cred_hist_length_brack = case_when(
      cb_person_cred_hist_length >= 18 ~ "18+",
      TRUE ~ as.character(cb_person_cred_hist_length)
    ),
    loan_percent_income_brack = case_when(
      loan_percent_income >= .3 ~ ".3+",
      loan_percent_income < .3 & loan_percent_income >= .2 ~ ".2 - .29",
      loan_percent_income < .2 & loan_percent_income >= .1 ~ ".1 - .19",
      loan_percent_income < .1 ~ "0-.09"
    ),
    loan_int_rate_brack = case_when(
      loan_int_rate < 10 ~ "<10",
      loan_int_rate >= 10 & loan_int_rate <= 15 ~ "10 - 15",
      loan_int_rate > 15 ~ "15+"
    ),
    loan_amnt_brack = case_when(
      loan_amnt < 5000 ~ "<5k",
      loan_amnt >= 5000 & loan_amnt <= 9999 ~ "5k - 9.99k",
      loan_amnt >= 10000 & loan_amnt <= 15000 ~ "10k - 14.99k",
      loan_amnt >= 15000 & loan_amnt <= 20000 ~ "15k - 19.99k",
      loan_amnt >= 20000 ~ "20k+"
    ),
    person_income_brack = case_when(
      person_income < 30000 ~ "<30k",
      person_income >= 30000 & person_income < 50000 ~ "30k - 49,999",
      person_income >= 50000 & person_income < 70000 ~ "50k - 69,999",
      person_income >= 70000 & person_income < 90000 ~ "70k - 89,999",
      person_income >= 90000  ~ "90k"
    ),
    person_age_brack = case_when(
      person_age < 30 ~ "<30",
      person_age >= 30 & person_age < 40 ~ "30-39",
      person_age >= 40  ~ "40+"
    )
  ) |>
  drop_na(
    person_age_brack
  ) |>
  filter(
    person_home_ownership != "OTHER"
  ) |>
  select(
    matches(      
      "_brack$"
    ),
    person_gender,
    person_education,
    person_home_ownership,
    loan_intent,
    previous_loan_defaults_on_file,
    loan_status
  )

loan_cat <- loan_cat |>
  mutate(
    across(
      everything(),
      ~as.factor(.x)
    )
  )

# write_csv(
#   loan_cat,
#   here::here("kaggle", "loan_data", "loan_data_only_cat.csv")
# )
