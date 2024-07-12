library(tidyverse)
library(coda)
library(cmdstanr)
library(posterior)
library(bayesplot)

options(mc.cores = parallel::detectCores())
# rstan_options(auto_write = TRUE)

df <- measr::ecpe_data
q <- measr::ecpe_qmatrix

stat_sd <- stats::sd
select <- dplyr::select
post_rhat <- posterior::rhat
plot_rhat <- bayesplot::rhat


df_bn <- df |>
  mutate(
    across(
      -resp_id,
      ~as.numeric(.x)
    ),
    across(
      -resp_id,
      ~if_else(
        .x == 2, 1, 0
      )
    )
  )

glimpse(df_bn)


df_long <- df_bn |>
  mutate(
    resp_id = paste0("id", resp_id)
  ) |>
  pivot_longer(
    -resp_id,
    names_to = "item",
    values_to = "prob"
  ) |>
  pivot_wider(
    names_from = resp_id,
    values_from = prob
  )

df_long <- df_long |>
  select(-item)

t(df_long)


lat <- tibble(
  class = seq(1, 8, 1),
  morphosyntactic = c(0, 1, 0, 0, 1, 1, 0, 1),
  cohesive = c(0, 0, 1, 0, 1, 1, 1, 0),
  lexical = c(0, 0, 0, 1, 1, 0, 1, 1)
)

lat

lat <- lat |>
  mutate(
    class = paste0("class", class)
  ) |>
  pivot_longer(
    -class,
    names_to = "att",
    values_to = "value"
  ) |>
  pivot_wider(
    names_from = class,
    values_from = value
  )

bayestestR::distribution_beta(
  n = 1000,
  shape1 = 1,
  shape2 = .9
  ) |> 
as_tibble() |>  summarize(mean = mean(value))

bayestestR::distribution_uniform(
  n = 100, min = 0, max = 10
  ) |>
  as_tibble() |> summarize(mean = mean(value))

bayestestR::distribution_binom(
  n = 100,
  prob = .7
)

bayestestR::distribution_normal(
  n = 100,
  mean = 0,
  sd = 2
) |> 
as_tibble() |> 
  ggplot(
    aes(
      value
    )
  ) +
  geom_histogram(
    fill = "dodgerblue",
    color = "white"
  ) +
  theme_classic()

bayestestR::distribution_beta(
  n = 100,
  shape1 = 16.5,
  shape2 = 7
) |> 
as_tibble() |> 
  ggplot(
    aes(
      value
    )
  ) +
  geom_histogram(
    fill = "dodgerblue",
    color = "white"
  ) +
  theme_classic()

bayestestR::distribution_beta(
  n = 100,
  shape1 = 7,
  shape2 = 16.5
) |> 
as_tibble() |> 
  ggplot(
    aes(
      value
    )
  ) +
  geom_histogram(
    fill = "dodgerblue",
    color = "white"
  ) +
  theme_classic()

bayestestR::distribution_binom(
  n = 100,
  prob = .7
) |>
  as_tibble() |>
  ggplot(
    aes(
      value
    )
  ) +
  geom_histogram(
    fill = "dodgerblue",
    color = "white"
  ) +
  theme_classic()

stan_df <- list(
  J = nrow(df_bn), #number of students
  I = nrow(q), #number of items
  K = ncol(q[, -1]), # attributes/skills
  C = 2^ncol(q[, -1]), # attribute profile
  Y = t(df_long)
)

glimpse(stan_df)
