library(tidyverse)
library(cmdstanr)
library(bayestestR)
library(bayesplot)
library(posterior)

set.seed(12345)
bern_dist <- function(prob_value)(
  rbinom(n = 1000, size = 1, prob = prob_value)
)

y <- tibble(
  y1 = bern_dist(prob = .8),
  y2 = bern_dist(prob = .74),
  y3 = bern_dist(prob = .88),
  y4 = bern_dist(prob = .90),
  y5 = bern_dist(prob = .64),
  y6 = bern_dist(prob = .61),
  y7 = bern_dist(prob = .79),
  y8 = bern_dist(prob = .89),
  y9 = bern_dist(prob = .81),
  y10 = bern_dist(prob = .54),
  y11 = bern_dist(prob = .60),
  y12 = bern_dist(prob = .46),
  y13 = bern_dist(prob = .37),
  y14 = bern_dist(prob = .3),
  y15 = bern_dist(prob = .65),
) |>
  rowid_to_column() |>
  rename(
    studentid = rowid
  )


q_matrix <- tibble(
  item_id = map_chr(1:15, ~paste0("y", .x)),
  att1 = c(1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0),
  att2 = c(0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0),
  att3 = c(0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1)
) 

stan_data <- list(
  J = nrow(y[, -1]), # Number of students/rows
  I = ncol(y[, -1]), # Number of items
  K = ncol(q_matrix[, -1]), #Number of latent attributes/skills
  Y = y[,-1], # Student responses on all items
  Q = q_matrix[,-1] # Items that measure each attribute
)

print(stan_data)
glimpse(stan_data)