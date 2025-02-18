library(tidyverse)
library(cmdstanr)
library(posterior)
library(bayesplot)

react_table <- function(data){
  reactable::reactable(
    {{data}},
    sortable = TRUE,
    filterable = TRUE,
    searchable = TRUE
  )
}

library(edmdata)
data("items_ecpe")
data("qmatrix_ecpe")

ecpe <- items_ecpe |> as_tibble()
q <- qmatrix_ecpe |> as_tibble()

rm(items_ecpe, qmatrix_ecpe)
gc()

ecpe |> head()
q |> head()

q <- 
q |>
  rename(
    att1 = Trait1,
    att2 = Trait2,
    att3 = Trait3
  ) |>
  mutate(
    across(
      everything(),
      ~as.numeric(.x)
    )
  )

atts <- 3
att_combo <- rep(list(0:1), atts)
alpha <- expand.grid(att_combo)

alpha <- alpha |>
  rename(
    att1 = Var1,
    att2 = Var2,
    att3 = Var3
  ) |>
  mutate(
    class = seq(1:nrow(alpha)),
    .before = att1
  )

library(bnlearn)
library(Rgraphviz)

q1 <-
q |>
  mutate(
    across(
      everything(),
      ~as.factor(.x)
    )
  )

q1 <- as.data.frame(q1)

set.seed(12345)
hc_bn <- hc(q1)

graphviz.plot(hc_bn)

ecpe1 <-
  ecpe |> 
  slice_sample(
    prop = .3
  )

stan_list <- list(
  J = nrow(ecpe1),
  I = ncol(ecpe1),
  K = ncol(q),
  C = nrow(alpha[, -1]),
  X = ecpe1,
  Q = q,
  alpha = alpha[,-1]
)

rm(mod, fit)
gc()

set.seed(12345)
#mod <- cmdstan_model(here::here("educational_assessment_data/ecpe_bn_paper_model.stan"))
mod <- cmdstan_model(here::here("educational_assessment_data/structure_learn_model.stan"))

fit <- mod$sample(
  data = stan_list,
  seed = 12345,
  chains = 4,
  iter_warmup = 1000,
  iter_sampling = 1000,
  #max_treedepth = 10,
  #adapt_delta = .9,
  #step_size = .01,
  parallel_chains = parallel::detectCores() - 1
)

fit$diagnostic_summary()

bn_converge <- summarize_draws(fit$draws(), default_convergence_measures())
bn_measure <- summarize_draws(fit$draws(), default_summary_measures())

bn_converge |> arrange(desc(rhat)) |> head()

bn_measure |> mutate(across(-variable, ~round(.x, 2))) |> react_table()

bn_measure |> 
  mutate(across(-variable, ~round(.x, 2))) |> 
  filter(str_detect(variable, "prob_resp_attr")) |>
  react_table()

theta <- fit$draws(c("theta1", "theta2", "theta3")) |> as_draws_matrix()

mcmc_trace(theta)
mcmc_areas(theta)

y_rep <- fit$draws("x_rep") |> as_draws_matrix()
stu_resp_attr <- fit$draws("prob_resp_attr") |> as_draws_matrix()

mcmc_trace(exp(y_rep[,seq(1, 450, 30)])) +
  scale_y_continuous(limits = c(0, 1))

y |> react_table()

mcmc_intervals(exp(y_rep[,seq(1, 450, 30)]))

mcmc_areas(exp(y_rep[,seq(1, 450, 30)]))

ppc_intervals(
  y = y |> pull(y1) |> as.vector(),
  yrep = exp(y_rep[, 1:30])
) +
geom_hline(yintercept = .5, color = "black", linetype = 2) +
coord_flip()

actual_stu_resp_attr <- tibble(
  studentid = 1:nrow(y),
  att1 = runif(nrow(y), 0, 1),
  att2 = runif(nrow(y), 0, 1),
  att3 = runif(nrow(y), 0, 1)
) |>
  mutate(
    across(
      -studentid,
      ~if_else(.x > .5, 1, 0)
    )
  )

stu_resp_attr_mean <- stu_resp_attr |>
  as_tibble() |>
  summarize(
    across(
      everything(),
      ~mean(.x)
      )
  )

stu_resp_attr_class <- stu_resp_attr_mean |>
  mutate(
    across(
      everything(),
      ~if_else(.x > .5, 1, 0)
    )
  )

stu_resp_attr_class <- stu_resp_attr_class |>
  pivot_longer(
    everything()
  ) |>
  separate(
    name,
    into = c("stu", "att"),
    sep = ","
  ) |>
  mutate(
    stu = str_remove(stu, "\\["),
    att = str_remove(att, "\\]"),
    att = paste0("att", att),
    stu = str_remove(stu, "prob_resp_attr")
  ) |>
  pivot_wider(
    names_from = att,
    values_from = value
  )

map2(
  stu_resp_attr_class[,2:4],
  actual_stu_resp_attr[,2:4],
  ~table(.x, .y)
)

map2(
 stu_resp_attr_class[,2:4],
  actual_stu_resp_attr[,2:4],
  ~prop.table(
    table(.x, .y)
  )
)

stu_resp_attr_long <- stu_resp_attr_class |>
  pivot_longer(-stu)

actual_stu_resp_attr_long <- actual_stu_resp_attr |>
  pivot_longer(-studentid)

accuracy_att <- mean(stu_resp_attr_long$value == actual_stu_resp_attr_long$value)
accuracy_att