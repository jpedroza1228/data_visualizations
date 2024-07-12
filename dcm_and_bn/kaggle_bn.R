library(tidyverse)
library(here)
library(Rgraphviz)
library(bnlearn)

train <- read_rds("dcm_and_bn/kaggle_bn_data/train.rds")

train_labels <- read_csv("dcm_and_bn/kaggle_bn_data/train_labels.csv")

# rm(train)

glimpse(train)
glimpse(train_labels)

train_labels <- train_labels |>
  separate(
    session_id,
    into = c(
      "session_id", "question"
    ),
    sep = "_"
  )

train_labels <- train_labels |>
  distinct(
    session_id,
    .keep_all = TRUE
  )

train_labels <- train_labels |>
  mutate(
    session_id = as.numeric(session_id)
  )

joined <- inner_join(
  train,
  train_labels
)

inspectdf::inspect_na(joined) |>
  inspectdf::show_plot()


set.seed(12345)
j <- joined |>
    slice_sample(
      prop = .10
    )

j <- j |>
  mutate(
    across(
      c(
      index,
      event_name,
      name,
      level,
      room_fqid,
      fullscreen,
      hq,
      music,
      level_group,
      correct
      ),
    ~as.factor(.x)
    )
  ) |>
  select_if(
    is.factor
  )

names(j)

count(j, index)

inspectdf::inspect_na(j)

j <- as.data.frame(j)

set.seed(12345)
hc_bn <- hc(
  j
)

graphviz.plot(hc_bn)
