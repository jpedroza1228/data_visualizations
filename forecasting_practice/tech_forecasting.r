library(tidyverse)
library(tidymodels)
library(modeltime)
library(forecast)
library(timetk)

parallel_start(parallel::detectCores(), .method = 'parallel')
theme_set(theme_light())


link <- "https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-02-07/big_tech_stock_prices.csv"

link2 <- "https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-02-07/big_tech_companies.csv"

data <- map(
  c(link, link2),
  ~read_csv(.x)
)

data <- left_join(
  data[[1]],
  data[[2]],
  by = "stock_symbol"
)

glimpse(data)

sub <- data |>
  filter(stock_symbol == "GOOGL")

sub |>
  tail()

sub |>
  ggplot(
    aes(
      date,
      open,
      group = 1
    )
  ) +
  geom_line(
    color = "dodgerblue"
  ) +
  ggrepel::geom_text_repel(
    data = data |>
      filter(
        date == "2022-12-29" &
        stock_symbol == "GOOGL"
      ),
    aes(
      label = company
    ),
    nudge_y = -1.5,
    color = "black",
    size = 6
  )

sub |>
  tk_anomaly_diagnostics(date, open) |>
  ggplot(aes(date, observed)) + 
  geom_line() + 
  geom_point(aes(color = anomaly))

sub |>
  tk_anomaly_diagnostics(date, open)

sub |>
  plot_time_series(
    date,
    open
  )
