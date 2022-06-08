# package functions

library(devtools)
library(roxygen2)
# install.packages("roxygen2")

create("reg.diagnostics")


# color palettes
car_color_palettes <- list(datsun1 = c("#6a1f25", "#387448", "#7fb7b7", "#669b3e", "#ddca72", "#7f5d47", "#d74122"),
  datsun2 = c("#d72d2b", "#386350", "#507b8b", "#a86428", "#b23a2d"),
  datsun3 = c("#5b6242", "#2c6195", "#c0cc56", "#b23a2d"))

car_color <- function(name, n) {
  
  pal <- car_color_palettes[[name]]
  if (is.null(pal))
    stop("Palette not found.")
  
  if (missing(n)) {
    n <- length(pal)
  }
  
  if (n > length(pal)) {
    stop("Number of requested colors greater than what palette can offer")
  }
  
  out <- pal[1:n]
  
  structure(out, class = "palette", name = name)
}

car_color_palettes[["datsun3"]]

library(tidyverse)
mtcars %>% 
  ggplot(aes(hp, mpg)) +
  geom_point(aes(color = as.factor(cyl)), size = 4) +
  theme_minimal() +
  scale_color_manual(values = car_color("datsun1"))


residual_view <- function(data, x, y, se = c(TRUE, FALSE), alpha, size, line_size, color, line_color){
  
  y <- deparse(substitute(y))
  
  ivs <- paste(x, collapse = " + ")
  my_formula <- as.formula(paste(y, "~", ivs))
  model <- lm(my_formula, data = data)
  
  library(ggplot2)
  
  fort <- fortify(model)
  
  res_plot <- ggplot(fort, aes(.fitted, .resid)) +
    geom_point(color = {{color}}, alpha = {{alpha}}, size = {{size}}) +
    geom_smooth(method = "lm", color = {{line_color}}, se = {{se}}, size = {{line_size}}) +
    labs(title = "Plot of Residual Errors") +
    theme_minimal()
  
  qq_plot_model <- ggplot(model, aes(sample = .stdresid)) +
    geom_qq(color = {{color}}, size = {{size}}, alpha = {{alpha}}) +
    stat_qq_line(size = {{line_size}}, color = {{line_color}}) + 
    theme_minimal()
  
  message("Make sure there is not a pattern in your Residuals")
  message("Include multiple predictors using c()")
  message("see website for example of multiple predictors")
  
  gridExtra::grid.arrange(res_plot, qq_plot_model, nrow = 2)
  
}

predictors <- c("hp", "carb", "gear")
residual_view(data = mtcars, x = predictors, y = mpg, se = FALSE,
              alpha = .3, size = 2, line_size = 1,
              color = "blue", line_color = "dodgerblue")


uni_bi_dist <- function(data, x, y, alpha, bins, fill, color, loess_color, line_color, se = c(TRUE, FALSE), size){
  library(dplyr)
  library(ggplot2)

  x_histogram <- {{data}} %>%
    ggplot(aes({{x}})) +
    geom_histogram(color = 'white', fill = {{fill}}, alpha = {{alpha}}, bins = {{bins}}) +
    labs(title = 'Histogram to Assess Normality of X') + 
    theme_minimal()
  
  y_histogram <- {{data}} %>%
    ggplot(aes({{y}})) +
    geom_histogram(color = 'white', fill = {{fill}}, alpha = {{alpha}}, bins = {{bins}}) +
    labs(title = 'Histogram to Assess Normality of Y') + 
    theme_minimal()
  
  line_plot <- ggplot({{data}}, aes({{x}}, {{y}})) +
    geom_point(color = {{color}}, alpha = {{alpha}}) + 
    geom_smooth(color = {{loess_color}}, se = {{se}}, size = {{size}}) +
    geom_smooth(method = 'lm', color = {{line_color}}, se = {{se}}, size = {{size}}) +
    labs(title = 'Scatterplot Between X and Y') +
    theme_minimal()

  gridExtra::grid.arrange(x_histogram, y_histogram, line_plot, nrow = 3)
}

uni_bi_dist(data = mtcars, x = hp, y = mpg,
               alpha = .8, bins = 15,
              fill = "dodgerblue", color = "black",
              loess_color = "darkgreen", line_color = "red", 
              se = FALSE, size = 1.25) 


# working on
freq_bar <- function(data, x, fill, total){
  
  bar_plot <- {{data}} %>% 
    ggplot(aes({{x}})) +
    geom_bar(color = "white", fill = {{fill}}) +
    theme_minimal()
  
  summary <- {{data}} %>% 
      group_by({{x}}) %>% 
      summarize(n = n(),
                prop = n/total,
                percent = prop*100)
    
  table <- reactable::reactable(summary)
  
  return(list(bar_plot, table))

}

freq_bar(mtcars, as.factor(cyl), fill = "dodgerblue", total = 32)[[1]]
freq_bar(mtcars, as.factor(cyl), fill = "dodgerblue", total = 32)[[2]]



glm_assump <- function(data, x, y, alpha, se = c(TRUE, FALSE), size, line_size, color, loess_color, line_color){
  library(dplyr)
  library(tidyr)
  
  y <- deparse(substitute(y))
  
  ivs <- paste(x, collapse = " + ")
  my_formula <- as.formula(paste(y, "~", ivs))
  model <- glm(my_formula, data = data, family = "binomial")
  
  plot_data <- predict(model, type = "response")
  model_predictors <- colnames(data)
  
  data <- {{data}} %>% 
    drop_na({{x}}, {{y}}) %>% 
    dplyr::select({{x}}, {{y}}) %>% 
    mutate(logit = log(plot_data/(1 - plot_data))) %>% 
    gather(key = "predict_variables", value = "predictor_values", -logit, -{{y}})
  
  assumption <- data %>% 
                  ggplot(aes(logit, predictor_values)) +
                  geom_point(size = {{size}}, alpha = {{alpha}}, color = {{color}}) +
                  geom_smooth(method = "loess", se = {{se}}, size = {{line_size}}, color = {{loess_color}}) +
                  geom_smooth(method = "lm", se = {{se}}, size = {{line_size}}, color = {{line_color}}) +
                  theme_classic() + 
                  facet_wrap(~predict_variables, scales = "free_y")
  
  model_data <- broom::augment(model) %>% 
    mutate(index = 1:n())
  
  outlier <- model_data %>% 
              ggplot(aes(index, .std.resid)) +
              geom_point(aes(color = {{y}}), alpha = {{alpha}}) +
              theme_classic()
  
  gridExtra::grid.arrange(assumption, outlier, nrow = 2)
}

glm_assump(data = mtcars, predictors, vs, alpha = .5, se = FALSE,
           size = 2, line_size = 1.25,
           color = "dodgerblue", loess_color = "red", line_color = "blue")


# this function needs work
ordinal_assump <- function(data, x, y){
  library(dplyr)
  library(tidyr)
  
  warning("This requires downloading brant package")
  
  y <- deparse(substitute(y))
  
  ivs <- paste(x, collapse = " + ")
  my_formula <- as.formula(paste(y, "~", ivs))
  model <- MASS::polr(my_formula, data = data, Hess = TRUE)

  (freq_table <- coef(summary(model)))
  
  p <- pnorm(abs(freq_table[, "t value"]), lower.tail = FALSE) * 2
  
  (table <- cbind(freq_table, "p value" = p))
  
  model_vif <- car::vif(model)
  
  brant_model <- brant::brant(model)
  
  list(table, model_vif, brant_model)
}

# county_pred <- c("rec_resource", "rural", "inactivity", "county_crime")
# ordinal_assump(data, county_pred, as.factor(state))[[1]]
# ordinal_assump(data, county_pred, as.factor(state))[[2]]
# ordinal_assump(data, county_pred, as.factor(state))[[3]]

data <- read_csv("https://raw.githubusercontent.com/jpedroza1228/dissertation/master/analytic_data/county_20.csv")
