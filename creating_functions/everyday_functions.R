# Model Diagnostics
library(tidyverse)
theme_set(theme_minimal())


linearity <- function(data, x, y, se = FALSE){
  library(dplyr)
  library(ggplot2)
  
  line_plot <- {{data}} %>% 
    ggplot(aes({{x}}, {{y}})) +
      geom_point(color = 'gray70', alpha = .7) + 
      geom_smooth(color = 'black', se = {{se}}) +
      geom_smooth(method = 'lm', se = {{se}}) +
    labs(title = 'Scatterplot to Assess Linearity')
  
  return(line_plot)
}

linearity(mtcars, cyl, disp)

# residual_view <- function(data, se = FALSE, x, y){
#     m1 <- lm({{y}} ~ {{x}}, data = {{data}})
# 
#   library(ggplot2)
#   
#   fort <- fortify(m1)
#   
#   res_plot <- ggplot(fort, aes(.fitted, .resid)) +
#     geom_point(color = 'gray70', alpha = .7) + 
#     geom_smooth(method = 'lm', color = 'black', se = {{se}}) +
#     labs(title = 'Plot of Residual Errors')
#   
#   return(res_plot)
# }

names(mtcars)

# residual_view(mtcars, mpg, gear)




# normality_view <- function(data, x, y, alpha = .7, bins = 15, ...){
#   library(dplyr)
#   library(ggplot2)
#   
#   m1 <- lm({{y}} ~ {{x}} + ..., data = {{data}})
#   
#   univariate_histogram <- {{data}} %>% 
#     ggplot(aes({{x}})) +
#     geom_histogram(alpha = {{alpha}}, bins = {{bins}}) +
#     labs(title = 'Histogram to Assess Normality')
# 
#   fort <- fortify(m1)
#   
#   qq_plot_model <- ggplot(m1, aes(sample = .stdresid)) +
#     geom_qq(color = 'gray70', size = 4) +
#     stat_qq_line()
#   
#   library(gridExtra)
#   grid.arrange(univariate_histogram, qq_plot_model, nrow = 2)
# }

# normality_view(mtcars, mpg, gear)
