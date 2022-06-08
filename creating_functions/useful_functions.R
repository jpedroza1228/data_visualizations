# Model Diagnostics
library(tidyverse)
theme_set(theme_minimal())

#www.colorhexa.com
#https://grasshoppermouse.github.io/2017/10/18/put-your-data-in-an-r-package/

datsun_colors <- tibble::tibble(color_names = c('maroon', 'pagoda_red', 'cherry_red',
                                        'red', 'mexican_red', 'red_persimmon', 'light_maroon',
                                        'green', 'kasumi_green', 'leaf_green', 'dark_green',
                                        'olive_green', 'cactus_green', 'dragon_green',
                                        'kelly_green', 'avocado_green', 'chartreuse_green',
                                        'gray_green', 'turquoise', 'mount_fuji_blue',
                                        'sky_blue', 'nissan_blue', 'blue', 'dark_blue',
                                        'toba_aqua', 'true_blue', 'island_turquoise',
                                        'seaview_aqua', 'yellow', 'bamboo_tan',
                                        'houston_yellow', 'cream_ivory', 'gold',
                                        'california_yellow', 'sahara_gold',
                                        'mellow_yellow', 'yellow_lime', 'eggshell',
                                        'cream', 'desert_gold', 'sunshine_yellow',
                                        'dark_golden_brown', 'orange', 'sunbird_orange',
                                        'sunset_orange', 'mexican_orange'),
                        hex_color_code = c('#5e2228', '#d22a2c', '#9b2a30', '#c80010',
                                           '#6a1f25', '#d72d2b', '#5b2c2f', '#387448',
                                           '#c0c5ba', '#888980', '#344039', '#4f4c3c',
                                           '#807f67', '#386350', '#387448', '#5b6242',
                                           '#669b3e', '#808572', '#7fb7b7', '#53636b',
                                           '#85b3c1', '#507b8b', '#2c6195', '#4b5f71',
                                           '#4b5f71', '#3f6698', '#26657f', '#398b98',
                                           '#ddca72', '#c3b37d', '#d3bb95', '#c4bb9b',
                                           '#a26540',
                                           '#dbd5a5', '#a86428', '#c7a860', '#c0cc56', 
                                           '#e5e1d4', '#d3bb95', '#a26540', '#ead568',
                                           '#7f5d47', '#b23a2d', '#c33431', 'd74122',
                                           '#b23a2d')
                        )

datsun_color_palettes <- list(
	datsun_reds = c('#5e2228', '#d22a2c', '#9b2a30', '#c80010', '#6a1f25', '#d72d2b', '#5b2c2f'),##831924
	datsun_greens = c('#808572', '#455e56', '#4c5249', '#2d3b35', '#387448', '#4b5d59',
	                  '#435043', '#467f4d', '#5b6242', '#3b4640', '#344039', '#386350', '#4f4c3c',
	                  '#387448', '#669b3e'),
	datsun_blues = c('#7fb7b7', '#53636b', '#85b3c1', '#a0b2ba', '#507b8b', '#385a72', '#2c6195',
	                 '#30394c', '#3f6698', '#26657f', '#398b98'),
	datsun_yellows = c('#ddca72', '#d3bb95', '#a26347', '#dbd5a5', '#c7a860', '#c0cc56', '#ead568'),
	datsun_oranges = c('#b23a2d', '#ea5227', '#c33431', 'd74122', '#b23a2d')
)


# color palettes
car_color_palettes <- list(
  datsun = c(),
  porsche = c(),
  autopia = c(),
  modern_autopia = c("#515050", "#B6B7BC", "#80D0F4", "#112516", "#6B1111", "#18407C", "#141415")
)

car_palette <- function(name, n, type = c("discrete", "continuous")) {
  type <- match.arg(type)
  
  pal <- car_color_palettes$name
  if (is.null(pal))
    stop("Palette not found.")
  
  if (missing(n)) {
    n <- length(pal)
  }
  
  if (type == "discrete" && n > length(pal)) {
    stop("Number of requested colors greater than what palette can offer")
  }
  
  out <- switch(type,
                continuous = grDevices::colorRampPalette(pal)(n),
                discrete = pal[1:n]
  )
  structure(out, class = "palette", name = name)
}

car_palette('datsun_reds', 4)
wesanderson::wes_palettes

datsun_color_palettes

autopia_color_palettes <- list(
	fifties = c('#FFF6B7','#8CD4C9', '#F7471A', '#D1E6F2', '#23A1E3', '#AA1818', '#601a35'),
	early_2000s = c('purple/lavender', 'bronze', 'soft yellow', 'metallic green', 'silver/bronze',
					'light blue','forest green', 'brown', 'soft red', 'bright yellow',
					'gold', 'silver', 'dark purple', 'royal blue')
)


not_working1 <- function(data, x, y, se = c(TRUE, FALSE), size){
  library(dplyr)
  library(ggplot2)
  
  warning("Make sure X is numeric to show linear association")
  warning("Requires Tidyverse & lm.beta packages installed")
  
  line_plot <- ggplot({{data}}, aes({{x}}, {{y}})) +
    geom_point(color = 'gray70', alpha = .5) + 
    geom_smooth(se = {{se}}, size = {{size}}) +
    geom_smooth(method = 'lm', color = 'black', se = {{se}}, size = {{size}}) +
    labs(title = 'Scatterplot to Assess Linearity') +
    theme_minimal()
  
  model <- lm({{y}} ~ {{x}}, data = {{data}})
  standardized <- lm.beta::lm.beta(model)
  
  slope <- standardized$standardized.coefficients[[2]]
  
  return(list(line_plot, slope))
}


# may need to be two functions
residual_view <- function(data, x, y, se = c(TRUE, FALSE), size){

  ivs <- paste(x, collapse = " + ")
  my_formula <- as.formula(paste(y, '~', ivs))
  model <- lm(my_formula, data = data)

  library(ggplot2)

  fort <- fortify(model)

  res_plot <- ggplot(fort, aes(.fitted, .resid)) +
    geom_point(color = 'gray70', alpha = .5) +
    geom_smooth(method = 'lm', color = 'black', se = {{se}}, size = {{size}}) +
    labs(title = 'Plot of Residual Errors')

  return(res_plot)
}

predictors <- c('hp', 'carb', 'gear', 'cyl')
residual_view(data = mtcars, x = predictors, y = 'mpg', se = FALSE, size = 1)


normality_view <- function(data, x, model_x, y, alpha, bins, se = c(TRUE, FALSE)){
  library(dplyr)
  library(ggplot2)

  ivs <- paste(model_x, collapse = " + ")
  my_formula <- as.formula(paste(y, '~', ivs))
  model <- lm(my_formula, data = data)

  univariate_histogram <- {{data}} %>%
    ggplot(aes({{x}})) +
    geom_histogram(color = 'white', alpha = {{alpha}}, bins = {{bins}}) +
    labs(title = 'Histogram to Assess Normality') + 
    theme_minimal()

  qq_plot_model <- ggplot(model, aes(sample = .stdresid)) +
    geom_qq(color = 'gray70', size = 4) +
    stat_qq_line() + 
    theme_minimal()

  gridExtra::grid.arrange(univariate_histogram, qq_plot_model, nrow = 2)
}

predictors <- c('hp', 'carb', 'gear', 'cyl')

normality_view(data = mtcars, x = hp, model_x = predictors, y = 'mpg', 
               alpha = .5, bins = 10, se = FALSE) 
