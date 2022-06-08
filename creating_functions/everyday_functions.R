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
	
	datsun_maroon = c('#5e2228', '#5b2c2f'),
	datsun_pagoda_red = c('#be2327', '#d22a2c'),
	datsun_cherry_red = c('#9b2a30'),
	red = c('#AB0014', '#c80010', '#ba0015', '#831924'),
	mexican_red = c('#6a1f25'),
	red_persimmon = c('#d72d2b'),
	light_maroon = c('#5b2c2f'),
	
	datsun_green = c('#808572', '#455e56', '#4c5249', '#2d3b35', '#387448', '#4b5d59',
	                 '#435043', '#467f4d', '#5b6242', '#3b4640'),
	datsun_kasumi_green = c('#c0c5ba'),
	leaf_green = c('#888980'),
	dark_green = c('#3f4944', '#344039'),
	olive_green = c('#4f4c3c'),
	cactus_green = c('#807f67'),
	dragon_green = c('#386350'),
	kelly_green = c('#387448'),
	avocado_green = c('#5b6242'),
	chartreuse_green = c('#669b3e'),
	gray_green = c('#808572'),
	
	datsun_turquoise = c('#7fb7b7'),
	mount_fuji_blue = c('#53636b'),
	sky_blue = c('#85b3c1', '#a0b2ba'),
	nissan_blue = c('#507b8b'),
	blue = c('#5f6d72', '#385a72', '#2c6195'),
	dark_blue = c('#414955', '#30394c', '#344039', '#4b5f71'),
	toba_aqua = c('#4b5f71'),
	true_blue = c('#3f6698'),
	island_turquoise = c('#26657f'),
	seaview_aqua = c('#398b98'),
	
	yellow = c('#dbd1ad', '#ddca72'),
	bamboo_tan = c('#a48e87', '#c3b37d'),
	houston_yellow = c('#d3bb95'),
	cream_ivory = c('#c4bb9b'),
	gold = c('#a26347', '#ae6132', '#a26540'),
	california_yellow = c('#dbd5a5'),
	sahara_gold = c('#a86428'),
	mellow_yellow = c('#c7a860'),
	yellow_lime = c('#c0cc56'),
	eggshell = c('#e5e1d4'),
	cream = c('#d3bb95'),
	desert_gold = c('#a26540'),
	sunshine_yellow = c('#ead568'),
	
	dark_golden_brown = c('#7f5d47'),
	
	orange = c('#b23a2d', '#ea5227'),
	sunbird_orange = c('#c33431'),
	sunset_orange = c('d74122'),
	mexican_orange = c('#b23a2d'),
	
	datsun_reds = c('#5e2228', '#d22a2c', '#9b2a30', '#c80010', '#6a1f25', '#d72d2b', '#5b2c2f'),##831924
	datsun_greens = c('#808572', '#455e56', '#4c5249', '#2d3b35', '#387448', '#4b5d59',
	                  '#435043', '#467f4d', '#5b6242', '#3b4640', '#344039', '#386350', '#4f4c3c',
	                  '#387448', '#669b3e'),
	datsun_blues = c('#7fb7b7', '#53636b', '#85b3c1', '#a0b2ba', '#507b8b', '#385a72', '#2c6195',
	                 '#30394c', '#3f6698', '#26657f', '#398b98'),
	datsun_yellows = c('#ddca72', '#d3bb95', '#a26347', '#dbd5a5', '#c7a860', '#c0cc56', '#ead568'),
	datsun_oranges = c('#b23a2d', '#ea5227', '#c33431', 'd74122', '#b23a2d')
)

install.packages('wesanderson')

wesanderson::wes_palettes

datsun_color_palettes

autopia_color_palettes <- list(
	fifties = c('#FFF6B7','#8CD4C9', '#F7471A', '#D1E6F2', '#23A1E3', '#AA1818', '#601a35'),
	early_2000s = c('purple/lavender', 'bronze', 'soft yellow', 'metallic green', 'silver/bronze',
					'light blue','forest green', 'brown', 'soft red', 'bright yellow',
					'gold', 'silver', 'dark purple', 'royal blue')
)



linearity <- function(data, x, y, se = c(TRUE, FALSE)){
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
