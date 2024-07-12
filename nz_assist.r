library(tidyverse)
library(lme4)
library(geepack)
library(glmulti)


data <- mtcars %>% as_tibble() %>% 
  mutate(cyl = factor(cyl))

# lmer version
basic_lmm <- lmer(mpg ~ hp + disp + wt + (1|cyl), data)

# use glmulti to find the best parameters

# custom fit function
lmer.glmulti <- function (formula, data, random = "", ...) {
  lmer(paste(deparse(formula), random), data = data, REML=F, ...)
}

res_glmulti <- glmulti::glmulti(
  mpg ~ hp  + disp + wt, data =  data, fitfunc = lmer.glmulti, random = "+(1|cyl)", 
  crit="aicc", method = "h", confsetsize = 20, level = 1)

print(res_glmulti)

gee.glmulti <- function (formula, data, ...) {
  geeglm(formula, data = data, family = gaussian, corstr = "independence", id = data$cyl, ...)
}

# gee version
basic_gee <- geeglm(mpg ~ hp  + disp + wt, id = cyl, data = data, family = gaussian, corstr = "independence")

gee.glmulti <- function (formula, data, id, ...) {
  geeglm(paste(deparse(formula)), data = data, family = gaussian, corstr = "independence", id = "cyl", ...)
}

res_gee <- glmulti(
  mpg ~ hp  + disp + wt, data =  data, fitfunc = gee.glmulti, id = "cyl",
  crit="aicc", method = "h", confsetsize = 20, level = 1)

print(res_gee)


reg_fun <- function(
    data,
    y,
    x,
    rand
){
 y <- deparse(substitute(y))
 
 ivs <- paste(x, collapse = " + ")
 random <- paste("1 | ", rand)
 my_formula <- as.formula(paste(y, "~", ivs, " + ", random))
 model <- lme4::lmer(my_formula, data = data)

 model
}

model1 <- reg_fun(
    data = data,
    y = mpg,
    x = c("hp", "disp", "wt"),
    rand = "cyl"
)

model2 <- reg_fun(
    data = data,
    y = mpg,
    x = var,
    rand = "cyl"
)

tibble(
    model = attributes(AIC(model1, model2, basic_lmm))$row.names,
    df = AIC(model1, model2, basic_lmm)[, 1],
    AIC = AIC(model1, model2, basic_lmm)[, 2]
)



var <- c("disp", "hp", "drat", "wt", "qsec", "vs", "am", "gear", "carb")

var_func <- function(
  begin,
  end,
  start){
  map(
  begin:end,
  ~var[start:.x]
)
}

var_func(
  begin = 1,
  end = 9,
  start = 1
)

var_func(
  begin = 2,
  end = 9,
  start = 2
)

var_func(
  begin = 3,
  end = 9,
  start = 3
)

var_func(
  begin = 4,
  end = 9,
  start = 4
)

var_func(
  begin = 5,
  end = 9,
  start = 5
)

var_func(
  begin = 6,
  end = 9,
  start = 6
)

var_func(
  begin = 7,
  end = 9,
  start = 7
)

var_func(
  begin = 8,
  end = 9,
  start = 8
)

var_func(
  begin = 9,
  end = 9,
  start = 9
)
