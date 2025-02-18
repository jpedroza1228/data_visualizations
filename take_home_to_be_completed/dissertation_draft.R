#install.packages(c("tidyverse", "sf", "psych", "tidycensus", "spdep", "spatialreg", "rgdal", "rgeos", "tmap", "inspectdf", "reactable"))

library(tidyverse)
library(sf)
library(psych)
library(tidycensus)
library(spdep)
library(spatialreg)
# library(rgdal)
# library(rgeos)
library(tmap)
library(inspectdf)
library(reactable)

options(scipen = 999)
theme_set(theme_light())

set.seed(123021)

spatial_stand_coef <- function(object){
  object$model <- cbind(object$y, object$X)
  
  model <- as.matrix(object$model)
  model <- as.data.frame(model)
  
  b <- as.matrix(object$coefficients)
  sy <- sd(model[, 1])
  sx <- purrr::map_dbl(model[, -1], ~sd(.x))
  beta <- function(b, sx, sy){
    b*sx/sy
  }
  beta(b, sx, sy)
}


error_model_residuals <- function(data, object, listw, zero.policy = c(TRUE, FALSE, NULL), nsim = 9999, seed = 12345){
	visual <- ggplot(data = data, aes(x = object$fitted.values, y = object$residuals)) +
				geom_point() + 
	      geom_hline(yintercept = 0,
	                 linetype = 2,
	                 size = 1.25,
	                 color = 'dodgerblue')
				
	set.seed(seed)
	moran_find <- moran.mc(object$residuals, listw = listw, zero.policy = zero.policy, nsim = nsim)
  
  statistics <- if(moran_find$statistic < 0){
    upper <- (1 - moran_find$p.value)
    upper*2
    
  } else{
    moran_find$p.value*2
  }
	return(list(visual, statistics))
}


jn_sem <- function(model, x, m, alpha = .05){
  library(stringi)
  
  b1 = model$coefficients[x]
  b3 = model$coefficients[stri_startswith_fixed(names(model$coefficients), 
                                                paste0(x,":"))]
  
  se_b1 = model$rest.se[paste0("I(x - lambda * WX)", x)]
  se_b3 = model$rest.se[paste0("I(x - lambda * WX)", x, ":", m)]
  
  cov_b1b3 = vcov(model)[x, paste0(x, ":", m)]
  
  z_crit = qt(1-alpha/2, (nrow(model$tarX) - model$parameters - 1))
  # see Bauer & Curran, 2005
  a = z_crit^2 * se_b3^2 - b3^2
  b = 2 * (z_crit^2 * cov_b1b3 - b1 * b3)
  c = z_crit^2 * se_b1^2 - b1^2
  jn_final = c(
    (-b - sqrt(b^2 - 4 * a * c)) / (2 * a),
    (-b + sqrt(b^2 - 4 * a * c)) / (2 * a)
  )
  
  jn = sort(unname(jn_final))
  jn_min = jn[jn >= min(model$tarX[, paste0("I(x - lambda * WX)", m)])]
  
  jn_max = jn[jn <= max(model$tarX[, paste0("I(x - lambda * WX)", m)])]
  
  return(list(jn_min, jn_max))
}


theta_plot_sem <- function(model, x, m, alpha = .05, jn = FALSE) {
  require(dplyr)
  require(ggplot2)
  require(stringi)
  
  data = tibble(b1 = model$coefficients[x],
                b3 = model$coefficients[stri_startswith_fixed(names(model$coefficients), 
                                                              paste0(x,":"))],
                # instead of model look for this
                z = quantile(model$tarX[, paste0("I(x - lambda * WX)", m)], seq(0,1,.01)),
                theta = b1 + z * b3,
                se_b1 = model$rest.se[paste0("I(x - lambda * WX)", x)],
                cov_b1b3 = vcov(model)[x, paste0(x, ":", m)],
                se_b3 = model$rest.se[paste0("I(x - lambda * WX)", x, ":", m)],
                
                se_theta = sqrt(se_b1^2 + 2 * z * cov_b1b3 + z^2 * se_b3^2),
                ci.lo_theta = theta + qt(alpha/2, (nrow(model$tarX) - model$parameters - 1))*se_theta,
                ci.hi_theta = theta + qt(1-alpha/2, (nrow(model$tarX) - model$parameters - 1))*se_theta)
  jn_plot1 <- if (jn) {
    jn_fun = jn_sem(model = model, x = x, m = m, alpha = alpha)[[1]]
    jn_lines = geom_vline(xintercept = jn_fun, linetype = 2)
    jn_regions = ifelse(length(jn_fun) == 0, "no significance regions", paste(round(jn_fun, 2), collapse = "; "))
    xlab = paste0(m, " (JN Significance Regions: ", jn_regions,")")
  }
  else {
    xlab = m
    jn_lines = NULL
  }
  
  jn_ggplot1 <- data %>%
                  ggplot(aes(z, theta, ymin = ci.lo_theta, ymax = ci.hi_theta)) +
                  geom_ribbon(alpha = .2) +
                  geom_line(color = "dodgerblue", size = 1) +
                  # ggtitle(paste("Conditional Effect of", x, "as function of", m)) +
                  geom_hline(yintercept = 0, linetype = 2) +
                  labs(x = xlab, y = expression(theta)) +
                  jn_lines
  
  jn_plot2 <- if (jn) {
    jn_fun = jn_sem(model = model, x = x, m = m, alpha = alpha)[[2]]
    jn_lines = geom_vline(xintercept = jn_fun, linetype = 2)
    jn_regions = ifelse(length(jn_fun) == 0, "no significance regions", paste(round(jn_fun, 2), collapse = "; "))
    xlab = paste0(m, " (JN Significance Regions: ", jn_regions,")")
  }
  else {
    xlab = m
    jn_lines = NULL
  }
  
  jn_ggplot2 <- data %>%
                  ggplot(aes(z, theta, ymin = ci.lo_theta, ymax = ci.hi_theta)) +
                  geom_ribbon(alpha = .2) +
                  geom_line(color = "dodgerblue", size = 1) +
                  # ggtitle(paste("Conditional Effect of", x, "as function of", m)) +
                  geom_hline(yintercept = 0, linetype = 2) +
                  labs(x = xlab, y = expression(theta)) +
                  jn_lines
  
  return(list(jn_ggplot1, jn_ggplot2))
}

county <- read_csv('https://raw.githubusercontent.com/jpedroza1228/dissertation/master/analytic_data/analytic_data2021.csv') %>% 
  janitor::clean_names()

census_var <- c('P2_002N', #latino
                'P2_003N', #not_latino
                'P2_001N') #total

cen21 <- get_decennial(geography = 'county',
                       variables = census_var,
                       year = 2020,
                       survey = 'pl',
                       output = 'wide') %>% 
  janitor::clean_names()

cen10 <- get_decennial(geography = 'county',
                       variables = "P013001",
                       year = 2010,
                       geometry = TRUE,
                       output = 'wide') %>% 
  janitor::clean_names() %>%
  separate(col = name, into = c("county_name", "state_name"), sep = ", ") %>% 
  mutate(state_name = str_to_lower(state_name),
         county_name = str_to_lower(county_name),
         state = recode(state_name, 'alabama' = 'AL','alaska' = 'AK','arizona' = 'AZ','arkansas' = 'AR',
                        'california' = 'CA','colorado' = 'CO','connecticut' = 'CT',
                        'delaware' = 'DE', 'district of columbia' = 'DC',
                        'florida' = 'FL',
                        'georgia' = 'GA',
                        'hawaii' = 'HI',
                        'idaho' = 'ID','illinois' = 'IL','indiana' = 'IN','iowa' = 'IA',
                        'kansas' = 'KS','kentucky' = 'KY',
                        'louisiana' = 'LA',
                        'maine' = 'ME','maryland' = 'MD','massachusetts' = 'MA','michigan' = 'MI','minnesota' = 'MN','mississippi' = 'MS','missouri' = 'MO','montana' = 'MT',
                        'nebraska' = 'NE','nevada' = 'NV','new hampshire' = 'NH','new jersey' = 'NJ','new mexico' = 'NM','new york' = 'NY','north carolina' = 'NC','north dakota' = 'ND',
                        'ohio' = 'OH','oklahoma' = 'OK','oregon' = 'OR',
                        'pennsylvania' = 'PA',
                        'rhode island' = 'RI',
                        'south carolina' = 'SC','south dakota' = 'SD',
                        'tennessee' = 'TN','texas' = 'TX',
                        'utah' = 'UT',
                        'vermont' = 'VT','virginia' = 'VA',
                        'washington' = 'WA','west virginia' = 'WV','wisconsin' = 'WI','wyoming' = 'WY',
                        'puerto rico' = "PR"))


cen21 <- cen21 %>% 
  separate(col = name, into = c("county_name", "state_name"), sep = ", ") %>% 
  rename(latino = p2_002n,
         not_latino = p2_003n,
         total = p2_001n)

cen21 %>% 
  group_by(state_name) %>% 
  summarize(mean_latino = mean(latino),
            mean_nl = mean(not_latino)) %>%
  t()

cen21 <- cen21 %>% 
  mutate(state_name = str_to_lower(state_name),
         county_name = str_to_lower(county_name),
         state = recode(state_name, 'alabama' = 'AL','alaska' = 'AK','arizona' = 'AZ','arkansas' = 'AR',
                        'california' = 'CA','colorado' = 'CO','connecticut' = 'CT',
                        'delaware' = 'DE', 'district of columbia' = 'DC',
                        'florida' = 'FL',
                        'georgia' = 'GA',
                        'hawaii' = 'HI',
                        'idaho' = 'ID','illinois' = 'IL','indiana' = 'IN','iowa' = 'IA',
                        'kansas' = 'KS','kentucky' = 'KY',
                        'louisiana' = 'LA',
                        'maine' = 'ME','maryland' = 'MD','massachusetts' = 'MA','michigan' = 'MI','minnesota' = 'MN','mississippi' = 'MS','missouri' = 'MO','montana' = 'MT',
                        'nebraska' = 'NE','nevada' = 'NV','new hampshire' = 'NH','new jersey' = 'NJ','new mexico' = 'NM','new york' = 'NY','north carolina' = 'NC','north dakota' = 'ND',
                        'ohio' = 'OH','oklahoma' = 'OK','oregon' = 'OR',
                        'pennsylvania' = 'PA',
                        'rhode island' = 'RI',
                        'south carolina' = 'SC','south dakota' = 'SD',
                        'tennessee' = 'TN','texas' = 'TX',
                        'utah' = 'UT',
                        'vermont' = 'VT','virginia' = 'VA',
                        'washington' = 'WA','west virginia' = 'WV','wisconsin' = 'WI','wyoming' = 'WY',
                        'puerto rico' = "PR"))

county <- county %>% 
  rename(state = state_abbreviation,
         county_name = name) %>% 
  mutate(county_name = str_to_lower(county_name))

cen21 <- full_join(cen21, cen10)

data <- left_join(cen21, county, by = c("county_name", "state"))

data <- data %>% 
  select(!ends_with("ci_low") &
         !ends_with("ci_high") &
         !ends_with("numerator") &
         !ends_with("denominator")) %>% 
  mutate(state2 = state,
         region = recode(state2, "AL" = "South",
                         "AK" = "West",
                         "AZ" = "West",
                         "AR" = "South",
                         "CA" = "West",
                         "CO" = "West",
                         "CT" = "Northeast",
                         "DE" = "South",
                         "DC" = "Northeast",
                         "FL" = "South",
                         "GA" = "South",
                         "HI" = "West",
                         "ID" = "West",
                         "IL" = "Midwest",
                         "IN" = "Midwest",
                         "IA" = "Midwest",
                         "KS" = "Midwest",
                         "KY" = "South",
                         "LA" = "South",
                         "ME" = "Northeast",
                         "MD" = "South",
                         "MA" = "Northeast",
                         "MI" = "Midwest",
                         "MN" = "Midwest",
                         "MS" = "South",
                         "MO" = "Midwest",
                         "MT" = "West",
                         "NE" = "Midwest",
                         "NV" = "West",
                         "NH" = "Northeast",
                         "NJ" = "Northeast",
                         "NM" = "West",
                         "NY" = "Northeast",
                         "NC" = "South",
                         "ND" = "Midwest",
                         "OH" = "Midwest",
                         "OK" = "South",
                         "OR" = "West",
                         "PA" = "Northeast",
                         "RI" = "Northeast",
                         "SC" = "South",
                         "SD" = "Midwest",
                         "TN" = "South",
                         "TX" = "South",
                         "UT" = "West",
                         "VT" = "Northeast",
                         "VA" = "South",
                         "WA" = "West",
                         "WV" = "South",
                         "WI" = "Midwest",
                         "WY" = "West",
                         "PR" = "Outside"))


model_data <- data %>% 
  select(geoid:release_year,
         adult_smoking_raw_value:alcohol_impaired_driving_deaths_raw_value,
         uninsured_raw_value:ratio_of_population_to_mental_health_providers,
         flu_vaccinations_raw_value,
         unemployment_raw_value,
         children_in_poverty_raw_value,
         social_associations_raw_value:injury_deaths_raw_value,
         air_pollution_particulate_matter_raw_value,
         drinking_water_violations_raw_value,
         driving_alone_to_work_raw_value,
         long_commute_driving_alone_raw_value,
         diabetes_prevalence_raw_value:drug_overdose_deaths_raw_value,
         motor_vehicle_crash_deaths_raw_value,
         insufficient_sleep_raw_value:ratio_of_population_to_primary_care_providers_other_than_physicians,
         median_household_income_raw_value,
         children_eligible_for_free_or_reduced_price_lunch_raw_value,
         homicides_raw_value,
         suicides_raw_value:crude_suicide_rate,
         firearm_fatalities_raw_value,
         juvenile_arrests_raw_value,
         traffic_volume_raw_value,
         homeownership_raw_value,
         broadband_access_raw_value,
         population_raw_value:percent_rural_raw_value,
         region,
         geometry) %>% 
  mutate(county = str_replace_all(county_name, pattern = " county", replacement = ""))

model_data <- model_data %>% 
  mutate(geoid = as.factor(geoid),
         county_name = as.factor(county_name),
         state_name = as.factor(state_name),
         state = as.factor(state),
         release_year = as.factor(release_year),
         region = as.factor(region),
         state_fips_code = as.factor(state_fips_code),
         county_fips_code = as.factor(county_fips_code),
         x5_digit_fips_code = as.factor(x5_digit_fips_code),
         drinking_water_violations_raw_value = as.factor(drinking_water_violations_raw_value),
         county = as.factor(county)) %>% 
  mutate_if(is.character, as.numeric)

model_data <- model_data %>% 
  mutate_if(is.numeric, ~ifelse(is.na(.), median(., na.rm = TRUE), .)) %>% 
  filter(region != "Outside") %>% 
  mutate(region = relevel(region, ref = "West"),
         state = relevel(state, ref = "CA"))

west_data <- model_data %>% 
  filter(region == 'West',
         state != 'HI',
         state != 'AK')

west_data <- west_data %>% 
  rename(food_envir_index = food_environment_index_raw_value,
         social_associations = social_associations_raw_value,
         violent_crime = violent_crime_raw_value,
         injury_death = injury_deaths_raw_value,
         air_pollution = air_pollution_particulate_matter_raw_value,
         homicide = homicides_raw_value,
         suicide = suicides_raw_value,
         suicide_rate = crude_suicide_rate,
         gun_death = firearm_fatalities_raw_value,
         juvenile_arrest = juvenile_arrests_raw_value,
         traffic_volume = traffic_volume_raw_value,
         population = population_raw_value) %>% 
  mutate(smoking = adult_smoking_raw_value*100,
         obesity = adult_obesity_raw_value*100,
         inactivity = physical_inactivity_raw_value*100,
         access = access_to_exercise_opportunities_raw_value*100,
         excess_drink = excessive_drinking_raw_value*100,
         drunk_drive_death = alcohol_impaired_driving_deaths_raw_value*100,
         uninsured = uninsured_raw_value*100,
         pcp = primary_care_physicians_raw_value*100,
         dentist = dentists_raw_value*100,
         mental_provider = mental_health_providers_raw_value*100,
         flu_vaccine = flu_vaccinations_raw_value*100,
         unemployment = unemployment_raw_value*100,
         children_poverty = children_in_poverty_raw_value*100,
         drive_alone = driving_alone_to_work_raw_value*100,
         long_commute = long_commute_driving_alone_raw_value*100,
         diabetes = diabetes_prevalence_raw_value*100,
         hiv = hiv_prevalence_raw_value*100,
         food_insecure = food_insecurity_raw_value*100,
         limited_health_food_access = limited_access_to_healthy_foods_raw_value*100,
         drug_od = drug_overdose_deaths_raw_value*100,
         vehicle_crash_death = motor_vehicle_crash_deaths_raw_value*100,
         lack_sleep = insufficient_sleep_raw_value*100,
         eligible_free_reduce_lunch = children_eligible_for_free_or_reduced_price_lunch_raw_value*100,
         homeowner = homeownership_raw_value*100,
         broadband_access = broadband_access_raw_value*100,
         rural = percent_rural_raw_value*100,
         smoking_c = scale(smoking, center = TRUE, scale = FALSE),
         obesity_c = scale(obesity, center = TRUE, scale = FALSE),
         inactivity_c = scale(inactivity, center = TRUE, scale = FALSE),
         access_c = scale(access, center = TRUE, scale = FALSE),
         excess_drink_c = scale(excessive_drinking_raw_value, center = TRUE, scale = FALSE),
         drunk_drive_death_c = scale(excess_drink, center = TRUE, scale = FALSE),
         uninsured_c = scale(uninsured, center = TRUE, scale = FALSE),
         pcp_c = scale(pcp, center = TRUE, scale = FALSE),
         dentist_c = scale(dentist, center = TRUE, scale = FALSE),
         mental_provider_c = scale(mental_provider, center = TRUE, scale = FALSE),
         flu_vaccine_c = scale(flu_vaccine, center = TRUE, scale = FALSE),
         unemployment_c = scale(unemployment, center = TRUE, scale = FALSE),
         children_poverty_c = scale(children_poverty, center = TRUE, scale = FALSE),
         drive_alone_c = scale(drive_alone, center = TRUE, scale = FALSE),
         long_commute_c = scale(long_commute, center = TRUE, scale = FALSE),
         diabetes_c = scale(diabetes, center = TRUE, scale = FALSE),
         hiv_c = scale(hiv, center = TRUE, scale = FALSE),
         food_insecure_c = scale(food_insecure, center = TRUE, scale = FALSE),
         limited_health_food_access_c = scale(limited_health_food_access, center = TRUE, scale = FALSE),
         drug_od_c = scale(drug_od, center = TRUE, scale = FALSE),
         vehicle_crash_death_c = scale(vehicle_crash_death, center = TRUE, scale = FALSE),
         lack_sleep_c = scale(lack_sleep, center = TRUE, scale = FALSE),
         median_income_thousand = median_household_income_raw_value*.001,
         pop_001 = population*.001,
         med_inc_th_c = scale(median_income_thousand, center = TRUE, scale = FALSE),
         eligible_free_reduce_lunch_c = scale(eligible_free_reduce_lunch, center = TRUE, scale = FALSE),
         homeowner_c = scale(homeowner, center = TRUE, scale = FALSE),
         broadband_access_c = scale(broadband_access, center = TRUE, scale = FALSE),
         rural_c = scale(rural, center = TRUE, scale = FALSE))

west_data <- west_data %>%
  mutate(lat_per = latino/total,
    not_lat_per = not_latino/total,
    lat_per = lat_per*100,
    not_lat_per = not_lat_per*100)

library(lme4)
library(lmerTest)
mixed_model <- lmerTest::lmer(inactivity ~ access + violent_crime + median_income_thousand +
                            rural + air_pollution + not_lat_per + 
                              (1 | state),
                          data = west_data,
                          REML = FALSE)
summary(mixed_model)

county_icc_2level <- function(multi_model){
  between <- multi_model$vcov[1]
  total <- multi_model$vcov[1] + multi_model$vcov[2]
  
  between/total
}

mixed_icc <- as_tibble(VarCorr(mixed_model))
mixed_icc
county_icc_2level(mixed_icc)

main_effects_mixed <- ranef(mixed_model, condVar = TRUE)
main_effects_mixed

main_effects_mixed_df <- as_tibble(main_effects_mixed)

main_effects_mixed_df <- main_effects_mixed_df %>% 
  mutate(main_effects_term = term,
         state = grp,
         main_effects_diff = condval,
         main_effects_se = condsd,
         state_code = as.numeric(state))

main_effects_mixed_df$state2 <- unique(west_data$state)

main_effects_mixed_df %>% 
  select(state:main_effects_se) %>% 
  mutate(main_effects_diff = round(main_effects_diff, 3),
         main_effects_se = round(main_effects_diff, 3)) %>% 
  reactable()

main_effects_mixed_df %>% 
  ggplot(aes(fct_reorder(state2, main_effects_diff), main_effects_diff)) +
  geom_errorbar(aes(ymin = main_effects_diff + qnorm(0.025)*main_effects_se,
                  ymax = main_effects_diff + qnorm(0.975)*main_effects_se)) +
  geom_point(aes(color = state2)) +
  coord_flip() +
  labs(x = ' ',
     y = 'Differences in Physical Inactivity',
     title = 'Variation in Physical Inactivity Across Western States') +
  theme(legend.position = 'none')

library(nlme)
# install.packages("robustlmm")
library(robustlmm)

inactive_model <- nlme::lme(inactivity ~ access + violent_crime + median_income_thousand +
                            rural + air_pollution + not_lat_per,
                           random = ~1 | state,
                          data = west_data,
                          na.action = na.omit,
                          method = 'ML')

opt <- nlmeControl(maxIter = 100, opt = 'nlm')

inactive_model_check <- nlme::lme(inactivity ~ access + violent_crime + median_income_thousand +
                                    rural + air_pollution + not_lat_per,
                                  random = list(state = pdDiag(~ 1)),
                                  weights = varIdent(form = ~ 1 | state), 
                                  data = west_data,
                                  na.action = na.omit,
                                  method = 'ML',
                                  control = opt)

summary(inactive_model)
summary(inactive_model_check)

model_compare <- anova(inactive_model, inactive_model_check)
p_value <- model_compare$`p-value`[2]

poly_join <- west_data %>% 
  drop_na() %>% #remove counties with anything missing
  rowid_to_column() %>%
  st_sf()

poly_join <- poly_join[!st_is_empty(poly_join), ]

spatial_join <- as(poly_join, 'Spatial')

set.seed(123021)
poly_nb <- poly2nb(spatial_join, row.names = spatial_join$rowid, queen = TRUE)
poly_listw <- nb2listw(poly_nb, style = "W", zero.policy = TRUE)
summary(poly_listw, zero.policy = TRUE)

west_data %>% 
  inspect_cor(with_col = 'not_lat_per') %>% 
  rowid_to_column() %>% 
  filter(rowid %in% c(1:4)) %>% 
  show_plot()

activity_model <- 
  inactivity ~
  access +
  violent_crime +
  median_income_thousand +
  rural + 
  air_pollution +
  not_lat_per + 
  state

activity_int_model <-
  inactivity ~ 
  access*violent_crime + 
  median_income_thousand + 
  rural + 
  air_pollution +
  not_lat_per + 
  state

set.seed(123021)
ols_main <- lm(activity_model,
              data = spatial_join)
summary(ols_main)

ggplot(data = ols_main, aes(ols_main$fitted.values, ols_main$residuals)) + 
  geom_point(aes(color = state)) +
  geom_hline(yintercept = 0,
             color = 'black',
             linetype = 2) +
  theme(legend.position = 'none')

moran_resid <- lm.morantest(ols_main,
             poly_listw,
             zero.policy = TRUE,
             alternative = 'two.sided')
moran_resid

lagrange_multiplier <- lm.LMtests(ols_main,
           poly_listw,
           test = c("LMerr", "LMlag", "RLMerr", "RLMlag"),
           zero.policy = TRUE)
lagrange_multiplier

sem_main <- errorsarlm(activity_model,
                      data = spatial_join,
                      listw = poly_listw,
                      zero.policy = TRUE)
summary(sem_main,
        Nagalkerke = TRUE,
        zstats = TRUE)
spatial_stand_coef(sem_main)

error_model_residuals(data = poly_join,
                      object = sem_main,
                      listw = poly_listw,
                      zero.policy = TRUE,
                      nsim = 9999,
                      seed = 123021)

# install.packages(c("broom", "reactable", "htmltools"))

library(broom)
library(reactable)
library(htmltools)

summary(sem_main)

sem_main %>% 
  tidy() %>% 
  mutate(estimate = round(estimate, 3),
         std.error = round(std.error, 3),
         statistic = round(statistic, 3),
         p.value = round(p.value, 3)) %>% 
  reactable(highlight = TRUE) 


set.seed(123021)
ols_int <- lm(activity_int_model,
              data = spatial_join)
summary(ols_int)

lm.morantest(ols_int,
             poly_listw,
             zero.policy = TRUE,
             alternative = 'two.sided')

lm.LMtests(ols_int,
           poly_listw,
           test = c("LMerr", "LMlag", "RLMerr", "RLMlag"),
           zero.policy = TRUE)

sem_int <- errorsarlm(activity_int_model,
                      data = spatial_join,
                      listw = poly_listw,
                      zero.policy = TRUE)
summary(sem_int,
        Nagalkerke = TRUE,
        zstats = TRUE)
spatial_stand_coef(sem_int)

error_model_residuals(data = poly_join,
                      object = sem_int,
                      listw = poly_listw,
                      zero.policy = TRUE,
                      nsim = 9999,
                      seed = 123021)

sem_int %>% 
  tidy() %>% 
  mutate(estimate = round(estimate, 3),
         std.error = round(std.error, 3),
         statistic = round(statistic, 3),
         p.value = round(p.value, 3)) %>% 
  reactable(highlight = TRUE) 

theta_plot_sem(sem_int, 
               x = 'access',
               m = 'violent_crime',
               jn = TRUE)[2]
