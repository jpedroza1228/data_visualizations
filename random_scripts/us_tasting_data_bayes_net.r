library(tidyverse)
library(inspectdf)
library(bnlearn)
library(Rgraphviz)
library(reactable)

theme_set(theme_light())

coffee <- read_csv(here::here("random_data", "hoffmann_america_taste_data.csv")) |>
  janitor::clean_names()

coffee_drop <- coffee[, which(colMeans(!is.na(coffee)) > 0.5)]


coffee_drop <- coffee_drop |>
  select(
    -c(
      where_do_you_typically_drink_coffee,
      how_do_you_brew_coffee_at_home,
      do_you_usually_add_anything_to_your_coffee,
      why_do_you_drink_coffee
    )
  ) |>
  rename(
    age = what_is_your_age,
    cup_per_day = how_many_cups_of_coffee_do_you_typically_drink_per_day,
    drink_at_home = where_do_you_typically_drink_coffee_at_home,
    drink_at_office = where_do_you_typically_drink_coffee_at_the_office,
    drink_on_go = where_do_you_typically_drink_coffee_on_the_go,
    drink_at_cafe = where_do_you_typically_drink_coffee_at_a_cafe,
    drink_none_of_these = where_do_you_typically_drink_coffee_none_of_these,
    home_brew_pour_over = how_do_you_brew_coffee_at_home_pour_over,
    home_brew_french_press = how_do_you_brew_coffee_at_home_french_press,
    home_brew_espresso = how_do_you_brew_coffee_at_home_espresso,
    home_brew_mr_coffee = how_do_you_brew_coffee_at_home_coffee_brewing_machine_e_g_mr_coffee,
    home_brew_pods = how_do_you_brew_coffee_at_home_pod_capsule_machine_e_g_keurig_nespresso,
    home_brew_instant = how_do_you_brew_coffee_at_home_instant_coffee,
    home_brew_bean2cup = how_do_you_brew_coffee_at_home_bean_to_cup_machine,
    home_brew_cold_brew = how_do_you_brew_coffee_at_home_cold_brew,
    home_brew_cometeer = how_do_you_brew_coffee_at_home_coffee_extract_e_g_cometeer,
    home_brew_other = how_do_you_brew_coffee_at_home_other,
    favorite_coffee_drink = what_is_your_favorite_coffee_drink,
    coffee_black = do_you_usually_add_anything_to_your_coffee_no_just_black,
    coffee_milk_alt_creamer = do_you_usually_add_anything_to_your_coffee_milk_dairy_alternative_or_coffee_creamer,
    coffee_sugar = do_you_usually_add_anything_to_your_coffee_sugar_or_sweetener,
    coffee_syrup = do_you_usually_add_anything_to_your_coffee_flavor_syrup,
    coffee_other = do_you_usually_add_anything_to_your_coffee_other,
    coffee_characteristic_preference = before_todays_tasting_which_of_the_following_best_described_what_kind_of_coffee_you_like,
    coffee_strength = how_strong_do_you_like_your_coffee,
    roast_preference = what_roast_level_of_coffee_do_you_prefer,
    caffeine_preference = how_much_caffeine_do_you_like_in_your_coffee,
    expertise = lastly_how_would_you_rate_your_own_coffee_expertise,
    preference_a_to_b = between_coffee_a_coffee_b_and_coffee_c_which_did_you_prefer,
    preference_a_to_d = between_coffee_a_and_coffee_d_which_did_you_prefer,
    favorite_abcd = lastly_what_was_your_favorite_overall_coffee,
    remote_work = do_you_work_from_home_or_in_person,
    money_spend_a_month = in_total_much_money_do_you_typically_spend_on_coffee_in_a_month,
    why_drink_taste_good = why_do_you_drink_coffee_it_tastes_good,
    why_drink_caffeine = why_do_you_drink_coffee_i_need_the_caffeine,
    why_drink_ritual = why_do_you_drink_coffee_i_need_the_ritual,
    why_drink_makes_bathroom = why_do_you_drink_coffee_it_makes_me_go_to_the_bathroom,
    why_drink_other = why_do_you_drink_coffee_other,
    like_taste = do_you_like_the_taste_of_coffee,
    know_where_coffee_comes_from = do_you_know_where_your_coffee_comes_from,
    most_spent_on_cup_coffee = what_is_the_most_youve_ever_paid_for_a_cup_of_coffee,
    willing_to_spend_cup_coffee = what_is_the_most_youd_ever_be_willing_to_pay_for_a_cup_of_coffee,
    good_value_cafe = do_you_feel_like_you_re_getting_good_value_for_your_money_when_you_buy_coffee_at_a_cafe,
    equipment_spent_5years = approximately_how_much_have_you_spent_on_coffee_equipment_in_the_past_5_years,
    good_value_equipment = do_you_feel_like_you_re_getting_good_value_for_your_money_with_regards_to_your_coffee_equipment
  )

coffee_logical <- coffee_drop |>
  select_if(is.logical)

coffee_drop <- coffee_drop |>
  drop_na(
    colnames(coffee_logical)
  )

coffee_drop <- coffee_drop |>
  mutate(
    across(
      where(
        is.logical
      ),
      ~case_when(
        .x == TRUE ~ 1,
        .x == FALSE ~ 0
      )
    ),
    across(
      where(
        is.character
      ),
      ~as.factor(.x)
    )
  )

coffee_drop <- coffee_drop |>
  select(
    -matches(
      "_notes"
    )
  )

nona <- 
  coffee_drop |>
  select(
    submission_id,
    gender,
    age,
    cup_per_day,
    home_brew_pour_over,
    home_brew_french_press,
    home_brew_espresso,
    home_brew_mr_coffee,
    home_brew_pods,
    home_brew_instant,
    home_brew_bean2cup,
    home_brew_cold_brew,
    home_brew_cometeer,
    home_brew_other,
    favorite_coffee_drink,
    roast_preference,
    expertise,
    favorite_abcd
  ) |>
  drop_na() |>
  mutate(
    gender = case_when(
      gender == "Female" ~ "Female",
      gender == "Male" ~ "Male",
      TRUE ~ "Other"
    ),
    age = case_when(
      age == "<18 years old" ~ "under24",
      age == "18-24 years old" ~ "under24",
      age == "45-54 years old" ~ "over44",
      age == "55-64 years old" ~ "over44",
      age == ">65 years old" ~ "over44",
      TRUE ~ age
    ),
    cup_per_day = case_when(
      cup_per_day == "More than 4" ~ "three_or_more",
      cup_per_day == "4" ~ "three_or_more",
      cup_per_day == "3" ~ "three_or_more",
      cup_per_day == "Less than 1" ~ "one_or_less",
      cup_per_day == "1" ~ "one_or_less",
      TRUE ~ cup_per_day
    ),
    favorite_coffee_drink = case_when(
      favorite_coffee_drink == "Regular drip coffee" ~ "drip",
      favorite_coffee_drink == "Pourover" ~ "pourover",
      favorite_coffee_drink == "Other" ~ "other",
      favorite_coffee_drink == "Mocha" ~ "other",
      favorite_coffee_drink == "Latte" ~ "latte",
      favorite_coffee_drink == "Iced coffee" ~ "other",
      favorite_coffee_drink == "Espresso" ~ "espresso",
      favorite_coffee_drink == "Cortado" ~ "cortado",
      favorite_coffee_drink == "Cold brew" ~ "other",
      favorite_coffee_drink == "Cappuccino" ~ "cappuccino",
      favorite_coffee_drink == "Blended drink (e.g. Frappuccino)" ~ "other",
      favorite_coffee_drink == "Americano" ~ "americano"
    ),
    roast_preference = case_when(
      roast_preference == "Nordic" ~ "light",
      roast_preference == "Medium" ~ "medium",
      roast_preference == "Light" ~ "light",
      roast_preference == "Italian" ~ "dark",
      roast_preference == "French" ~ "dark",
      roast_preference == "Dark" ~ "dark",
      roast_preference == "Blonde" ~ "light",
    )
  )

id <- nona$submission_id

no_fact <- nona |>
  mutate(
    across(
      everything(),
      ~as.factor(.x)
    )
  ) |>
  select(
    -submission_id
  )

glimpse(no_fact)
class(no_fact)

no_fact <- as.data.frame(no_fact)

# Building Bayesian Network
dag <- empty.graph(nodes = colnames(no_fact))

arcs <- matrix(
  c("gender", "cup_per_day",
    "gender", "favorite_coffee_drink",
    "gender", "home_brew_pour_over",
    "gender", "home_brew_french_press",
    "gender", "home_brew_espresso",
    "gender", "home_brew_mr_coffee",
    "gender", "home_brew_pods",
    "gender", "home_brew_instant",
    "gender", "home_brew_bean2cup",
    "gender", "home_brew_cold_brew",
    "gender", "home_brew_cometeer",
    "gender", "home_brew_other",
    "age", "cup_per_day",
    "age", "favorite_coffee_drink",
    "age", "home_brew_pour_over",
    "age", "home_brew_french_press",
    "age", "home_brew_espresso",
    "age", "home_brew_mr_coffee",
    "age", "home_brew_pods",
    "age", "home_brew_instant",
    "age", "home_brew_bean2cup",
    "age", "home_brew_cold_brew",
    "age", "home_brew_cometeer",
    "age", "home_brew_other",

    "cup_per_day", "roast_preference",
    "favorite_coffee_drink", "roast_preference",
    "home_brew_pour_over", "roast_preference",
    "home_brew_french_press", "roast_preference",
    "home_brew_espresso", "roast_preference",
    "home_brew_mr_coffee", "roast_preference",
    "home_brew_pods", "roast_preference",
    "home_brew_instant", "roast_preference",
    "home_brew_bean2cup", "roast_preference",
    "home_brew_cold_brew", "roast_preference",
    "home_brew_cometeer", "roast_preference",
    "home_brew_other", "roast_preference",

    "roast_preference", "expertise",
    "roast_preference", "favorite_abcd",
    "expertise", "favorite_abcd"),
  byrow = TRUE,
  ncol = 2,
  dimnames = list(NULL, c("from", "to"))
)

arcs(dag) <- arcs

graphviz.plot(dag)

set.seed(12345)
dag_fit <- bn.fit(dag, data = no_fact, method = "bayes", iss = 5000)

dag_fit$gender
dag_fit$age

dag_fit$cup_per_day
dag_fit$favorite_coffee_drink
dag_fit$home_brew_pour_over
dag_fit$home_brew_french_press
dag_fit$home_brew_espresso
dag_fit$home_brew_mr_coffee
dag_fit$home_brew_pods
dag_fit$home_brew_instant
dag_fit$home_brew_bean2cup
dag_fit$home_brew_cold_brew
dag_fit$home_brew_cometeer
dag_fit$home_brew_other

dag_fit$home_brew_espress$prob |> as_tibble() |>
  ggplot(
    aes(
      gender,
      n
    )
  ) +
  geom_col(
    aes(
      fill = home_brew_espresso
    ),
    position = position_dodge()
  ) +
  geom_hline(
    yintercept = .5,
    color = "black",
    linetype = 2,
    lwd = 1
    ) + 
  facet_wrap(
    ~age
  )

dag_fit$roast_preference$prob |> str()
# This shows roast preference for the choices of cups of coffee per day when they make coffee at home using a pourover for all of the 8 choices of favorite coffee drinks
dag_fit$roast_preference$prob[1:3, 1:3, 1:2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1:8] |>
  as_tibble() |>
  mutate(
    n = round(n, 2)
  ) |>
  pivot_wider(
    names_from = cup_per_day,
    values_from = n
  ) |>
  reactable(
    filterable = TRUE,
    searchable = TRUE,
    highlight = TRUE,
    minRows = 10
  )

# This shows roast preference for the choices of cups of coffee per day when they make coffee at home using a french press for all of the 8 choices of favorite coffee drinks
dag_fit$roast_preference$prob[1:3, 1:3, 1, 1:2, 1, 1, 1, 1, 1, 1, 1, 1, 1:8] |>
  as_tibble() |>
  mutate(
    n = round(n, 2)
  ) |>
  pivot_wider(
    names_from = cup_per_day,
    values_from = n
  ) |>
  reactable(
    filterable = TRUE,
    searchable = TRUE,
    highlight = TRUE,
    minRows = 10
  )

# This shows roast preference for the choices of cups of coffee per day when they make espresso at home for all of the 8 choices of favorite coffee drinks
dag_fit$roast_preference$prob[1:3, 1:3, 1, 1, 1:2, 1, 1, 1, 1, 1, 1, 1, 1:8] |>
  as_tibble() |>
  mutate(
    n = round(n, 2)
  ) |>
  pivot_wider(
    names_from = cup_per_day,
    values_from = n
  ) |>
  reactable(
    filterable = TRUE,
    searchable = TRUE,
    highlight = TRUE,
    minRows = 10
  )

# This shows roast preference for the choices of cups of coffee per day when they make mr coffee at home for all of the 8 choices of favorite coffee drinks
dag_fit$roast_preference$prob[1:3, 1:3, 1, 1, 1, 1:2, 1, 1, 1, 1, 1, 1, 1:8] |>
  as_tibble() |>
  mutate(
    n = round(n, 2)
  ) |>
  pivot_wider(
    names_from = cup_per_day,
    values_from = n
  ) |>
  reactable(
    filterable = TRUE,
    searchable = TRUE,
    highlight = TRUE,
    minRows = 10
  )

# This shows roast preference for the choices of cups of coffee per day when they make coffee at home using pods for all of the 8 choices of favorite coffee drinks
dag_fit$roast_preference$prob[1:3, 1:3, 1, 1, 1, 1, 1:2, 1, 1, 1, 1, 1, 1:8] |>
  as_tibble() |>
  mutate(
    n = round(n, 2)
  ) |>
  pivot_wider(
    names_from = cup_per_day,
    values_from = n
  ) |>
  reactable(
    filterable = TRUE,
    searchable = TRUE,
    highlight = TRUE,
    minRows = 10
  )

# This shows roast preference for the choices of cups of coffee per day when they make coffee at home using instant coffee for all of the 8 choices of favorite coffee drinks
dag_fit$roast_preference$prob[1:3, 1:3, 1, 1, 1, 1, 1, 1:2, 1, 1, 1, 1, 1:8] |>
  as_tibble() |>
  mutate(
    n = round(n, 2)
  ) |>
  pivot_wider(
    names_from = cup_per_day,
    values_from = n
  ) |>
  reactable(
    filterable = TRUE,
    searchable = TRUE,
    highlight = TRUE,
    minRows = 10
  )

# This shows roast preference for the choices of cups of coffee per day when they make coffee at home using a bean 2 cup machine for all of the 8 choices of favorite coffee drinks
dag_fit$roast_preference$prob[1:3, 1:3, 1, 1, 1, 1, 1, 1, 1:2, 1, 1, 1, 1:8] |>
  as_tibble() |>
  mutate(
    n = round(n, 2)
  ) |>
  pivot_wider(
    names_from = cup_per_day,
    values_from = n
  ) |>
  reactable(
    filterable = TRUE,
    searchable = TRUE,
    highlight = TRUE,
    minRows = 10
  )

# This shows roast preference for the choices of cups of coffee per day when they make cold brew at home for all of the 8 choices of favorite coffee drinks
dag_fit$roast_preference$prob[1:3, 1:3, 1, 1, 1, 1, 1, 1, 1, 1:2, 1, 1, 1:8] |>
  as_tibble() |>
  mutate(
    n = round(n, 2)
  ) |>
  pivot_wider(
    names_from = cup_per_day,
    values_from = n
  ) |>
  reactable(
    filterable = TRUE,
    searchable = TRUE,
    highlight = TRUE,
    minRows = 10
  )

# This shows roast preference for the choices of cups of coffee per day when they make cometeer coffee at home for all of the 8 choices of favorite coffee drinks
dag_fit$roast_preference$prob[1:3, 1:3, 1, 1, 1, 1, 1, 1, 1, 1, 1:2, 1, 1:8] |>
  as_tibble() |>
  mutate(
    n = round(n, 2)
  ) |>
  pivot_wider(
    names_from = cup_per_day,
    values_from = n
  ) |>
  reactable(
    filterable = TRUE,
    searchable = TRUE,
    highlight = TRUE,
    minRows = 10
  )

# This shows roast preference for the choices of cups of coffee per day when they make other types of coffee at home for all of the 8 choices of favorite coffee drinks
dag_fit$roast_preference$prob[1:3, 1:3, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1:2, 1:8] |>
  as_tibble() |>
  mutate(
    n = round(n, 2)
  ) |>
  pivot_wider(
    names_from = cup_per_day,
    values_from = n
  ) |>
  reactable(
    filterable = TRUE,
    searchable = TRUE,
    highlight = TRUE,
    minRows = 10
  )

dag_fit$roast_preference$prob |> as_tibble() |>
  pivot_longer(
    matches("home_brew_")
  ) |>
  filter(
    value == 1
  )
  ggplot(
    aes(
      roast_preference,
      n
    )
  ) +
  geom_col(
    aes(
      fill = favorite_coffee_drink
    ),
    position = position_dodge()
  )


expertise_tbl <- dag_fit$expertise$prob |> as_tibble() |>
  mutate(
    roast_preference = str_to_title(roast_preference),
    across(
      -n,
      ~as.factor(.x)
    ),
    expertise = fct_relevel(
      expertise,
      "1",
      "2",
      "3",
      "4",
      "5",
      "6",
      "7",
      "8",
      "9",
      "10"
    ),
    roast_preference = fct_relevel(
      roast_preference,
      "Light",
      "Medium",
      "Dark"
    )
  ) 

expertise_tbl |>
  ggplot(
    aes(
      roast_preference,
      n
    )
  ) +
  geom_col(
    aes(
      fill = expertise
    ),
    position = position_dodge()
  ) +
  geom_text(
    data = expertise_tbl |> filter(roast_preference == "Light"),
    aes(
      label = expertise,
      group = expertise,
      color = expertise
    ),
    position = position_dodge(width = .9),
    vjust = -.5
  ) +
   labs(
    title = "Probability of One's Roast Preference From The Great American Tasting",
    subtitle = "Based on Self-Defined Expertise Level",
    x = "",
    y = "Probability",
    caption = "Note: Probabilities range from 0 to 1. The scale is reduced to visually compare groups."
  ) +
  viridis::scale_color_viridis(
    discrete = TRUE
    ) +
  viridis::scale_fill_viridis(
    discrete = TRUE
    ) +
  # scale_y_continuous(
  #   limits = c(0, 1),
  #   breaks = seq(0, 1, .1)
  # ) +
  scale_x_discrete(
    expand = c(0, .5)
  ) +
  theme(
    legend.position = "none",
    axis.text = element_text(
      color = "black"
    ),
    axis.title = element_text(
      color = "black"
    ),
    plot.title = element_text(
      color = "black"
    ),
    plot.subtitle = element_text(
      color = "black"
    ),
    plot.caption = element_text(
      color = "black"
    )
  )


favorite_abcd_prob <- dag_fit$favorite_abcd$prob |> as_tibble() |>
  mutate(
    across(
      -n,
      ~as.factor(.x)
    ),
    expertise = fct_relevel(
      expertise,
      "1",
      "2",
      "3",
      "4",
      "5",
      "6",
      "7",
      "8",
      "9",
      "10"
    ),
    roast_preference = fct_relevel(
      roast_preference,
      "light",
      "medium",
      "dark"
    ),
    favorite_abcd = fct_relevel(
      favorite_abcd,
      "Coffee A",
      "Coffee B",
      "Coffee C",
      "Coffee D"
    )
  )

# scales::show_col(viridis::viridis_pal(option = "E")(3))

favorite_abcd_prob |>
  ggplot(
    aes(
      favorite_abcd,
      n
    )
  ) +
  geom_col(
    aes(
      fill = roast_preference
    ),
    position = position_dodge()
  ) +
  geom_text(
    data = favorite_abcd_prob |>
    filter(
        expertise == "4" &
      favorite_abcd == "Coffee D" &
      roast_preference == "light"
    ),
    label = "Light\nRoast",
    nudge_y = .03,
    color = "#00204DFF"
  ) +
  geom_text(
    data = favorite_abcd_prob |>
    filter(
        expertise == "4" &
      favorite_abcd == "Coffee C" &
      roast_preference == "medium"
    ),
    label = "Medium\nRoast",
    nudge_y = .03,
    color = "#7C7B78FF"
  ) +
  geom_text(
    data = favorite_abcd_prob |>
    filter(
        expertise == "4" &
      favorite_abcd == "Coffee B" &
      roast_preference == "dark"
    ),
    label = "Dark\nRoast",
    nudge_y = .03,
    color = "#FFEA46FF"
  ) +
  facet_wrap(
    ~expertise,
    ncol = 5
  ) +
  scale_y_continuous(
    breaks = seq(.1, .6, .1)
  ) +
  viridis::scale_fill_viridis(
    discrete = TRUE,
    option = "cividis"
  ) +
  labs(
    title = "Probability of One's Favorite Coffees From The Great American Tasting",
    subtitle = "Based on Self-Defined Expertise Level & Roast Level Preference",
    x = "",
    y = "Probability"
  ) +
  theme(
    legend.position = "none",
    strip.background = element_rect(
      fill = "#7C7B78FF"
    ),
    axis.text = element_text(
      color = "#7C7B78FF"
    ),
    axis.title = element_text(
      color = "#7C7B78FF"
    ),
    plot.title = element_text(
      color = "#7C7B78FF"
    ),
    plot.subtitle = element_text(
      color = "#7C7B78FF"
    )
  )

score(
  dag,
  data = nona, 
  type = "bde",
  iss = 5000
)

cpquery(
  dag_fit,
  event = (favorite_abcd == "Coffee A"),
  evidence = (gender == "Male")
)

cpquery(
  dag_fit,
  event = (favorite_abcd == "Coffee A"),
  evidence = (gender == "Female")
)

cpquery(
  dag_fit,
  event = (favorite_abcd == "Coffee D"),
  evidence = (gender == "Male")
)

cpquery(
  dag_fit,
  event = (favorite_abcd == "Coffee D"),
  evidence = (gender == "Female")
)


cpquery(
  dag_fit,
  event = (favorite_abcd == "Coffee A") |
  (favorite_abcd == "Coffee D"),
  evidence = (gender == "Male")
)

cpquery(
  dag_fit,
  event = (favorite_abcd == "Coffee A") |
  (favorite_abcd == "Coffee D"),
  evidence = (gender == "Female")
)


expert1 <- cpquery(
  dag_fit,
  event = (favorite_abcd == "Coffee A") |
  (favorite_abcd == "Coffee D"),
  evidence = (expertise == "1")
)
expert2 <- cpquery(
  dag_fit,
  event = (favorite_abcd == "Coffee A") |
  (favorite_abcd == "Coffee D"),
  evidence = (expertise == "2")
)
expert3 <- cpquery(
  dag_fit,
  event = (favorite_abcd == "Coffee A") |
  (favorite_abcd == "Coffee D"),
  evidence = (expertise == "3")
)
expert4 <- cpquery(
  dag_fit,
  event = (favorite_abcd == "Coffee A") |
  (favorite_abcd == "Coffee D"),
  evidence = (expertise == "4")
)
expert5 <- cpquery(
  dag_fit,
  event = (favorite_abcd == "Coffee A") |
  (favorite_abcd == "Coffee D"),
  evidence = (expertise == "5")
)
expert6 <- cpquery(
  dag_fit,
  event = (favorite_abcd == "Coffee A") |
  (favorite_abcd == "Coffee D"),
  evidence = (expertise == "6")
)
expert7 <- cpquery(
  dag_fit,
  event = (favorite_abcd == "Coffee A") |
  (favorite_abcd == "Coffee D"),
  evidence = (expertise == "7")
)
expert8 <- cpquery(
  dag_fit,
  event = (favorite_abcd == "Coffee A") |
  (favorite_abcd == "Coffee D"),
  evidence = (expertise == "8")
)
expert9 <- cpquery(
  dag_fit,
  event = (favorite_abcd == "Coffee A") |
  (favorite_abcd == "Coffee D"),
  evidence = (expertise == "9")
)
expert10 <- cpquery(
  dag_fit,
  event = (favorite_abcd == "Coffee A") |
  (favorite_abcd == "Coffee D"),
  evidence = (expertise == "10")
)

tibble(
  expertise_level = seq(1, 10, 1),
  probability_of_a_or_d = c(expert1, expert2, expert3, expert4, expert5, expert6, expert7, expert8, expert9, expert10)
) |>
  ggplot(
    aes(
      as.factor(expertise_level),
      probability_of_a_or_d
    )
  ) +
  geom_col(
    aes(fill = as.factor(expertise_level)),
    position = position_dodge()
  ) +
  geom_text(
    aes(
      label = round(probability_of_a_or_d, 2)
    ),
    color = "black",
    vjust = -.3
  ) +
  scale_y_continuous(
    expand = c(0, 0)
  ) +
  scale_y_continuous(
    limits = c(0, 1),
    breaks = seq(0, 1, .1)
  ) +
  labs(
    title = "Probability of Choosing Coffee A or D as Their Favorite Coffee",
    subtitle = "By Level of Self-Defined Expertise",
    x = "Expertise",
    y = "Probability"
  ) +
  scale_fill_brewer(type = "qual", palette = "Set3") +
  theme(
    legend.position = "none"
  )

psych::dummy.code(nona$gender) |> head()

nona$male <- psych::dummy.code(nona$gender)[, 1]
nona$female <- psych::dummy.code(nona$gender)[, 2]
nona$gen_other <- psych::dummy.code(nona$gender)[, 3]

# age
psych::dummy.code(nona$age) |> head()

nona$twenty534 <- psych::dummy.code(nona$age)[, 1]
nona$thirty544 <- psych::dummy.code(nona$age)[, 2]
nona$over44 <- psych::dummy.code(nona$age)[, 3]
nona$under24 <- psych::dummy.code(nona$age)[, 4]

# cup_per_day
psych::dummy.code(nona$cup_per_day) |> head()

nona$cup_2 <- psych::dummy.code(nona$cup_per_day)[, 1]
nona$cup_1orless <- psych::dummy.code(nona$cup_per_day)[, 2]
nona$cup_3ormore <- psych::dummy.code(nona$cup_per_day)[, 3]

# favorite_coffee_drink
psych::dummy.code(nona$favorite_coffee_drink) |> head()

nona$pourover <- psych::dummy.code(nona$favorite_coffee_drink)[, 1]
nona$latte <- psych::dummy.code(nona$favorite_coffee_drink)[, 2]
nona$other <- psych::dummy.code(nona$favorite_coffee_drink)[, 3]
nona$drip <- psych::dummy.code(nona$favorite_coffee_drink)[, 4]
nona$cappuccino <- psych::dummy.code(nona$favorite_coffee_drink)[, 5]
nona$espresso <- psych::dummy.code(nona$favorite_coffee_drink)[, 6]
nona$cortado <- psych::dummy.code(nona$favorite_coffee_drink)[, 7]
nona$americano <- psych::dummy.code(nona$favorite_coffee_drink)[, 8]

# roast_preference
psych::dummy.code(nona$roast_preference) |> head()

nona$roast_light <- psych::dummy.code(nona$roast_preference)[, 1]
nona$roast_medium <- psych::dummy.code(nona$roast_preference)[, 2]
nona$roast_dark <- psych::dummy.code(nona$roast_preference)[, 3]

# expertise          
psych::dummy.code(nona$expertise) |> head()

nona$expert7 <- psych::dummy.code(nona$expertise)[, 1]
nona$expert6 <- psych::dummy.code(nona$expertise)[, 2]
nona$expert5 <- psych::dummy.code(nona$expertise)[, 3]
nona$expert8 <- psych::dummy.code(nona$expertise)[, 4]
nona$expert4 <- psych::dummy.code(nona$expertise)[, 5]
nona$expert3 <- psych::dummy.code(nona$expertise)[, 6]
nona$expert2 <- psych::dummy.code(nona$expertise)[, 7]
nona$expert1 <- psych::dummy.code(nona$expertise)[, 8]
nona$expert9 <- psych::dummy.code(nona$expertise)[, 9]
nona$expert10 <- psych::dummy.code(nona$expertise)[, 10]

id <- nona$submission_id

nona <- nona |>
  select(
    favorite_abcd:expert10
  )

# remove all the reference groups
nona <- nona |>
  select(
    -c(
      male,
      twenty534,
      cup_2,
      pourover,
      roast_light,
      expert5
    )
  )

stan_list <- list(
  N = nrow(nona[,-1]),
  I = ncol(nona[,-1]),
  K = count(nona, favorite_abcd) |> nrow(),

  Y = nona$favorite_abcd,
  X = as.matrix(nona[,-1])
)

glimpse(stan_list)

set.seed(12345)
mod <- cmdstan_model(here::here("random_scripts", "jp_stan2.stan"))

fit <- mod$sample(
  data = stan_list,
  seed = 12345,
  iter_warmup = 2000,
  iter_sampling = 2000,
  # adapt_delta = .90,
  chains = 4,
  # step_size = .01,
  parallel_chains = 8
)

fit$diagnostic_summary()

bn_measure <- summarize_draws(fit$draws(), default_summary_measures())
bn_converge <- summarize_draws(fit$draws(), default_convergence_measures())

bn_converge |>
  arrange(rhat) |>
  mutate(
    across(
      -variable,
      ~round(.x, 2)
    )
  ) |>
  reactable()

bn_measure |>
  filter(
    str_detect(
      variable,
      "theta"
    )
  ) |> 
  mutate(
    across(
      -variable,
      ~round(.x, 2)
    )
  ) |>
  select(
    variable,
    mean,
    sd
  ) |>
  reactable()

bn_measure |>
  filter(
    str_detect(
      variable,
      "^a_"
    ) |
    str_detect(
      variable,
      "^b_"
    )
  ) |> 
  mutate(
    across(
      -variable,
      ~round(.x, 2)
    )
  ) |>
  select(
    variable,
    mean,
    sd
  ) |>
  reactable()