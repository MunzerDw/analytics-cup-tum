# Business Analytics and Machine Learning WS 2021/2022
# Data Preparation - Exercise

#################
##  Exercise 7.2  
#################
library(tidyverse)
library(lubridate)
library(tidymodels)

set.seed(2021)

# Reat the power_plants dataset
plants <- read_csv(
  'power_plants.csv',
  col_types = cols(
    other_fuel3 = col_character(),
    wepp_id = col_character(),
    production_gwh = col_double()
  )
)

# Remove irrelevant variables
plants <- plants %>% select(-source, -url, -geolocation_source, -wepp_id,)

# Basic preprocessing based on data exploration
plants <- plants %>% 
  mutate(
    commissioning_year = as_date(date_decimal(commissioning_year))
  ) %>% 
  rename(
    id = gppd_idnr,
    commission_date = commissioning_year
  )

# Bring data in wide format
df <- plants %>% pivot_wider(
  names_from = generation_year,
  names_prefix = 'generation_gwh_',
  values_from = production_gwh
)

# In this special case, we know the train/test split 
# We want to predict the power generation for those entries that do not have a value
train <- df %>% filter(!is.na(generation_gwh_2017))
test <- df %>% anti_join(train)

###############
##  a)  ##  
###############

# Read country data
countries <- read_csv(
  'country.csv') %>%
  pivot_longer(cols = -country,
               names_to = "fuel",
               values_to = "generation_gwh") %>%
  mutate(generation_year = 2014)

# Join country data with plants
df <- df %>%
  left_join(
    countries %>% rename(country_gen_by_fuel=generation_gwh),
    by=c("primary_fuel" = "fuel")
  ) %>% 
  left_join(
    countries %>% filter(fuel == 'Total') %>% transmute(country, country_gen_total = generation_gwh),
    by = c("country_long" = "country")
  )

# Create additional interesting variables
df <- df %>% 
  mutate(
    country_gen_by_fuel = 1000/24/365.25 * country_gen_by_fuel,
    country_gen_total   = 1000/24/365.25 * country_gen_total
  ) %>% 
  mutate(cap_share_of_country_gen_by_fuel = capacity_mw/country_gen_by_fuel,
         cap_share_of_country_gen_total = capacity_mw / country_gen_total
  ) %>%
  mutate(
    cap_share_of_country_gen_by_fuel = if_else(is.infinite(cap_share_of_country_gen_by_fuel), 1.0, cap_share_of_country_gen_by_fuel)
  )

# Remove uninteresting columns
df <- df %>% select(-country, -country_long, -name, -latitude, -longitude,
                    -other_fuel2, -other_fuel3, -owner,
                    -generation_gwh_2013, -generation_gwh_2014, -generation_gwh_2015, -generation_gwh_2016,
                    -country_gen_by_fuel, -country_gen_total)

###############
##  b)  ##  
###############

rec <- recipe(
  #specific predictors, target and data to enable code autocompletion
  generation_gwh_2017 ~ ., data = train) %>% 
  # tell tidymodels that `id` is an ID and should not be used in any model
  update_role(id, new_role = "ID") %>% 
  # turn dates into decimals, e.g. 2019-07-01 becomes 2019.5
  step_mutate_at(where(is.Date), fn=decimal_date) %>% 
  step_mutate_at(cap_share_of_country_gen_by_fuel, cap_share_of_country_gen_total,
                 # this is a shorthand notation for (function(x) {replace_na(x,0)})
                 fn= ~replace_na(.,0)) %>%
  # impute all other numeric columns with their mean
  step_impute_mean(all_numeric()) %>% 
  # impute all other nominal (character + factor) columns with the value "none"
  step_unknown(all_nominal(), new_level = "none") %>% 
  # convert all strings to factors
  step_string2factor(all_nominal(), -all_outcomes(), -has_role("ID")) %>% 
  # remove constant columns
  step_zv(all_predictors())

rec

###############
##  c)  ##  
###############

wflow <- 
  workflow() %>%
  add_recipe(rec) 

wflow