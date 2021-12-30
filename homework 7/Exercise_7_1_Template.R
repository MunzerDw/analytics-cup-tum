# Business Analytics and Machine Learning WS 2021/2022
# Data Preparation - Homework

###############
##  Exercise 7.3  
###############

library(tidyverse)
# Lubridate is a package for handling date-time data
library(lubridate)

###############
##  a)  ##  
###############

# Load data
df<-read_csv('raw_data.csv')
glimpse(df)
# this didn't work --> we have ';' as deliminator, not ','
?read_csv
df <- read_delim('raw_data.csv', delim = ";")
glimpse(df)

## Rename the columns
df <- df %>% rename(
  ID=ID,
  order_date=od,
  delivery_date=dd,
  size=size,
  price=price,
  tax=tax,
  salutation=a6,
  date_of_birth=a7,
  state=a8,
  return_shipment=a9
)
df

## Exercise b)

# salutation, state and return_shipment are nominal attributes
df <- df %>% mutate(
  salutation = factor(salutation,
                      levels = c(2, 3, 4),
                      labels = c("Company", "Mr.", "Mrs.")),
  state = factor(state,
                 labels = c("BW", "BY", "BE", "BB", "HB", "HH", "HE", "MV", "NI", "NW", "RP", "SL", "SN", "ST", "SH", "TH")),
  return_shipment= factor(return_shipment,
                          labels = c("no","yes") )
)
df

## Exercise c)

# size should be convert to upper case with toupper() function
table(df$size)
df <- df %>%  mutate(
  size=toupper(size),
)
table(df$size)

# size is ordinal so we should use an ordered factor
df <- df %>% mutate(
  size=factor(size, ordered = TRUE, levels = c('S', 'M', 'L', 'XL', 'XXL', 'XXXL')),
)
table(df$size)

## Exercise d)

# order date, delivery_date and date_of_birth should be dates
# but only order date was parsed correctly, because the others contain '?'
# dplyr::mutate_at allows us to transform multiple columns at once
# lubridate::as_date provides better date parsing than R's standard as.date

# mutate_at applies all listed functions to each of the given columns where
# the named list of function look like list(desired_column_name = function)
# mutate_at(.tbl, .vars, .funs, ...) where .tbl= dataframe, .vars= vars( selected columns ), .funs = c(functions) 
?mutate_at

df <- df %>% mutate(across(.cols = c(delivery_date, date_of_birth), .fns = as_date))
df
df <- df %>% mutate_at(vars(order_date, delivery_date, date_of_birth), as_date)
df

# all used functions are from the lubridate package.
df <- df %>% mutate_at(
  .vars = vars(od=order_date),
  .funs = list(
    weekday=wday,
    year=year,
    month=month,
    day=day,
    quarter=quarter))

glimpse(df)

df %>% mutate(
  across(
    .cols = c(od=order_date),
    .fns = c(weekday=wday, year=year, month=month, day=day, quarter=quarter)
  )
)
df

## Exercise e)

# Find missing values (only NA)
df %>% summarise(across(.cols = everything(), .fns = ~sum(is.na(.))))

df %>% mutate_all(is.na) %>% summarize_all(sum)

mean_tax <- mean(df$tax, na.rm = T)
mean_tax

mean_price <- mean(df$price, na.rm = T)
mean_price

# Fill the NA values of price and tax with the respective mean value
df <- df %>% mutate(
  # Fill the NA values of tax with the mean value
  tax = if_else(is.na(tax), mean_tax , tax ),
  # Fill the NA values of tax with the mean value
  price = if_else(is.na(price), mean_price , price )
)
df %>% mutate_all(is.na) %>% summarize_all(sum)

# Remove the instances with NA values
df <- df %>% na.omit()
  
df %>% mutate_all(is.na) %>% summarize_all(sum)

## Exercise f)

df <- df %>% mutate(
  delivery_time = delivery_date - order_date) %>% 
  # the previous operation returned a lubridate::duration object (try it!), let's convert to
  # numeric
  mutate(delivery_time = as.numeric(delivery_time, unit = 'days')) %>% 
  # get rid of invalid values
  mutate(delivery_time = if_else(delivery_time < 0.0, NA_real_, delivery_time))

df %>% count(delivery_time, sort = T) 

## Exercise g)

## bins
df <- df %>% mutate(
  dt_binned = case_when(
    delivery_time <= 5 ~ "<= 5d",
    delivery_time > 5 ~ "> 5d",
    is.na(delivery_time) ~ "NA",
  )
)
df$dt_binned

barplot(table(df$dt_binned))
# historgram has a continous x value, unlike barplot

## Exercise h)

# Compute the correlation matrix for numerical attributes
?cor
corr <- df %>% select_if(is.numeric) %>% cor(use = 'pairwise.complete.obs')
corr


?pairs
# Plot the scatterplots
df %>% select_if(is.numeric) %>% pairs

# GGally extends ggplot2 
#install.packages("GGally")
library(GGally)
?ggcorr
# Plot the correlation matrix heatmap
df %>% select_if(is.numeric) %>% ggcorr


## Exercise i)

# Standardization
df <- df %>% select_if(is.numeric) %>% TODO
corr <- 
corr

