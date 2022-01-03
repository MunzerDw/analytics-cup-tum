library(tidyverse)
library(lubridate)
# data exploration
library(summarytools) # for user-friendly html summaries of data
library(ggmap) # for plotting data on a map
# for meta-ml
library(tidymodels)

#library(ranger) # ranger random forest model. Import not explicitly necessary, will be loaded by parsnips

# let's set some global options
options(dplyr.width = Inf) # show all columns when printing to console
theme_set(theme_minimal()) # select a lightweight ggplot theme for cleaner plotting
set.seed(2022)

# Read

customers <- read_csv(
  'Training_Data_AC2022/customers.csv'
)
customers

transactions <- read_csv(
  'Training_Data_AC2022/transactions.csv'
)
transactions

geo <- read_csv(
  'Training_Data_AC2022/geo.csv'
)
geo

# Preprocessing

## Fix country string

customers$COUNTRY <- gsub('Switzerland', 'CH', customers$COUNTRY)
customers$COUNTRY <- gsub('France', 'FR', customers$COUNTRY)

## Fix customer string

transactions <- transactions %>% mutate(CUSTOMER = str_replace_all(transactions$CUSTOMER, '"', ''))
transactions <- transactions %>% mutate(CUSTOMER = str_replace_all(transactions$CUSTOMER, "\\\\", ''))

transactions <- transactions %>% 
  mutate(
    CUSTOMER = as.double(CUSTOMER)
  )

## Create customer id

customers$CUSTOMER_ID <- paste(customers$CUSTOMER,customers$COUNTRY,sep="")

## Merge

df <- merge(x = transactions, y = geo, by = "SALES_LOCATION", all.x = TRUE)

## Create customer id

df$CUSTOMER_ID <- paste(df$CUSTOMER,df$COUNTRY,sep="")

df_all <- merge(x = df, y = customers, by = "CUSTOMER_ID", all.x = TRUE)
df <- df_all
df[500,]

## Handle offers with one MO_ID only
# df[duplicated(df[,c('MO_ID')]),]
df$MULTIPLE_OFFER <- if_else(!duplicated(df$MO_ID), 0, 1)

# which column to drop?
# END_CUSTOMER
df <- df %>% select(-END_CUSTOMER, -END_CUSTOMER, -SALES_OFFICE, -SO_ID, )
df <- df %>% select(-CUSTOMER.x, -CUSTOMER.y, -COUNTRY.y, -COUNTRY.x, )

# which rows to drop?
# SALES_LOCATION,ISIC
df <- df %>% drop_na(REV_CURRENT_YEAR.1)
df <- df %>% drop_na(SALES_LOCATION)
df <- df %>% drop_na(ISIC)

train <- df[is.na(df$TEST_SET_ID), ]
submission <- df[!is.na(df$TEST_SET_ID), ]

train <- train %>% select(-TEST_SET_ID, )

#####################################

# "recipe"

## which columns to drop?
## 1. drop or not?
df <- df %>% select(-source,)
## 2. convert to right type (factor, num, date)
# factor
df <- df %>% mutate(
  SALES_LOCATION = factor(SALES_LOCATION, labels = as.vector(unique(df$SALES_LOCATION))[!is.na(as.vector(unique(df$SALES_LOCATION)))]),
  PRICE_LIST = factor(PRICE_LIST, labels = as.vector(unique(df$PRICE_LIST))),
  BUSINESS_TYPE = factor(BUSINESS_TYPE, labels = as.vector(unique(df$BUSINESS_TYPE))),
  OFFER_STATUS = factor(OFFER_STATUS, labels = as.vector(unique(df$OFFER_STATUS))[!is.na(as.vector(unique(df$OFFER_STATUS)))]),
  SALES_BRANCH = factor(SALES_BRANCH, labels = as.vector(unique(df$SALES_BRANCH))[!is.na(as.vector(unique(df$SALES_BRANCH)))]),
)
# date
df <- df %>% mutate(across(.cols = c(MO_CREATED_DATE, SO_CREATED_DATE), .fns = as_date))
df <- df %>% mutate(
  across(
    .cols = c(MCD=MO_CREATED_DATE),
    .fns = c(WEEKDAY=wday, YEAR=year, MONTH=month, DAY=day, QUARTER=quarter)
  )
)
## 3. check how many different values
# factor
df_uniq <- unique(df$OFFER_STATUS)
length(df_uniq)
# numeric
summary(df$SERVICE_COST)
# date
summary(df$MO_CREATED_DATE)
## 4. get number of NAs
df %>%
  as.data.frame() %>%
  summarise_all(funs(sum(is.na(.))))
# drop rows
df <- df %>% drop_na(REV_CURRENT_YEAR.1)
# drop column
df$CUSTOMER.x <- NULL
# replace
df$END_CUSTOMER <- ifelse(is.na(df$END_CUSTOMER), df$CUSTOMER_ID, df$END_CUSTOMER)

##########################################

# TODO

## research customer ID, MO_ID
# Max and Jessica
## clean individual columns if needed (typos etc.)
## type cast columns
# Sarah and Munzer
## exploratory Analysis
## multicolinearity
## correlation for each variable
## ask about IDs
## ask about END_CUSTOMER

##########################################

# old code

######## Munzer

# SALES_LOCATION - chr (nominal)

# SERVICE_LIST_PRICE - num
## no NAs

# PRICE_LIST - chr (nominal)

# MO_CREATED_DATE - date

# SO_CREATED_DATE - date

# BUSINESS_TYPE - chr (nominal)

# OFFER_STATUS - chr (nominal)
df <- df %>% mutate(
  OFFER_STATUS=toupper(OFFER_STATUS)
)
df$OFFER_STATUS[df$OFFER_STATUS == "LOSE"] <- "LOST"
df$OFFER_STATUS[df$OFFER_STATUS == "WIN"] <- "WON"

# TEST_SET_ID - id/ num

# SALES_BRANCH - chr (nominal)

# chr (nominal)
df <- df %>% mutate(
  SALES_LOCATION = factor(SALES_LOCATION, labels = as.vector(unique(df$SALES_LOCATION))[!is.na(as.vector(unique(df$SALES_LOCATION)))]),
  PRICE_LIST = factor(PRICE_LIST, labels = as.vector(unique(df$PRICE_LIST))),
  BUSINESS_TYPE = factor(BUSINESS_TYPE, labels = as.vector(unique(df$BUSINESS_TYPE))),
  OFFER_STATUS = factor(OFFER_STATUS, labels = as.vector(unique(df$OFFER_STATUS))[!is.na(as.vector(unique(df$OFFER_STATUS)))]),
  SALES_BRANCH = factor(SALES_BRANCH, labels = as.vector(unique(df$SALES_BRANCH))[!is.na(as.vector(unique(df$SALES_BRANCH)))]),
)

# date
df <- df %>% mutate(across(.cols = c(MO_CREATED_DATE, SO_CREATED_DATE), .fns = as_date))
df <- df %>% mutate(
  across(
    .cols = c(MCD=MO_CREATED_DATE),
    .fns = c(WEEKDAY=wday, YEAR=year, MONTH=month, DAY=day, QUARTER=quarter)
  )
)
df <- df %>% mutate(
  across(
    .cols = c(SCD=SO_CREATED_DATE),
    .fns = c(WEEKDAY=wday, YEAR=year, MONTH=month, DAY=day, QUARTER=quarter)
  )
)

###### Max

sort(unique(df$END_CUSTOMER)) #na no yes
unique(df$MATERIAL_COST) #0
unique(df$ISIC) # NA, 0
unique(df$TECH) #EPS
unique(df$COSTS_PRODUCT_A)         
unique(df$OFFER_STATUS)
sort(unique(df$TEST_SET_ID)) #NA Capitalization
df$TEST_SET_ID[is.na(df$TEST_SET_ID)] <- 0  # replace NA with 0
unique(df$SALES_BRANCH) #NA, not all are regions
unique(df$REV_CURRENT_YEAR.1) #NA
unique(df$OWNERSHIP) #NA
unique(df$MULTIPLE_OFFER)