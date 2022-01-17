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

## Merge data
df_all <- merge(x = df, y = customers, by = "CUSTOMER_ID", all.x = TRUE)
df <- df_all

## Handle offers with one MO_ID only
df$MULTIPLE_OFFER <- if_else(!duplicated(df$MO_ID), 0, 1)

df <- df %>% select(-END_CUSTOMER, -END_CUSTOMER, -SALES_OFFICE, -SO_ID, )
df <- df %>% select(-CUSTOMER.x, -CUSTOMER.y, -COUNTRY.y, -COUNTRY.x, )

df <- df %>% drop_na(REV_CURRENT_YEAR.1)
df <- df %>% drop_na(SALES_LOCATION)
df <- df %>% drop_na(ISIC)


df <- df %>% mutate(
  OFFER_STATUS=toupper(OFFER_STATUS)
)
df$OFFER_STATUS[df$OFFER_STATUS == "LOSE"] <- "LOST"
df$OFFER_STATUS[df$OFFER_STATUS == "WIN"] <- "WON"

#remove Tech EPS 
df <- df[df$TECH !='EPS', ]  

#Create factors
df <- df %>% mutate(
  SALES_LOCATION = factor(SALES_LOCATION, labels = as.vector(unique(df$SALES_LOCATION))[!is.na(as.vector(unique(df$SALES_LOCATION)))]),
  PRICE_LIST = factor(PRICE_LIST, labels = as.vector(unique(df$PRICE_LIST))),
  BUSINESS_TYPE = factor(BUSINESS_TYPE, labels = as.vector(unique(df$BUSINESS_TYPE))),
  OFFER_STATUS = factor(OFFER_STATUS, labels = as.vector(unique(df$OFFER_STATUS))[!is.na(as.vector(unique(df$OFFER_STATUS)))]),
  SALES_BRANCH = factor(SALES_BRANCH, labels = as.vector(unique(df$SALES_BRANCH))[!is.na(as.vector(unique(df$SALES_BRANCH)))]),
  TECH = factor(TECH, labels = as.vector(unique(df$TECH))[!is.na(as.vector(unique(df$TECH)))]),
  OFFER_TYPE = factor(OFFER_TYPE, labels = as.vector(unique(df$OFFER_TYPE))[!is.na(as.vector(unique(df$OFFER_TYPE)))]),
  OWNERSHIP = factor(OWNERSHIP, labels = as.vector(unique(df$OWNERSHIP))[!is.na(as.vector(unique(df$OWNERSHIP)))]),
  CURRENCY = factor(CURRENCY, labels = as.vector(unique(df$CURRENCY))[!is.na(as.vector(unique(df$CURRENCY)))]),
)

# remove negative values
df <- df[df$SERVICE_COST >=0, ]    
df <- df[df$COSTS_PRODUCT_B >=0, ]  

df <- df %>% mutate(REV_CURRENT_YEAR = str_replace_all(df$REV_CURRENT_YEAR, '"', ''))
df <- df %>% mutate(REV_CURRENT_YEAR = str_replace_all(df$REV_CURRENT_YEAR, "\\\\", ''))
df$REV_CURRENT_YEAR <- as.integer(df$REV_CURRENT_YEAR)

df <- df %>% mutate(CREATION_YEAR = str_replace_all(df$CREATION_YEAR, "/", '.'))
df$CREATION_YEAR <- as.Date(df$CREATION_YEAR, format="%d.%m.%Y")

df %>% mutate(CREATION_YEAR = str_replace_all(df$CREATION_YEAR, "/", '.'))

#remove Seconds
for (i in 1:length(df$SO_CREATED_DATE)) {
  if (nchar(df$SO_CREATED_DATE[i], type = "chars") > 16) {
    df$SO_CREATED_DATE[i] <- substring(df$SO_CREATED_DATE[i],1,nchar(df$SO_CREATED_DATE[i])-3)
  }}
for (i in 1:length(df$MO_CREATED_DATE)) {
  if (nchar(df$MO_CREATED_DATE[i], type = "chars") > 16) {
    df$MO_CREATED_DATE[i] <- substring(df$MO_CREATED_DATE[i],1,nchar(df$MO_CREATED_DATE[i])-3)
  }}

#als Datum
df$SO_CREATED_TIME <- as_datetime(df$SO_CREATED_DATE, format = '%d.%m.%Y %H:%M') 
for (i in 1:length(df$SO_CREATED_TIME)) {
  if(is.na(df$SO_CREATED_TIME[i])) {
    df$SO_CREATED_TIME[i] <- as_datetime(df$SO_CREATED_DATE[i], format = '%Y-%m-%d %H:%M') 
  }}
df$SO_CREATED_DATE <- df$SO_CREATED_TIME
df$SO_CREATED_TIME <- NULL 


df$MO_CREATED_TIME <- as_datetime(df$MO_CREATED_DATE, format = '%d.%m.%Y %H:%M') 
for (i in 1:length(df$MO_CREATED_TIME)) {
  if(is.na(df$MO_CREATED_TIME[i])) {
    df$MO_CREATED_TIME[i] <- as_datetime(df$MO_CREATED_DATE[i], format = '%Y-%m-%d %H:%M') 
  }}
df$MO_CREATED_DATE <- df$MO_CREATED_TIME
df$MO_CREATED_TIME <- NULL

#dates

df$MO_Weekday <- format(df$MO_CREATED_DATE, format="%a")
df$SO_Weekday <- format(df$SO_CREATED_DATE, format="%a")

df$SO_day <- format(df$SO_CREATED_DATE, format = "%d")
df$MO_day <- format(df$MO_CREATED_DATE, format = "%d")

df$SO_month <- format(df$SO_CREATED_DATE, format = "%m")
df$MO_month <- format(df$MO_CREATED_DATE, format = "%m")

df$SO_year <- format(df$SO_CREATED_DATE, format = "%Y")
df$MO_year <- format(df$MO_CREATED_DATE, format = "%Y")

#SO_Time <- hour(df$SO_CREATED_DATE)*60 + minute(df$SO_CREATED_DATE)
#MO_Time <- hour(df$MO_CREATED_DATE)*60 + minute(df$MO_CREATED_DATE)

df$MO_hours <- format(df$MO_CREATED_DATE, format = "%H")
df$SO_hours <- format(df$SO_CREATED_DATE, format = "%H")

df <- df %>% mutate(
  #SO_Time = factor(SO_Time, labels = as.vector(unique(df$SO_Time))[!is.na(as.vector(unique(df$SO_Time)))]),
  #MO_Time = factor(MO_Time, labels = as.vector(unique(df$MO_Time))[!is.na(as.vector(unique(df$MO_Time)))]),
  MO_Weekday = factor(MO_Weekday, labels = as.vector(unique(df$MO_Weekday))[!is.na(as.vector(unique(df$MO_Weekday)))]),
  SO_Weekday = factor(SO_Weekday, labels = as.vector(unique(df$SO_Weekday))[!is.na(as.vector(unique(df$SO_Weekday)))]),
  
)


train <- df[is.na(df$TEST_SET_ID), ]
submission <- df[!is.na(df$TEST_SET_ID), ]

train <- train %>% select(-TEST_SET_ID, )


