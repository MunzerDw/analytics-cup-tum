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
  'TUM/BAML/Training_Data_AC2022/customers.csv'
)
customers

transactions <- read_csv(
  'TUM/BAML/Training_Data_AC2022/transactions.csv'
)
transactions

geo <- read_csv(
  'TUM/BAML/Training_Data_AC2022/geo.csv'
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

## Max: Every third

## Jessica: Every second

## Munzer: Every first
## +MO_CREATED_DATE


#SALES_LOCATION
df_uniq <- unique(df$SALES_LOCATION)
length(df_uniq)
#--> 46 different values

#CUSTOMER.x
df$CUSTOMER.x==df$CUSTOMER.y
#same as CUSTOMER y
#we dont need ids for model

#SERVICE_LIST_PRICE
df$SERVICE_LIST_PRICE
#numeric variable --> standardize TODO
SERVICE_LIST_PRICE <- scale(df$SERVICE_LIST_PRICE)
# Standardization
bla <- df$SERVICE_LIST_PRICE %>% select_if(is.numeric)  %>% mutate_all(scale)



#PRICE_LIST
df$PRICE_LIST
df_uniq <- unique(df$PRICE_LIST)
length(df_uniq)
# 4 different
state = factor(df$PRICE_LIST)
library('fastDummies')
dataf <- dummy_cols(df, select_columns = 'PRICE_LIST')
#remove column
dataf$`PRICE_LIST_Tarif public` <- NULL

#BUSINESS_TYPE 
df$BUSINESS_TYPE 
df_uniq <- unique(df$BUSINESS_TYPE)
df_uniq
length(df_uniq)
#11

#COSTS_PRODUCT_A
df$COSTS_PRODUCT_A
df$COSTS_PRODUCT_B

df$COSTS_PRODUCT_A %>% filter(df$COSTS_PRODUCT_A > 0)


df$OFFER_STATUS
df_uniq <- unique(df$OFFER_STATUS)
df_uniq
#to lowercase
df$OFFER_STATUS = tolower(df$OFFER_STATUS)
df_uniq <- unique(df$OFFER_STATUS)
length(df_uniq)
#5
#lose & lost?
# count frequency
as.data.frame(table(df$OFFER_STATUS))



#SALES_OFFICE
df$SALES_OFFICE
df_uniq <- unique(df$SALES_OFFICE)
df_uniq
length(df_uniq)
#39

df$REV_CURRENT_YEAR

df  %>% mutate(CREATION_YEAR1 = as.numeric(CREATION_YEAR, unit='year'))

summary(df)

