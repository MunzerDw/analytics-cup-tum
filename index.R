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

## Max: Every third

df %>%
  as.data.frame() %>%
  summarise_all(funs(sum(is.na(.))))

df$TEST_SET_ID[is.na(df$TEST_SET_ID)] <- 0
df$END_CUSTOMER <- ifelse(is.na(df$END_CUSTOMER), df$CUSTOMER_ID, df$END_CUSTOMER)
sum(is.na(df$END_CUSTOMER))
df$CUSTOMER.x <- NULL
df$CUSTOMER.y <- NULL
df$TEST_SET_ID[is.na(df$TEST_SET_ID)] <- 0
df$ISIC[is.na(df$ISIC)] <- 0 #double check against official DB?
df <- df %>% drop_na(SALES_BRANCH)
df <- df %>% drop_na(REV_CURRENT_YEAR.1)
df <- df %>% drop_na(OFFER_STATUS)
df <- df %>% mutate(OFFER_STATUS = toupper(OFFER_STATUS))
df$OFFER_STATUS <- gsub("WIN", 1, df$OFFER_STATUS)
df$OFFER_STATUS <- gsub("WON", 1, df$OFFER_STATUS)
df$OFFER_STATUS <- gsub("LOSE", 0, df$OFFER_STATUS)
df$OFFER_STATUS <- gsub("LOST", 0, df$OFFER_STATUS)
df$OFFER_STATUS <- as.logical(as.integer(df$OFFER_STATUS))

df %>%
  as.data.frame() %>%
  summarise_all(funs(sum(is.na(.))))
## Jessica: Every second

## Munzer: Every first
## +MO_CREATED_DATE


summary(df)

