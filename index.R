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

customers$COUNTRY <- gsub('Switzerland', 'CH', customers$COUNTRY)
customers$COUNTRY <- gsub('France', 'FR', customers$COUNTRY)

transactions <- transactions %>% mutate(CUSTOMER = str_replace_all(transactions$CUSTOMER, '"', ''))
transactions <- transactions %>% mutate(CUSTOMER = str_replace_all(transactions$CUSTOMER, "\\\\", ''))

transactions <- transactions %>% 
  mutate(
    CUSTOMER = as.double(CUSTOMER)
  )

customers$CUSOMTER_ID <- paste(customers$CUSTOMER,customers$COUNTRY,sep="")
# Merge

df <- merge(x = transactions, y = geo, by = "SALES_LOCATION", all.x = TRUE)
df$CUSOMTER_ID <- paste(df$CUSTOMER,df$COUNTRY,sep="")
df[500,]

df_all <- merge(x = df, y = customers, by = "CUSOMTER_ID", all.x = TRUE)


