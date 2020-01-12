# 1: Data Cleaning 3

# Objective: 
# 1) Density of products sold.
# 2) Week over week sales, visitors, transactions, AVG order value.
# 3) 4 line charts by week , same four line charts with counts.
# 4) What countries sold the most.
# 5) What items sold the most.
# 6) Perform customer segementation.

## packages

library(tidyverse)
library(lubridate)
library(readr)
library(dplyr)
library(ggplot2)
library(DataExplorer)

# Bring in the data


retail_dataimport <- read_csv("Online_Retail.csv",
                   col_types = cols(
                     InvoiceNo = col_character(),
                     StockCode = col_character(),
                     Description = col_character(),
                     Quantity = col_integer(),
                     InvoiceDate = col_datetime("%m/%d/%Y %H:%M"),
                     UnitPrice = col_double(),
                     CustomerID = col_integer(),
                     Country = col_character()
                   ))

# Rename data for a working version

retail <- retail_dataimport

# Inspect the data

glimpse(retail)

# Create a table to inspect the first 10 observations

retail %>% 
  sample_n(10)

# Inspect dataset for missing values and create visual plot using DataExplorer package

options(repr.plot.width=8, repr.plot.height=3)
plot_missing(retail)

# It appears that CustomerID is most absent in the data.

# Check the dataset to find out which column has the most missing values 
retail %>% 
  map(., ~sum(is.na(.)))

# Ignore observations that hae missing values.

retail <- retail[complete.cases(retail), ]

# Check if missing values have been removed

retail %>% 
  map(., ~sum(is.na(.)))

# Missing values have been successfully removed

# See how many transactions resulted in cancellations

retail %>% 
  filter(grepl("C", retail$InvoiceNo)) %>% 
  summarise(Total = n())

#Remove transactions involving cancellations
retail  <- retail %>% 
  filter(!grepl("C", retail$InvoiceNo)) 

#Remove manually entered description codes. Most likely happened after a return or issue with product.

retail %>% 
  filter(Quantity <= 0) %>% 
  group_by(Description, UnitPrice) %>% 
  summarise(count = n()) %>%
  arrange(desc(count)) %>% 
  ungroup()

# Remove all rows not containing a positive quanitity.

retail  <- retail %>% 
  filter(Quantity > 0)

# Remove all non-product related stock codes

# Non-product related codes
stc <- c('AMAZONFEE', 'BANK CHARGES', 'C2', 'DCGSSBOY', 'DCGSSGIRL',
         'DOT', 'gift_0001_', 'PADS', 'POST')

retail %>%  
  filter(grepl(paste(stc, collapse = "|"), StockCode))  %>% 
  group_by(StockCode, Description) %>% 
  summarise(count = n()) %>%
  arrange(desc(count)) %>% 
  ungroup()

retail <- filter(retail, !grepl(paste(stc, collapse = "|"), StockCode))

# Data Cleaning


# Convert datetime to format yyyy-mm-dd to assess daily sales.
# Also need to change Description, Country, and InvoiceNo to factor for analysis.

retail_cleaned <- retail %>% 
  mutate(day = parse_date(format(InvoiceDate, "%Y-%m-%d")),
         day_of_week = wday(day, label = TRUE),
         time = parse_time(format(InvoiceDate, "%H:%M")),
         month = format(InvoiceDate, "%m"))

# Many of the variables are in <chr> format.
# Also need to change Description, Country, and InvoiceNo to factor for analysis.

retail_cleaned <- retail_cleaned %>% 
  mutate(Description = factor(Description, levels = unique(Description)),
          total_income = Quantity * UnitPrice,
         Country = factor(Country, levels = unique(Country)),
         InvoiceNo = factor(InvoiceNo, levels = unique(InvoiceNo)))

# Save data 

save(retail_cleaned, file = "retail_cleaned.RData")

# Data has been cleaned. Move on to EDA
