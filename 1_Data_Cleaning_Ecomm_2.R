# 1: Data Cleaning Test 2


#https://www.kaggle.com/chrisbow/e-commerce-eda-and-segmentation-with-r - Grab some figures from this.

#https://shiring.github.io/forecasting/2017/05/28/retail_forcasting_part1 - Get inspiration from plots

#https://rstudio-pubs-static.s3.amazonaws.com/430563_d38c12b53d724fa6852949b1f3e4ffbf.html# Use this to set up environment

#file:///C:/Users/dlars/Dropbox/Data_Science/PDG/23_and_me_project/Sales_Operations_Analysis.html


# More ideas can be found from Kaggle kernels

#https://www.kaggle.com/carrie1/ecommerce-data/kernels?sortBy=hotness&group=everyone&pageSize=20&datasetId=1985&language=R



#https://www.kaggle.com/chinarxo/e-commerce-eda-clustering - Use this for customer segmentation

#https://www.kaggle.com/alves9/rfm-customer-segmentation - Use for work with Josh

#https://github.com/satarupa5/online-retail-data-analysis/blob/master/codes/01_datacleaning_retail.R

After the time series stuff. Who are the best customers. WEhat do they buy. Where are they.


# Objective: 
# 1) Density of products sold.
# 2) Week over week sales, visitors, transactions, AVG order value.
# 3) 4 line charts by week , same four line charts with counts.
# 4) What countries sold the most.
# 5) What items sold the most.
# 6) Perform customer segementation.


# You can delete the above packages now

## packages

library(tidyverse)
library(lubridate)
library(readr)
library(dplyr)
library(ggplot2)
library(DataExplorer)

# Bring in the data

retail_dataimport <- read_csv("./Online_Retail.csv")

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

# Data Cleaning

# Many of the variables are in <chr> format.
# Need to change InvoiceDate to datetime object.
# Convert datetime to format yyyy-mm-dd to assess daily sales.
# Also need to change Description, Country, and InvoiceNo to factor for analysis.


retail_cleaned <- retail %>%
  mutate(sale_time_dt = lubridate::mdy_hms(InvoiceDate)) %>%#coerces InvoiceDate in a Date Time format
  mutate(sale_time_2 = as.Date(as.POSIXct(sale_time_dt), format="%m/%d/%Y")) %>% # Allow for view of daily sales
  mutate(Description = factor(Description, levels = unique(Description))) %>% 
  #coerces Description as a factor with each item as individual level of a factor
  
  mutate(Country = factor(Country, levels = unique(Country)))%>%
  mutate(InvoiceNo = factor(InvoiceNo, levels = unique(InvoiceNo))) %>%
  mutate(TotalPrice = Quantity * UnitPrice) 
 
  # Extract and create additional time series information
  
  
  

  
  
  
  
  # Need to split up date and time

# I think this is the best

#https://shiring.github.io/forecasting/2017/05/28/retail_forcasting_part1

# It doesn't need to be perfect. It just needs to be done.
# Look at Pete's stuff for time series.
#Parse into day time and month. I'm pretty close. May need to change the format as to how the data comes in.


# mutate(day = parse_date(format(InvoiceDate, "%Y-%m-%d")) %>% 
# mutate(day_of_week = wday(day, label = TRUE)) %>% 
# mutate(time = parse_time(format(InvoiceDate, "%H:%M"))) %>% 
# mutate(month = format(InvoiceDate, "%m"))

