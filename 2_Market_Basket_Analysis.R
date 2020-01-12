# Market Basket Analysis of UK Ecommerce Data

## packages

library(tidyverse)
library(lubridate)
library(readr)
library(dplyr)
library(ggplot2)
library(DataExplorer)

# Bring in the data

load(file = "retail_cleaned.RData")

dta <- retail_cleaned
