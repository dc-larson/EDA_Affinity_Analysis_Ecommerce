# 1: Data Cleaning

# This will be the data cleaning of an Ecommerce retail dataset
# Use Links below for guidance
#https://www.kaggle.com/chrisbow/e-commerce-eda-and-segmentation-with-r
#https://shiring.github.io/forecasting/2017/05/28/retail_forcasting_part1
#https://rstudio-pubs-static.s3.amazonaws.com/430563_d38c12b53d724fa6852949b1f3e4ffbf.html#
#file:///C:/Users/dlars/Dropbox/Data_Science/PDG/23_and_me_project/Sales_Operations_Analysis.html

# More ideas can be found from Kaggle kernels
#https://www.kaggle.com/carrie1/ecommerce-data/kernels?sortBy=hotness&group=everyone&pageSize=20&datasetId=1985&language=R



## packages

library(tidyverse)
library(lubridate)


# Bring in the data
retail_dataimport <- read_csv("./Online_Retail.csv")
