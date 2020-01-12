
#2_EDA_UK_Retail_Data

#Purpose: Perform an EDA on UK Online Retail data to get a better understanding 
# of the dataset

#Research Questions:
#1. Which products were purchased the most?
#2. Day of week products are most purchased - Density plot, tab 2
#3. What time do most customers buy, tab3
#4. What is the purchase trend from 2010 - 2011? Take a look at second tab
#5. Which country had the most sales? - Then do total sales minus the UK
#6. Trend of total sales for the top 5 countries.



# Load Packages
suppressMessages(library(tidyverse)) 
suppressMessages(library(purrr)) 
suppressMessages(library(lubridate)) 
suppressMessages(library(funModeling))
suppressMessages(library(ggplot2))
suppressMessages(library(DataExplorer))
suppressMessages(library(data.table))


# Bring in the data

load(file = "retail_cleaned.RData")

dta <- retail_cleaned

#1. Which products were purchased the most?

options(repr.plot.width=6, repr.plot.height=3)
purchase<- dta %>% group_by(StockCode, Description) %>% summarise(count= n()) %>% arrange(desc(count))  
purchase<- head(purchase, n=5)
 
purchase %>% 
   ggplot(aes(x=reorder (Description,count), y=count, fill = count)) + geom_bar(stat= "identity") + coord_flip() + 
  labs(y="Number of items purchased", x="Product")

# So it seems that the white hanging heart t-light holder is the most purchased product 
# with over 2000 units purchased.

#2. What day of week are products most purchased?

# Let's construct a few new variables to understand if there are any trends in when during the week a product is purchased.
#dta$date <- as.Date(dta$InvoiceDate, "%m/%d/%Y")
#dta$time <- as.factor(dta$time)
dta$month <- as.factor(dta$month)
dta$day_of_week <- as.factor(dta$day_of_week)


weekdaySummary <- dta %>%
  group_by(day, day_of_week) %>%
  summarise(revenue = sum(total_income), transactions = n_distinct(InvoiceNo)) %>%
  mutate(aveOrdVal = (round((revenue / transactions),2))) %>%
  ungroup()

head(weekdaySummary, n = 10)


# Lets create a density plot to visualize which days of the week have the most transactions.
ggplot(weekdaySummary, aes(transactions, fill = day_of_week)) + geom_density(alpha = 0.2)

# The data appear skewed. Lets perform a one way ANOVA (Kruskal-Wallis) test to inspect how each day ranks in the number of transactions.

kruskal.test(transactions ~ day_of_week, data = weekdaySummary)

# The test is significant (p < 0.05). There are significant differences in the number of transactions during different days of the week.

#The agricolae package allows us to determine which days of the week had significantly more transactions than the others.

kruskal(weekdaySummary$transactions, weekdaySummary$day_of_week, console = TRUE)

#Results from the Kruskal test show that Thursday has the highest number of transactions, and Sunday having the lowest number of transactions.

# This has important implications for advertising budgets and customer incentives.

# 3. What is the most popular time for making purchases?

dta$Time<- format(dta$InvoiceDate, "%H:%M:%S")
dta$hours <-hour(dta$time)



dta %>% 
  ggplot(aes(x=hours))+
  geom_histogram(stat = "count", fill = "lightblue")+labs(title="What Time Do Most Customers Buy", x = "Time")+
  theme_classic()

# It appears that most items are purchased between 10am and 3pm.

#4. What is the purchase trend from 2010 - 2011? Lets take a look at the overall trends.

options(repr.plot.width=8, repr.plot.height=3)
dta %>%
  group_by(day) %>%
  summarise(revenue = sum(total_income)) %>%
  ggplot(aes(x = day, y = revenue)) + geom_line() + geom_smooth(method = 'auto', se = FALSE) + labs(x = 'Date', y = 'Revenue (£)', title = 'Revenue by Date')

# It appears that sales trended upwards particularly in fall and going into the holiday shopping season.

#5. Which country had the most sales? - Then do total sales minus the UK

countrySummary <- dta %>%
  group_by(Country) %>%
  summarise(revenue = sum(total_income), transactions = n_distinct(InvoiceNo)) %>%
  mutate(aveOrdVal = (round((revenue / transactions),2))) %>%
  ungroup() %>%
  arrange(desc(revenue))

head(countrySummary, n = 10)
unique(countrySummary$Country)

# Well the UK cleared had a lot of transactions an resulting revenue. Let's remove the UK from the dataset and view the data.

top10_countries <- countrySummary%>%
  group_by(Country) %>%
   filter(Country != "United Kingdom") %>% 
  summarize(top_revenue = sum(revenue)/1000000) %>% 
  top_n(n = 10, wt = top_revenue)


top10_countries %>% 
  mutate(Country = fct_reorder(Country, top_revenue)) %>% 
  ggplot(aes(Country, top_revenue))+
  geom_bar(stat = "identity",fill = "steelblue1")+
  theme_minimal()+
  ggtitle('Top 10 Highest Revenue Producing Countries')+
  coord_flip()+
  ylab('Total Sales ($M)')+
  theme(plot.title = element_text(hjust = 0.5))

# So the Neterlands, Ireland, and Gemany lead the way


#6. Trend of total sales for the top 5 countries.

# 10 countries is a lot to view. Let's just take a look at the top 5.

top5_countries<-c("Netherlands","EIRE","Germany","France","Australia")

options(repr.plot.width=6, repr.plot.height=6)
dta %>% filter(Country %in% top5_countries) %>% group_by(day, Country) %>% summarise(Revenue = sum(total_income)) %>%
  ggplot(aes(x=day, y=Revenue, col=Country)) + geom_smooth(method ="loess", se=F)+
  labs(x="Date")

# Netherlands and Australia have similar purchasing patterns.

# Write up summary. EDA complete.
