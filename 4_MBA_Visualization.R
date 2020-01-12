# 4_Market_Basket_Analysis

#Purpose: Perform a Market Basket Analysis (MBA) to visualize recommendations
# and which products were purchased together.

# Load Packages
library(readxl)
library(readr)
library(arules)
library(arulesViz)
library(magrittr)
library(dplyr)
library(lubridate)
library(forcats)
library(ggplot2)
library(data.table)

# install mbar package for data pre-processing courtesy of Rsquaredacademy
#install.packages("devtools")
devtools::install_github("rsquaredacademy/mbar")


# Bring in the data

load(file = "retail_cleaned.RData")

retail <- retail_cleaned

# Process data for use in arules package.
# Use mbar_prep_data() from the mbar package to reshape the data so 
# that there is one row per transaction with items across columns excluding the column names

mba_retail<-retail_cleaned

transactions <- mbar_prep_data(mba_retail, InvoiceNo, Description)
head(transactions)

# Write to .csv to be pulled into arules package

write.csv(transactions,"transaction_data.csv", quote = FALSE, row.names = FALSE)

basket_data <- read.transactions("transaction_data.csv", format = "basket", 
                               sep = ",")
basket_data

summary(basket_data)


# Item Frequency Plot #

# Generate an Item Frequency Plot to visualize the most frequent items in the dataset##

itemFrequencyPlot(basket_data, topN = 10, type = 'absolute')

if (!require("RColorBrewer")) {
  # install color package of R
  install.packages("RColorBrewer")
  #include library RColorBrewer
  library(RColorBrewer)
}
itemFrequencyPlot(tr,topN=20,type="absolute",col=brewer.pal(8,'Pastel2'), main="Absolute Item Frequency Plot")

itemFrequencyPlot(basket_data, topN = 10, type = 'relative')

# This didn't work above and it's not necesary.
# Generate Rules

rules <- apriori(basket_data, parameter = list(supp=0.009, conf=0.8, 
                                               target = "rules", maxlen = 4))
summary(rules)

# Inspect Rules

basket_rules <- sort(rules, by = 'confidence', decreasing = TRUE)
inspect(basket_rules[1:10])

# Inspect Redundant Rules

inspect(rules[is.redundant(rules)])

# Inspect non-redundant rules

inspect(rules[!is.redundant(rules)])

# Skip section of what purchases of X influenced Y #

# Top Rules # Take a look at the top rules

# Support

supp_rules <- sort(rules, by = 'support', decreasing = TRUE)
top_rules <- supp_rules[1:10]
inspect(top_rules)

# Confidence

conf_rules <- sort(rules, by = 'confidence', decreasing = TRUE)
top_rules <- conf_rules[1:10]
inspect(top_rules)

# Lift

lift_rules <- sort(rules, by = 'lift', decreasing = TRUE)
top_rules <- lift_rules[1:10]
inspect(top_rules)

## Visualization

# Scatter Plot - Make a plot of support and confidence

plot(basket_rules)

# Network Plot
# We are most interested in visualizing results in a network plot
plot(top_rules, method = 'graph')

## Notes
# Directionality of rules is lost when using lift.
# Confidence of a measure of its own can be misleading.


# Filter rules with confidence greater than 0.4 or 40%
subRules<-top_rules[quality(top_rules)$confidence>0.4]

# Make another network plot
plot(subRules, method = "graph",  engine = "htmlwidget")

# Make an interactive plot using igraph


##------------> Consider new section <-----------#
# Implementing Apriori Algorithm
rules <- apriori(basket_data, parameter = list(support = 0.009, confidence = 0.8, target="rules",maxlen=4))

# Remove redundant rule    
rules <- rules[!is.redundant(rules)]
rules_dt <- data.table( lhs = labels( lhs(rules) ), 
                        rhs = labels( rhs(rules) ), 
                        quality(rules) )[ order(-lift), ]
head(rules_dt,5)



# Tweak the number of items and rule sets below. Just look at 5


subrules2 <- head(sort(rules, by="confidence"), 10)
ig <- plot( subrules2, method="graph", control=list(type="items"))

ig_df <- toVisNetworkData(ig, idToLabel = FALSE)

visNetwork(ig_df$nodes, ig_df$edges) %>%
  visNodes(size = 20) %>%
  visLegend() %>%
  visEdges(smooth = FALSE) %>%
  visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE) %>%
  visInteraction(navigationButtons = TRUE) %>%
  visEdges(arrows = 'from') %>%
  visPhysics(
    solver = "barnesHut",
    maxVelocity = 35,
    forceAtlas2Based = list(gravitationalConstant = -6000))

#Will need to look at association rules and strengths. Nodes are not revealing much

plot(subRules,method="graph",engine = 'interactive',shading=NA)



