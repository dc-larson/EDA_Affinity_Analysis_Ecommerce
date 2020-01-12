# 3_Market_Basket_Analysis

#Purpose: Perform a Market Basket Analysis (MBA) to build a product recommendation engine.

# Load Packages
library(data.table)
library(tidyverse)            
library(knitr)
library(recommenderlab)
library(purrr)

# Bring in the data

load(file = "retail_cleaned.RData")

retail <- retail_cleaned

# Create a Rating Matrix

# Arrange purchase history into a rating matrix. This places orders in rows and products in columns.
#This can be problematic if an order contains the same item more than once.
# Remove duplicated orders.

retail <- retail %>% 
  # create unique identifier
  mutate(InNo_Desc = paste(InvoiceNo, Description, sep = ' ')) 
# filter out duplicates and drop unique identifier
retail <- retail[!duplicated(retail$InNo_Desc), ] %>% 
  select(-InNo_Desc)

## -----------> Construct a Rating Matrix <--------------------

ratings_matrix <- retail %>%
  # Select only needed variables
  select(InvoiceNo, Description) %>% 
  # Add a column of 1s
  mutate(value = 1) %>%
  # Spread into user-item format
  spread(Description, value, fill = 0) %>%
  select(-InvoiceNo) %>%
  # Convert to matrix
  as.matrix() %>%
  # Convert to recommenderlab class 'binaryRatingsMatrix'
  as("binaryRatingMatrix")

# Create a training and test dataset

scheme <- ratings_matrix %>% 
  evaluationScheme(method = "cross",
                   k      = 5, 
                   train  = 0.8,  
                   given  = -1)

scheme

# Set up algorithms #

# Recommenderlab can estimate between a series of algorithms. Create a list of algorithms needed for estimation.

algorithms <- list(
  "association rules" = list(name  = "AR", param = list(supp = 0.01, conf = 0.01)),
  "random items"      = list(name  = "RANDOM",  param = NULL),
  "popular items"     = list(name  = "POPULAR", param = NULL),
  "item-based CF"     = list(name  = "IBCF", param = list(k = 5)),
  "user-based CF"     = list(name  = "UBCF", param = list(method = "Cosine", nn = 500))
)

# Now pass scheme and algorithms to evaluate(), select type=topNlist, this evaluates a topNlist of product recommendations.
#Specify how many parameters to calculate.

results <- recommenderlab::evaluate(scheme, 
                                    algorithms, 
                                    type  = "topNList", 
                                    n     = c(1, 3, 5, 10, 15, 20)
)

save(results, file = "MBA_results.RData")

#load(file = "MBA_results.RData")

# Inspect results from popular model.

results$'popular' %>% 
  getConfusionMatrix() 


plot(results)

# It appears that item-based CF is the winner.

# Sort results into a tidy format for easy plotting.

#Arrange confusion matrix ouput for one model in convenient format.

# Pull into a list all confusion matrix information for one model 
tmp <- results$`user-based CF` %>%
  getConfusionMatrix()  %>%  
  as.list() 
# Calculate average value of 5 cross-validation rounds 
as.data.frame( Reduce("+",tmp) / length(tmp)) %>% 
  # Add a column to mark the number of recommendations calculated
  mutate(n = c(1, 3, 5, 10, 15, 20)) %>%
  # Select only columns needed and sorting out order 
  select('n', 'precision', 'recall', 'TPR', 'FPR') 

# Put previous steps into the formula below
avg_conf_matr <- function(results) {
  tmp <- results %>%
    getConfusionMatrix()  %>%  
    as.list() 
  as.data.frame( Reduce("+",tmp) / length(tmp)) %>% 
    mutate(n = c(1, 3, 5, 10, 15, 20)) %>%
    select('n', 'precision', 'recall', 'TPR', 'FPR') 
  
  
}

# Use the map() function in purrr package to bring everything into a tidy format

# Using map() to iterate function across all models
results_tbl <- results %>%
  map(avg_conf_matr) %>% 
  # Turning into an unnested tibble
  enframe() %>%
  # Unnesting to have all variables on same level
  unnest()

results_tbl

# ROC Curve

# Compare classification models using an ROC curve.
# This plots a TPR (True Posititive Rate) and a FPR (False Positive Rate)

# IBCF is the best model as it has the best TPR.

# Now plot results.

results_tbl %>%
  ggplot(aes(FPR, TPR, colour = fct_reorder2(as.factor(name), FPR, TPR))) +
  geom_line() +
  geom_label(aes(label = n))  +
  labs(title = "ROC curves",
       colour = "Model") +
  theme_grey(base_size = 14)

# Precision Recall Curve

# Compae the performance of classification models by plotting a Precision vs. Recall Curve.
# Precision shows how sensitive different models are to FPR.
# Recall looks at how sensitie models are to False Negatives.

# We want to maximize Recall for the same Precision.

# Plot Precision vs. Recall

results_tbl %>%
  ggplot(aes(recall, precision, 
             colour = fct_reorder2(as.factor(name),  precision, recall))) +
  geom_line() +
  geom_label(aes(label = n))  +
  labs(title = "Precision-Recall curves",
       colour = "Model") +
  theme_grey(base_size = 14)

# The plot shows that IBCF is the best model because it has a higher Recall at an level of Precision.
# The IBCF model minimizes False Negatives for all levels of False Positives.

# Creating predictions for a new user.

# Generate predictions using the best performing model.
# Create a dummy purchase order to do this.

# Create a string of 5 different products.

dummycustomer_order <- c("WHITE HANGING HEART T-LIGHT HOLDER	",
                    "	RED WOOLLY HOTTIE WHITE HEART",
                    "LUNCH BOX I LOVE LONDON",
                    "WHITE METAL LANTERN",
                    "SAVE THE PLANET MUG")

# Put this into a format that recommenderlab accepts.

new_order_rat_matrx <- retail %>% 
  # Select item descriptions from retail dataset
  select(Description) %>% 
  unique() %>% 
  # Add a 'value' column with 1's for customer order items
  mutate(value = as.numeric(Description %in% dummycustomer_order)) %>% 
  # Spread into sparse matrix format
  spread(key = Description, value = value) %>% 
  # Change to a matrix
  as.matrix() %>% 
  # Convert to recommenderlab class 'binaryRatingsMatrix'
  as("binaryRatingMatrix")

# Create a Recommender Engine. Use getData to retrieve training data and set the method
# to IBCF (best performing model). 

recomm <- Recommender(getData(scheme, 'train'), 
                      method = "IBCF",  
                      param = list(k = 5))

recomm

# Pass Recommender and the made up dummy order to the predict function.
#Then create a list of the top 5 products recommendations for the customer.

pred <- predict(recomm, 
                newdata = new_order_rat_matrx, 
                n       = 5)

as(pred,'list')

# An inspection of the list reveals the top 5 product recommendations.