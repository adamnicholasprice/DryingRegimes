#####################################################################
##
## Script name: dryingMetrics_validateKmeans.R
##
## Author: Adam N. Price
##
## Date Created: 2020-12-02
##
## Copyright (c) Adam N. Price, 2020
## Email: adnprice@ucsc.edu
##
############################# Description ##########################
##
## 
##   
##
############################# Packages #############################

library(h2o)
library(tidyverse)

############################# Code ################################

clust = read.csv('data/kmeans.csv')




hyper_params <- list( k = c(1, 2, 3,4,5,6,7,8,9,10,11,12,13,14,15) )

# this example uses cartesian grid search because the search space is small
# and we want to see the performance of all models. For a larger search space use
# random grid search instead: list(strategy = "RandomDiscrete")
grid <- h2o.grid(x = predictors, training_frame = train, validation_frame = valid,
                 algorithm = "kmeans", grid_id = "seeds_grid", hyper_params = hyper_params,
                 search_criteria = list(strategy = "Cartesian"), seed = 1234)

## Sort the grid models by TotSS
sorted_grid <- h2o.getGrid("seeds_grid", sort_by  = "tot_withinss", decreasing = F)
sorted_grid