#####################################################################
##
## Script name: dryingMetrics_compileData.R
##
## Author: Adam N. Price
##
## Date Created: 2020-08-14
##
## Copyright (c) Adam N. Price, 2020
## Email: adnprice@ucsc.edu
##
############################# Description ##########################
##
## Pulls all relevent data files and merges into 3 data files for analysis
##   - 
##
############################# Packages #############################

library(tidyverse)

############################# Code ################################

event = read.csv('../data/metrics_by_event.csv')
