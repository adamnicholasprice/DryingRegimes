#####################################################################
##
## Script name: DryingRegimes_cForest.R
##
## Author: Sam Zipper
## Edits: Adam Price for drying regimes study
##
## Github repo: https://github.com/dry-rivers-rcn/IntermittencyTrends
##
## Date Created: 2020-12-13
##
## Copyright (c) Adam N. Price, 2020
## Email: adnprice@ucsc.edu
##
############################# Description ##########################
##
## # extract variable importance - conditional variable importance from party package
#  useful blog post: https://www.r-bloggers.com/be-aware-of-bias-in-rf-variable-importance-metrics/
#   - suggests permutation importance as more robust
#   - this accounts for highly correlated predictor variables, but is super slow
#  useful slides by authors of party package: https://www.statistik.uni-dortmund.de/useR-2008/slides/Strobl+Zeileis.pdf
##   
##
############################# Packages #############################
library(tidymodels)
library(ranger)
library(partykit)
library(future.apply)
library(ggplot2)
############################# Code ################################


## 01_RandomForest_PreliminaryVariableImportance.R
#' This script selects the variables that will be used for each random forest model
#' with the following approach:
#'   - Build model with all predictor variables
#'   - Repeat using 80% of reference gages in each sample
#' This variable importance will be used to build models.
#' 
#' A total of 21 models will be built: (6 regions + National) * (3 metrics) = 21 models

# source(file.path("code", "paths+packages.R"))




#######################################################
################# Step: Load data         
#######################################################


df  = read.csv("data/kmeans_NoFreq.csv")


#######################################################
################# Step:     set up predictions       
#######################################################


metrics <- "kmeans"

# all possible predictors
lulc.pred = c('lulc_water_prc','lulc_dev_prc','lulc_forest_prc','lulc_barren_prc','lulc_grass_prc','lulc_ag_prc','lulc_wetland_prc')

phys.pred = c('DRAIN_SQKM','SNOW_PCT_PRECIP','GEOL_REEDBUSH_DOM','FRESHW_WITHDRAWAL',
              'AWCAVE','PERMAVE','CLAYAVE','SILTAVE','SANDAVE',
              'TOPWET','ELEV_MEAN_M_BASIN','porosity','storage_m')
clim.pred = c('P_mm','PET_mm','SWE_mm','melt_mm','Tmax_C',
              'P_90','PET_90','Tmax_90','melt_90',
              'P.PET','P.PET90')

predictors_all = read.csv("data/rfe_predictors.csv")[[1]]



#######################################################
################# Step: Select all predictors and filter data
#######################################################

df = df %>% 
  rename(TOPWET = TOPWET.x,
         index = X) %>% 
  select("index","gage","kmeans",all_of(predictors_all)) %>%
  subset(complete.cases(.))

df$GEOL_REEDBUSH_DOM = as.factor(df$GEOL_REEDBUSH_DOM)
df$kmeans = as.factor(df$kmeans)

# Subset data to training and testing
set.seed(42)
sub.split = initial_split(df,prop=.8)
sub.train = training(sub.split)
sub.test = testing(sub.split)

#######################################################
################# Step: Calculate conditional inference trees 
#######################################################
## load hyperparameter tuning results
tune_res_all <- readr::read_csv('data/tuning_pararams.csv')

# Define parameters for model
tune_res <-
  tune_res_all %>%
  subset(.metric == "mn_log_loss") %>%
  dplyr::filter(mean == min(mean))



# number of cores to use
ncores <- (parallel::detectCores() - 2)

fit_rf <- partykit::cforest(
  kmeans ~ .,
  data = dplyr::select(sub.train, -gage,-index),
  control = ctree_control(mincriterion = 0.95),
  cores = ncores,
  ntree = tune_res$trees[1], 
  mtry = tune_res$mtry[1]
)


vi <- partykit::varimp(fit_rf, 
                       conditional = F, 
                       cores = ncores)

fit_rf_imp_i <- tibble::tibble(predictor = names(vi),
                               ImpCondPerm = vi)

# write csv file separately for each metric and region
fit_rf_imp_i %>% 
  readr::write_csv(file ='data/cforest_Importance.csv')

    
