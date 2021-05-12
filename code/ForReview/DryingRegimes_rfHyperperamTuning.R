#####################################################################
##
## Script name: DryingRegimes_rfHyperparamTuning.R
##
## Author: Adam N. Price
##
## Date Created: 2020-12-13
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
library(tidymodels)
library(ggplot2)
library()
############################# Code ################################


df = read.csv('data/kmeans_NoFreq.csv')
metrics <- "kmeans"

# all possible predictors
lulc.pred = c('lulc_water_prc','lulc_dev_prc','lulc_forest_prc','lulc_barren_prc','lulc_grass_prc','lulc_ag_prc','lulc_wetland_prc')

phys.pred = c('DRAIN_SQKM','GEOL_REEDBUSH_DOM','FRESHW_WITHDRAWAL',
              'AWCAVE','CLAYAVE','SILTAVE',
              'TOPWET','ELEV_MEAN_M_BASIN','porosity','storage_m')
clim.pred = c('P_mm','PET_mm','SWE_mm','melt_mm','Tmax_C',
              'P_90','PET_90','Tmax_90','melt_90',
              'P.PET','P.PET90','SNOW_PCT_PRECIP')




#### Load predictors from RFE
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



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Step -------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

sub = df

set.seed(42)
sub.split = initial_split(sub,prop=.8)
sub.train = training(sub.split)
sub.test = testing(sub.split)


### Recipe

sub.rec = sub.train %>% 
  recipe(kmeans ~.) %>%
  update_role(gage,new_role = "ID") %>%
  update_role(index,new_role = "ID")

sub.prep = prep(sub.rec)
juiced = juice(sub.prep)

### Tuning

tune_spec <- rand_forest(
  mtry = tune(),
  trees = tune(),
  min_n = tune()
) %>%
  set_mode("classification") %>%
  set_engine("ranger")

rf_tune_grid <- grid_regular(trees(range = c(200, 800)),
                             mtry(range = c(10, 30)),
                             min_n(range = c(5, 40)),
                             levels = 4)

tune_wf <- workflow() %>%
  add_recipe(sub.rec) %>%
  add_model(tune_spec)

tune_folds <- vfold_cv(sub.train, v = 5)

class_and_probs_metrics <- metric_set(roc_auc, pr_auc, accuracy,mn_log_loss,kap)

doParallel::registerDoParallel()

tune_res <-
  tune_wf %>% 
  tune_grid(
    resamples = tune_folds,
    grid = rf_tune_grid,
    metrics = class_and_probs_metrics
  )



tune_res %>%
  collect_metrics() %>%
  filter(.metric == "kap") %>%
  select(mean, min_n, mtry,trees) %>%
  pivot_longer(min_n:trees,
               values_to = "value",
               names_to = "parameter"
  ) %>%
  ggplot(aes(value, mean, color = parameter)) +
  geom_point(show.legend = FALSE) +
  facet_wrap(~parameter, scales = "free_x") +
  labs(x = NULL, y = "AUC")

tune_res2 = tune_res %>%
  collect_metrics() %>% as.data.frame()


tune_res2 %>% 
  readr::write_csv('data/tuning_pararams_resub.csv')
