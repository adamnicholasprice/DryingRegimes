#####################################################################
##
## Script name: 
##
## Author: Adam N. Price
##
## Date Created: 2020-12-14
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
library(randomForest)
library(permimp)
############################# Code ################################

df = read.csv('data/kmeans_NoFreq.csv')
metrics <- "kmeans"


df$GEOL_REEDBUSH_DOM = as.factor(df$GEOL_REEDBUSH_DOM)
df$kmeans = as.factor(df$kmeans)


# all possible predictors
lulc.pred = c('lulc_water_prc','lulc_dev_prc','lulc_forest_prc','lulc_barren_prc','lulc_grass_prc','lulc_ag_prc','lulc_wetland_prc')

phys.pred = c('DRAIN_SQKM','GEOL_REEDBUSH_DOM','FRESHW_WITHDRAWAL',
              'AWCAVE','CLAYAVE','SILTAVE',
              'TOPWET','ELEV_MEAN_M_BASIN','porosity','storage_m')
clim.pred = c('SWE_mm','melt_mm','Tmax_C',
              'Tmax_90','melt_90',
              'P.PET','P.PET90','SNOW_PCT_PRECIP')
exclude_cols = c("index","gage")

#### Load predictors from RFE
predictors_all = read.csv("data/rfe_predictors.csv")[[1]]


df = df %>% 
  rename(TOPWET = TOPWET.x,
         index = X) %>% 
  select("index","gage","kmeans",all_of(predictors_all)) %>%
  subset(complete.cases(.))

## load hyperparameter tuning results
tune_res_all <- readr::read_csv('data/tuning_pararams.csv')

# Define parameters for model
tune_res <-
  tune_res_all %>%
  subset(.metric == "mn_log_loss") %>%
  dplyr::filter(mean == min(mean))

####################### 


sub = df

# Subset data
set.seed(42)
sub.split = initial_split(sub,prop=.8)
sub.train = training(sub.split)
sub.test = testing(sub.split)

######################################
#  randomForest
######################################

rf_fit <-
  randomForest::randomForest(
    kmeans ~ .,
    data = sub.train[ !names(sub.train) %in% exclude_cols],
    importance = TRUE,
    ntree = tune_res$trees[1],
    mtry = tune_res$mtry[1],
    nodesize = tune_res$min_n[1],
    keep.forest = TRUE,
    keep.inbag = TRUE,
    type = "classification"
  )


# predict
sub.train.predict <- 
  predict(rf_fit, sub.train)%>% 
  as.data.frame()%>%
  setNames("pred_class")

sub.test.predict <-  
  predict(rf_fit, sub.test) %>% 
  as.data.frame()%>%
  setNames("pred_class")


sub.train.confusion = 
  caret::confusionMatrix(sub.train.predict$pred_class,sub.train$kmeans)
sub.test.confusion = 
  caret::confusionMatrix(sub.test.predict$pred_class,sub.test$kmeans)




# Combine with train and test datasets
sub.train = cbind(sub.train,sub.train.predict)
sub.test = cbind(sub.test,sub.test.predict)

# combine training and test output
fit_data_i <- 
  dplyr::bind_rows(sub.train, sub.test)

fit_rf_imp_i <- rf_fit$importance %>% 
  as.data.frame()%>%
  mutate(predictor = rownames(rf_fit$importance))

fit_rf_imp_i %>%
  readr::write_csv(paste0('data/all_rf_importance.csv'))

fit_data_i %>%
  readr::write_csv(paste0('data/all_rf_data.csv'))

data.frame(sub.test.confusion$overall)%>%
  readr::write_csv(paste0('data/all_overallPerformMetrics.csv'))

data.frame(sub.test.confusion$byClass)%>%
  readr::write_csv(paste0('data/all_classPerformMetrics.csv'))

data.frame(sub.test.confusion$table) %>%
  readr::write_csv(paste0('data/all_confusionMetrics.csv'))



######################################
#Conditional Permutation Importance
######################################
# This step takes 14 hours to run.

permimp_rf <- permimp::permimp(rf_fit, conditional = TRUE)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Step: partial dependence plots for all variables
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

doParallel::registerDoParallel(3)

df_pdp_out = data.frame()

for (v in predictors_all){
  var <- v
  print(var)
  for (class in 1:4){
    print(paste0("Class ",class))
    df_pdp_var <-
      pdp::partial(rf_fit,
                   pred.var = var,
                   mtry = tune_res$mtry[1],
                   min.node.size = tune_res$min_n[1],
                   num.threads = (parallel::detectCores() - 1),
                   parallel = T,
                   which.class=class) %>%
      magrittr::set_colnames(c("value", "yhat"))
    df_pdp_var$predictor <- var
    df_pdp_var$class <- class
    class(df_pdp_var) <- "data.frame"
    if (class == 1){
      df_pdp <- df_pdp_var
    } else {
      df_pdp <- dplyr::bind_rows(df_pdp, df_pdp_var)
    }
  }
  df_pdp_out <- dplyr::bind_rows(df_pdp_out, df_pdp)
}

beepr::beep(sound=8)

# save data
df_pdp_out %>%
  readr::write_csv('data/all_PDP.csv')







############ Old RF



# # set up model engine
# tune_res <-
#   tune_res_all %>% 
#   subset(.metric == "mn_log_loss") %>% 
#   dplyr::filter(mean == min(mean))
# 
# rf_engine <- 
#   rand_forest(trees = tune_res$trees[1], 
#               mtry = tune_res$mtry[1], 
#               min_n = tune_res$min_n[1]) %>% 
#   set_engine("randomForest", 
#              num.threads = (parallel::detectCores() - 1),
#              importance = TRUE,
#              keep.forest=TRUE,
#              keep.inbag=TRUE) %>% 
#   set_mode("classification")##### Classification not regression
# 
# 
# 
# # set up recipe
# rf_recipe = sub.train %>%
#   recipe(kmeans ~.) %>%
#   update_role(gage,new_role = "ID") %>%
#   update_role(index,new_role="ID")
# # update_role(-event_id, new_role = 'predictor'))
# 
# 
# # sub.prep = prep(rf_recipe)
# # juiced = juice(sub.prep)
# 
# 
# 
# # set up workflow
# rf_workflow <-
#   workflow() %>% 
#   add_model(rf_engine) %>% 
#   add_recipe(rf_recipe)
# 
# # fit model
# rf_fit <- 
#   rf_workflow %>% 
#   fit(data = sub.train)
