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
library(ranger)

############################# Code ################################

df = read.csv("data/kmeans_NoFreq.csv")


#######################################################
################# Step: set up predictiors      
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

predictors_all = c(lulc.pred,phys.pred,clim.pred)


#######################################################
################# Step: Select all predictors and filter data
#######################################################

df = df %>% 
  rename(TOPWET = TOPWET.x,
         index = X) %>% 
  select("index","gage","kmeans",all_of(predictors_all)) %>%
  subset(complete.cases(.)) %>%
  subset(is.finite(P.PET))

df$GEOL_REEDBUSH_DOM = as.factor(df$GEOL_REEDBUSH_DOM)
df$kmeans = as.factor(df$kmeans)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Step #: Subset data by clusters -------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


c1 = sub %>%
  mutate(kmeans = recode(kmeans,
                         '2'='0',
                         '3'='0',
                         '4'='0'))

c2 = sub %>%
  mutate(kmeans = recode(kmeans,
                         '1'='0',
                         '2' = '1',
                         '3'='0',
                         '4'='0'))
c3 = sub %>%
  mutate(kmeans = recode(kmeans,
                         '1'='0',
                         '2'='0',
                         '3' ='1',
                         '4'='0'))
c4 = sub %>%
  mutate(kmeans = recode(kmeans,
                         '1'='0',
                         '2'='0',
                         '3'='0',
                         '4'='1'))



######## List predictors here

lulc.pred = c('lulc_water_prc','lulc_dev_prc','lulc_forest_prc','lulc_barren_prc','lulc_grass_prc','lulc_ag_prc','lulc_wetland_prc')

phys.pred = c('DRAIN_SQKM','SNOW_PCT_PRECIP','GEOL_REEDBUSH_DOM','FRESHW_WITHDRAWAL',
              'AWCAVE','PERMAVE','CLAYAVE','SILTAVE','SANDAVE',
              'TOPWET','ELEV_MEAN_M_BASIN','porosity','storage_m')
clim.pred = c('P_mm','PET_mm','SWE_mm','melt_mm','Tmax_C',
              'P_90','PET_90','Tmax_90','melt_90',
              'P.PET','P.PET90')

all.pred = c(lulc.pred,phys.pred,clim.pred)

## load hyperparameter tuning results
tune_res_all <- readr::read_csv('data/tuning_pararams.csv')

## loop through metrics and regions
# choose number of predictors - based on script 02_RandomForest_FigureOutNumPredictors.R
# npred_final <- tibble::tibble(metric = c("annualnoflowdays", "zeroflowfirst", "peak2z_length"),
#                               npred = c(22, 27, 27)) 

backup = sub
names = c("all","c1","c2","c3","c4")
df = list(sub,c1,c2,c3,c4)

sub =df
for (i in 1:length(df)){
  sub = data.frame(df[i])
  print(paste("starting",names[i]))


  # Subset data
  set.seed(42)
  sub.split = initial_split(sub,prop=.8)
  sub.train = training(sub.split)
  sub.test = testing(sub.split)
  
  
  # set up model engine
  tune_res <-
    tune_res_all %>% 
    subset(.metric == "mn_log_loss") %>% 
    dplyr::filter(mean == min(mean))
    
  rf_engine <- 
    rand_forest(trees = tune_res$trees[1], 
                mtry = tune_res$mtry[1], 
                min_n = tune_res$min_n[1]) %>% 
    set_engine("randomForest", 
               num.threads = (parallel::detectCores() - 1),
               importance = TRUE) %>% 
    set_mode("classification") ##### Classification not regression
  
  
      
  # set up recipe
  rf_recipe = sub.train %>%
    recipe(kmeans ~.) %>%
    update_role(gage,new_role = "ID") %>%
    update_role(index,new_role="ID")
                   # update_role(-event_id, new_role = 'predictor'))
    
  
  # sub.prep = prep(rf_recipe)
  # juiced = juice(sub.prep)
  
  
  
  # set up workflow
  rf_workflow <-
    workflow() %>% 
    add_model(rf_engine) %>% 
    add_recipe(rf_recipe)
    
  # fit model
  rf_fit <- 
    rf_workflow %>% 
    fit(data = sub.train)
  
  
  # predict
  sub.train.predict <- predict(rf_fit, sub.train)
  sub.test.predict <-  predict(rf_fit, sub.test)
  
  print(paste("Done predicting",names[i]))
  
  
  sub.train.confusion = 
    caret::confusionMatrix(sub.train.predict$.pred_class,sub.train$kmeans)
  sub.test.confusion = 
    caret::confusionMatrix(sub.test.predict$.pred_class,sub.test$kmeans)
  
  
  print(paste("Confusion matrix",names[i]))
  
  
  # Combine with train and test datasets
  sub.train = cbind(sub.train,sub.train.predict)
  sub.test = cbind(sub.test,sub.test.predict)
      
  # combine training and test output
  fit_data_i <- 
    dplyr::bind_rows(sub.train, sub.test)
      
  # extract variable importance
  fit_rf_imp_i <- tibble::tibble(predictor = names(pull_workflow_fit(rf_fit)$fit$variable.importance),
                                 IncMSE = pull_workflow_fit(rf_fit)$fit$variable.importance,
                                 oobMSE = pull_workflow_fit(rf_fit)$fit$prediction.error)
  
  fit_rf_imp_i %>%
    readr::write_csv(paste0('data/',names[i],'_rf_importance.csv'))
  
  fit_data_i %>%
    readr::write_csv(paste0('data/',names[i],'_rf_data.csv'))
  
  data.frame(sub.test.confusion$overall)%>%
    readr::write_csv(paste0('data/',names[i],"_overallPerformMetrics.csv"))
  
  data.frame(sub.test.confusion$byClass)%>%
    readr::write_csv(paste0('data/',names[i],"_classPerformMetrics.csv"))
  
  data.frame(sub.test.confusion$table) %>%
    readr::write_csv(paste0('data/',names[i],"_confusionMetrics.csv"))
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Step -------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# partial dependence plots for all variables
ranger_fit <- ranger::ranger(kmeans ~ .,
                             data = dplyr::bind_cols(rf_fit$pre$mold$outcomes,
                                                     rf_fit$pre$mold$predictors),
                             num.trees = tune_res$trees[1],
                             mtry = tune_res$mtry[1],
                             min.node.size = tune_res$min_n[1],
                             num.threads = (parallel::detectCores() - 1),
                             probability = T,
                            importance = "permutation",
                            classification = T)

doParallel::registerDoParallel(3)

df_pdp_out = data.frame()

for (v in all.pred){
  var <- v
  print(var)
  for (class in 1:4){
    print(paste0("Class ",class))
    df_pdp_var <-
      pdp::partial(ranger_fit,
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
