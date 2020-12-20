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

clust = read.csv('data/kmeans.csv')
clust$gage = as.numeric(clust$gage)

dat = read.csv("data/metrics_by_event_combined.csv")
dat$gage = as.numeric(dat$gage)
dat$Name[dat$Name == "Ignore"] = "Mediterranean California" 
dat = dat[dat$peak_quantile>.25 & dat$drying_rate>=0,]

ant.cond = read.csv("data/metrics_by_event_withAntCondNoFlow.csv")
ant.cond = ant.cond[ant.cond$peak_quantile>.25 & ant.cond$drying_rate>=0,]
ant.cond$Date = as.Date(ant.cond$Date)

df = dat %>% left_join(.,ant.cond,by=c("gage","event_id"))

### Timeseries LULC
lulc = read.csv("data/TS_lulc_clust.csv")

# This is the only way I could get the data to join
df = cbind(df,clust,lulc)

######### Select variables and clean up for rf

df = df %>% subset(., select=which(!duplicated(names(.))))

df$kmeans = as.factor(df$kmeans)




sub = df %>%  select(
  kmeans,
  lulc_water_prc,lulc_dev_prc,lulc_forest_prc,lulc_barren_prc,lulc_grass_prc,lulc_ag_prc,lulc_wetland_prc,
  DRAIN_SQKM,SNOW_PCT_PRECIP,GEOL_REEDBUSH_DOM,FRESHW_WITHDRAWAL,
  AWCAVE,PERMAVE,CLAYAVE,SILTAVE,SANDAVE,
  TOPWET,ELEV_MEAN_M_BASIN,
  porosity,storage_m,
  P_mm,PET_mm,SWE_mm,melt_mm,Tmax_C,
  P_90,PET_90,Tmax_90,melt_90
)%>%
  mutate(P.PET = P_mm/PET_mm,
         P.PET90 = P_90/PET_90
  )


sub[is.na(sub$P.PET),'P.PET']=0
# sub[is.na(sub$P.PET7),'P.PET7']=0

sub = sub %>% filter_all(all_vars(!is.infinite(.)))
sub = sub[complete.cases(sub),]

#### Clean up data
rm(ant.cond,clust,dat,lulc,df)


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
  set_engine("ranger", 
             num.threads = (parallel::detectCores() - 1),
             importance = "permutation") %>% 
  set_mode("classification")
    
# set up recipe
rf_recipe = recipe(kmeans ~.,
                 data = sub.train,
                 update_role(event_id,new_role = "ID"))

sub.prep = prep(rf_recipe)
juiced = juice(sub.prep)

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
sub.train.predict <- predict(rf_fit, sub.train,type = "prob") %>% magrittr::set_colnames(c("pred_clust1", "pred_clust2","pred_clust3","pred_clust4"))
sub.test.predict <-  predict(rf_fit, sub.test,type = "prob")%>% magrittr::set_colnames(c("pred_clust1", "pred_clust2","pred_clust3","pred_clust4"))

# Combine with train and test datasets

sub.train = cbind(sub.train,sub.train.predict)
sub.test = cbind(sub.test,sub.test.predict)
    
# combine training and test output
fit_data_i <- 
  dplyr::bind_rows(sub.train, sub.test)
  # dplyr::select(event_id, kmeans, predicted) %>% 
  # dplyr::left_join(dplyr::select(sub, gage, event_id, Sample), 
  #                  by = c("gage", "event_id"))
    
# extract variable importance
fit_rf_imp_i <- tibble::tibble(predictor = names(pull_workflow_fit(rf_fit)$fit$variable.importance),
                               IncMSE = pull_workflow_fit(rf_fit)$fit$variable.importance,
                               oobMSE = pull_workflow_fit(rf_fit)$fit$prediction.error)


ggplot(fit_rf_imp_i,aes(x = reorder(predictor,IncMSE),y = IncMSE)) +
  geom_bar(stat="identity", position="dodge")

# partial dependence plots for all variables
ranger_fit <- ranger::ranger(kmeans ~ ., 
                             data = dplyr::bind_cols(rf_fit$pre$mold$outcomes, 
                                                     rf_fit$pre$mold$predictors),
                             num.trees = tune_res$trees[1],
                             mtry = tune_res$mtry[1], 
                             min.node.size = tune_res$min_n[1],
                             num.threads = (parallel::detectCores() - 1),
                             probability = T,
                              importance = "permutation")

doParallel::registerDoParallel(3)
    
for (v in all.pred){
  var <- v
  print(var)
  for (class in 1:4){
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
  if (var == 1){
    df_pdp <- df_pdp_var
  } else {
    df_pdp <- dplyr::bind_rows(df_pdp, df_pdp_var)
  }
}
  
ggplot(data=df_pdp)+ geom_point(aes(x=value,y=yhat,color=factor(class)))+ggtitle("P.PET90") +xlim(c(0,2))

    
    # combine
    if (m == metrics[1] & r == regions[1]){
      fit_data_out <- fit_data_i
      fit_rf_imp <- fit_rf_imp_i
      fit_pdp_out <- df_pdp
    } else {
      fit_data_out <- dplyr::bind_rows(fit_data_out, fit_data_i)
      fit_rf_imp <- dplyr::bind_rows(fit_rf_imp, fit_rf_imp_i)
      fit_pdp_out <- dplyr::bind_rows(fit_pdp_out, df_pdp)
    }
    
    # status update
    print(paste0(m, " ", r, " complete, ", Sys.time()))
    
  }
}

# save data
fit_data_out %>% 
  dplyr::select(gage_ID, currentclimyear, observed, predicted, region_rf, metric) %>% 
  readr::write_csv(file.path("results", "04_RandomForest_RunModels_Predictions.csv"))

fit_rf_imp %>% 
  readr::write_csv(file.path("results", "04_RandomForest_RunModels_VariableImportance.csv"))

fit_pdp_out %>% 
  readr::write_csv(file.path("results", "04_RandomForest_RunModels_PartialDependence.csv"))

# plots
min(subset(fit_data_out, metric == "annualnoflowdays")$predicted)

ggplot(subset(fit_data_out, metric == "annualnoflowdays" & Sample == "Test"), 
       aes(x = predicted, y = observed, color = region)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, color = col.gray) +
  scale_color_manual(values = pal_regions) +
  facet_wrap(~region_rf)

ggplot(subset(fit_data_out, metric == "annualnoflowdays"), 
       aes(x = predicted, y = observed, color = region)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, color = col.gray) +
  scale_color_manual(values = pal_regions) +
  facet_wrap(~region_rf)

ggplot(subset(fit_data_out, metric == "annualnoflowdays" & Sample == "Test"), 
       aes(x = currentclimyear, y = (predicted - observed), color = region)) +
  geom_hline(yintercept = 0, color = col.gray) +
  geom_point() +
  scale_color_manual(values = pal_regions) +
  facet_wrap(~region_rf)