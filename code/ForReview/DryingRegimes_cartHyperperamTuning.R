#####################################################################
##
## Script name: DryingRegimes_cartHyperparamTuning.R
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

tune_spec <- decision_tree(
  tree_depth = tune(),
  cost_complexity = tune(),
  min_n = tune()
) %>%
  set_mode("classification") %>%
  set_engine("rpart")

rf_tune_grid <- grid_random(cost_complexity(range = c(0,1)),
                            tree_depth(range = c(5,20)),
                             min_n(range = c(1, 40)),
                             size=30)

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
  select(mean, min_n, cost_complexity,tree_depth) %>%
  pivot_longer(min_n:tree_depth,
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
  readr::write_csv('data/tuning_pararamsCART.csv')


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Step: CART
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


mod_bag <- bag_tree() %>%
  set_mode("regression") %>%
  set_engine("rpart", times = 10)


tune_res <-
  tune_res2 %>% 
  subset(.metric == "mn_log_loss") %>% 
  dplyr::filter(mean == min(mean))


rf_engine <- 
  decision_tree(cost_complexity = tune_res$cost_complexity[1], 
              tree_depth = tune_res$tree_depth[1], 
              min_n = tune_res$min_n[1]) %>% 
  set_engine("rpart") %>% 
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
  fit(data = sub.train,)


sub.train.predict <- predict(rf_fit, sub.train)
sub.test.predict <-  predict(rf_fit, sub.test)

sub.train.confusion = 
  caret::confusionMatrix(sub.train.predict$.pred_class,sub.train$kmeans)
sub.test.confusion = 
  caret::confusionMatrix(sub.test.predict$.pred_class,sub.test$kmeans)

sub.train = cbind(sub.train,sub.train.predict)
sub.test = cbind(sub.test,sub.test.predict)

######## Plot the model #######

tree_fit <- rf_fit %>% 
  pull_workflow_fit()
rpart.plot(tree_fit$fit)



bag_roots <-  function(x){
  x %>% 
    select(.extracts) %>% 
    unnest(cols = c(.extracts)) %>% 
    mutate(models = map(.extracts,
                        ~.x$model_df)) %>% 
    select(-.extracts) %>% 
    unnest(cols = c(models)) %>% 
    mutate(root = map_chr(model,
                          ~as.character(.x$fit$frame[1, 1]))) %>%
    select(root)  
}

# plot
bag_roots(rf_fit) %>% 
  ggplot(mapping = aes(x = fct_rev(fct_infreq(root)))) + 
  geom_bar() + 
  coord_flip() + 
  labs(x = "root", y = "count")
