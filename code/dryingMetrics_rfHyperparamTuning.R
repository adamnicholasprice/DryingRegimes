#####################################################################
##
## Script name: dryingMetrics_rfHyperparamTuning.R
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
  kmeans,event_id,
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

sub$GEOL_REEDBUSH_DOM <- as.factor(sub$GEOL_REEDBUSH_DOM)


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


### save datasets


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Step -------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~



set.seed(42)
sub.split = initial_split(sub,prop=.8)

sub.train = training(sub.split)
sub.test = testing(sub.split)


### Recipe

sub.rec = recipe(kmeans ~.,
                 data = sub.train,
                 update_role(event_id,new_role = "ID"))

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
  readr::write_csv('data/tuning_pararams.csv')
