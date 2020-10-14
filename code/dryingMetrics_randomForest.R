#####################################################################
##
## Script name: dryingMetrics_randomForest.r
##
## Author: Adam N. Price
##
## Date Created: 2020-09-24
##
## Copyright (c) Adam N. Price, 2020
## Email: adnprice@ucsc.edu
##
############################# Description ##########################
##
## 
##   https://uc-r.github.io/random_forests 
##  https://docs.h2o.ai/h2o/latest-stable/h2o-r/docs/reference/h2o.randomForest.html
##
############################# Packages #############################

library(ggplot2)
library(tidyverse)
library(lubridate)
library(foreach)
library(caret)
library(ranger)
library(h2o)



# library(GGally)
# library(RColorBrewer)
# library(rsample)
# library(foreign)
# library(rgdal)
# library(Metrics)
# library(plm)
# library(viridis)
# library(ggRandomForests)
# library(purrr)

############################# Code ################################

####### Load and Filter Data #########

clust = read.csv('../data/clustering_results_allevents.csv')
clust$gage = as.numeric(clust$gage)

dat = read.csv("../data/metrics_by_event_combined.csv")
dat$gage = as.numeric(dat$gage)
dat$Name[dat$Name == "Ignore"] = "Mediterranean California" 
dat = dat[dat$peak_quantile>.25 & dat$drying_rate>=0,]

ant.cond = read.csv("../data/metrics_by_event_withAntCondNoFlow.csv")
ant.cond = ant.cond[ant.cond$peak_quantile>.25 & ant.cond$drying_rate>=0,]
ant.cond$Date = as.Date(ant.cond$Date)

df = dat %>% left_join(.,ant.cond,by=c("gage","event_id"))

# This is the only way I could get the data to join
df = cbind(df,clust)

######### Select variables and clean up for rf

df = df %>% subset(., select=which(!duplicated(names(.))))

df$hier.4.clust = as.factor(df$hier.4.clust)
df$kmeans.clust = as.factor(df$kmeans.clust)
df$gmm.clust = as.factor(df$gmm.clust)
df$hier.4.clust = as.factor(df$hier.4.clust)

df = df %>% select(gage,Name,CLASS,dec_lat_va,dec_long_va,
kmeans,
peak2zero,drying_rate,p_value.x,dry_dur,rel_freq,peak_quantile,peak_value.x,
dry_date_start,dry_date_mean.x,peak_date.x,calendar_year.x,season.x,meteorologic_year.x,
DRAIN_SQKM,SNOW_PCT_PRECIP,GEOL_REEDBUSH_DOM,FRESHW_WITHDRAWAL,PCT_IRRIG_AG,
DEVNLCD06,FORESTNLCD06,PLANTNLCD06,WATERNLCD06,SNOWICENLCD06,IMPNLCD06,ELEV_MEAN_M_BASIN,
SLOPE_PCT,AWCAVE,PERMAVE,CLAYAVE,SILTAVE,SANDAVE,TOPWET,depth_bedrock_m,porosity,storage_m,
P_mm,PET_mm,Tmin_C,Tmax_C,Srad_wm2,SWE_mm,melt_mm,P_7,PET_7,Tmax_7,Tmin_7,melt_7,
P_14,PET_14,Tmax_14,Tmin_14,melt_14,P_30,PET_30,Tmax_30,Tmin_30,melt_30,
P_60,PET_60,Tmax_60,Tmin_60,melt_60,P_90,PET_90,Tmax_90,Tmin_90,melt_90,
P_180,PET_180,Tmax_180,Tmin_180,melt_180,Q_180,
API_7,API_14,API_40,API_60,API_90,API_180,APETI_7,APETI_14,APETI_40,
APETI_60,APETI_90,APETI_180,AMELTI_7,AMELTI_14,AMELTI_40,AMELTI_60,AMELTI_90,AMELTI_180)


df = df[!is.na(df$dec_lat_va),]

df = df[complete.cases(df),]


sub = df %>% select(
                   kmeans,
                   DRAIN_SQKM,SNOW_PCT_PRECIP,GEOL_REEDBUSH_DOM,FRESHW_WITHDRAWAL,PCT_IRRIG_AG,
                   DEVNLCD06,FORESTNLCD06,PLANTNLCD06,WATERNLCD06,SNOWICENLCD06,IMPNLCD06,ELEV_MEAN_M_BASIN,
                   SLOPE_PCT,AWCAVE,PERMAVE,CLAYAVE,SILTAVE,SANDAVE,TOPWET,
                   depth_bedrock_m,porosity,storage_m,
                   P_mm,PET_mm,Tmin_C,Tmax_C,Srad_wm2,SWE_mm,melt_mm,P_7,PET_7,Tmax_7,Tmin_7,melt_7,
                   P_14,PET_14,Tmax_14,Tmin_14,melt_14,
                   P_30,PET_30,Tmax_30,Tmin_30,melt_30,
                   P_60,PET_60,Tmax_60,Tmin_60,melt_60,
                   P_90,PET_90,Tmax_90,Tmin_90,melt_90,
                   P_180,PET_180,Tmax_180,Tmin_180,melt_180,
                   API_7,API_14,API_40,API_60,API_90,API_180,APETI_7,APETI_14,APETI_40,
                   APETI_60,APETI_90,APETI_180,AMELTI_7,AMELTI_14,AMELTI_40,AMELTI_60,AMELTI_90,AMELTI_180)


#### Clean up data
rm(ant.cond,clust,dat)

################# Random forest ###############

#####################################################################################
#####################################################################################
############################# Hier Cluster #################################
#####################################################################################
#####################################################################################

seed = sample(1:10000,100)

# # Setup cluster
# cl <- parallel::makeCluster(3)
# doParallel::registerDoParallel(cl)

# Set seed and select training data

i=10
print(i)
# Set seed and split data
set.seed(seed[i])
print(seed[i])
sub$index <- seq(1:nrow(sub))
training_size = nrow(sub)*0.7
training <- as.data.frame(sub[sample(1:nrow(sub),training_size, replace = F),])
testing <- as.data.frame(sub[!sub$index %in% training$index,])
  
testing$index = NULL
training$index = NULL



# create feature names
y <- "hier.4.clust"
x <- setdiff(names(training), y)

# Initialize an h2o cluster
h2o.init(max_mem_size = "5g")


# turn training set into h2o object
train.h2o <- as.h2o(training)

# Setup your hyperparameter grid  
hyper_grid.h2o <- list(
  ntrees      = seq(100, 500, by = 100),
  mtries      = seq(10, 50, by = 10),
  max_depth   = seq(10, 40, by = 5),
  # min_rows    = seq(1, 5, by = 2),
  # nbins       = seq(10, 30, by = 5),
  sample_rate = c(.25,.55, .632, .75, .80)
)

# random grid search criteria
search_criteria <- list(
  strategy = "RandomDiscrete",
  stopping_metric = "mse",
  stopping_tolerance = 0.005,
  stopping_rounds = 10,
  max_runtime_secs = 30*60
)

# build grid search 
random_grid <- h2o.grid(
  algorithm = "randomForest",
  grid_id = "rf_grid2",
  x = x, 
  y = y, 
  training_frame = train.h2o,
  hyper_params = hyper_grid.h2o,
  search_criteria = search_criteria
)

# collect the results and sort by our model performance metric of choice
grid_perf2 <- h2o.getGrid(
  grid_id = "rf_grid2",
  sort_by = "mse",
  decreasing = FALSE
)
print(grid_perf2)

# Hyper-Parameter Search Summary: ordered by increasing mse
# max_depth mtries ntrees sample_rate         model_ids                 mse
# 1         30     40    500         0.8  rf_grid2_model_2 0.30797846869895606
# 2         40     40    500         0.8  rf_grid2_model_4 0.30809128817199155
# 3         40     50    300         0.8 rf_grid2_model_19  0.3086719215338706
# 4         20     30    500         0.8 rf_grid2_model_17 0.30972088830583067
# 5         25     40    200        0.75  rf_grid2_model_9  0.3100404356691361

best_model_id <- grid_perf2@model_ids[[1]]
best_model <- h2o.getModel(best_model_id)




test.h2o <- as.h2o(testing)
best_model_perf <- h2o.performance(model = best_model, newdata = test.h2o)

# RMSE of best model
h2o.mse(best_model_perf) %>% sqrt()


## Random Forest
cars_drf <- h2o.randomForest(x = x, y = y,
                             training_frame = train.h2o, nfolds = 5,
                             seed = 1234)
h2o.varimp_plot(cars_drf)



h2o.shutdown(prompt = TRUE)


rf = ranger(formula = hier.4.clust ~.,
            data = training,
            num.trees = 500,
            mtry = 40,
            sample.fraction = .8,
            importance = 'impurity')

ggplot(rf$variable.importance, aes(x=reorder(variable,importance), y=importance,fill=importance))+ 
  geom_bar(stat="identity", position="dodge")+ coord_flip()+
  ylab("Variable Importance")+
  xlab("")+
  ggtitle("Information Value Summary")+
  guides(fill=F)+
  scale_fill_gradient(low="red", high="blue")




