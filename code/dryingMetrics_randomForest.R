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

############################# Code ################################

####### Load and Filter Data #########

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

# This is the only way I could get the data to join
df = cbind(df,clust)

######### Select variables and clean up for rf

df = df %>% subset(., select=which(!duplicated(names(.))))

df$kmeans = as.factor(df$kmeans)

df = df %>% select(gage,Name,CLASS,dec_lat_va,dec_long_va,
kmeans,
peak2zero,drying_rate,p_value.x,dry_dur,freq_local,peak_quantile,peak_value.x,
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
                   FORESTNLCD06,PLANTNLCD06,WATERNLCD06,SNOWICENLCD06,IMPNLCD06,ELEV_MEAN_M_BASIN,
                   SLOPE_PCT,AWCAVE,PERMAVE,CLAYAVE,SILTAVE,SANDAVE,TOPWET,
                   depth_bedrock_m,porosity,storage_m,
                   P_mm,PET_mm,Tmin_C,Tmax_C,Srad_wm2,SWE_mm,melt_mm,
                   P_7,PET_7,Tmax_7,Tmin_7,melt_7,
                   P_14,PET_14,Tmax_14,Tmin_14,melt_14,
                   P_30,PET_30,Tmax_30,Tmin_30,melt_30,
                   P_60,PET_60,Tmax_60,Tmin_60,melt_60,
                   P_90,PET_90,Tmax_90,Tmin_90,melt_90,
                   P_180,PET_180,Tmax_180,Tmin_180,melt_180,
                   API_7,API_14,API_40,API_60,API_90,API_180,APETI_7,APETI_14,APETI_40,
                   APETI_60,APETI_90,APETI_180,AMELTI_7,AMELTI_14,AMELTI_40,AMELTI_60,AMELTI_90,AMELTI_180)

sub = sub %>%  select(
  kmeans,
  DRAIN_SQKM,SNOW_PCT_PRECIP,GEOL_REEDBUSH_DOM,FRESHW_WITHDRAWAL,PCT_IRRIG_AG,
  FORESTNLCD06,PLANTNLCD06,WATERNLCD06,SNOWICENLCD06,IMPNLCD06,
  AWCAVE,PERMAVE,CLAYAVE,SILTAVE,SANDAVE,
  TOPWET,ELEV_MEAN_M_BASIN,
  depth_bedrock_m,porosity,storage_m,
  P_mm,PET_mm,SWE_mm,melt_mm,Tmax_C,
  P_90,PET_90,Tmax_90,melt_90
  )%>%
  mutate(P.PET = P_mm/PET_mm,
               P.PET90 = P_90/PET_90
               )

### Fix devide by 0

sub[is.na(sub$P.PET),'P.PET']=0
# sub[is.na(sub$P.PET7),'P.PET7']=0

sub = sub %>% filter_all(all_vars(!is.infinite(.)))

#### Clean up data
rm(ant.cond,clust,dat)

###### Check out correlation structure of data#####
# Source: https://stats.stackexchange.com/questions/141619/wont-highly-correlated-variables-in-random-forest-distort-accuracy-and-feature
# https://stats.stackexchange.com/questions/168622/why-is-multicollinearity-not-checked-in-modern-statistics-machine-learning
#
###################################################
tt = cor(sub[,c(-1,-4)])
corrplot::corrplot(tt,order='hclust',type = "upper")

rm(tt)
############# Binary classification by cluster ###############

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


################# Random forest ###############

#####################################################################################
#####################################################################################
############################# Cluster Membership ####################################
#####################################################################################
#####################################################################################

sub = c4

# Set seed and split data
set.seed(42)
sub$index <- seq(1:nrow(sub))
training_size = round(nrow(sub)*0.7,0)
training <- as.data.frame(sub[sample(1:nrow(sub),training_size, replace = F),])
testing <- as.data.frame(sub[!sub$index %in% training$index,])

index = sub$index  
sub$index= NULL
testing$index = NULL
training$index = NULL



# create feature names
y <- "kmeans"
x <- setdiff(names(training), y)

# Initialize an h2o cluster
h2o.init(max_mem_size = "8g")


# turn training set into h2o object
train.h2o <- as.h2o(training)

# Setup your hyperparameter grid  
hyper_grid.h2o <- list(
  ntrees      = seq(100, 500, by = 100),
  mtries      = seq(10, 30, by = 10),
  max_depth   = seq(10, 40, by = 5),
  # min_rows    = seq(1, 5, by = 2),
  # nbins       = seq(10, 30, by = 5),
  sample_rate = c(.5, .75, .80)
)

# random grid search criteria
search_criteria <- list(
  strategy = "RandomDiscrete",
  stopping_metric = "logloss",
  stopping_tolerance = 0.05,
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
  decreasing = F
)
print(grid_perf2)

best_model_id <- grid_perf2@model_ids[[1]]
bm <- h2o.getModel(best_model_id)

# h2o.saveGrid('data/',grid_perf2@grid_id)
h2o.saveModel(bm,'data/rf_model/')

####################### Random Forest ####################################


test.h2o <- as.h2o(testing)
best_model_perf <- h2o.performance(model = bm, newdata = test.h2o)

# RMSE of best model
h2o.mse(best_model_perf) %>% sqrt()

# Var Imp Plot
h2o.varimp_plot(bm)

# 
# 
h2o.shutdown(prompt = FALSE)
# 
# 








########### Load and plot models ############
h2o.init(max_mem_size = '4G')

am = h2o.loadModel('data/rf_model/all_model')
h2o.varimp_plot(am,num_of_features = 30)

c1.m = h2o.loadModel('data/rf_model/c1_model')
h2o.varimp_plot(c1.m,num_of_features = 30)

c2.m = h2o.loadModel('data/rf_model/c2_model')
h2o.varimp_plot(c2.m,num_of_features = 30)

c3.m = h2o.loadModel('data/rf_model/c3_model')
h2o.varimp_plot(c3.m,num_of_features = 30)

c4.m = h2o.loadModel('data/rf_model/c4_model')
h2o.varimp_plot(c4.m,num_of_features = 30)

h2o.shutdown()


best_model_perf <- h2o.performance(model = am, newdata = test.h2o)




#### Scratch
# sub= c4
# 
# set.seed(42)
# seed = sample(1:10000,100)

# Set seed and select training data

# i=10
# print(i)
# # Set seed and split data
# set.seed(seed[i])
# print(seed[i])
# sub$index <- seq(1:nrow(sub))
# training_size = round(nrow(sub)*0.7,0)
# training <- as.data.frame(sub[sample(1:nrow(sub),training_size, replace = F),])
# testing <- as.data.frame(sub[!sub$index %in% training$index,])
# 
# testing$index = NULL
# training$index = NULL
# 
# rf = ranger(formula = kmeans ~.,
#             data = training,
#             num.trees = 400,
#             mtry = 10,
#             sample.fraction = .8,
#             importance = 'impurity',
#             num.threads = 7,
#             oob.error = T)

# ggplot(rf$variable.importance, aes(x=reorder(variable,importance), y=importance,fill=importance))+ 
#   geom_bar(stat="identity", position="dodge")+ coord_flip()+
#   ylab("Variable Importance")+
#   xlab("")+
#   ggtitle("Information Value Summary")+
#   guides(fill=F)+
#   scale_fill_gradient(low="red", high="blue")

# barplot(head(sort(rf$variable.importance,decreasing = T),10),angle = 0)
# 
# head(sort(rf$variable.importance,decreasing = T),25)


# model = bm
# column_name = "Tmax_C"
# data.pdp = train.h2o
# bins = unique(h2o.quantile(train.h2o[, column_name], probs = seq(0.05,1,0.05)) )
# mean_responses = c()
# 
# for(bin in bins ){
#   data.pdp[, column_name] = bin
#   response = h2o.predict(model, data.pdp[, column_name])
#   mean_response = mean(response[,ncol(response)])
#   mean_responses = c(mean_responses, mean_response)
# }
# 
# pdp_manual = data.frame(Tmax_C = bins, mean_response = mean_responses)
# plot(pdp_manual, type = "l")
# 
# h2o.partialPlot(object = bm, data = train.h2o, cols="Tmax_C")
# 
