#####################################################################
##
## Script name: 
##
## Author: Adam N. Price
##
## Date Created: 2020-05-01
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

## load up the packages we will need:  (uncomment as required)

############################# Code ################################


library(ggplot2)
library(GGally)
library(lubridate)
library(tidyverse)
#install.packages('caret')
library(caret)
#install.packages('randomForest')
library(randomForest)
#install.packages('RcolorBrewer')
library(RColorBrewer)
#install.packages('rsample')
library(rsample)
#install.packages('foreign')
library(foreign)
#install.packages('rgdal')
library(rgdal)
#install.packages('Metrics')
library(Metrics)
#install.packages('plm')
library(plm)
library(viridis)
library(ggRandomForests)
library(foreach)
library(purrr)
############################# Functions ################################
# Multiplot function
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

############################# Code ################################

data = read.csv('../data/meanannualnf_climate_peak2zero.csv')

p2z_data = read.csv('../data/p2z_seasonal.csv')[2:26]

reg_data = read.csv('../data/regression_Seasonal.csv')[2:29]

geo_data = read.csv('../data/mean_annual_no_flow_climate_watershed_EPA1_032920.csv')

# Subset data based on reference gages
temp = which(geo_data$Class == 'Ref')
sub = geo_data[temp,]

refs = sub$gage_ID


# Subset JH data
ref_data =  data.frame(matrix(NA, nrow = length(refs), ncol = ncol(data)))

for (i in seq(refs)){
  ref_data[i,] = data[data$site==as.integer(refs[i]),]
}

colnames(ref_data) = colnames(data)


# Subset p2z data
p2z_df =  data.frame(matrix(NA, nrow = length(refs), ncol = ncol(p2z_data)))

for (i in seq(refs)){
  p2z_df[i,] = p2z_data[p2z_data$site_no==as.integer(refs[i]),]
}

colnames(p2z_df) = colnames(p2z_data)

# # Subset NJ data
# reg_df =  data.frame(matrix(NA, nrow = length(refs), ncol = ncol(reg_data)))
# 
# for (i in seq(refs)){
#   if (reg_data$site_no==refs[i],] == NA{
#     print('no way')
#   }
#   else{
#     reg_df = reg_data[reg_data$site_no==refs[i],]
#   }
# }



full_data = cbind(ref_data,p2z_df,sub)

# Remove 1 gage that doesnt have seasonal Temperature stats
full_data = full_data[!is.na(full_data$T_max_c_jja),]
ref_data = ref_data[!is.na(ref_data$T_max_c_jja),]


# Extract coordinates for subset of gages
coords = cbind(full_data$dec_long_va,full_data$dec_lat_va)
colnames(coords) = c('dec_long_va','dec_lat_va')

# Remove duplicate columns
full_data = full_data[!duplicated(as.list(full_data))]

# Convert Geology type to factor

ref_data$GEOL_REEDBUSH_DOM <- as.factor(ref_data$GEOL_REEDBUSH_DOM)
# Subset non-timeseries data
nonTS_metrics <- ref_data %>% select(SNOW_PCT_PRECIP,ELEV_MEAN_M_BASIN,SLOPE_PCT,TOPWET)

geol_metrics <- ref_data %>% select(GEOL_REEDBUSH_DOM,AWCAVE,PERMAVE,CLAYAVE,SILTAVE,SANDAVE)

landcover_metrics <- ref_data %>% select(DEVNLCD06,PCT_IRRIG_AG,FORESTNLCD06,PLANTNLCD06,WATERNLCD06,SNOWICENLCD06,IMPNLCD06)


# Subset data by season
winter = grepl('djf', colnames(full_data))
w_ref = cbind(full_data$site,coords,full_data[,winter],nonTS_metrics,geol_metrics,landcover_metrics)
w_sub = w_ref[w_ref$p2z_count_djf>0 & w_ref$djffractionnoflow>0,]
colnames(w_sub) = str_remove(colnames(w_sub),'djf')

spring = grepl('mam', colnames(full_data))
s_ref = cbind(full_data$site,coords,full_data[,spring],nonTS_metrics,geol_metrics,landcover_metrics)
s_sub = s_ref[s_ref$p2z_count_mam>0 & s_ref$mamfractionnoflow>0,]
colnames(s_sub) = str_remove(colnames(s_sub),'mam')

summer = grepl('jja', colnames(full_data))
su_ref = cbind(full_data$site,coords,full_data[,summer],nonTS_metrics,geol_metrics,landcover_metrics)
su_sub = su_ref[su_ref$p2z_count_jja>0 & su_ref$jjafractionnoflow>0,]
colnames(su_sub) = str_remove(colnames(su_sub),'jja')

fall = grepl('son', colnames(full_data))
f_ref = cbind(full_data$site,coords,full_data[,fall],nonTS_metrics,geol_metrics,landcover_metrics)
f_sub = f_ref[f_ref$p2z_count_son>0 & f_ref$sonfractionnoflow>0,]
colnames(f_sub) = str_remove(colnames(f_sub),'son')





# # # # # # # # # # Random Forest # # # # # # # # # # # # # # # # # # # # # 
# http://topepo.github.io/caret/index.html

#####################################################################################
#####################################################################################
############################# Fraction of No Flow ###################################
#####################################################################################
#####################################################################################

seed = sample(1:10000,100)

# Setup cluster
cl <- parallel::makeCluster(3)
doParallel::registerDoParallel(cl)

# Set seed and select training data
season = f_sub

f_output = foreach(i= 1:100,.packages = c('randomForest','caret','foreach')) %dopar% {
       
        # Set seed and split data
        set.seed(seed[i])
        print(seed[i])
        season$index <- seq(1:nrow(season))
        training_size = nrow(season)*0.7
        training <- as.data.frame(season[sample(1:nrow(season),training_size, replace = F),])
        testing <- as.data.frame(season[!season$index %in% training$index,])
        
        #####CHOOSE METAPARAMETERS WITH CROSSFOLD VALIDATION THEN GROW RF#####
        trainControl <- trainControl(method = "oob", number = 1, returnResamp = 'all', verboseIter=F)
        
        # build the model and test for best mtry to use 
        oob.vals <- vector(length=20)
        foreach (j = 1:10) %do% {
                rf <- randomForest(fractionnoflow ~ p_mm_ + pet_mm_ + T_max_c_ + T_min_c_ + p2z_cv_ + SNOW_PCT_PRECIP +  SNOW_PCT_PRECIP   +   ELEV_MEAN_M_BASIN   +
                                   SLOPE_PCT          +         TOPWET       +        GEOL_REEDBUSH_DOM  +  AWCAVE      +         PERMAVE         +     CLAYAVE    +         
                                   SILTAVE         +     SANDAVE        +      DEVNLCD06       +     PCT_IRRIG_AG     +    FORESTNLCD06     +    PLANTNLCD06     +     WATERNLCD06  +       
                                   SNOWICENLCD06    +    IMPNLCD06   
                                     , data = training, mtry = j,importance = T,na.action = na.omit,ntree=500)
                oob.vals[j] = rf$mse[length(rf$mse)]
                }
        
        mtry_best = which.min(oob.vals)
        
        
        ## Run the optimized model
        rf <- randomForest(fractionnoflow ~ p_mm_ + pet_mm_ + T_max_c_ + T_min_c_ + p2z_cv_ + SNOW_PCT_PRECIP +  SNOW_PCT_PRECIP   +   ELEV_MEAN_M_BASIN   +
                             SLOPE_PCT          +         TOPWET       +        GEOL_REEDBUSH_DOM  +  AWCAVE      +         PERMAVE         +     CLAYAVE    +         
                             SILTAVE         +     SANDAVE        +      DEVNLCD06       +     PCT_IRRIG_AG     +    FORESTNLCD06     +    PLANTNLCD06     +     WATERNLCD06  +       
                             SNOWICENLCD06    +    IMPNLCD06   
                           , data = training, mtry = mtry_best,importance = T,na.action = na.omit,ntree=500)
        
        
        # Predict the result of the model
        testing <- predict(rf, testing)
        training <- predict(rf, training)
        
        
        out = list(rf=rf,
                   testing=testing,
                   training = training,
                   mtry_best = mtry_best,
                   w_seed_store = seed[i],
                   rf_importance = rf$importance[,1])
        
        return(out)

}


# Stop the cluster
parallel::stopCluster(cl)


# basic plots
varImpPlot(w_output[[1]]$rf,main = "No-flow fraction (Winter)")
varImpPlot(s_output[[1]]$rf,main = "No-flow fraction (Spring)")
varImpPlot(su_output[[1]]$rf,main = "No-flow fraction (Summer)")
varImpPlot(f_output[[1]]$rf,main = "No-flow fraction (Fall)")




#####################################################################################
#####################################################################################
############################# First Date of No Flow #################################
#####################################################################################
#####################################################################################

seed = sample(1:10000,100)

# Setup cluster
cl <- parallel::makeCluster(3)
doParallel::registerDoParallel(cl)

# Set seed and select training data
season = su_sub

su_output_firstNo = foreach(i= 1:100,.packages = c('randomForest','caret','foreach')) %dopar% {
  
  # Set seed and split data
  set.seed(seed[i])
  print(seed[i])
  season$index <- seq(1:nrow(season))
  training_size = nrow(season)*0.7
  training <- as.data.frame(season[sample(1:nrow(season),training_size, replace = F),])
  testing <- as.data.frame(season[!season$index %in% training$index,])
  
  #####CHOOSE METAPARAMETERS WITH CROSSFOLD VALIDATION THEN GROW RF#####
  trainControl <- trainControl(method = "oob", number = 1, returnResamp = 'all', verboseIter=F)
  
  # build the model and test for best mtry to use 
  oob.vals <- vector(length=20)
  foreach (j = 1:10) %do% {
    rf <- randomForest(zeroflowfirst ~ p_mm_ + pet_mm_ + T_max_c_ + T_min_c_ + p2z_cv_ + SNOW_PCT_PRECIP +  SNOW_PCT_PRECIP   +   ELEV_MEAN_M_BASIN   +
                         SLOPE_PCT          +         TOPWET       +        GEOL_REEDBUSH_DOM  +  AWCAVE      +         PERMAVE         +     CLAYAVE    +         
                         SILTAVE         +     SANDAVE        +      DEVNLCD06       +     PCT_IRRIG_AG     +    FORESTNLCD06     +    PLANTNLCD06     +     WATERNLCD06  +       
                         SNOWICENLCD06    +    IMPNLCD06   
                       , data = training, mtry = j,importance = T,na.action = na.omit,ntree=500)
    oob.vals[j] = rf$mse[length(rf$mse)]
  }
  
  mtry_best = which.min(oob.vals)
  
  
  ## Run the optimized model
  rf <- randomForest(zeroflowfirst ~ p_mm_ + pet_mm_ + T_max_c_ + T_min_c_ + p2z_cv_ + SNOW_PCT_PRECIP +  SNOW_PCT_PRECIP   +   ELEV_MEAN_M_BASIN   +
                       SLOPE_PCT          +         TOPWET       +        GEOL_REEDBUSH_DOM  +  AWCAVE      +         PERMAVE         +     CLAYAVE    +         
                       SILTAVE         +     SANDAVE        +      DEVNLCD06       +     PCT_IRRIG_AG     +    FORESTNLCD06     +    PLANTNLCD06     +     WATERNLCD06  +       
                       SNOWICENLCD06    +    IMPNLCD06   
                     , data = training, mtry = mtry_best,importance = T,na.action = na.omit,ntree=500)
  
  
  # Predict the result of the model
  testing <- predict(rf, testing)
  training <- predict(rf, training)
  
  
  out = list(rf=rf,
             testing=testing,
             training = training,
             mtry_best = mtry_best,
             w_seed_store = seed[i],
             rf_importance = rf$importance[,1])
  
  return(out)
  
}


varImpPlot(w_output_firstNo[[1]]$rf,main = "First No-Flow (Winter)")
varImpPlot(s_output_firstNo[[1]]$rf,main = "First No-Flow (Spring)")
varImpPlot(su_output_firstNo[[1]]$rf,main = "First No-Flow (Summer)")
varImpPlot(f_output_firstNo[[1]]$rf,main = "First No-Flow (Fall)")


#####################################################################################
#####################################################################################
############################# Centroid Date of No Flow ##############################
#####################################################################################
#####################################################################################

seed = sample(1:10000,100)

# Setup cluster
cl <- parallel::makeCluster(3)
doParallel::registerDoParallel(cl)

# Set seed and select training data
season = su_sub

su_output_zCenter = foreach(i= 1:100,.packages = c('randomForest','caret','foreach')) %dopar% {
  
  # Set seed and split data
  set.seed(seed[i])
  print(seed[i])
  season$index <- seq(1:nrow(season))
  training_size = nrow(season)*0.7
  training <- as.data.frame(season[sample(1:nrow(season),training_size, replace = F),])
  testing <- as.data.frame(season[!season$index %in% training$index,])
  
  #####CHOOSE METAPARAMETERS WITH CROSSFOLD VALIDATION THEN GROW RF#####
  trainControl <- trainControl(method = "oob", number = 1, returnResamp = 'all', verboseIter=F)
  
  # build the model and test for best mtry to use 
  oob.vals <- vector(length=10)
  foreach (j = 1:10) %do% {
    rf <- randomForest(zeroflowcentroiddate ~ p_mm_ + pet_mm_ + T_max_c_ + T_min_c_ + p2z_cv_ + SNOW_PCT_PRECIP +  SNOW_PCT_PRECIP   +   ELEV_MEAN_M_BASIN   +
                         SLOPE_PCT          +         TOPWET       +        GEOL_REEDBUSH_DOM  +  AWCAVE      +         PERMAVE         +     CLAYAVE    +         
                         SILTAVE         +     SANDAVE        +      DEVNLCD06       +     PCT_IRRIG_AG     +    FORESTNLCD06     +    PLANTNLCD06     +     WATERNLCD06  +       
                         SNOWICENLCD06    +    IMPNLCD06   
                       , data = training, mtry = j,importance = T,na.action = na.omit,ntree=500)
    oob.vals[j] = rf$mse[length(rf$mse)]
  }
  
  mtry_best = which.min(oob.vals)
  
  
  ## Run the optimized model
  rf <- randomForest(zeroflowcentroiddate ~ p_mm_ + pet_mm_ + T_max_c_ + T_min_c_ + p2z_cv_ + SNOW_PCT_PRECIP +  SNOW_PCT_PRECIP   +   ELEV_MEAN_M_BASIN   +
                       SLOPE_PCT          +         TOPWET       +        GEOL_REEDBUSH_DOM  +  AWCAVE      +         PERMAVE         +     CLAYAVE    +         
                       SILTAVE         +     SANDAVE        +      DEVNLCD06       +     PCT_IRRIG_AG     +    FORESTNLCD06     +    PLANTNLCD06     +     WATERNLCD06  +       
                       SNOWICENLCD06    +    IMPNLCD06   
                     , data = training, mtry = mtry_best,importance = T,na.action = na.omit,ntree=500)
  
  
  # Predict the result of the model
  testing <- predict(rf, testing)
  training <- predict(rf, training)
  
  
  out = list(rf=rf,
             testing=testing,
             training = training,
             mtry_best = mtry_best,
             w_seed_store = seed[i],
             rf_importance = rf$importance[,1])
  
  return(out)
  
}


varImpPlot(w_output_zCenter[[1]]$rf,main = "No-Flow Centroid (Winter)")
varImpPlot(s_output_zCenter[[1]]$rf,main = "No-Flow Centroid (Spring)")
varImpPlot(su_output_zCenter[[1]]$rf,main = "No-Flow Centroid (Summer)")
varImpPlot(f_output_zCenter[[1]]$rf,main = "No-Flow Centroid (Fall)")


#####################################################################################
#####################################################################################
############################# CV / Mean peak2zero ###################################
#####################################################################################
#####################################################################################

seed = sample(1:10000,100)

# Setup cluster
cl <- parallel::makeCluster(3)
doParallel::registerDoParallel(cl)

# Set seed and select training data
season = f_sub

f_output_p2z = foreach(i= 1:100,.packages = c('randomForest','caret','foreach')) %dopar% {
  
  # Set seed and split data
  set.seed(seed[i])
  print(seed[i])
  season$index <- seq(1:nrow(season))
  training_size = nrow(season)*0.7
  training <- as.data.frame(season[sample(1:nrow(season),training_size, replace = F),])
  testing <- as.data.frame(season[!season$index %in% training$index,])
  
  #####CHOOSE METAPARAMETERS WITH CROSSFOLD VALIDATION THEN GROW RF#####
  trainControl <- trainControl(method = "oob", number = 1, returnResamp = 'all', verboseIter=F)
  
  # build the model and test for best mtry to use 
  oob.vals <- vector(length=10)
  foreach (j = 1:10) %do% {
    rf <- randomForest(p2z_cv_ ~ p_mm_ + pet_mm_ + T_max_c_ + T_min_c_  + SNOW_PCT_PRECIP +  SNOW_PCT_PRECIP   +   ELEV_MEAN_M_BASIN   +
                         SLOPE_PCT          +         TOPWET       +        GEOL_REEDBUSH_DOM  +  AWCAVE      +         PERMAVE         +     CLAYAVE    +         
                         SILTAVE         +     SANDAVE        +      DEVNLCD06       +     PCT_IRRIG_AG     +    FORESTNLCD06     +    PLANTNLCD06     +     WATERNLCD06  +       
                         SNOWICENLCD06    +    IMPNLCD06   
                       , data = training, mtry = j,importance = T,na.action = na.omit,ntree=500)
    oob.vals[j] = rf$mse[length(rf$mse)]
  }
  
  mtry_best = which.min(oob.vals)
  
  
  ## Run the optimized model
  rf <- randomForest(zeroflowcentroiddate ~ p_mm_ + pet_mm_ + T_max_c_ + T_min_c_ + p2z_cv_ + SNOW_PCT_PRECIP +  SNOW_PCT_PRECIP   +   ELEV_MEAN_M_BASIN   +
                       SLOPE_PCT          +         TOPWET       +        GEOL_REEDBUSH_DOM  +  AWCAVE      +         PERMAVE         +     CLAYAVE    +         
                       SILTAVE         +     SANDAVE        +      DEVNLCD06       +     PCT_IRRIG_AG     +    FORESTNLCD06     +    PLANTNLCD06     +     WATERNLCD06  +       
                       SNOWICENLCD06    +    IMPNLCD06   
                     , data = training, mtry = mtry_best,importance = T,na.action = na.omit,ntree=500)
  
  
  # Predict the result of the model
  testing <- predict(rf, testing)
  training <- predict(rf, training)
  
  
  out = list(rf=rf,
             testing=testing,
             training = training,
             mtry_best = mtry_best,
             w_seed_store = seed[i],
             rf_importance = rf$importance[,1])
  
  return(out)
  
}


varImpPlot(w_output_p2z[[1]]$rf,main = "Peak-2-Zero (Winter)")
varImpPlot(s_output_p2z[[1]]$rf,main = "Peak-2-Zero (Spring)")
varImpPlot(su_output_p2z[[1]]$rf,main = "Peak-2-Zero (Summer)")
varImpPlot(f_output_p2z[[1]]$rf,main = "Peak-2-Zero (Fall)")


#####################################################################################
#####################################################################################
############################# Recession constant A ##################################
#####################################################################################
#####################################################################################


#####################################################################################
#####################################################################################
############################# Recession constant B ##################################
#####################################################################################
#####################################################################################






#####################################################################################
#####################################################################################
############################# SCRAPS ################################################
#####################################################################################
#####################################################################################




# Visualize distribution of data
# par(mfrow = c(2,1))
# hist(w_sub$fractionnoflow,breaks=seq(0,1,.1))
# hist(training$fractionnoflow,breaks=seq(0,1,.1))











# error estimates
# rmse
rmse.test.rf <- round(rmse(testing$djfnoflow, testing$djfnoflow.rf), digits = 2)
rmse.train.rf <- round(rmse(training$djfnoflow, training$djfnoflow.rf), digits = 2)
rmse.oob.rf <- round(rmse(rf$predicted, training$djfnoflow), digits = 2)
#rsquared
lm.test <- lm(testing$djfnoflow.rf~ testing$djfnoflow) %>% summary()
lm.test.r2 <- lm.test$r.squared

lm.train <- lm(training$djfnoflow.rf~ training$djfnoflow) %>% summary()
lm.train.r2 <- lm.train$r.squared

lm.oob <- lm(rf$predicted~training$djfnoflow) %>% summary()
lm.oob.r2 <- lm.oob$r.squared
#fill the error matrix
error <- c(rmse.train.rf,rmse.oob.rf,rmse.test.rf,lm.train.r2,lm.oob.r2,lm.test.r2)


############################### plot modeled output #####################################

# plot(rf$y, rf$predicted, 
#      ylim = c(-1,4), xlim = c(-1,4), pch = 21, bg = 'white', col = 'white', typ = 'n',
#      ylab = 'Modeled N Removed (mg/L)', xlab = 'Measured N Removed (mg/L)', las = 1, main = 'Calibration/Validataion')
# abline(0,1, col = 'red', lty = 2)


plot(rf$y, rf$predicted)
abline(0,1, col = 'red', lty = 2)

#training measured data vs. oob predictions
points(training$djfnoflow, rf$predicted, pch = 21, bg = '#8da0cb', col = 'black', cex = 1.4)

#training measured data vs. modeled training data
points(training$djfnoflow, training$djfnoflow.rf, pch = 21, bg = '#66c2a5', col = 'black', cex = 1.4)
#testing measured data vs. modeled testing data
points(testing$djfnoflow, testing$djfnoflow.rf, pch = 21, bg = '#fc8d62', col = 'black', cex = 1.4)
legend('topleft', text.col = c('#7570b3','#1b9e77','#d95f02'), legend = c(paste('oob prediction',rmse.oob.rf), paste('training',rmse.train.rf), paste('testing',rmse.test.rf)), bty = 'n',title = 'RMSE')

############## plot the residuals
# plot(rf$y, rf$y-rf$predicted, ylim = c(-2,2), xlim = c(-1,4), pch = 21, bg = 'white', col = 'white', typ = 'n',
#      ylab = 'Measured - Modeled N Removed (mg/L)', xlab = 'Measured N Removed (mg/L)', las = 1, main = 'Residual Plot')

plot(rf$y, rf$y-rf$predicted)
abline(0,0, col = 'red', lty = 2)
#training measured data vs. measured - oob modeled
points(rf$y, rf$y-rf$predicted, pch = 21, bg = '#8da0cb', col = 'black', cex = 1.4)
#training measured data vs. measured-modeled
points(rf$y, rf$y-training$djfnoflow.rf, pch = 21, bg = '#66c2a5', col = 'black', cex = 1.4)
#testing training vs training measured - training modeled
points(testing$djfnoflow, testing$djfnoflow-testing$djfnoflow.rf, pch = 21, bg = '#fc8d62', col = 'black', cex = 1.4)
legend('topleft', text.col = c('#7570b3','#1b9e77','#d95f02'), legend = c(paste('oob prediction',rmse.oob.rf), paste('training',rmse.train.rf), paste('testing',rmse.test.rf)), bty = 'n',title = 'RMSE')