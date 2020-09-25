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
##   
##
############################# Packages #############################

library(ggplot2)
library(tidyverse)
library(lubridate)

# library(GGally)
# library(lubridate)
# library(caret)
# library(randomForest)
# library(RColorBrewer)
# library(rsample)
# library(foreign)
# library(rgdal)
# library(Metrics)
# library(plm)
# library(viridis)
# library(ggRandomForests)
# library(foreach)
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


df = df %>% select(gage,Name,CLASS,dec_lat_va,dec_long_va,
hier.4.clust,gmm.clust,kmeans.clust,dbscan.cluster,
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

################# Random forest ###############



