#####################################################################
##
## Script name: dryingMetrics_mergeAntecedant.R
##
## Author: Adam N. Price
##
## Date Created: 2020-08-14
##
## Copyright (c) Adam N. Price, 2020
## Email: adnprice@ucsc.edu
##
############################# Description ##########################
## Merges drying metrics by event with antcedant conditions on both
## peak and no-flow occurance
##   
##
############################# Packages #############################

library(tidyverse)
library(lubridate)

############################# Code ################################




############## Read in and join event dataset and antecedent conditions by peak date occurance / drying start date 
event_dat = read.csv("../data/metrics_by_event.csv")

event_dat = transform(event_dat, Date = as.Date(paste0(calendar_year, "-1-1")) + peak_date)

out = data.frame()
               
files = list.files('../data/daily_flow_and_climate_and_API/all/',pattern = '*csv',full.names = T)

for (i in files){
  ant_dat = read.csv(i)
  ant_dat$Date = ymd(ant_dat$Date)
  out = dplyr::inner_join(event_dat,ant_dat,by=c("Date","gage")) %>% bind_rows(out,.)
}

#### Write data
write.csv(out,"../data/metrics_by_event_withAntCondPeak.csv")

###############Read in and join event dataset and antecedent conditions by start of zeroflow

event_dat = read.csv("../data/metrics_by_event.csv")

event_dat = transform(event_dat, Date = as.Date(paste0(calendar_year, "-1-1")) + dry_date_start)

out = data.frame()

files = list.files('../data/daily_flow_and_climate_and_API/all/',pattern = '*csv',full.names = T)

for (i in files){
  print(which(files==i))
  ant_dat = read.csv(i)
  ant_dat$Date = ymd(ant_dat$Date)
  out = dplyr::inner_join(event_dat,ant_dat,by=c("Date","gage")) %>% bind_rows(out,.)
}


#### Write data
write.csv(out,"../data/metrics_by_event_withAntCondNoFlow.csv")
