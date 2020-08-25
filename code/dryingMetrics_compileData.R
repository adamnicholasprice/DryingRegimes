#####################################################################
##
## Script name: dryingMetrics_compileData.R
##
## Author: Adam N. Price
##
## Date Created: 2020-08-14
##
## Copyright (c) Adam N. Price, 2020
## Email: adnprice@ucsc.edu
##
############################# Description ##########################
##
## Pulls all relevent data files and merges into 3 data files for analysis
##   - 
##
############################# Packages #############################

library(tidyverse)

############################# Code ################################

### Load datasets

# Event based dataset
event = read.csv('../data/metrics_by_event.csv') %>% na.omit()

# Ecoregion dataset
er.dat = read.csv('../data/gages_with_ecoregion.csv')

# Meta data and landuse
metdata = read.csv('../data/mean_annual_no_flow_and_climate_metric_means_for_no_flow_sites_082819_with_info.csv')

# Watershed storage characteristics from GAGES2
ws.dat = read.csv('../data/20200211_watershed_storage.csv')

# Percentile data for 909 subset gages
percentile.dat = read.csv('../data/gages_percentile.csv')

# Antecedent conditions at peak

peak.ant = read.csv('../data/metrics_by_event_withAntCondPeak.csv')

noflow.ant = read.csv('../data/metrics_by_event_withAntCondNoFlow.csv')


############## Filter event data

alpha = 0.10
event.filt = event[event$drying_rate >0 & event$p_value<= alpha,] 

peak.ant = peak.ant[peak.ant$drying_rate >0 & peak.ant$p_value<= alpha,]
peak.ant = peak.ant[!is.na(peak.ant$drying_rate),]

noflow.ant = noflow.ant[noflow.ant$drying_rate >0 & noflow.ant$p_value<= alpha,]
noflow.ant = noflow.ant[!is.na(noflow.ant$drying_rate),]

# calculate event counts and mean data by gage

mean.event = event[,c("gage","peak_date","peak_value","peak_quantile","peak2zero","drying_rate","dry_date_start","dry_date_mean",'dry_dur')] %>% 
  group_by(gage) %>% 
  summarise(.,mean_drying_rate = mean(drying_rate),
            n_events = n(),
            mean_peak_date = mean(peak_date),
            mean_peak_value = mean(peak_value),
            mean_peak_quantile = mean(peak_quantile),
            mean_peak2zero = mean(peak2zero),
            mean_dry_date_start = mean(dry_date_start),
            mean_dry_dur = mean(dry_dur))


########### Merge all data

# raw data
event.raw= event %>% 
  left_join(.,er.dat,by=c('gage'='unique.dat.gage.')) %>% 
  inner_join(.,metdata,by=c('gage'="site")) %>%
  inner_join(.,ws.dat,by=c('gage'='uid')) %>%
  inner_join(.,percentile.dat,by='gage')

# filtered data
event.filtered= event.filt %>% 
  left_join(.,er.dat,by=c('gage'='unique.dat.gage.')) %>% 
  inner_join(.,metdata,by=c('gage'="site")) %>%
  inner_join(.,ws.dat,by=c('gage'='uid')) %>%
  inner_join(.,percentile.dat,by='gage')

# Mean data
event.mean = mean.event %>% 
  left_join(.,er.dat,by=c('gage'='unique.dat.gage.')) %>% 
  inner_join(.,metdata,by=c('gage'="site")) %>%
  inner_join(.,ws.dat,by=c('gage'='uid')) %>%
  inner_join(.,percentile.dat,by='gage')

############### Write all data

write.csv(event.raw,'../data/metrics_by_event_combined_raw.csv')

write.csv(event.filtered,'../data/metrics_by_event_combined.csv')

write.csv(event.mean,'../data/metrics_by_event_mean.csv')

write.csv(noflow.ant,'../data/metrics_by_event_withAntCondNoFlow_FILTERED.csv')

write.csv(peak.ant,'../data/metrics_by_event_withAntCondPeak_FILTERED.csv')


