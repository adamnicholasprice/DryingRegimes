#####################################################################
##
## Script name:
##
## Author: Adam N. Price
##
## Date Created: 2021-01-07
##
## Copyright (c) Adam N. Price, 2021
## Email: adnprice@ucsc.edu
##
############################# Description ##########################
##
## 
##   
##
############################# Packages #############################
##
## 
library(tidyverse)   
##
############################# Code ################################

dat = read.csv('data/kmeans.csv')




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Average events per gage and range
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

n = dat %>%
  select(gage) %>%
  group_by(gage) %>%
  count()

summary(n)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# SI Table S#: Box plot significance test
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
dat = read.csv('data/kmeans.csv') %>% 
  select(kmeans,peak2zero,drying_rate,dry_dur,peak_quantile,freq_local,dry_date_start)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# SI Table S#: Cluster stats
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

dat = read.csv('data/kmeans.csv') %>% 
  select(kmeans,peak2zero,drying_rate,dry_dur,peak_quantile,freq_local,dry_date_start)

dat$peak_quantile = dat$peak_quantile*100

cv <-function(x){
  sd(x)/mean(x)
}

dat %>% select(-kmeans) %>% summarize_all(list(mean,cv))

dat %>% group_by(kmeans) %>% 
  summarize_all(list(mean,cv))


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# section 3.2 timing
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

dat = read.csv('data/kmeans.csv')

j1 = lubridate::yday("1971-07-01")

s1 = lubridate::yday("1971-09-01")

j2s.prop = length(dat[dat$dry_date_start>=j1 & dat$dry_date_start<=s1,"dry_date_start"])/length(dat$dry_date_start)
