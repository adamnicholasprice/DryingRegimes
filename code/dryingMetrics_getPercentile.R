#####################################################################
##
## Script name: dryingMetics_getPercentile.R
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
## Reads in streamflow time series for each gage and returns
##   - 5th percentile (q5)
##   -25th percentile (q25)
##   -50th percentile (q50)
##   -75th percentile (q75)
##   - Gage Number (gage)
############################# Packages #############################

library(tidyverse)

############################# Code ################################




out = data.frame()

files = list.files('../data/daily_flow_and_climate_and_API/all/',pattern = '*csv',full.names = T)


# files = files[1:5]
for (i in files){
  ant_dat = read.csv(i) %>% dplyr::summarise(q5 = quantile(Q_cfs, probs = c(0.05),na.rm = TRUE),
                                   q25 = quantile(Q_cfs, probs = c(0.25),na.rm = TRUE),
                                   q50 = quantile(Q_cfs, probs = c(0.50),na.rm = TRUE),
                                   q75= quantile(Q_cfs, probs = c(0.75),na.rm = TRUE),
                                   gage=unique(gage))
  out = rbind(out,ant_dat)

}


write.csv(out,"../data/gages_percentile.csv")
