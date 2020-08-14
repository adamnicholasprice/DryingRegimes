#####################################################################
##
## Script name: DryingMetrics_data
##
## Author: John Hammond
## Co-Author: Adam N. Price
##
## Date Updated: 2020-07-31
##
## Copyright (c) Adam N. Price, 2020
## Email: adnprice@ucsc.edu
##
############################# Description ##########################
##
## Joins drying metrics, watershed characteristics, antecedent conditions,
##   streamflow stats, and gage metadata
##
############################# Packages #############################

## load up the packages we will need:  (uncomment as required)

library(dplyr)
library(lubridate)
library(vroom)
library(purrr)

############################# Code ################################


files <- list.files('../data/daily_flow_and_climate_and_API/all',pattern = ".csv",full.names = T)
sites <- list.files('../data/daily_flow_and_climate_and_API/all',pattern = ".csv")

# i ran this loop to add site # as a column to each file
# for(i in seq_along(files)){
#   # i = 1
#   example <- read.csv(files[i])
#   site <- strsplit(sites[i],split = "_")[[1]][1]
#   site <- as.numeric(site)
#   example$gage <- site
#   write.csv(example, paste0(files[i]))
#   print(site)
# }


# make a long dataset from all files
# library(plyr)
# setwd("C:\\Users\\jhammond\\Desktop\\GRL_060220\\drying_regimes\\daily_flow_and_climate_and_API\\all")
# 
# files <- list.files(pattern = ".csv")
# 
# alldata <-  do.call(rbind.fill, lapply(files, read.csv))
# alldata$Date <- as.Date(as.character(alldata$Date))


# with long dataset calculate flow percentiles


output = data.frame()
for (i in seq_along(files)){
  out = vroom::vroom(files[i],col_select = c(Q_cfs,gage),col_types = c(Q_cfs = "d",gage = 'd')) %>% dplyr::summarise(percentile25_mmd = quantile(Q_cfs, probs = c(0.25),na.rm = TRUE),
                                                                                                                  percentile50_mmd = quantile(Q_cfs, probs = c(0.50),na.rm = TRUE),
                                                                                                                  percentile75_mmd = quantile(Q_cfs, probs = c(0.75),na.rm = TRUE),
                                                                                                                  gage=unique(gage))
  output = rbind(output,out)
}
###### I want to add the following to the above dataset
# - Stream flow at time of drying 
# - Antecedant conditions "
# - Raditation "
# - Mean precip during drying ?
# - 

t =vroom(files[1])
t$Date = yday(t$Date)

gage9t$gage == events$gage

# read in events dataset, then merge with antecedent conditions
events <- read.csv("../data/metrics_by_event.csv")

events$start_of_year <- paste0(events$calendar_year,"-01","-01")
events$dry_date_start_as_date <- as.Date(events$start_of_year)+events$dry_date_start-1
events_with_antecedent <- merge(events, output, by = "gage", all.x= TRUE)
events_with_antecedent <- merge(events_with_antecedent, alldata, by.x = c("gage","dry_date_start_as_date"), by.y = c("gage","Date"), all.x= TRUE)

write.csv(events_with_antecedent, "metrics_by_event_with_antecedent_climate_072420.csv")