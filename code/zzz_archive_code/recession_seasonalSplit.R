#####################################################################
##
## Script name: 
##
## Author: Adam N. Price
##
## Date Created: 2020-04-22
##
## Copyright (c) Adam N. Price, 2020
## Email: adnprice@ucsc.edu
##
############################# Description ##########################
## 1. Split logistic regressions into seasons, 
## 2. calculate mean, median, and std dev
## 3. write out to file
##
############################# Packages #############################

## load up the packages we will need:  (uncomment as required)
library(tidyverse)
library(lubridate)

############################# Code ################################

data = read.csv('../data/metrics_by_event.csv')
data$dry_date_start = lubridate::as_date(data$dry_date_start)

gages = unique(data$gage)


output = data.frame()

for (i in seq(gages)){
  ind_w = rbind(data[which(data$gage==gages[i] & between(lubridate::month(data$dry_date_start),1,2)),],data[which(data$gage==gages[i] & between(lubridate::month(data$dry_date_start),12,13)),])
  ind_s = data[which(data$gage==gages[i] & between(lubridate::month(data$dry_date_start),3,5)),]
  ind_su = data[which(data$gage==gages[i] & between(lubridate::month(data$dry_date_start),6,8)),]
  ind_f = data[which(data$gage==gages[i] & between(lubridate::month(data$dry_date_start),9,11)),]
  
  
  temp_df = as.data.frame(cbind(gages[i],
                                length(ind_w$event_id),
                                bw_mean = mean(ind_w$b), bw_median = median(ind_w$b), bw_sd = sd(ind_w$b),
                                aw_mean = mean(ind_w$log_a_norm), aw_median = median(ind_w$log_a_norm), aw_sd = sd(ind_w$log_a_norm),
                                length(ind_s$event_id),
                                bs_mean = mean(ind_s$b), bs_median = median(ind_s$b), bs_sd = sd(ind_s$b),
                                as_mean = mean(ind_s$log_a_norm), as_median = median(ind_s$log_a_norm), as_sd = sd(ind_s$log_a_norm),
                                length(ind_su$event_id),
                                bsu_mean = mean(ind_su$b), bsu_median = median(ind_su$b), bsu_sd = sd(ind_su$b),
                                asu_mean = mean(ind_su$log_a_norm), asu_median = median(ind_su$log_a_norm), asu_sd = sd(ind_su$log_a_norm),
                                length(ind_f$event_id),
                                bf_mean = mean(ind_f$b), bf_median = median(ind_f$b), bf_sd = sd(ind_f$b),
                                af_mean = mean(ind_f$log_a_norm), af_median = median(ind_f$log_a_norm), af_sd = sd(ind_f$log_a_norm)))
  output = rbind(output,temp_df)
  rm(temp_df)
}


# Clean data
output[output == "NaN"] <- NA
output[output == "-Inf"] <- NA
output[output == "Inf"] <- NA

colnames(output) <- c("site_no",
                      'b_count_djf','b_mean_djf','b_median_djf','b_sd_djf','log_a_norm_mean_djf','log_a_norm_median_djf','log_a_norm_sd_djf',
                      'b_count_mam','b_mean_mam','b_median_mam','b_sd_mam','log_a_norm_mean_mam','log_a_norm_median_mam','log_a_norm_sd_mam',
                      'b_count_jja','b_mean_jja','b_median_jja','b_sd_jja','log_a_norm_mean_jja','log_a_norm_median_jja','log_a_norm_sd_jja',
                      'b_count_son','b_mean_son','b_median_son','b_sd_son','log_a_norm_mean_son','log_a_norm_median_son','log_a_norm_sd_son')

write.csv(output,'../data/regression_Seasonal.csv')
