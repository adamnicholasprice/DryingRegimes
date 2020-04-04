###################################################
#
# Author: Adam Price
# Date: 2020-01-24
# Code used to get all proxies in one data frame
#
#
#
####################################################

library(here)


site = read.csv('../data/mean_annual_no_flow_and_climate_metric_means_for_no_flow_sites_082819_with_info.csv')['site']

ref = read.csv('../data/mean_annual_no_flow_and_climate_metric_means_for_no_flow_sites_082819_with_info.csv')['CLASS']

nf_frac  = read.csv('../data/mean_annual_no_flow_and_climate_metric_means_for_no_flow_sites_082819_with_info.csv')['annualfractionnoflow']

nf_first = read.csv('../data/mean_annual_no_flow_and_climate_metric_means_for_no_flow_sites_082819_with_info.csv')['zeroflowfirst']

nf_cvnoflow = read.csv('../data/mean_annual_no_flow_and_climate_metric_means_for_no_flow_sites_082819_with_info.csv')['cvlengthnoflow']

nf_mediannoflow = read.csv('../data/mean_annual_no_flow_and_climate_metric_means_for_no_flow_sites_082819_with_info.csv')['medianlengthnoflow']

nf_meannoflow = read.csv('../data/mean_annual_no_flow_and_climate_metric_means_for_no_flow_sites_082819_with_info.csv')['meanlengthnoflow']

nf_freq = read.csv('../data/mean_annual_no_flow_and_climate_metric_means_for_no_flow_sites_082819_with_info.csv')['totalnoflowperwyear']

nf_p2z = read.csv('../data/peak2zero.csv')

new  = nf_p2z

colnames(new)[2:ncol(new)] = c('site','p2z_freq','p2z_mean','p2z_median','p2z_sd','p2z_cv')

new = new[,2:ncol(new)]

data = data.frame(site,ref,nf_frac,nf_first,nf_cvnoflow,nf_mediannoflow,nf_meannoflow,nf_freq)  

tt= merge(data,new,by='site')


write.csv(tt,'../data/framework_metrics_peak2zero.csv',row.names = FALSE)

all = read.csv('../data/mean_annual_no_flow_and_climate_metric_means_for_no_flow_sites_082819_with_info.csv')

all_p2z = merge(all,new,by='site')


write.csv(all_p2z,'meanannualnf_climate_peak2zero.csv',row.names = FALSE)
