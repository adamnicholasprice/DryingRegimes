#######################################################
#
# Author: Adam Price
# Date: 20200116
# Description: This code performs a PCA of the gages2 no-flow dataset
#
#
#
#
#######################################################

library(feather)
library(tidyr)
library(here)


## Load data

data = feather::read_feather(here::here('../data/mean_annual_no_flow_and_climate_metric_means_for_no_flow_sites_082819_with_info.feather'))


ref_data = data[data$CLASS=='Ref',]


ref.pca <- prcomp(ref_data[,c(71,)], center = TRUE,scale. = TRUE)
