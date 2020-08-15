#####################################################################
##
## Script name: getAggRegions.R
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
##  Script returns the aggrigated ecoregions as specified in Hammond et al. 2020
##   
##
############################# Packages #############################

library(exactextractr)
library(sf)
library(raster)
library(sp)
library(rgdal)
library(tidyverse)

############################# Code ################################


############ Load rasters and shapefile #######
data <- raster("../data/rasterized_regions.tif")
crs <- crs(data)
gages<- st_read("../data/all_conus_v2/all_conus_v2.shp") %>%st_transform(.,"+proj=laea +lat_0=45 +lon_0=-100 +x_0=0 +y_0=0 +a=6370997 +b=6370997 +units=m +no_defs")


###### Extract the ecoregions 
test <- exact_extract(data, gages, 'majority')

######### Duplicate output into data frame
ext = as.data.frame(test)
ext$gage <- gages$GAGE_ID

########### Load the aggregated ecoregion file
agg_eco =as.data.frame(readOGR('../data/dissolved_ecoregions_060220/dissolved_ecoregions_060220.shp'))
agg_eco$NA_L1CODE = as.numeric(agg_eco$NA_L1CODE)


################## Join extracted data and aggrigated ecoregion file
tt = left_join(ext,agg_eco,by=c("test"="NA_L1CODE"))
tt$test = NULL
tt$gage = as.numeric(as.character(tt$gage))


#################### Extract the ~900 gages that correspond to event data
dat = read.csv('../data/metrics_by_event.csv')
gage = as.data.frame(unique(dat$gage))
to.write = left_join(gage,tt,by=c("unique(dat$gage)" = "gage"))


################### Write to file

write.csv(to.write, "../data/gages_with_ecoregion.csv")

