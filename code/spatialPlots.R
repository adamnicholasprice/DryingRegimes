#####################################################################
##
## Script name: 
##
## Author: John Hammond
## Co-author: Adam N. Price
##
## Date Created: 2020-07-15
##
## Copyright (c) Adam N. Price, 2020
## Email: adnprice@ucsc.edu
##
############################# Description ##########################
## CONUS plot of drying metrics 
##  
##   
##
############################# Packages #############################

library(ggplot2)
library(ggmap)
library(maps)
library(mapdata)
library(dplyr)
library(scales)
library(maptools)
library(dataRetrieval)
library(viridis)
library(gridExtra)

############################# Code ################################

# plots of subannual metrics

states <- map_data("state")


######## Distribution of gages

dist<- ggplot(data = states) + 
  geom_polygon(aes(x = long, y = lat, group = group), fill = "gray", color = "black") + 
  ylab('Mean Number of Drying Events\n(days)\nlat') +
  coord_fixed(1.3, xlim = c(-124.25,-70), ylim = c(26,48.5)) +
  theme_linedraw() + 
  geom_point(data=dat, aes(x=dec_long_va, y=dec_lat_va, shape = factor(CLASS), colour = Name), size = 2, alpha=1) 
  # geom_text(data=winter[!is.na(winter$n_event_mean),], aes(x=long_cent, y=lat_cent,label=round(n_event_mean,3)))+
  # scale_color_gradientn(name = "Winter Mean Number Drying Events", colours = viridis(10),labels=trans_format("identity", function(x) round(x,2)),breaks= scale_breaks,values = rescale(scale_breaks))+
  # scale_color_viridis(breaks= scale_breaks,values = rescale(scale_breaks))+
  # scale_color_gradientn(trans = 'log',labels=trans_format("identity", function(x) round(x,2)),limits = c(0.01,lmax),colors = viridis(10))+
  # scale_shape_manual(values=c(20, 17))+guides(shape = FALSE)


######## Distribution of events
counts <- ggplot(data = states) + 
  geom_polygon(aes(x = long, y = lat, group = group), fill = "gray", color = "black") + 
  ylab('Mean Number of Drying Events\n(days)\nlat') +
  coord_fixed(1.3, xlim = c(-124.25,-70), ylim = c(26,48.5)) +
  theme_linedraw() + 
  geom_point(data=dat.mean, aes(x=dec_long_va, y=dec_lat_va, shape = factor(CLASS), colour = n_events), size = 2, alpha=1) +
  scale_color_gradientn(trans = 'log',labels=trans_format("identity", function(x) round(x,2)),limits = c(1,max(dat.mean$n_events)),colors = viridis(20),breaks = c(0,1,10,50,100,200))

counts  


