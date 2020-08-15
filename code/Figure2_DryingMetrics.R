#####################################################################
##
## Script name: 
##
## Author: Adam N. Price
##
## Date Created: 2020-08-07
##
## Copyright (c) Adam N. Price, 2020
## Email: adnprice@ucsc.edu
##
############################# Description ##########################
##
## 
##   
##
############################# Packages #############################

library(ggridges)
library(ggplot2)
library(dplyr)
library(lubridate)
library(ggpubr)
library(RColorBrewer)
library(tidyverse)
library(patchwork)

############################# Code ################################



dat = read.csv('../data/metrics_by_event.csv')

dat.mean = dat %>% group_by(gage) %>% summarise_all(funs(mean),na.rm=TRUE)
dat.mean = dat %>% group_by(gage) %>% summarise_all(funs(median),na.rm=TRUE)

gage.type = read.csv('../data/mean_annual_no_flow_and_climate_metric_means_for_no_flow_sites_082819_with_info.csv')[]



dat = left_join(dat,gage.type, by = c("gage"= "site"))
dat = left_join(dat,test, by = "gage")

dat.mean = left_join(dat.mean,gage.type, by = c("gage"= "site"))
dat.mean = left_join(dat.mean,test, by = "gage")

counts = dat %>% group_by(gage) %>% count()
dat.mean = left_join(dat.mean,counts,by="gage")

###### Drying Metrics to plot
# Drying rate (Rate of change)
# Peak2zero (?)
# Drying Duration (Duration)
# Number of Drying events (Frequency)
# Centroid of drying (Timing)
# ? (Magnitude/severity)





#### Plots of events

plot_lay <-function(){
  list(
    geom_density_ridges(scale=2,alpha = .75,panel_scaling = FALSE,rel_min_height=0.01),
    # scale_fill_manual(values = pal_regions),
    # scale_color_manual(values = pal_regions),
    # ylab("Mean drying event length\n(days)") ,
    # xlim(0,4) ,
    theme(
      # panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      # Change plot and panel background
      plot.background = element_blank(),
      # panel.background = element_blank(),
      axis.text.y = element_blank(),
      axis.title.x = element_blank(),
      axis.ticks.y = element_blank()
    )
  )
}

drying_rate = ggplot(data=dat, aes(x = drying_rate,y=NA_L1NAME,group=paste(NA_L1NAME,CLASS),fill=NA_L1NAME,linetype=CLASS)) + 
  plot_lay() +xlim(0,4) + ylab("Drying Rate\n(1/days)")

peak2zero = ggplot(data=dat, aes(x = peak2zero,y=NA_L1NAME,group=paste(NA_L1NAME,CLASS),fill=NA_L1NAME,linetype=CLASS)) + 
  plot_lay() + xlim(0,200) + ylab("Peak2Zero \n(1/days)")

dry_dur = ggplot(data=dat, aes(x = dry_dur,y=NA_L1NAME,group=paste(NA_L1NAME,CLASS),fill=NA_L1NAME,linetype=CLASS)) + 
  plot_lay() + xlim(0,100) + ylab("No Flow Duration\n(days)")

dry_date_mean = ggplot(data=dat, aes(x = dry_date_mean,y=NA_L1NAME,group=paste(NA_L1NAME,CLASS),fill=NA_L1NAME,linetype=CLASS)) + 
  plot_lay()+ ylab("Drying Centroid\n(days)")

all.points  = drying_rate + peak2zero + dry_dur + dry_date_mean + plot_layout(ncol = 4, guides = "collect") & theme(legend.position = 'right') 

all.points


########## Mean events

drying_rate = ggplot(data=dat.mean, aes(x = drying_rate,y=NA_L1NAME,group=paste(NA_L1NAME,CLASS),fill=NA_L1NAME,linetype=CLASS)) + 
  plot_lay() + ylab("Drying Rate\n(1/days)")

peak2zero = ggplot(data=dat, aes(x = peak2zero,y=NA_L1NAME,group=paste(NA_L1NAME,CLASS),fill=NA_L1NAME,linetype=CLASS)) + 
  plot_lay() + xlim(0,100) + ylab("Peak2Zero \n(1/days)")

dry_dur = ggplot(data=dat, aes(x = dry_dur,y=NA_L1NAME,group=paste(NA_L1NAME,CLASS),fill=NA_L1NAME,linetype=CLASS)) + 
  plot_lay() + xlim(0,100) + ylab("No Flow Duration\n(days)")

dry_date_mean = ggplot(data=dat, aes(x = dry_date_mean,y=NA_L1NAME,group=paste(NA_L1NAME,CLASS),fill=NA_L1NAME,linetype=CLASS)) + 
  plot_lay() + ylab("Drying Centroid\n(days)")

num_event = ggplot(data=dat.mean, aes(x = n,y=NA_L1NAME,group=paste(NA_L1NAME,CLASS),fill=NA_L1NAME,linetype=CLASS)) + 
  plot_lay() + xlim(0,250) + ylab("Number of Drying Events")


mean.events = drying_rate + peak2zero + dry_dur + dry_date_mean + num_event +plot_layout(ncol = 5, guides = "collect") & theme(legend.position = 'right') 

mean.events







