#####################################################################
##
## Script name:
##
## Author: Adam N. Price
##
## Date Created: 2020-12-15
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
library(ggplot2)
library(tidyverse)
############################# Code ################################


df = read.csv("data/kmeans.csv")

cols <- 
  c("1" = "#4477AA",
    "2" = "#66CCEE",
    "3" = "#228833",
    "4" = "#CCBB44",
    "5" = "#EE6677",
    "6" = "#AA3377",
    "7" = "#BBBBBB",
    "8" = "#999944",
    "9" = "#332288")



bp <- function(metric){
  ggplot(data=df)+
    geom_boxplot(aes(x=paste('Cluster',factor(kmeans)),y=metric,group=kmeans,fill = factor(kmeans)),outlier.colour = NA)+
    scale_fill_manual(values = cols,"Cluster Membership")
}

peak2zero = bp(df$peak2zero)+
  ylim(c(0,100))+
  ylab("Peak2Zero\n(Days)")


drying_rate = bp(df$drying_rate) + 
  ylim(0,2)+
  ylab("Drying Rate\n(1/days)")

dry_date_start = bp(df$dry_date_start)+
  ylab("Dry Date Start\n(Day of Year)")


dry_dur = bp(df$dry_dur) + 
  ylim(c(0,500))+
  ylab("No Flow Duration\n(Days)")

peakQ = bp(df$peak_quantile)+
  ylab("Peak Quantile")


ann_freq = bp(df$freq_local)+
  ylab("Annual Frequency")


p = peak2zero  + drying_rate + dry_date_start + dry_dur + peakQ + ann_freq + 
  plot_layout(ncol = 2, guides = "collect") & 
  theme_light() &
  labs(x=NULL)

source('code/dryingMetrics_medianHydrograph.R')


out = p | (hydro/hydro)

# jpeg("docs//boxplot.jpg", width = 10, height=6, units = "in", res=300)
out
# dev.off()
