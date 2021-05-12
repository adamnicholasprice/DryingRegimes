#####################################################################
##
## Script name: DryingRegimes_Plots.R
##
## Author: Adam N. Price
##
## Date Created: 2020-09-24
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
library(patchwork)
library(viridis)
library(tidyverse)
library(rwrfhydro)
library(maps)
library(sf)
############################# Code #########

# Load data

df = read.csv("data/kmeans_NoFreq.csv")

################# Color Palettes #################################
cols <- 
  c("1" = "#4477AA",
    "2" = "#66CCEE",
    "3" = "#228833",
    "4" = "#CCBB44",
    "5" = "#FF0000"
  )



pal_regions <- 
  c("Eastern Forests" = "#009E73",
    "Mediterranean California" = "#F0E442",
    "Northern Great Plains" = "#0072B2",
    "Southern Great Plains" = "#E69F00",
    "Western Deserts" = "#D55E00",
    "Western Mountains" = "#56B4E9",
    "Ignore"="lightgrey")


############################# Box Plots #########


bp <- function(metric){
  ggplot(data=df)+
    geom_boxplot(aes(x=paste('Cluster',factor(kmeans)),y=metric,group=kmeans,fill = factor(kmeans)),outlier.colour = NA)+
    scale_fill_manual(values = cols,"Cluster Membership")+
    theme_light()
}

peak2zero = bp(df$peak2zero)+
  ylim(c(0,70))+
  ylab("Dry-down duration\n(Days)")+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank())


drying_rate = bp(df$drying_rate) + 
  ylim(0,2)+
  ylab("Drying rate\n(1/Days)")+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank())

dry_date_start = bp(df$dry_date_start)+
  ylab("Dry date start\n(Day of Year)")+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank())


dry_dur = bp(df$dry_dur) + 
  ylim(c(0,400))+
  ylab("No flow duration\n(Days)")+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank())

peakQ = bp(df$peak_quantile)+
  ylab("Antecedent peak quantile")+
  xlab(NULL)

ann_freq = bp(df$freq_local)+
  ylim(c(0,15))+
  ylab("Annual event requency")+
  xlab(NULL)

p = peak2zero + drying_rate + dry_dur + peakQ + dry_date_start + ann_freq +
  plot_layout(ncol = 2, guides = "collect") & theme(legend.position = "none")


pdf("docs/response_plots/boxPlots_noFreq.pdf")
p
dev.off()


p = peak2zero + drying_rate + dry_dur + peakQ + 
  plot_layout(ncol = 2, guides = "collect") & theme(legend.position = "none")


pdf("docs/response_plots/boxPlots_noFreqClusterMetrics.pdf")
p
dev.off()














 