#####################################################################
##
## Script name: 
##
## Author: Adam N. Price
##
## Date Created: 2020-10-02
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
############# Load Data
clust = read.csv("../data/clustering_results_allevents.csv")

########## Define Colors
pal_regions <- 
  c("Eastern Forests" = "#009E73",
    "Mediterranean California" = "#F0E442",
    "Northern Great Plains" = "#0072B2",
    "Southern Great Plains" = "#E69F00",
    "Western Deserts" = "#D55E00",
    "Western Mountains" = "#56B4E9")

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








plot_lay_left <-function(){
  list(
    geom_density_ridges(stat = "binline", scale = 1, bins=4,draw_baseline = FALSE,alpha=.),
    scale_fill_manual(values = pal_regions),
    # scale_color_manual(values = pal_regions),
    # ylab("Mean drying event length\n(days)") ,
    # xlim(0,4) ,
    theme(
      # panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      # Change plot and panel background
      plot.background = element_blank(),
      # panel.background = element_blank(),
      # axis.text.y = element_blank(),
      axis.title.x = element_blank(),
      axis.ticks.y = element_blank(),
    )
  )
}
plot_lay <-function(){
  list(
    geom_density_ridges(stat = "binline", scale = 1, bins=5,draw_baseline = FALSE,alpha=.75),
    scale_fill_manual(values = pal_regions),
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
      axis.ticks.y = element_blank(),
    )
  )
}

get_season_counts<- function(season,metric){
  rows = !is.na(season[,which(colnames(season)==metric)])
  season[rows,] %>% group_by(paste(Aggregated_region,Class)) %>% count()
}
get_counts <- function(df,metric){
  counts = df %>% group_by(Name,CLASS) %>% count()
}
y_pos = sort(c(seq(1.5,6.75,1),seq(1.25,6.25,1)))


counts = get_counts(clust,hier.4.clust)
hier = ggplot(clust, aes(x = hier.4.clust, y = Name, height = stat(density),group=paste(Name,CLASS),fill=Name,linetype=CLASS)) + 
  plot_lay_left() + 
  # xlim(0,200) + 
  annotate("text", x = 4, y = y_pos, label = paste0(abbreviate(paste(counts$Name,counts$CLASS),2),' = ',counts$n))+
  ylab("Hierarchical") 

kmean = ggplot(clust, aes(x = kmeans.clust, y = Name, height = stat(density),group=paste(Name,CLASS),fill=Name,linetype=CLASS)) + 
  plot_lay() + 
  ylab("Kmeans") 


gmm = ggplot(clust, aes(x = gmm.clust, y = Name, height = stat(density),group=paste(Name,CLASS),fill=Name,linetype=CLASS)) + 
  plot_lay() + 
  ylab("GMM") 

hier + kmean + gmm +plot_layout(ncol = 3, guides = "collect") & theme(legend.position = 'right') 
