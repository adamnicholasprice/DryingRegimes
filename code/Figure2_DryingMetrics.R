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
library(viridis)

############################# Code ################################

dat = read.csv("data/clustering_results_allevents.csv")


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



############################# Ridge plots by ecoregion #######################

plot_lay_left <-function(){
  list(
    geom_density_ridges(scale=2,alpha = .75,panel_scaling = FALSE,rel_min_height=0.01),
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
    geom_density_ridges(scale=2,alpha = .75,panel_scaling = FALSE,rel_min_height=0.01),
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
y_pos = sort(c(seq(1.75,6.75,1),seq(1.25,6.25,1)))


counts = get_counts(dat,peak2zero)
peak2zero = ggplot(data=dat, aes(x = log(peak2zero),y=Name,group=paste(Name,CLASS),fill=Name,linetype=CLASS)) + 
  plot_lay_left() + 
  # xlim(0,200) + 
  annotate("text", x = 6, y = y_pos, label = paste0(abbreviate(paste(counts$Name,counts$CLASS),2),' = ',counts$n))+
  ylab("Peak2Zero \n(log(days))") 


drying_rate = ggplot(data=dat, aes(x = drying_rate,y=Name,group=paste(Name,CLASS),fill=Name,linetype=CLASS)) + 
  plot_lay() +
  xlim(0,4) + 
  ylab("Drying Rate\n(1/days)")


dry_date_start = ggplot(data=dat, aes(x = dry_date_start,y=Name,group=paste(Name,CLASS),fill=Name,linetype=CLASS)) + 
  plot_lay()+ ylab("Dry Start Date\n(days)")

dry_dur = ggplot(data=dat, aes(x = log(dry_dur),y=Name,group=paste(Name,CLASS),fill=Name,linetype=CLASS)) + 
  plot_lay() + ylab("No Flow Duration\n(log(days))")

peakQ = ggplot(data=dat, aes(x = peak_quantile,y=Name,group=paste(Name,CLASS),fill=Name,linetype=CLASS)) + 
  plot_lay() + ylab("Peak Quantile")

rel_freq = ggplot(data=dat, aes(x = log(rel_freq),y=Name,group=paste(Name,CLASS),fill=Name,linetype=CLASS)) + 
  plot_lay() + ylab("Relative Frequency")

all.points  = peak2zero  + drying_rate + dry_date_start + dry_dur + peakQ + rel_freq + plot_layout(ncol = 6, guides = "collect") & theme(legend.position = 'right') 

all.points




############################# Ridge plots by cluster #######################

peak2zero = ggplot(data=dat,aes(x=log(peak2zero),y=factor(dbscan.cluster),group = paste(factor(dbscan.cluster),CLASS),linetype=CLASS,fill = factor(dbscan.cluster)))+
  geom_density_ridges(scale=2,alpha = .75,panel_scaling = FALSE,rel_min_height=0.01)+ 
  scale_fill_manual(values = cols)

drying_rate = ggplot(data=dat,aes(x=drying_rate,y=factor(dbscan.cluster),group = paste(factor(dbscan.cluster),CLASS),linetype=CLASS,fill = factor(dbscan.cluster)))+
  geom_density_ridges(scale=2,alpha = .75,panel_scaling = FALSE,rel_min_height=0.01)+ 
  scale_fill_manual(values = cols)

dry_date_start = ggplot(data=dat,aes(x=dry_date_start,y=factor(dbscan.cluster),group = paste(factor(dbscan.cluster),CLASS),linetype=CLASS,fill = factor(dbscan.cluster)))+
  geom_density_ridges(scale=2,alpha = .75,panel_scaling = FALSE,rel_min_height=0.01)+ 
  scale_fill_manual(values = cols)

dry_dur = ggplot(data=dat,aes(x=log(dry_dur),y=factor(dbscan.cluster),group = paste(factor(dbscan.cluster),CLASS),linetype=CLASS,fill = factor(dbscan.cluster)))+
  geom_density_ridges(scale=2,alpha = .75,panel_scaling = FALSE,rel_min_height=0.01)+ 
  scale_fill_manual(values = cols)

peakQ = ggplot(data=dat,aes(x=peak_quantile,y=factor(dbscan.cluster),group = paste(factor(dbscan.cluster),CLASS),linetype=CLASS,fill = factor(dbscan.cluster)))+
  geom_density_ridges(scale=2,alpha = .75,panel_scaling = FALSE,rel_min_height=0.01)+ 
  scale_fill_manual(values = cols)

rel_freq = ggplot(data=dat,aes(x=log(rel_freq),y=factor(dbscan.cluster),group = paste(factor(dbscan.cluster),CLASS),linetype=CLASS,fill = factor(dbscan.cluster)))+
  geom_density_ridges(scale=2,alpha = .75,panel_scaling = FALSE,rel_min_height=0.01)+ 
  scale_fill_manual(values = cols)

p = peak2zero  + drying_rate + dry_date_start + dry_dur + peakQ + rel_freq + plot_layout(ncol = 6, guides = "collect") & theme(legend.position = 'right') 

p






######################## Box plots ##################################


bp <- function(metric){
  ggplot(data=dat)+
    geom_boxplot(aes(x=paste('kmeans',factor(kmeans.clust)),y=metric,group=kmeans.clust,fill = factor(kmeans.clust)),outlier.colour = NA)+
    geom_boxplot(aes(x=paste("hier",factor(hier.4.clust)),y=metric,group=hier.4.clust,fill=factor(hier.4.clust)),outlier.colour = NA)+
    geom_boxplot(aes(x=paste("gmm",factor(gmm.clust)),y=metric,group=gmm.clust,fill=factor(gmm.clust)),outlier.colour = NA)+
    # geom_boxplot(aes(x=paste("db",factor(dbscan.cluster)),y=metric,group=dbscan.cluster,fill=factor(dbscan.cluster)),outlier.colour = NA)+
    scale_fill_manual(values = cols)
}

peak2zero = bp(dat$peak2zero)+
  ylim(c(0,200))+
  ylab("Peak2Zero\n(Days)")
  

drying_rate = bp(dat$drying_rate) + 
  ylim(0,2)+
  ylab("Drying Rate\n(1/Days)")

dry_date_start = bp(dat$dry_date_start)+
  ylab("Dry Date Start\n(Day of Year)")


dry_dur = bp(dat$dry_dur) + 
  ylim(c(0,500))+
  ylab("Dry Duration\n(Days)")

peakQ = bp(dat$peak_quantile)+
  ylab("Peak Quantile")


rel_freq = bp(dat$rel_freq)+
  ylab("Relative Event Frequency")


p = peak2zero  + drying_rate + dry_date_start + dry_dur + peakQ + rel_freq + 
  plot_layout(ncol = 2, guides = "collect") & 
  theme_light() &
  labs(x=NULL)

p


########################## Stacked Area ##############################

plot_lay <-function(){
  list(
    theme_bw(),
      ylab('Proportion of Drying Events'),
      xlab('Julian Date'),
      #Axes Options
      theme(
        axis.title = element_text(size=14),
        axis.text  = element_text(size = 10)),
      #Legend Options
      theme(legend.position = "bottom", 
            legend.title = element_text(size=14), 
            legend.text = element_text(size=10)) 
  )
}

# Summarize Data
##Kmeans
library(dplyr)
tt <- dat  %>%
  group_by(dry_date_start,kmeans.clust) %>%
  summarise(n = sum(kmeans.clust)) %>%
  mutate(percentage = round(n / sum(n),4))

k = ggplot(tt, aes(x=dry_date_start, y=percentage, fill=factor(kmeans.clust))) + 
  geom_area(size=.5, colour="black")+
  scale_fill_manual(values=cols,"Cluster Membership")+
  ggtitle("Kmeans")+
  plot_lay()

k

## Hier
tt <- dat  %>%
  group_by(dry_date_start,hier.4.clust) %>%
  summarise(n = sum(hier.4.clust)) %>%
  mutate(percentage = round(n / sum(n),4))

h = ggplot(tt, aes(x=dry_date_start, y=percentage, fill=factor(hier.4.clust))) + 
  geom_area(alpha=1 , size=.5, colour="black")+
  scale_fill_manual(values=cols,"Cluster Membership")+
  ggtitle("Hier")+
  plot_lay()


##GMM
tt <- dat  %>%
  group_by(dry_date_start,gmm.clust) %>%
  summarise(n = sum(gmm.clust)) %>%
  mutate(percentage = round(n / sum(n),4))


g = ggplot(tt, aes(x=dry_date_start, y=percentage, fill=factor(gmm.clust))) + 
  geom_area(alpha=1 , size=.5, colour="black")+
  scale_fill_manual(values=cols,"Cluster Membership")+
  ggtitle("GMM")+
  plot_lay()

 
k+h+g +plot_layout(ncol = 1, guides = "collect") & theme(legend.position = "none") 


########################## 2-D Binned Events #########################

region.mean  = dat.mean %>% group_by(Name,CLASS)%>%
  select(Name,CLASS,mean_dry_date_start,mean_drying_rate,mean_peak2zero,mean_dry_dur,n_events) %>%
  summarise(.,mean.dry.date = mean(mean_dry_date_start),
            mean.drying.rate = mean(mean_drying_rate),
            mean.peak2zero = mean(mean_peak2zero),
            mean.dry_dur = mean(mean_dry_dur),
            mean.n_events = mean(n_events)
  )


drying_rate = ggplot(dat,aes(x=dry_date_start,y = drying_rate))+
  geom_bin2d(binwidth = c(5, 0.15))+
  xlab("Dry Start Date (day)")+
  ylab("Drying Rate\n(1/day)") +
  scale_fill_viridis(limit=c(0,120))
# scale_fill_gradientn(trans = 'log',labels=trans_format("identity", function(x) round(x,2)),limits = c(1,300),colors = viridis(10),breaks =c(1,10,50,100,200))
drying_rate = drying_rate + 
  geom_point(data=region.mean,aes(x=mean.dry.date,y=mean.drying.rate,group=paste(Name,CLASS),shape = factor(CLASS)),colour="white",size = 5, alpha=.75)


drying_rate = drying_rate + geom_point(data=region.mean,aes(x=mean.dry.date,y=mean.drying.rate,group=paste(Name,CLASS),color=Name,shape = factor(CLASS)),size = 4, alpha=.75)+
  scale_color_manual(values = pal_regions)

peak2zero = ggplot(dat,aes(x=dry_date_start,y = peak2zero))+
  geom_bin2d(binwidth = c(5,5)) +
  ylim(0,365)+
  xlab("Dry Start Date (day)")+
  ylab("Peak2Zero\n(day)") +
  scale_fill_viridis(limit=c(0,120))
  # scale_fill_gradientn(trans = 'log',labels=trans_format("identity", function(x) round(x,2)),limits = c(1,300),colors = viridis(10),breaks =c(1,10,50,100,200))

peak2zero = peak2zero + 
  geom_point(data=region.mean,aes(x=mean.dry.date,y=mean.peak2zero,group=paste(Name,CLASS),shape = factor(CLASS)),colour="white",size = 5, alpha=.75)


peak2zero = peak2zero + geom_point(data=region.mean,aes(x=mean.dry.date,y=mean.peak2zero,group=paste(Name,CLASS),color=Name,shape = factor(CLASS)),size = 4, alpha=.75)+
  scale_color_manual(values = pal_regions)

dry_dur = ggplot(dat,aes(x=dry_date_start,y = dry_dur)) +
  geom_bin2d(binwidth = c(5,5)) +
  ylim(0,365) +
  xlab("Dry Start Date (day)")+
  ylab("Drying Duration\n(day)") +
  scale_fill_viridis(limit=c(0,120))
# scale_fill_gradientn(trans = 'log',labels=trans_format("identity", function(x) round(x,2)),limits = c(1,300),colors = viridis(10),breaks =c(1,10,50,100,200))

dry_dur = dry_dur + 
  geom_point(data=region.mean,aes(x=mean.dry.date,y=mean.dry_dur,group=paste(Name,CLASS),shape = factor(CLASS)),colour="white",size = 5, alpha=.75)

dry_dur = dry_dur + geom_point(data=region.mean,aes(x=mean.dry.date,y=mean.dry_dur,group=paste(Name,CLASS),color=Name,shape = factor(CLASS)),size = 4, alpha=.75)+
  scale_color_manual(values = pal_regions)



n_events = ggplot(dat,aes(x=dry_date_start,y = n)) +
  geom_bin2d(binwidth = c(5,7)) +
  # ylim(0,365) +
  xlab("Dry Start Date (day)")+
  ylab("Number of Events") +
  scale_fill_viridis(limit=c(0,120))
# scale_fill_gradientn(trans = 'log',labels=trans_format("identity", function(x) round(x,2)),limits = c(1,300),colors = viridis(10),breaks =c(1,10,50,100,200))

n_events = n_events + 
  geom_point(data=region.mean,aes(x=mean.dry.date,y=mean.n_events,group=paste(Name,CLASS),shape = factor(CLASS)),colour="white",size = 5, alpha=.75)
n_events = n_events+geom_point(data=region.mean,aes(x=mean.dry.date,y=mean.n_events,group=paste(Name,CLASS),color=Name,shape = factor(CLASS)),size = 4, alpha=.75)+
  scale_color_manual(values = pal_regions)

binned  = drying_rate + peak2zero  + n_events + dry_dur + plot_layout(ncol = 4, guides = "collect") & theme(legend.position = 'right')

binned

############## Violin Plots ################
plot_lay_left <-function(){
  list(
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

drying_rate = ggplot(data=dat, aes(x = log(drying_rate),y=Name,group=paste(Name,CLASS),fill=Name,linetype=CLASS)) + 
  geom_violin() + plot_lay_left()+
  ylab("Drying Rate\n(log(1/days))") +
  annotate("text", x = 0, y = y_pos, label = paste0(abbreviate(paste(counts$Name,counts$CLASS),2),' = ',counts$n))

peak2zero = ggplot(data=dat, aes(x = log(peak2zero),y=Name,group=paste(Name,CLASS),fill=Name,linetype=CLASS)) + 
  geom_violin()+ 
  # xlim(0,300)+
  ylab("Peak2Zero \n(log(days))")+
  plot_lay()

dry_dur = ggplot(data=dat, aes(x = log(dry_dur),y=Name,group=paste(Name,CLASS),fill=Name,linetype=CLASS)) + 
  geom_violin() +
  ylab("Drying Duration\n(log(days))")+
  plot_lay()

dry_date_mean = ggplot(data=dat, aes(x = dry_date_start,y=Name,group=paste(Name,CLASS),fill=Name,linetype=CLASS)) + 
  geom_violin()+
  plot_lay()+
  ylab("Dry Date Start \n(days)")

all.points  = drying_rate + peak2zero + dry_dur + dry_date_mean + plot_layout(ncol = 4, guides = "collect") & theme(legend.position = 'right') 

all.points

########## Mean events #####################

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

       
