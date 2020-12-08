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

df = read.csv("data/kmeans.csv")



########## Define Colors
pal_regions <- 
  c("Eastern Forests" = "#009E73",
    "Mediterranean California" = "#F0E442",
    "Northern Great Plains" = "#0072B2",
    "Southern Great Plains" = "#E69F00",
    "Western Deserts" = "#D55E00",
    "Western Mountains" = "#56B4E9",
    "1" = "#4477AA",
    "2" = "#66CCEE",
    "3" = "#228833",
    "4" = "#CCBB44",
    "5" = "#EE6677",
    "6" = "#AA3377",
    "7" = "#BBBBBB",
    "8" = "#999944",
    "9" = "#332288")

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

######################################################################
######################################################################
############################# Ridge plots by ecoregion ###############
######################################################################
######################################################################




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
      axis.text.y = element_blank(),
      # axis.title.x = element_blank(),
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
      # axis.title.x = element_blank(),
      axis.ticks.y = element_blank(),
    )
  )
}

get_season_counts<- function(season,metric){
  rows = !is.na(season[,which(colnames(season)==metric)])
  season[rows,] %>% group_by(paste(Aggregated_region,Class)) %>% count()
}
get_counts <- function(df,metric){
  counts = df %>% group_by(Name) %>% count()
}
# y_pos = sort(c(seq(1.75,6.75,1),seq(1.25,6.25,1)))
y_pos = seq(1.5,6.5,1)


counts = get_counts(df,peak2zero)
peak2zero = ggplot(data=df, aes(x = log(peak2zero),y=Name,group=Name,fill=Name,linetype=CLASS)) + 
  plot_lay_left() + 
  # xlim(0,200) + 
  annotate("text", x = 6, y = y_pos, label = paste0(abbreviate(paste(counts$Name),2),' = ',counts$n))+
  xlab("Peak2Zero \n(log(days))")

drying_rate = ggplot(data=df, aes(x = drying_rate,y=Name,group=Name,fill=Name,linetype=CLASS)) + 
  plot_lay() +
  xlim(0,4) + 
  xlab("Drying Rate\n(1/days)")


dry_date_start = ggplot(data=df, aes(x = dry_date_start,y=Name,group=Name,fill=Name,linetype=CLASS)) + 
  plot_lay()+ xlab("Dry Start Date\n(days)")

dry_dur = ggplot(data=df, aes(x = log(dry_dur),y=Name,group=Name,fill=Name,linetype=CLASS)) + 
  plot_lay() + xlab("No Flow Duration\n(log(days))")

peakQ = ggplot(data=df, aes(x = peak_quantile,y=Name,group=Name,fill=Name,linetype=CLASS)) + 
  plot_lay() + xlab("Peak Quantile")

ann_freq = ggplot(data=df, aes(x = freq_local,y=Name,group=Name,fill=Name,linetype=CLASS)) + 
  plot_lay() + xlab("Event Frequency")

all.points  = peak2zero  + drying_rate + dry_date_start + dry_dur + peakQ + ann_freq + plot_layout(ncol = 6, guides = "collect") & 
  theme(legend.position = 'right', plot.title = element_text(hjust = 0.5))

all.points


######################################################################
######################################################################
############################# Ridge plots by cluster #################
######################################################################
######################################################################





get_counts <- function(df){
  counts = df %>% group_by(kmeans,CLASS) %>% count()
}
y_pos = sort(c(seq(1.75,4.75,1),seq(1.25,4.25,1)))


counts = get_counts(df)
peak2zero = ggplot(data=df,aes(x=log(peak2zero),y=factor(kmeans),group = paste(factor(kmeans),CLASS),linetype=CLASS,fill = factor(kmeans)))+
  geom_density_ridges(scale=2,alpha = .75,panel_scaling = FALSE,rel_min_height=0.01)+ 
  scale_fill_manual(values = cols,'Cluster Membership')+
  annotate("text", x = 6, y = y_pos, label = paste0(counts$CLASS,"-",counts$kmeans,' = ',counts$n))+
  ylab("Peak2Zero\n(log(days)")+
  ylab("Cluster Membership")

drying_rate = ggplot(data=df,aes(x=drying_rate,y=factor(kmeans),group = paste(factor(kmeans),CLASS),linetype=CLASS,fill = factor(kmeans)))+
  geom_density_ridges(scale=2,alpha = .75,panel_scaling = FALSE,rel_min_height=0.01)+ 
  scale_fill_manual(values = cols,'Cluster Membership')+
  xlab("Drying Rate\n(1/days)")+ylab(NULL)

dry_date_start = ggplot(data=df,aes(x=dry_date_start,y=factor(kmeans),group = paste(factor(kmeans),CLASS),linetype=CLASS,fill = factor(kmeans)))+
  geom_density_ridges(scale=2,alpha = .75,panel_scaling = FALSE,rel_min_height=0.01)+ 
  scale_fill_manual(values = cols,'Cluster Membership')+
  xlab("Dry Date Start\n(DOY)")+ylab(NULL)

dry_dur = ggplot(data=df,aes(x=log(dry_dur),y=factor(kmeans),group = paste(factor(kmeans),CLASS),linetype=CLASS,fill = factor(kmeans)))+
  geom_density_ridges(scale=2,alpha = .75,panel_scaling = FALSE,rel_min_height=0.01)+ 
  scale_fill_manual(values = cols,'Cluster Membership')+
  xlab("Drying Duration\n(log(days)")+ylab(NULL)

peakQ = ggplot(data=df,aes(x=peak_quantile,y=factor(kmeans),group = paste(factor(kmeans),CLASS),linetype=CLASS,fill = factor(kmeans)))+
  geom_density_ridges(scale=2,alpha = .75,panel_scaling = FALSE,rel_min_height=0.01)+ 
  scale_fill_manual(values = cols,'Cluster Membership')+
  xlab("Peak Quantile")+ylab(NULL)

ann_freq = ggplot(data=df,aes(x=freq_local,y=factor(kmeans),group = paste(factor(kmeans),CLASS),linetype=CLASS,fill = factor(kmeans)))+
  geom_density_ridges(scale=2,alpha = .75,panel_scaling = FALSE,rel_min_height=0.01)+ 
  scale_fill_manual(values = cols,'Cluster Membership')+
  xlab("Annual Frequency")+ylab(NULL)

p = peak2zero  + drying_rate + dry_date_start + dry_dur + peakQ + ann_freq + plot_layout(ncol = 6, guides = "collect") & theme(legend.position = 'right') 

p




######################################################################
######################################################################
######################## Box plots ##################################
######################################################################
######################################################################



bp <- function(metric){
  ggplot(data=df)+
    geom_boxplot(aes(x=paste('kmeans',factor(kmeans)),y=metric,group=kmeans,fill = factor(kmeans)),outlier.colour = NA)+
    scale_fill_manual(values = cols,"Cluster Membership")
}

peak2zero = bp(df$peak2zero)+
  ylim(c(0,100))+
  ylab("Peak2Zero\n(Days)")
  

drying_rate = bp(df$drying_rate) + 
  ylim(0,2)+
  ylab("Drying Rate\n(1/Days)")

dry_date_start = bp(df$dry_date_start)+
  ylab("Dry Date Start\n(Day of Year)")


dry_dur = bp(df$dry_dur) + 
  ylim(c(0,500))+
  ylab("No Flow Duration\n(Days)")

peakQ = bp(df$peak_quantile)+
  ylab("Peak Quantile")


ann_freq = bp(df$freq_local)+
  ylab("Event Frequency")


p = peak2zero  + drying_rate + dry_date_start + dry_dur + peakQ + ann_freq + 
  plot_layout(ncol = 2, guides = "collect") & 
  theme_light() &
  labs(x=NULL)

p




######################################################################
######################################################################
########################## Stacked Area ##############################
######################################################################
######################################################################


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
library(dplyr)
temp <- df  %>%
  group_by(dry_date_start,kmeans) %>%
  summarise(n = sum(kmeans)) %>%
  mutate(percentage = round(n / sum(n),4))

kmeans.stacked = ggplot(temp, aes(x=dry_date_start, y=percentage, fill=factor(kmeans))) + 
  geom_area(size=.5, colour="black")+
  scale_fill_manual(values=cols,"Cluster Membership")+
  ggtitle("Kmeans")+
  plot_lay()

kmeans.stacked


######################################################################
######################################################################
########################## 2-D Binned Events #########################
######################################################################
######################################################################



region.mean  = df %>% group_by(Name,CLASS)%>%
  select(Name,CLASS,dry_date_start,drying_rate,peak2zero,dry_dur,peak_quantile,freq_local) %>%
  summarise(.,median.dry.date = median(dry_date_start),
            median.drying.rate = median(drying_rate),
            median.peak2zero = median(peak2zero),
            median.dry_dur = median(dry_dur),
            median.peak_quantile = median(peak_quantile),
            median.freq_local = median(freq_local)
  )


cluster.median = df %>% group_by(kmeans,CLASS) %>%
  select(kmeans,CLASS,dry_date_start,drying_rate,peak2zero,dry_dur,peak_quantile,freq_local) %>%
  summarise(.,median.dry.date = median(dry_date_start),
            median.drying.rate = median(drying_rate),
            median.peak2zero = median(peak2zero),
            median.dry_dur = median(dry_dur),
            median.peak_quantile = median(peak_quantile),
            median.freq_local = median(freq_local)
  )


drying_rate = ggplot(df,aes(x=dry_date_start,y = drying_rate))+
  geom_bin2d(binwidth = c(5, 0.15))+
  xlab("Dry Start Date (day)")+
  ylab("Drying Rate\n(1/day)") +
  scale_fill_viridis(limit=c(0,120))
# scale_fill_gradientn(trans = 'log',labels=trans_format("identity", function(x) round(x,2)),limits = c(1,300),colors = viridis(10),breaks =c(1,10,50,100,200))

#### Add Ecoregion medians
drying_rate = drying_rate + 
  geom_point(data=region.mean,aes(x=median.dry.date,y=median.drying.rate,group=paste(Name,CLASS),shape = factor(CLASS)),colour="white",size = 5, alpha=.75)
drying_rate = drying_rate + geom_point(data=region.mean,aes(x=median.dry.date,y=median.drying.rate,group=paste(Name,CLASS),color=Name,shape = factor(CLASS)),size = 4, alpha=.75)+
  scale_color_manual(values = pal_regions)


#### Add cluster medians
drying_rate = drying_rate + 
  geom_point(data=cluster.median,aes(x=median.dry.date,y=median.drying.rate,group=paste(kmeans,CLASS),shape = factor(CLASS)),colour="white",size = 5, alpha=.75)
drying_rate = drying_rate + 
  geom_point(data=cluster.median,aes(x=median.dry.date,y=median.drying.rate,group=paste(kmeans,CLASS),
                                                               color=factor(kmeans),shape = factor(CLASS)),size = 4, alpha=.75)



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


       
