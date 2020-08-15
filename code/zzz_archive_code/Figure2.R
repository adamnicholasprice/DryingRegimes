#####################################################################
##
## Script name: Figure 2
##
## Author: Adam N. Price
##
## Date Created: 2020-05-22
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
library(dplyr)
library(lubridate)
library(ggpubr)
library(RColorBrewer)
library(tidyverse)
library(patchwork)

############################# functions ################################



############################# Code ################################

dat = read.csv('../data/annual_no_flow_and_climate_metrics_climatic_year_050820.csv')

sites = unique(dat$sitewith0)

# Load ecoregion data

region_dat = read.csv('../data/mean_annual_no_flow_climate_watershed_EPA1_032920.csv') 

region = region_dat %>% select(gage_ID,Aggregated_region,Class,lat_cent,long_cent)

# Load drying data

dry_dat = read.csv('../data/metrics_by_season.csv')

dry_dat = spread(dry_dat,name,value)

dry_fall = filter(dry_dat,season=="Fall") %>% select(gage,n_event_mean,dry_dur_mean,peak2zero_mean,drying_rate_mean)
dry_spr = filter(dry_dat,season=="Spring") %>% select(gage,n_event_mean,dry_dur_mean,peak2zero_mean,drying_rate_mean)
dry_sum = filter(dry_dat,season=="Summer") %>% select(gage,n_event_mean,dry_dur_mean,peak2zero_mean,drying_rate_mean)
dry_win = filter(dry_dat,season=="Winter") %>% select(gage,n_event_mean,dry_dur_mean,peak2zero_mean,drying_rate_mean)
  
  
## Calculate mean seasonal

# Subset seasonal
jfm = cbind(dat$sitewith0,dat[,grepl('jfm', colnames(dat))])
amj = cbind(dat$sitewith0,dat[,grepl('amj', colnames(dat))])
jas = cbind(dat$sitewith0,dat[,grepl('jas', colnames(dat))])
ond = cbind(dat$sitewith0,dat[,grepl('ond', colnames(dat))])


# Calculate mean stats

winter = jfm %>% group_by(`dat$sitewith0`) %>% summarise_all(funs(mean),na.rm = TRUE) %>% left_join(., region, by = c("dat$sitewith0"="gage_ID")) %>% left_join(.,dry_win,by=c("dat$sitewith0"="gage"))
spring = amj %>% group_by(`dat$sitewith0`) %>% summarise_all(funs(mean),na.rm = TRUE) %>% left_join(., region, by = c("dat$sitewith0"="gage_ID"))%>% left_join(.,dry_spr,by=c("dat$sitewith0"="gage"))
summer = jas %>% group_by(`dat$sitewith0`) %>% summarise_all(funs(mean),na.rm = TRUE) %>% left_join(., region, by = c("dat$sitewith0"="gage_ID"))%>% left_join(.,dry_sum,by=c("dat$sitewith0"="gage"))
fall = ond %>% group_by(`dat$sitewith0`) %>% summarise_all(funs(mean),na.rm = TRUE) %>% left_join(., region, by = c("dat$sitewith0"="gage_ID"))%>% left_join(.,dry_fall,by=c("dat$sitewith0"="gage"))


dat = dat %>% group_by(sitewith0)  %>% summarise_all(funs(mean),na.rm = TRUE)
all_season = dry_dat %>% group_by(gage) %>% summarise_all(funs(mean)) %>% left_join(., region, by = c("gage"="gage_ID")) %>% left_join(.,dat,by=c("gage"="sitewith0"))
all_season_w = dry_dat %>% group_by(gage) %>% left_join(., region, by = c("gage"="gage_ID")) %>% left_join(.,dat,by=c("gage"="sitewith0"))

# Just select reference gages

# winter = filter(winter,Class=="Ref")
# spring = filter(spring,Class=="Ref")
# summer = filter(summer,Class=="Ref")
# fall = filter(fall,Class=="Ref")

all_season = all_season[!is.na(all_season$Class),]
# all_season  =filter(all_season,Class=="Ref")
# all_season_w  =filter(all_season_w,Class=="Ref")

# Create color palette
pal_regions <- 
  c("Eastern Forests" = "#009E73",
    "Mediterranean California" = "#F0E442",
    "North Great Plains" = "#0072B2",
    "South Great Plains" = "#E69F00",
    "Western Desert" = "#D55E00",
    "Western Mountains" = "#56B4E9")

back_col = brewer.pal(n = 8, name = "Greys")

######################### Mean number of drying events #######################

zScore <- function(x, na.rm = FALSE) (x - mean(x, na.rm = TRUE)) / sd(x, na.rm=TRUE)

de_winter<- ggplot()+
  geom_density(data=winter, aes(x = zScore(n_event_mean), group=Aggregated_region,color = Aggregated_region)) +
  scale_color_manual(values = pal_regions)+
  ylab('# of\nNo Flow Events')+
  theme(
    # get rid of panel grids
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    # Change plot and panel background
    plot.background = element_rect(fill = back_col[1]),
    panel.background = element_rect(fill = back_col[1]),
    axis.title.x = element_blank())

de_spring <- ggplot()+
  geom_density(data=spring, aes(x = zScore(n_event_mean), group=Aggregated_region,color = Aggregated_region),alpha=0.2) +
  scale_color_manual(values = pal_regions)+
  theme(
    # get rid of panel grids
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    # Change plot and panel background
    plot.background = element_rect(fill = back_col[1]),
    panel.background = element_rect(fill = back_col[1]),
    axis.title.x = element_blank(),
    axis.title.y = element_blank())

de_summer <- ggplot()+
  geom_density(data=summer, aes(x = zScore(n_event_mean), group=Aggregated_region,color = Aggregated_region),alpha=0.2) +
  scale_color_manual(values = pal_regions)+
  theme(
    # get rid of panel grids
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    # Change plot and panel background
    plot.background = element_rect(fill = back_col[1]),
    panel.background = element_rect(fill = back_col[1]),
    axis.title.x = element_blank(),
    axis.title.y = element_blank())


de_fall <- ggplot()+
  geom_density(data=fall, aes(x = zScore(n_event_mean), group=Aggregated_region,color = Aggregated_region),alpha=0.2) +
  scale_color_manual(values = pal_regions)+
  theme(
    # get rid of panel grids
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    # Change plot and panel background
    plot.background = element_rect(fill = back_col[1]),
    panel.background = element_rect(fill = back_col[1]),
    axis.title.x = element_blank(),
    axis.title.y = element_blank())


######################### Mean duration of drying events######################### 
dedur_winter<- ggplot()+
  geom_density(data=winter, aes(x = dry_dur_mean, group=Aggregated_region, color = Aggregated_region)) +
  scale_color_manual(values = pal_regions)+
  ylab('Mean Duration\nof No Flow Events')+
  theme(
    # get rid of panel grids
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    # Change plot and panel background
    plot.background = element_rect(fill = back_col[2]),
    panel.background = element_rect(fill = back_col[2]),
    axis.title.x = element_blank())

dedur_spring <- ggplot()+
  geom_density(data=spring, aes(x = dry_dur_mean, group=Aggregated_region,color = Aggregated_region),alpha=0.2) +
  scale_color_manual(values = pal_regions)+
  theme(
    # get rid of panel grids
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    # Change plot and panel background
    plot.background = element_rect(fill = back_col[2]),
    panel.background = element_rect(fill = back_col[2]),
    axis.title.x = element_blank(),
    axis.title.y = element_blank())

dedur_summer <- ggplot()+
  geom_density(data=summer, aes(x = dry_dur_mean, group=Aggregated_region,color = Aggregated_region),alpha=0.2) +
  scale_color_manual(values = pal_regions)+
  theme(
    # get rid of panel grids
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    # Change plot and panel background
    plot.background = element_rect(fill = back_col[2]),
    panel.background = element_rect(fill = back_col[2]),
    axis.title.x = element_blank(),
    axis.title.y = element_blank())

dedur_fall <- ggplot()+
  geom_density(data=fall, aes(x = n_event_mean, group=Aggregated_region,color = Aggregated_region),alpha=0.2) +
  scale_color_manual(values = pal_regions)+
  theme(
    # get rid of panel grids
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    # Change plot and panel background
    plot.background = element_rect(fill = back_col[2]),
    panel.background = element_rect(fill = back_col[2]),
    axis.title.x = element_blank(),
    axis.title.y = element_blank())


######################### Mean Peak2Zero length######################### 

p2z_winter<- ggplot()+
  geom_density(data=winter, aes(x = peak2zero_mean, group=Aggregated_region, color = Aggregated_region)) +
  scale_color_manual(values = pal_regions)+
  ylab('Mean\nPeak2Zero Length')+
  theme(
    # get rid of panel grids
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    # Change plot and panel background
    plot.background = element_rect(fill = back_col[1]),
    panel.background = element_rect(fill = back_col[1]),
    axis.title.x = element_blank())

p2z_spring <- ggplot()+
  geom_density(data=spring, aes(x = peak2zero_mean, group=Aggregated_region,color = Aggregated_region),alpha=0.2) +
  scale_color_manual(values = pal_regions)+
  theme(
    # get rid of panel grids
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    # Change plot and panel background
    plot.background = element_rect(fill = back_col[1]),
    panel.background = element_rect(fill = back_col[1]),
    axis.title.x = element_blank(),
    axis.title.y = element_blank())

p2z_summer <- ggplot()+
  geom_density(data=summer, aes(x = peak2zero_mean, group=Aggregated_region,color = Aggregated_region),alpha=0.2) +
  scale_color_manual(values = pal_regions)+
  theme(
    # get rid of panel grids
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    # Change plot and panel background
    plot.background = element_rect(fill = back_col[1]),
    panel.background = element_rect(fill = back_col[1]),
    axis.title.x = element_blank(),
    axis.title.y = element_blank())

p2z_fall <- ggplot()+
  geom_density(data=fall, aes(x = peak2zero_mean, group=Aggregated_region,color = Aggregated_region),alpha=0.2) +
  scale_color_manual(values = pal_regions)+
  theme(
    # get rid of panel grids
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    # Change plot and panel background
    plot.background = element_rect(fill = back_col[1]),
    panel.background = element_rect(fill = back_col[1]),
    axis.title.x = element_blank(),
    axis.title.y = element_blank())

######################### Mean drying rate######################### 

dr_winter<- ggplot()+
  geom_density(data=winter, aes(x = drying_rate_mean, group=Aggregated_region, color = Aggregated_region)) +
  scale_color_manual(values = pal_regions)+
  ylab('Mean\nDrying Rate')+
  theme(
    # get rid of panel grids
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    # Change plot and panel background
    plot.background = element_rect(fill = back_col[2]),
    panel.background = element_rect(fill = back_col[2]),
    axis.title.x = element_blank())

dr_spring <- ggplot()+
  geom_density(data=spring, aes(x = drying_rate_mean, group=Aggregated_region,color = Aggregated_region),alpha=0.2) +
  scale_color_manual(values = pal_regions)+
  theme(
    # get rid of panel grids
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    # Change plot and panel background
    plot.background = element_rect(fill = back_col[2]),
    panel.background = element_rect(fill = back_col[2]),
    axis.title.x = element_blank(),
    axis.title.y = element_blank())

dr_summer <- ggplot()+
  geom_density(data=summer, aes(x = drying_rate_mean, group=Aggregated_region,color = Aggregated_region),alpha=0.2) +
  scale_color_manual(values = pal_regions)+
  theme(
    # get rid of panel grids
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    # Change plot and panel background
    plot.background = element_rect(fill = back_col[2]),
    panel.background = element_rect(fill = back_col[2]),
    axis.title.x = element_blank(),
    axis.title.y = element_blank())

dr_fall <- ggplot()+
  geom_density(data=fall, aes(x = drying_rate_mean, group=Aggregated_region,color = Aggregated_region),alpha=0.2) +
  scale_color_manual(values = pal_regions)+
  theme(
    # get rid of panel grids
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    # Change plot and panel background
    plot.background = element_rect(fill = back_col[2]),
    panel.background = element_rect(fill = back_col[2]),
    axis.title.x = element_blank(),
    axis.title.y = element_blank())

######################### Centroid date######################### 

cd_winter <- ggplot()+
  geom_density(data=winter, aes(x = zeroflowcentroiddatejfm, group=Aggregated_region,color = Aggregated_region)) +
  scale_color_manual(values = pal_regions)+
  ylab('Zero Flow\nCentroid Date')+
  theme(
    # get rid of panel grids
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    # Change plot and panel background
    plot.background = element_rect(fill = back_col[1]),
    panel.background = element_rect(fill = back_col[1]),
    axis.title.x = element_blank())
  

cd_spring <- ggplot()+
  geom_density(data=spring, aes(x = zeroflowcentroiddateamj, group=Aggregated_region,color = Aggregated_region),alpha=0.2) +
  scale_color_manual(values = pal_regions)+
  theme(
    # get rid of panel grids
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    # Change plot and panel background
    plot.background = element_rect(fill = back_col[1]),
    panel.background = element_rect(fill = back_col[1]),
    axis.title.x = element_blank(),
    axis.title.y = element_blank())

cd_summer <- ggplot()+
  geom_density(data=summer, aes(x = zeroflowcentroiddatejas, group=Aggregated_region,color = Aggregated_region),alpha=0.2) +
  scale_color_manual(values = pal_regions)+
  theme(
    # get rid of panel grids
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    # Change plot and panel background
    plot.background = element_rect(fill = back_col[1]),
    panel.background = element_rect(fill = back_col[1]),
    axis.title.x = element_blank(),
    axis.title.y = element_blank())

cd_fall <- ggplot()+
  geom_density(data=fall, aes(x = zeroflowcentroiddateond, group=Aggregated_region,color = Aggregated_region),alpha=0.2) +
  scale_color_manual(values = pal_regions)+
  theme(
    # get rid of panel grids
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    # Change plot and panel background
    plot.background = element_rect(fill = back_col[1]),
    panel.background = element_rect(fill = back_col[1]),
    axis.title.x = element_blank(),
    axis.title.y = element_blank())
  

######################### Fraction of noflow ######################### 

nff_winter <- ggplot()+
  geom_density(data=winter, aes(x = jfmfractionnoflow, group=Aggregated_region,color = Aggregated_region),alpha=0.2) +
  scale_color_manual(values = pal_regions)+
  ylab('No Flow\nFraction') +
  xlab('Winter')+
  theme(
    # get rid of panel grids
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    # Change plot and panel background
    plot.background = element_rect(fill = back_col[2]),
    panel.background = element_rect(fill = back_col[2]))


nff_spring <- ggplot()+
  geom_density(data=spring, aes(x = amjfractionnoflow, group=Aggregated_region,color = Aggregated_region),alpha=0.2) +
  scale_color_manual(values = pal_regions)+
  xlab('Spring')+
  theme(
    # get rid of panel grids
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    # Change plot and panel background
    plot.background = element_rect(fill = back_col[2]),
    panel.background = element_rect(fill = back_col[2]),
    axis.title.y = element_blank())


nff_summer <- ggplot()+
  geom_density(data=summer, aes(x = jasfractionnoflow, group=Aggregated_region,color = Aggregated_region),alpha=0.2) +
  scale_color_manual(values = pal_regions)+
  xlab('Summer')+
  theme(
    # get rid of panel grids
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    # Change plot and panel background
    plot.background = element_rect(fill = back_col[2]),
    panel.background = element_rect(fill = back_col[2]),
    axis.title.y = element_blank())


nff_fall <- ggplot()+
  geom_density(data=fall, aes(x = ondfractionnoflow, group=Aggregated_region,color = Aggregated_region),alpha=0.2) +
  scale_color_manual(values = pal_regions)+
  xlab('Fall')+
  theme(
    # get rid of panel grids
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    # Change plot and panel background
    plot.background = element_rect(fill = back_col[2]),
    panel.background = element_rect(fill = back_col[2]),
    axis.title.y = element_blank()) 
  

######################### Combine all plots######################### 
# t <-ggarrange(cd_winter, cd_spring, cd_summer, cd_fall, nff_winter, nff_spring, nff_summer, nff_fall,nrow=2,ncol=4,common.legend = TRUE,legend = "right")


patched <- 
  de_winter + de_spring + de_summer + de_fall + 
  dedur_winter + dedur_spring + dedur_summer + dedur_fall +
  p2z_winter + p2z_spring + p2z_summer + p2z_fall +
  dr_winter + dr_spring + dr_summer + dr_fall +
  cd_winter + cd_spring + cd_summer + cd_fall + 
  nff_winter + nff_spring + nff_summer + nff_fall + 
  plot_layout(ncol = 4, guides = "collect")  & theme(legend.position = 'bottom') 

patched = patched + plot_annotation(
  title = "Figure 2: Distributions of Drying Metrics by Season and Ecoregion"
)


patched

# ggsave("../plots/figure2_reference.ps",patched,device='ps',dpi=600)
