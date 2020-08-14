#####################################################################
##
## Script name: 
##
## Author: Adam N. Price
##
## Date Created: 2020-07-15
##
## Copyright (c) Adam N. Price, 2020
## Email: adnprice@ucsc.edu
##
############################# Description ##########################
##
## Ridge plot for drying metrics
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

zScore <- function(x,metric_mean,metric_sd) (na.omit(x) - metric_mean) / metric_sd

######################### Number of drying events ######################### 
metric_max = max(c(winter$n_event_mean,spring$n_event_mean,summer$n_event_mean,fall$n_event_mean),na.rm = T)
metric_min = min(c(winter$n_event_mean,spring$n_event_mean,summer$n_event_mean,fall$n_event_mean),na.rm = T)
metric_mean = mean(c(winter$n_event_mean,spring$n_event_mean,summer$n_event_mean,fall$n_event_mean),na.rm = T)
metric_sd = sd(c(winter$n_event_mean,spring$n_event_mean,summer$n_event_mean,fall$n_event_mean),na.rm = T)

de_winter = ggplot(data=winter, aes(x = n_event_mean,y=Aggregated_region, group=Aggregated_region, color = Aggregated_region, fill=Aggregated_region)) + 
  geom_density_ridges(scale=2,alpha = .75,panel_scaling = FALSE,rel_min_height=0.01) +
  scale_fill_manual(values = pal_regions)+
  scale_color_manual(values = pal_regions)+
  ylab("Mean drying event length\n(days)") +
  xlim(0,4) +
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

de_spring = ggplot(data=spring, aes(x = n_event_mean,y=Aggregated_region, group=Aggregated_region, color = Aggregated_region, fill=Aggregated_region)) + 
  geom_density_ridges(scale=2,alpha = .75,panel_scaling = FALSE,rel_min_height=0.01) +
  scale_fill_manual(values = pal_regions)+
  scale_color_manual(values = pal_regions)+
  ylab(NULL) +
  xlim(0,4) +
  theme(
    # panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    # Change plot and panel background
    plot.background = element_blank(),
    # panel.background = element_blank(),
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.title.x = element_blank(),
    axis.ticks.y = element_blank()
  )

de_summer = ggplot(data=summer, aes(x = n_event_mean,y=Aggregated_region, group=Aggregated_region, color = Aggregated_region, fill=Aggregated_region)) + 
  geom_density_ridges(scale=2,alpha = .75,panel_scaling = FALSE,rel_min_height=0.01) +
  scale_fill_manual(values = pal_regions)+
  scale_color_manual(values = pal_regions)+
  ylab(NULL) +
  xlim(0,4) +
  theme(
    # panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    # Change plot and panel background
    plot.background = element_blank(),
    # panel.background = element_blank(),
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.title.x = element_blank(),
    axis.ticks.y = element_blank()
  )

de_fall = ggplot(data=fall, aes(x = n_event_mean,y=Aggregated_region, group=Aggregated_region, color = Aggregated_region, fill=Aggregated_region)) + 
  geom_density_ridges(scale=2,alpha = .75,panel_scaling = FALSE,rel_min_height=0.01) +
  scale_fill_manual(values = pal_regions)+
  scale_color_manual(values = pal_regions)+
  ylab(NULL) +
  xlim(0,4) +
  theme(
    # panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    # Change plot and panel background
    plot.background = element_blank(),
    # panel.background = element_blank(),
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.title.x = element_blank(),
    axis.ticks.y = element_blank()
  )

de_all = ggplot(data=all_season, aes(x = n_event_mean,y=Aggregated_region, group=Aggregated_region, color = Aggregated_region, fill=Aggregated_region)) + 
  geom_density_ridges(scale=2,alpha = .75,panel_scaling = FALSE,rel_min_height=0.01) +
  scale_fill_manual(values = pal_regions)+
  scale_color_manual(values = pal_regions)+
  ylab(NULL) +
  xlim(0,4) +
  theme(
    # panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    # Change plot and panel background
    plot.background = element_blank(),
    # panel.background = element_blank(),
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.title.x = element_blank(),
    axis.ticks.y = element_blank()
  )

# p  = de_winter + de_spring + de_summer + de_fall + plot_layout(ncol = 4, guides = "collect")
# p


######################### Mean peak 2 zero length #######################
# lmax = round(max(c(winter$n_event_mean,spring$n_event_mean,summer$n_event_mean,fall$n_event_mean),na.rm = T),1)
# lmin = round(min(c(winter$n_event_mean,spring$n_event_mean,summer$n_event_mean,fall$n_event_mean),na.rm = T),1)

p2z_winter = ggplot(data=winter, aes(x = peak2zero_mean,y=Aggregated_region, group=Aggregated_region, color = Aggregated_region, fill=Aggregated_region)) + 
  geom_density_ridges(scale=2,alpha = .75,panel_scaling = FALSE,rel_min_height=0.01) +
  scale_fill_manual(values = pal_regions)+
  scale_color_manual(values = pal_regions)+
  xlim(0,120) +
  ylab("Mean peak2Zero legnth\n(days)") +
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

p2z_spring <- ggplot(data=spring, aes(x = peak2zero_mean,y=Aggregated_region, group=Aggregated_region, color = Aggregated_region, fill=Aggregated_region)) + 
  geom_density_ridges(scale=2,alpha = .75,panel_scaling = FALSE,rel_min_height=0.01) +
  scale_fill_manual(values = pal_regions)+
  scale_color_manual(values = pal_regions)+
  xlim(0,120) +
  ylab(NULL) +
  theme(
    # panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    # Change plot and panel background
    plot.background = element_blank(),
    # panel.background = element_blank(),
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.title.x = element_blank(),
    axis.ticks.y = element_blank()
  )

p2z_summer <-ggplot(data=summer, aes(x = peak2zero_mean,y=Aggregated_region, group=Aggregated_region, color = Aggregated_region,fill=Aggregated_region)) + 
  geom_density_ridges(scale=2,alpha = .75,panel_scaling = FALSE,rel_min_height=0.01) +
  scale_fill_manual(values = pal_regions)+
  scale_color_manual(values = pal_regions)+
  xlim(0,120) +
  ylab(NULL) +
  theme(
    # panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    # Change plot and panel background
    plot.background = element_blank(),
    # panel.background = element_blank(),
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.title.x = element_blank(),
    axis.ticks.y = element_blank()
  )

p2z_fall <- ggplot(data=fall, aes(x = peak2zero_mean,y=Aggregated_region, group=Aggregated_region, color = Aggregated_region,fill=Aggregated_region)) + 
  geom_density_ridges(scale=2,alpha = .75,panel_scaling = FALSE,rel_min_height=0.01) +
  scale_fill_manual(values = pal_regions)+
  scale_color_manual(values = pal_regions)+
  xlim(0,120) +
  ylab(NULL) +
  theme(
    # panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    # Change plot and panel background
    plot.background = element_blank(),
    # panel.background = element_blank(),
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.title.x = element_blank(),
    axis.ticks.y = element_blank()
  ) 

p2z_all <- ggplot(data=all_season, aes(x = peak2zero_mean,y=Aggregated_region, group=Aggregated_region, color = Aggregated_region,fill=Aggregated_region)) + 
  geom_density_ridges(scale=2,alpha = .75,panel_scaling = FALSE,rel_min_height=0.01) +
  scale_fill_manual(values = pal_regions)+
  scale_color_manual(values = pal_regions)+
  xlim(0,360) +
  ylab(NULL) +
  theme(
    # panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    # Change plot and panel background
    plot.background = element_blank(),
    # panel.background = element_blank(),
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.title.x = element_blank(),
    axis.ticks.y = element_blank()
  )

# p  = p2z_winter + p2z_spring + p2z_summer + p2z_fall + plot_layout(ncol = 4, guides = "collect")
# p


######################### Mean drying rate #######################
# metric_max = max(c(winter$drying_rate_mean,spring$drying_rate_mean,summer$drying_rate_mean,fall$drying_rate_mean),na.rm = T)
# metric_min = min(c(winter$drying_rate_mean,spring$drying_rate_mean,summer$drying_rate_mean,fall$drying_rate_mean),na.rm = T)
# metric_mean = mean(c(winter$drying_rate_mean,spring$drying_rate_mean,summer$drying_rate_mean,fall$drying_rate_mean),na.rm = T)
# metric_sd = sd(c(winter$drying_rate_mean,spring$drying_rate_mean,summer$drying_rate_mean,fall$drying_rate_mean),na.rm = T)

dr_winter = ggplot(data=winter, aes(x = drying_rate_mean,y=Aggregated_region, group=Aggregated_region, color = Aggregated_region, fill=Aggregated_region)) + 
  geom_density_ridges(scale=2,alpha = .75,panel_scaling = FALSE,rel_min_height=0.01) +
  scale_fill_manual(values = pal_regions)+
  scale_color_manual(values = pal_regions)+
  xlim(0,2.5) +
  xlab('Winter') +
  ylab("Mean Drying Rate\n(1/day)") +
  theme(
    # panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    # Change plot and panel background
    plot.background = element_blank(),
    # panel.background = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank()
  )

dr_spring <- ggplot(data=spring, aes(x = drying_rate_mean,y=Aggregated_region, group=Aggregated_region, color = Aggregated_region, fill=Aggregated_region)) + 
  geom_density_ridges(scale=2,alpha = .75,panel_scaling = FALSE,rel_min_height=0.01) +
  scale_fill_manual(values = pal_regions)+
  scale_color_manual(values = pal_regions)+
  xlim(0,2.5) +
  xlab('Spring') +
  ylab(NULL) +
  theme(
    # panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    # Change plot and panel background
    plot.background = element_blank(),
    # panel.background = element_blank(),
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank()
  )

dr_summer <-ggplot(data=summer, aes(x = drying_rate_mean,y=Aggregated_region, group=Aggregated_region, color = Aggregated_region,fill=Aggregated_region)) + 
  geom_density_ridges(scale=2,alpha = .75,panel_scaling = FALSE,rel_min_height=0.01) +
  scale_fill_manual(values = pal_regions)+
  scale_color_manual(values = pal_regions)+
  xlim(0,2.5) +
  xlab('Summer') +
  ylab(NULL) +
  theme(
    # panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    # Change plot and panel background
    plot.background = element_blank(),
    # panel.background = element_blank(),
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank()
  )

dr_fall <- ggplot(data=fall, aes(x = drying_rate_mean,y=Aggregated_region, group=Aggregated_region, color = Aggregated_region,fill=Aggregated_region)) + 
  geom_density_ridges(scale=2,alpha = .75,panel_scaling = FALSE,rel_min_height=0.01) +
  scale_fill_manual(values = pal_regions)+
  scale_color_manual(values = pal_regions)+
  xlim(0,2.5) +
  xlab('Fall') +
  ylab(NULL) +
  theme(
    # panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    # Change plot and panel background
    plot.background = element_blank(),
    # panel.background = element_blank(),
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank()
    ) 

dr_all <- ggplot(data=all_season, aes(x = drying_rate_mean,y=Aggregated_region, group=Aggregated_region, color = Aggregated_region,fill=Aggregated_region)) + 
  geom_density_ridges(scale=2,alpha = .75,panel_scaling = FALSE,rel_min_height=0.01) +
  scale_fill_manual(values = pal_regions)+
  scale_color_manual(values = pal_regions)+
  xlim(0,2.5) +
  xlab('Annual') +
  ylab(NULL) +
  theme(
    # panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    # Change plot and panel background
    plot.background = element_blank(),
    # panel.background = element_blank(),
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank()
  )

p  = dr_winter + dr_spring + dr_summer + dr_fall + dr_all + plot_layout(ncol = 5, guides = "collect")
p

######################### Zero flow centriod date #######################

zf_winter = ggplot(data=winter, aes(x = zeroflowcentroiddatejfm,y=Aggregated_region, group=Aggregated_region, color = Aggregated_region, fill=Aggregated_region)) + 
  geom_density_ridges(scale=2,alpha = .75,panel_scaling = FALSE,rel_min_height=0.01) +
  scale_fill_manual(values = pal_regions)+
  scale_color_manual(values = pal_regions)+
  xlim(0,120) +
  ylab("Zero Flow Centroid Day\n(day of season)") +
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

zf_spring <- ggplot(data=spring, aes(x = zeroflowcentroiddateamj,y=Aggregated_region, group=Aggregated_region, color = Aggregated_region, fill=Aggregated_region)) + 
  geom_density_ridges(scale=2,alpha = .75,panel_scaling = FALSE,rel_min_height=0.01) +
  scale_fill_manual(values = pal_regions)+
  scale_color_manual(values = pal_regions)+
  xlim(0,120) +
  ylab(NULL) +
  theme(
    # panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    # Change plot and panel background
    plot.background = element_blank(),
    # panel.background = element_blank(),
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.title.x = element_blank(),
    axis.ticks.y = element_blank()
  )

zf_summer <-ggplot(data=summer, aes(x = zeroflowcentroiddatejas,y=Aggregated_region, group=Aggregated_region, color = Aggregated_region,fill=Aggregated_region)) + 
  geom_density_ridges(scale=2,alpha = .75,panel_scaling = FALSE,rel_min_height=0.01) +
  scale_fill_manual(values = pal_regions)+
  scale_color_manual(values = pal_regions)+
  xlim(0,120) +
  ylab(NULL) +
  theme(
    # panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    # Change plot and panel background
    plot.background = element_blank(),
    # panel.background = element_blank(),
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.title.x = element_blank(),
    axis.ticks.y = element_blank()
  )

zf_fall <- ggplot(data=fall, aes(x = zeroflowcentroiddateond,y=Aggregated_region, group=Aggregated_region, color = Aggregated_region,fill=Aggregated_region)) + 
  geom_density_ridges(scale=2,alpha = .75,panel_scaling = FALSE,rel_min_height=0.01) +
  scale_fill_manual(values = pal_regions)+
  scale_color_manual(values = pal_regions)+
  xlim(0,120) +
  ylab(NULL) +
  theme(
    # panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    # Change plot and panel background
    plot.background = element_blank(),
    # panel.background = element_blank(),
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.title.x = element_blank(),
    axis.ticks.y = element_blank()
  ) 

zf_all <- ggplot(data=all_season, aes(x = zeroflowcentroiddate,y=Aggregated_region, group=Aggregated_region, color = Aggregated_region,fill=Aggregated_region)) + 
  geom_density_ridges(scale=2,alpha = .75,panel_scaling = FALSE,rel_min_height=0.01) +
  scale_fill_manual(values = pal_regions)+
  scale_color_manual(values = pal_regions)+
  xlim(0,360) +
  ylab(NULL) +
  theme(
    # panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    # Change plot and panel background
    plot.background = element_blank(),
    # panel.background = element_blank(),
    axis.title.y = element_blank(),
    axis.title.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank()
  )

# p  = zf_winter + zf_spring + zf_summer + zf_fall + plot_layout(ncol = 4, guides = "collect")
# p



all = zf_winter + zf_spring + zf_summer + zf_fall + zf_all +
  p2z_winter + p2z_spring + p2z_summer + p2z_fall+ p2z_all +
  de_winter + de_spring + de_summer + de_fall + de_all +
  dr_winter + dr_spring + dr_summer + dr_fall + dr_all +
    plot_layout(ncol = 5, guides = "collect") & theme(legend.position = 'right') 

all
######################### No-flow fraction #######################

