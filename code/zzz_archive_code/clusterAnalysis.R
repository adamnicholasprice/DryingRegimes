#####################################################################
##
## Script name: 
##
## Author: Adam N. Price
##
## Date Created: 2020-06-03
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
library(tidyverse)
library(ggfortify)
library(cluster)
library(factoextra)
library(dendextend)

############################# Code ################################

dat = read.csv('../data/annual_no_flow_and_climate_metrics_climatic_year_050820.csv')

sites = unique(dat$sitewith0)

# Load ecoregion data

region_dat = read.csv('../data/mean_annual_no_flow_climate_watershed_EPA1_032920.csv') 

region = region_dat %>% select(gage_ID,Aggregated_region,Class)

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

# Subset just drying data

d_winter = winter %>% select("dat$sitewith0","jfmfractionnoflow","zeroflowfirstjfm","zeroflowcentroiddatejfm")
d_spring = spring %>% select("dat$sitewith0","amjfractionnoflow","zeroflowfirstamj","zeroflowcentroiddateamj")
d_summer = summer %>% select("dat$sitewith0","jasfractionnoflow","zeroflowfirstjas","zeroflowcentroiddatejas")
d_fall = fall %>% select("dat$sitewith0","ondfractionnoflow","zeroflowfirstond","zeroflowcentroiddateond")

dry_win = dry_win %>% left_join(.,d_winter,by=c("gage" = "dat$sitewith0"))
dry_spr = dry_spr %>% left_join(.,d_spring,by=c("gage" = "dat$sitewith0"))
dry_sum = dry_sum %>% left_join(.,d_summer,by=c("gage" = "dat$sitewith0"))
dry_fall = dry_fall %>% left_join(.,d_fall,by=c("gage" = "dat$sitewith0"))

# Zscore data
zScore <- function(x, na.rm = FALSE) (x - mean(x, na.rm = TRUE)) / sd(x, na.rm=TRUE)

dry_win =  dry_win %>% mutate_at(vars(-gage), zScore) %>% drop_na()
dry_spr =  dry_spr %>% mutate_at(vars(-gage), zScore)%>% drop_na()
dry_sum =  dry_sum %>% mutate_at(vars(-gage), zScore)%>% drop_na()
dry_fall =  dry_fall %>% mutate_at(vars(-gage), zScore)%>% drop_na()


# PCA all data
pca_win = prcomp(dry_win[,c(2:ncol(dry_win))])
pca_spr = prcomp(dry_spr[,c(2:ncol(dry_spr))])
pca_sum = prcomp(dry_sum[,c(2:ncol(dry_sum))])
pca_fall = prcomp(dry_fall[,c(2:ncol(dry_fall))])


w = autoplot(pca_win, loadings = TRUE, loadings.label = TRUE, main="Winter")
s = autoplot(pca_spr, loadings = TRUE, loadings.label = TRUE, main="Spring")
su = autoplot(pca_sum, loadings = TRUE, loadings.label = TRUE, main="Summer")
f = autoplot(pca_fall, loadings = TRUE, loadings.label = TRUE, main="Fall")


all = w +s +su +f +  plot_layout(ncol = 2, guides = "collect")

all

# Clustering
tt = diana(dry_fall[,2:ncol(dry_win)])

tt$dc

pltree(tt, cex = 0.6, hang = -1, main = "Dendrogram of diana")

clust <- cutree(tt, k = 2)

fviz_cluster(list(data = dry_fall[,2:ncol(dry_win)], cluster = clust))  ## from ‘factoextra’ package

