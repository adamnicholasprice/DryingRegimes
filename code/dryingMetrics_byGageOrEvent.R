#####################################################################
##
## Script name: 
##
## Author: Adam N. Price
##
## Date Created: 2021-02-09
##
## Copyright (c) Adam N. Price, 2021
## Email: adnprice@ucsc.edu
##
############################# Description ##########################
##
## 
##   
##
############################# Packages #############################

library(tidyverse)
library(ggplot2)
library(sf)
library(maps)

############################# Code ################################

dat = read.csv("data/dryingRegimes_data_RF.csv")

tt = dat %>% select(gage,DryingCluster,PredDryingCluster) %>% na.omit()

total = tt %>% 
  select(gage,DryingCluster,PredDryingCluster) %>%
  group_by(gage)%>%
  count() %>%
  setNames(c("gage","total"))

true.total = tt %>% 
  select(gage,DryingCluster,PredDryingCluster) %>%
  group_by(gage,DryingCluster)%>%
  count() %>%
  setNames(c("gage","DryingCluster","true.total")) %>%
  pivot_wider(names_from = DryingCluster,values_from=true.total)
  # setNames(c("gage","true1","true2","true3",'true4'))

pred.total = tt %>% 
  select(gage,DryingCluster,PredDryingCluster) %>%
  group_by(gage,PredDryingCluster)%>%
  count() %>%
  setNames(c("gage","PredDryingCluster","pred.total"))%>%
  pivot_wider(names_from = PredDryingCluster,values_from=pred.total)

true = total %>%
  left_join(.,true.total,by="gage") %>%
  mutate(`1` = `1`/total,
         `2` = `2`/total,
         `3` = `3`/total,
         `4` = `4`/total
         )%>%
  select(gage,`1`,`2`,`3`,`4`)%>%
  pivot_longer(!gage,names_to = "cluster", values_to="true.prop")

pred = total %>%
  left_join(.,pred.total,by="gage") %>%
  mutate(`1` = `1`/total,
         `2` = `2`/total,
         `3` = `3`/total,
         `4` = `4`/total
  )%>%
  select(gage,`1`,`2`,`3`,`4`)%>%
  pivot_longer(!gage,names_to = "cluster", values_to="pred.prop")

prop = true %>%
  left_join(.,pred,by=c('gage','cluster'))%>%
  na.omit()

########## Plots
cols <- 
  c("1" = "#4477AA",
    "2" = "#66CCEE",
    "3" = "#228833",
    "4" = "#CCBB44")

# Scatter

scat = ggplot(prop,aes(x=true.prop,y=pred.prop,color=factor(cluster)),alpha=.7)+
  geom_point()+
  # geom_smooth(method = "lm", fill = NA,alpha=.5)+
  geom_abline(slope = 1,alpha=.5,intercept = 0)+
  scale_color_manual(values = cols)+
  xlab("True cluster membership proportion")+
  ylab("Predicted cluster membership proportion")+
  theme_minimal()

pdf("docs/trueVSpred.pdf")
scat
dev.off()



## Spatial
spat.dat = read.csv("data/kmeans.csv") %>%
  select(gage,dec_long_va,dec_lat_va)%>%
  unique()

prop = prop %>%
  left_join(.,spat.dat,by="gage")%>%
  mutate(prop.diff = abs(true.prop-pred.prop)/((true.prop+pred.prop)/2))


pal_regions <-
  c("Eastern Forests" = "#009E73",
    "Mediterranean California" = "#F0E442",
    "Northern Great Plains" = "#0072B2",
    "Southern Great Plains" = "#E69F00",
    "Western Deserts" = "#D55E00",
    "Western Mountains" = "#56B4E9",
    "Ignore"="lightgrey")

states <- st_as_sf(map("state", plot = FALSE, fill = TRUE))


agg_eco = sf::st_read('data/dissolved_ecoregions_060220/dissolved_ecoregions_060220.shp')
agg_eco$NA_L1CODE = as.numeric(agg_eco$NA_L1CODE)
newData <- sf::st_transform(agg_eco, CRS=4326)
states <- sf::st_transform(states, CRS=4326)


map = ggplot(data=states)+
  geom_sf(alpha=0,lwd=.2)

map = map +
  geom_sf(data=newData,aes(fill=Name),color=NA,alpha=.4)+
  scale_fill_manual(values = pal_regions)

map = map +   
  geom_point(data=prop, aes(x=dec_long_va, y=dec_lat_va,alpha = prop.diff,colour = factor(cluster)))+
  geom_point(data=prop, aes(x=dec_long_va, y=dec_lat_va,alpha = prop.diff),colour = "black")+
  # scale_color_manual(values = cols)+
  # scale_radius(trans='sqrt',breaks = c(.2,.4,.6,.8,1),labels = c(5,4,3,2,1),name = "Number of Cluster Changes") +
  # scale_radius(trans = 'sqrt',breaks = c(0,.2,.4,.6,.8,1),name = "Proportion of Events in Cluster")+
  scale_shape(name="Gage Type")+
  facet_wrap(~cluster)+
  theme_void()

pdf("docs/test.pdf")
map
dev.off()

  






