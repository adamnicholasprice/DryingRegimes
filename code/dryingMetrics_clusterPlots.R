#####################################################################
##
## Script name: dryingMetrics_clusterPlots.r
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
############################# Code ################################

########## Load Data #############
df = read.csv("data/kmeans.csv")



# Clean up missing coordinates
tt = rwrfhydro::gages2Attr
tt$STAID = as.integer(tt$STAID)


noCoor = as.data.frame(df$gage)
colnames(noCoor) = 'gage'

coors = left_join(noCoor,tt,by=c('gage'='STAID'))

df$dec_lat_va = coors$LAT_GAGE
df$dec_long_va = coors$LNG_GAGE



######## Plots ##########
# Color scales
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



pal_regions <- 
  c("Eastern Forests" = "#009E73",
    "Mediterranean California" = "#F0E442",
    "Northern Great Plains" = "#0072B2",
    "Southern Great Plains" = "#E69F00",
    "Western Deserts" = "#D55E00",
    "Western Mountains" = "#56B4E9")







### Faceted by cluster membership

# Calculate mode and cluster membership proportion

## Plot
states <- map_data("state")

mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

prop = df %>% group_by(gage,kmeans,n) %>% summarise(kmeans.count = n())

prop$prop = prop$kmeans.count/prop$n


colnames(prop)  = c("gage",'cluster','total_events','event_count','proportion')

k.means = df %>% select(gage,dec_lat_va,dec_long_va,CLASS,Name) %>% left_join(prop,.,by="gage") %>% unique()

kmean_CLUST <- ggplot(data = states) + 
  geom_polygon(aes(x = long, y = lat, group = group), fill = "lightgray", color = "black") + 
  coord_fixed(1.3, xlim = c(-124.25,-70), ylim = c(26,48.5)) +
  theme_linedraw()
  # geom_point(data=k.means, aes(x=dec_long_va, y=dec_lat_va, shape = factor(CLASS),size = proportion+0.01),alpha=.25)+
  # scale_radius(trans = 'sqrt',limits = c(.1,.5),name = "Proportion of Events in Cluster")+
  # facet_wrap(~cluster)

counts = df %>% group_by(kmeans) %>% count()

labs = c("1" = paste0("Cluster 1\nn=",counts$n[1]),
         "2" = paste0("Cluster 2\nn=",counts$n[2]),
         "3" = paste0("Cluster 3\nn=",counts$n[3]),
         "4" = paste0("Cluster 4\nn=",counts$n[4]))

kmean_clust = kmean_CLUST +   geom_point(data=k.means, aes(x=dec_long_va, y=dec_lat_va,colour = factor(cluster), size = proportion),alpha=.6)+
  scale_color_manual(values = cols)+
  # scale_radius(trans='sqrt',breaks = c(.2,.4,.6,.8,1),labels = c(5,4,3,2,1),name = "Number of Cluster Changes") +
  scale_radius(trans = 'sqrt',breaks = c(0,.2,.4,.6,.8,1),name = "Proportion of Events in Cluster")+
  scale_shape(name="Gage Type")+
  facet_wrap(~cluster)+
  theme_void()



tiff("docs//byClusterCONUS.tiff", width = 12, height=12, units = "in", res=300)
kmean_clust + theme(legend.position = "bottom")
dev.off()

############### CONUS plots ####################


### Main cluster membership

# Calculate mode and cluster membership proportion

mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

prop = df %>% group_by(gage,kmeans,n) %>% summarise(kmeans.count = n())

k.means  = df  %>% group_by(gage) %>% summarise(kmeans.mode = mode(kmeans),
                                                kmeans.unique = n_distinct(kmeans))
k.means = k.means %>% left_join(.,prop,by=c("gage","kmeans.mode"="kmeans"))

k.means$prop = k.means$kmeans.count/k.means$n

colnames(k.means)  = c("gage",'mode','unique_membership','total_events','mode_count','proportion')

k.means = df %>% select(gage,dec_lat_va,dec_long_va,CLASS,Name) %>% left_join(k.means,.,by="gage") %>% unique()

## Plot
states <- map_data("state")

kmean_CLUST <- ggplot(data = states) + 
  geom_polygon(aes(x = long, y = lat, group = group), fill = "lightgray", color = "black") + 
  coord_fixed(1.3, xlim = c(-124.25,-70), ylim = c(26,48.5)) +
  theme_linedraw() + 
  # geom_point(data=k.means, aes(x=dec_long_va, y=dec_lat_va, shape = factor(CLASS),colour = factor(mode), size = proportion),alpha=.8)+
  geom_point(data=k.means, aes(x=dec_long_va, y=dec_lat_va ,colour = factor(mode), alpha = proportion),size=2)+
  scale_color_manual(name = "Cluster Membership Mode",values = cols)+
  # scale_radius(trans='sqrt',breaks = c(.2,.4,.6,.8,1),name = "Mode Proportion") +
  scale_shape(name="Gage Type")+
  theme_void()

kmean_CLUST

pdf("docs//ModeClusterCONUS.pdf")
kmean_CLUST+ theme(legend.position = "bottom")
dev.off()
