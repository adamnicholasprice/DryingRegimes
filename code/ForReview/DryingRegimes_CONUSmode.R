#####################################################################
##
## Script name: DryingMetrics_CONUSmode.R
##
## Author: Adam N. Price
##
## Date Created: 2021-05-10
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
library(ggplot2)
library(patchwork)
# library(viridis)
library(tidyverse)
# library(rwrfhydro)
library(maps)
############################# Code ################################


df = read.csv("data/kmeans_NoFreq.csv")

df = df %>% group_by(gage) %>%
  mutate(count = n())

mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

prop = df %>% group_by(gage,kmeans,count) %>% summarise(kmeans.count = n())

k.means  = df  %>% group_by(gage) %>% summarise(kmeans.mode = mode(kmeans),
                                                kmeans.unique = n_distinct(kmeans))
k.means = k.means %>% left_join(.,prop,by=c("gage","kmeans.mode"="kmeans"))

k.means$prop = k.means$kmeans.count/k.means$count

colnames(k.means)  = c("gage",'mode','unique_membership','total_events','mode_count','proportion')

k.means = df %>% select(gage,dec_lat_va,dec_long_va,AggEcoregion) %>% left_join(k.means,.,by="gage") %>% unique()

## Plot
cols <- 
  c("1" = "#4477AA",
    "2" = "#66CCEE",
    "3" = "#228833",
    "4" = "#CCBB44")


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

pdf("docs/response_plots/CONUSClusterMode.pdf")
kmean_CLUST+ theme(legend.position = "bottom")
dev.off()
