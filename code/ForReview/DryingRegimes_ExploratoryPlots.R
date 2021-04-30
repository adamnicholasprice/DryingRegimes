#####################################################################
##
## Script name: DryingRegimes_ExploratoryPlots.R
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




######################################################## 
############################ Plot ######################
######################################################## 




########################## Calculate mode and cluster membership proportion change 

mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}
df = df %>% select(gage,kmeans) %>% group_by(gage,kmeans) %>% count()

prop = df %>% group_by(gage,kmeans,n) %>% summarise(kmeans.count = n())

prop$prop = prop$kmeans.count/prop$n


colnames(prop)  = c("gage",'cluster','total_events','event_count','proportion')
colnames(prop_noFreq)  = c("gage",'cluster_noFreq','total_events_noFreq','event_count_noFreq','proportion_noFreq')
# df = kfreq




tt = prop_noFreq %>% group_by(gage,cluster_noFreq) %>% left_join(.,prop,by = c("gage","cluster_noFreq"="cluster"))

# tt[is.na(tt)]=0
tt$proportion_chg = (tt$proportion_noFreq - tt$proportion)

tt[is.na(tt)]=NA



k.means = df %>% select(gage,dec_lat_va,dec_long_va,AggEcoregion) %>% left_join(tt,.,by="gage") %>% unique()

k.means[is.na(k.means$proportion_chg),"cluster_noFreq"]<-5
k.means[is.na(k.means$proportion_chg),"proportion_chg"]<-1

### Load map data
states <- st_as_sf(map("state", plot = FALSE, fill = TRUE))


agg_eco = sf::st_read('data/dissolved_ecoregions_060220/dissolved_ecoregions_060220.shp')
agg_eco$NA_L1CODE = as.numeric(agg_eco$NA_L1CODE)
newData <- sf::st_transform(agg_eco, CRS=4326)
states <- sf::st_transform(states, CRS=4326)


counts = df %>% group_by(kmeans) %>% count()
labs = c("1" = paste0("Cluster 1\nn=",counts$n[1]),
         "2" = paste0("Cluster 2\nn=",counts$n[2]),
         "3" = paste0("Cluster 3\nn=",counts$n[3]),
         "4" = paste0("Cluster 4\nn=",counts$n[4]),
         "5" = "New cluster assignment")

# map <- ggplot(data = newData) +
#   geom_sf(aes(fill=Name),color=NA,alpha=.4)+
#   scale_fill_manual(values = pal_regions)

map = ggplot(data=states)+
  geom_sf(alpha=0,lwd=.2)

# map = map + 
#   geom_sf(data=newData,aes(fill=Name),color=NA,alpha=.4)+
#   scale_fill_manual(values = pal_regions)

# map = map + 
#   geom_point(data=k.means, aes(x=dec_long_va, y=dec_lat_va,alpha = abs(proportion_chg)),color="black",shape=1)+
#   geom_point(data=k.means, aes(x=dec_long_va, y=dec_lat_va,colour = factor(cluster_noFreq), alpha = abs(proportion_chg)))+
# scale_color_manual(values = cols)+
# scale_radius(trans='sqrt',breaks = c(.2,.4,.6,.8,1),labels = c(5,4,3,2,1),name = "Number of Cluster Changes") +
# scale_radius(trans = 'sqrt',breaks = c(0,.2,.4,.6,.8,1),name = "Proportion of Events in Cluster")+
map = map + 
  # geom_point(data=k.means, aes(x=dec_long_va, y=dec_lat_va),color="black",shape=1)+
  geom_point(data=k.means, aes(x=dec_long_va, y=dec_lat_va,color = proportion_chg),alpha=.8)+
  # scale_fill_viridis(option = "magma",limits = c(-0.6,0),mid=-0.3)+
  scale_color_continuous_divergingx(palette = 'RdYlBu', mid = 0) + 
  scale_shape(name="Gage Type")+
  facet_wrap(~cluster_noFreq)+
  theme_void()

# pdf("docs/response_plots/spatialCluster_noFreq_change.pdf")
map
# dev.off()


############### LULC by cluster

cols <- 
  c("1" = "#4477AA",
    "2" = "#66CCEE",
    "3" = "#228833",
    "4" = "#CCBB44"
  )

df = read.csv("data/dryingRegimes_data_RF.csv")


bp <- function(metric){
  ggplot(data=df)+
    geom_boxplot(aes(x=paste('Cluster',factor(kmeans)),y=metric,group=kmeans,fill = factor(kmeans)),outlier.colour = NA)+
    scale_fill_manual(values = cols,"Cluster Membership")+
    theme_light()
}

water = bp(df$lulc_water_prc)+
  ylim(c(0,.025))+
  ylab("(7) lulc_water_prc")+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank())

dev = bp(df$lulc_dev_prc)+
  ylim(c(0,.3))+
  ylab("(5) lulc_dev_prc")+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank())

forest = bp(df$lulc_forest_prc)+
  # ylim(c(0,.1))+
  ylab("(3) lulc_forest_prc")+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank())

barren = bp(df$lulc_barren_prc)+
  ylim(c(0,.02))+
  ylab("(17) lulc_barren_prc")+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank())

grass = bp(df$lulc_grass_prc)+
  # ylim(c(0,1))+
  ylab("(1) lulc_grass_prc")+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank())

ag = bp(df$lulc_ag_prc)+
  # ylim(c(0,1))+
  ylab("(9) lulc_ag_prc")+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank())

wetland  = bp(df$lulc_wetland_prc)+
  ylim(c(0,.05))+
  ylab("(2) lulc_wetland_prc")+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank())


plt = grass + wetland + forest + dev + water + ag + barren +
  plot_layout(ncol = 2, guides = "collect") & theme(legend.position = "none")

plt

pdf("docs/response_plots/LULCboxPlots_noFreq.pdf")
plt
dev.off()