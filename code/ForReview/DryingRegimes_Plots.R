#####################################################################
##
## Script name: DryingRegimes_Plots.R
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
############################# Code #########

# Load data

df = read.csv("data/kmeans_NoFreq.csv")

################# Color Palettes #################################
cols <- 
  c("1" = "#4477AA",
    "2" = "#66CCEE",
    "3" = "#228833",
    "4" = "#CCBB44",
    "5" = "#FF0000"
  )



pal_regions <- 
  c("Eastern Forests" = "#009E73",
    "Mediterranean California" = "#F0E442",
    "Northern Great Plains" = "#0072B2",
    "Southern Great Plains" = "#E69F00",
    "Western Deserts" = "#D55E00",
    "Western Mountains" = "#56B4E9",
    "Ignore"="lightgrey")


############################# Box Plots #########


bp <- function(metric){
  ggplot(data=df)+
    geom_boxplot(aes(x=paste('Cluster',factor(kmeans)),y=metric,group=kmeans,fill = factor(kmeans)),outlier.colour = NA)+
    scale_fill_manual(values = cols,"Cluster Membership")+
    theme_light()
}

peak2zero = bp(df$peak2zero)+
  ylim(c(0,70))+
  ylab("Dry-down duration\n(Days)")+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank())


drying_rate = bp(df$drying_rate) + 
  ylim(0,2)+
  ylab("Drying rate\n(1/Days)")+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank())

dry_date_start = bp(df$dry_date_start)+
  ylab("Dry date start\n(Day of Year)")+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank())


dry_dur = bp(df$dry_dur) + 
  ylim(c(0,400))+
  ylab("No flow duration\n(Days)")+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank())

peakQ = bp(df$peak_quantile)+
  ylab("Antecedent peak quantile")+
  xlab(NULL)

ann_freq = bp(df$freq_local)+
  ylim(c(0,15))+
  ylab("Annual event requency")+
  xlab(NULL)

p = peak2zero + drying_rate + dry_dur + peakQ + dry_date_start + ann_freq +
  plot_layout(ncol = 2, guides = "collect") & theme(legend.position = "none")


pdf("docs/response_plots/boxPlots_noFreq.pdf")
p
dev.off()


p = peak2zero + drying_rate + dry_dur + peakQ + 
  plot_layout(ncol = 2, guides = "collect") & theme(legend.position = "none")


pdf("docs/response_plots/boxPlots_noFreqClusterMetrics.pdf")
p
dev.off()



#############  Spatial Plots ########

# Replace missing coords

tt = rwrfhydro::gages2Attr
tt$STAID = as.integer(tt$STAID)


noCoor = as.data.frame(df$gage)
colnames(noCoor) = 'gage'

coors = left_join(noCoor,tt,by=c('gage'='STAID'))

df$dec_lat_va = coors$LAT_GAGE
df$dec_long_va = coors$LNG_GAGE


##### Get proportion of events
mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

tt = df %>% select(gage,kmeans) %>% group_by(gage,kmeans) %>% count()

prop = tt %>% group_by(gage,kmeans,n) %>% summarise(kmeans.count = n())

prop$prop = prop$kmeans.count/prop$n


colnames(prop)  = c("gage",'cluster','total_events','event_count','proportion')

k.means = df %>% select(gage,dec_lat_va,dec_long_va,AggEcoregion) %>% left_join(prop,.,by="gage") %>% unique()


######################################################## 
############################ Plot ######################
######################################################## 
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
         "4" = paste0("Cluster 4\nn=",counts$n[4]))

# map <- ggplot(data = newData) +
#   geom_sf(aes(fill=Name),color=NA,alpha=.4)+
#   scale_fill_manual(values = pal_regions)

map = ggplot(data=states)+
  geom_sf(alpha=0,lwd=.2)

map = map + 
  geom_sf(data=newData,aes(fill=Name),color=NA,alpha=.4)+
  scale_fill_manual(values = pal_regions)



map = map + 
  geom_point(data=k.means, aes(x=dec_long_va, y=dec_lat_va,alpha = proportion),color="black",shape=1)+
  geom_point(data=k.means, aes(x=dec_long_va, y=dec_lat_va,colour = factor(cluster), alpha = proportion))+
  scale_color_manual(values = cols)+
  # scale_radius(trans='sqrt',breaks = c(.2,.4,.6,.8,1),labels = c(5,4,3,2,1),name = "Number of Cluster Changes") +
  # scale_radius(trans = 'sqrt',breaks = c(0,.2,.4,.6,.8,1),name = "Proportion of Events in Cluster")+
  scale_shape(name="Gage Type")+
  facet_wrap(~cluster)+
  theme_void()

pdf("docs/response_plots/spatialCluster.pdf")
map
dev.off()




######################################################## 
################### Temporal Plot ######################
######################################################## 














 