library(ggplot2)
library(rgdal)
library(tidyverse)
library(rwrfhydro)
library(maps)
library(sf)
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
    "Western Mountains" = "#56B4E9",
    "Ignore"="lightgrey")


# Calculate mode and cluster membership proportion

## Plot

mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

prop = df %>% group_by(gage,kmeans,n) %>% summarise(kmeans.count = n())

prop$prop = prop$kmeans.count/prop$n


colnames(prop)  = c("gage",'cluster','total_events','event_count','proportion')

k.means = df %>% select(gage,dec_lat_va,dec_long_va,CLASS,Name) %>% left_join(prop,.,by="gage") %>% unique()


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

map = map +   geom_point(data=k.means, aes(x=dec_long_va, y=dec_lat_va,colour = factor(cluster), size = proportion),alpha=.2)+
# map = map +   geom_point(data=k.means, aes(x=dec_long_va, y=dec_lat_va, colour = factor(cluster), alpha = proportion))+
  scale_color_manual(values = cols)+
  # scale_radius(trans='sqrt',breaks = c(.2,.4,.6,.8,1),labels = c(5,4,3,2,1),name = "Number of Cluster Changes") +
  # scale_radius(trans = 'sqrt',breaks = c(0,.2,.4,.6,.8,1),name = "Proportion of Events in Cluster")+
  scale_shape(name="Gage Type")+
  facet_wrap(~cluster)+
  theme_void()

pdf("docs/spatialCluster.pdf")
map + theme(legend.position = NULL)
dev.off()



### Faceted by cluster membership
