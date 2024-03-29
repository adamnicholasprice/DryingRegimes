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
library(raster)
library(rwrfhydro)
############################# Code ################################

########## Load Data #############
df = read.csv("../data/kmeans.csv")



# Clean up missing coordinates
tt = gages2Attr
tt$STAID = as.integer(tt$STAID)


noCoor = as.data.frame(df$gage)
colnames(noCoor) = 'gage'

coors = left_join(noCoor,tt,by=c('gage'='STAID'))

df$dec_lat_va = coors$LAT_GAGE
df$dec_long_va = coors$LNG_GAGE


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




############### CONUS plots ####################


states <- map_data("state")

kmean_CLUST <- ggplot(data = states) + 
  geom_polygon(aes(x = long, y = lat, group = group), fill = "gray", color = "black") + 
  coord_fixed(1.3, xlim = c(-124.25,-70), ylim = c(26,48.5)) +
  theme_linedraw() + 
  geom_point(data=spat.clust, aes(x=dec_long_va, y=dec_lat_va, shape = factor(CLASS),size = 1.5/(kmeans.unique)),alpha=1)+
  geom_point(data=spat.clust, aes(x=dec_long_va, y=dec_lat_va, shape = factor(CLASS),colour = factor(kmeans.mode), size = 1/kmeans.unique),alpha=.7)+
  scale_color_manual(name = "Cluster Membership Mode",values = cols)+
  scale_radius(trans='sqrt',breaks = c(.2,.4,.6,.8,1),labels = c(5,4,3,2,1),name = "Number of Cluster Changes") +
  scale_shape(name="Gage Type")+
  ggtitle("k-means")

kmean_CLUST


hier_CLUST <- ggplot(data = states) + 
  geom_polygon(aes(x = long, y = lat, group = group), fill = "gray", color = "black") + 
  coord_fixed(1.3, xlim = c(-124.25,-70), ylim = c(26,48.5)) +
  theme_linedraw() + 
  geom_point(data=spat.clust, aes(x=dec_long_va, y=dec_lat_va, shape = factor(CLASS),size = 1.5/hier.unique),colour="black", alpha=1)+
  geom_point(data=spat.clust, aes(x=dec_long_va, y=dec_lat_va, shape = factor(CLASS), colour = factor(hier.mode),size = 1/(hier.unique)), alpha=.7)+
  scale_color_manual(name = "Cluster Membership Mode",values = cols)+
  scale_radius(trans='sqrt',breaks = c(.2,.4,.6,.8,1),labels = c(5,4,3,2,1),name = "Number of Cluster Changes") +
  scale_shape(name="Gage Type")+
  ggtitle("Hierarchical")

hier_CLUST

gmm_CLUST <- ggplot(data = states) + 
  geom_polygon(aes(x = long, y = lat, group = group), fill = "gray", color = "black") + 
  coord_fixed(1.3, xlim = c(-124.25,-70), ylim = c(26,48.5)) +
  theme_linedraw() + 
  geom_point(data=spat.clust, aes(x=dec_long_va, y=dec_lat_va, shape = factor(CLASS),size = 1.5/gmm.unique),colour="black", alpha=1)+
  geom_point(data=spat.clust, aes(x=dec_long_va, y=dec_lat_va, shape = factor(CLASS), colour = factor(gmm.mode),size = 1/(gmm.unique)), alpha=.7)+
  scale_color_manual(name = "Cluster Membership Mode",values = cols)+
  scale_radius(trans='sqrt',breaks = c(.2,.4,.6,.8,1),labels = c(5,4,3,2,1),name = "Number of Cluster Changes") +
  scale_shape(name="Gage Type")+
  ggtitle("GMM")

# gmm_CLUST + facet_wrap(~factor(gmm.clust))
gmm_CLUST


db_CLUST <- ggplot(data = states) + 
  geom_polygon(aes(x = long, y = lat, group = group), fill = "gray", color = "black") + 
  coord_fixed(1.3, xlim = c(-124.25,-70), ylim = c(26,48.5)) +
  theme_linedraw() + 
  geom_point(data=spat.clust, aes(x=dec_long_va, y=dec_lat_va, shape = factor(CLASS), colour = dbscan.mode), size = 2, alpha=1)+
  scale_color_viridis(option = "A",name="Cluster Membership Mode")+
  scale_
  ggtitle("DBSCAN")

# gmm_CLUST + facet_wrap(~factor(gmm.clust))
db_CLUST

###################### Centroids in 2d bins ###################

dat = dat %>% group_by(gage) %>% count() %>% left_join(dat,.,by="gage")

region.mean  = dat.mean %>% group_by(Name,CLASS)%>%
  select(Name,CLASS,mean_dry_date_start,mean_drying_rate,mean_peak2zero,mean_dry_dur,n_events) %>%
  summarise(.,mean.dry.date = mean(mean_dry_date_start),
            mean.drying.rate = mean(mean_drying_rate),
            mean.peak2zero = mean(mean_peak2zero),
            mean.dry_dur = mean(mean_dry_dur),
            mean.n_events = mean(n_events)
  )

kmean.mean = clust %>% group_by(kmeans.clust) %>%
  summarise(.,mean.dry.date = mean(dry_date_start),
            mean.drying.rate = mean(drying_rate),
            mean.peak2zero = mean(peak2zero),
            mean.dry_dur = mean(dry_dur),
            mean.rel_freq = mean(rel_freq))

drying_rate = ggplot(dat,aes(x=dry_date_start,y = drying_rate))+
  geom_bin2d(binwidth = c(5, 0.15))+
  xlab("Dry Start Date (day)")+
  ylab("Drying Rate\n(1/day)") +
  scale_fill_viridis(limit=c(0,120))
# scale_fill_gradientn(trans = 'log',labels=trans_format("identity", function(x) round(x,2)),limits = c(1,300),colors = viridis(10),breaks =c(1,10,50,100,200))
drying_rate = drying_rate + 
  geom_point(data=region.mean,aes(x=mean.dry.date,y=mean.drying.rate,group=paste(Name,CLASS),shape = factor(CLASS)),colour="white",size = 5, alpha=.75)
drying_rate = drying_rate + geom_point(data=region.mean,aes(x=mean.dry.date,y=mean.drying.rate,group=paste(Name,CLASS),color=Name,shape = factor(CLASS)),size = 4, alpha=.25)

drying_rate = drying_rate + 
  geom_point(data=kmean.mean,aes(x=mean.dry.date,y=mean.drying.rate,color=factor(kmeans.clust)),shape=15,size = 4)+
  scale_color_manual(values = c(pal_regions,cols))

peak2zero = ggplot(dat,aes(x=dry_date_start,y = peak2zero))+
  geom_bin2d(binwidth = c(5,5)) +
  ylim(0,365)+
  xlab("Dry Start Date (day)")+
  ylab("Peak2Zero\n(day)") +
  scale_fill_viridis(limit=c(0,120))
# scale_fill_gradientn(trans = 'log',labels=trans_format("identity", function(x) round(x,2)),limits = c(1,300),colors = viridis(10),breaks =c(1,10,50,100,200))

peak2zero = peak2zero + 
  geom_point(data=region.mean,aes(x=mean.dry.date,y=mean.peak2zero,group=paste(Name,CLASS),shape = factor(CLASS)),colour="white",size = 5, alpha=.25)
peak2zero = peak2zero + geom_point(data=region.mean,aes(x=mean.dry.date,y=mean.peak2zero,group=paste(Name,CLASS),color=Name,shape = factor(CLASS)),size = 4, alpha=.25)+
  scale_color_manual(values = pal_regions)

peak2zero = peak2zero + 
  geom_point(data=kmean.mean,aes(x=mean.dry.date,y=mean.peak2zero,color=factor(kmeans.clust)),shape=15,size = 4)+
  scale_color_manual(values = c(pal_regions,cols))

dry_dur = ggplot(dat,aes(x=dry_date_start,y = dry_dur)) +
  geom_bin2d(binwidth = c(5,5)) +
  ylim(0,365) +
  xlab("Dry Start Date (day)")+
  ylab("Drying Duration\n(day)") +
  scale_fill_viridis(limit=c(0,120))
# scale_fill_gradientn(trans = 'log',labels=trans_format("identity", function(x) round(x,2)),limits = c(1,300),colors = viridis(10),breaks =c(1,10,50,100,200))

dry_dur = dry_dur + 
  geom_point(data=region.mean,aes(x=mean.dry.date,y=mean.dry_dur,group=paste(Name,CLASS),shape = factor(CLASS)),colour="white",size = 5, alpha=.25)

dry_dur = dry_dur + geom_point(data=region.mean,aes(x=mean.dry.date,y=mean.dry_dur,group=paste(Name,CLASS),color=Name,shape = factor(CLASS)),size = 4, alpha=.25)+
  scale_color_manual(values = pal_regions)

dry_dur = dry_dur + 
  geom_point(data=kmean.mean,aes(x=mean.dry.date,y=mean.dry_dur,color=factor(kmeans.clust)),shape=15,size = 4)+
  scale_color_manual(values = c(pal_regions,cols))


n_events = ggplot(dat,aes(x=dry_date_start,y = n)) +
  geom_bin2d(binwidth = c(5,7)) +
  # ylim(0,365) +
  xlab("Dry Start Date (day)")+
  ylab("Number of Events") +
  scale_fill_viridis(limit=c(0,120))
# scale_fill_gradientn(trans = 'log',labels=trans_format("identity", function(x) round(x,2)),limits = c(1,300),colors = viridis(10),breaks =c(1,10,50,100,200))

n_events = n_events + 
  geom_point(data=region.mean,aes(x=mean.dry.date,y=mean.n_events,group=paste(Name,CLASS),shape = factor(CLASS)),colour="white",size = 5, alpha=.25)
n_events = n_events+geom_point(data=region.mean,aes(x=mean.dry.date,y=mean.n_events,group=paste(Name,CLASS),color=Name,shape = factor(CLASS)),size = 4, alpha=.25)+
  scale_color_manual(values = pal_regions)

binned  = drying_rate + peak2zero   + dry_dur + plot_layout(ncol = 3, guides = "collect") & theme(legend.position = 'right') & ggtitle("k-means")

binned

###################### PCA Plot #############

PCA = prcomp(dat.scale)
var <- get_pca_var(PCA)
corrplot(var$cos2, is.corr=FALSE)

df_out <- as.data.frame(PCA$x)
df_out$group <- sapply( strsplit(as.character(row.names(df)), "_"), "[[", 1 )
head(df_out)

k_means <- ggplot(df_out,aes(x=PC1,y=PC2,col=factor(clust$kmeans.clust)))+
  geom_point(size=3,alpha=0.5)+
  scale_color_manual(name = "K-means Cluster",values = cols)+
  theme_classic()

gmm.clust <- ggplot(df_out,aes(x=PC1,y=PC2,col=factor(clust$gmm.clust)))+
  geom_point(size=3,alpha=0.5)+
  scale_color_manual(name = "GMM Cluster",values = cols)+
  theme_classic()

hier <- ggplot(df_out,aes(x=PC1,y=PC2,col=factor(clust$hier.4.clust)))+
  geom_point(size=3,alpha=0.5)+
  scale_color_manual(name = "Hierarchical Cluster",values = cols)+
  theme_classic()

dbscan.clust <- ggplot(df_out,aes(x=PC1,y=PC2,col=factor(clust$dbscan.cluster)))+
  geom_point(size=3,alpha=0.5)+
  scale_color_manual(name = "DBSCAN Cluster",values = cols)+
  theme_classic()


# autoplot(PCA,colour = as.factor(clust$hier.4.clust),scale = 0,loadings=T)
# autoplot(PCA,colour = as.factor(clust$gmm.clust),scale = 0,loadings=T)
# autoplot(PCA,colour = as.factor(clust$kmeans.clust),scale = 0,loadings=T)
# autoplot(PCA,colour = as.factor(clust$dbscan.cluster),scale = 0,loadings=T)

# Stats plot
dat.metrics = df %>% select("peak2zero","drying_rate", 
                            "dry_date_start", "dry_dur",
                            "peak_quantile", "rel_freq")

h  = ggpairs(dat.metrics,
             aes(color = as.factor(clust$hier.4.clust)))
for(i in 1:h$nrow) {
  for(j in 1:h$ncol){
    h[i,j] <- h[i,j] + 
      scale_fill_manual(values=cols) +
      scale_color_manual(values=cols)  
  }
}

g = ggpairs(dat.metrics,
            aes(color = as.factor(clust$gmm.clust)))
for(i in 1:g$nrow) {
  for(j in 1:g$ncol){
    g[i,j] <- g[i,j] + 
      scale_fill_manual(values=cols) +
      scale_color_manual(values=cols)  
  }
}

k = ggpairs(dat.metrics,
            aes(color = as.factor(clust$kmeans.clust)))
for(i in 1:k$nrow) {
  for(j in 1:k$ncol){
    k[i,j] <- k[i,j] + 
      scale_fill_manual(values=cols) +
      scale_color_manual(values=cols)  
  }
}

d = ggpairs(dat.metrics,
            aes(color = as.factor(clust$dbscan.clust)))
for(i in 1:d$nrow) {
  for(j in 1:d$ncol){
    d[i,j] <- d[i,j] + 
      scale_fill_manual(values=cols) +
      scale_color_manual(values=cols)  
  }
}

h
g
k
d


tt = boxplot(dat.metrics$peak2zero~clust$dbscan.clust, outline=F, ylab = "Peak to Zero [days]", xlab=NULL)

