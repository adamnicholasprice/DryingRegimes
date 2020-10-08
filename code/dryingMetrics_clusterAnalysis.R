#####################################################################
##
## Script name: dryingMetrics_clusterAnalysis.r
##
## Author: Adam N. Price
##
## Date Created: 2020-07-31
##
## Copyright (c) Adam N. Price, 2020
## Email: adnprice@ucsc.edu
##
############################# Description ##########################
##  Gausian Mixture Models
## 
##   https://www.youtube.com/watch?v=qMTuMa86NzU
##  https://rdrr.io/github/mccreigh/rwrfhydro/man/gages2AttrPlus.html
##  https://en.proft.me/2017/02/1/model-based-clustering-r/
##  https://bioconductor.org/packages/release/bioc/vignettes/PCAtools/inst/doc/PCAtools.html
##
############################# Packages #############################

## load up the packages we will need:  (uncomment as required)
library(tidyverse)
library(mclust)
library(ggplot2)
library(mapdata)
library(ggfortify)
library(plotly)
library(ClusterR)
library(cluster)
library(factoextra)
library(GGally)
library(vegan)
library(clustsig)
library(dplyr)
library(patchwork)
library(parallel)
library(corrplot)
library(scales)
library(viridis)


############################# Code ################################

#################### Load  and filter data #################
df = read.csv("../data/metrics_by_event_combined_raw.csv")

df$Name[df$Name == "Ignore"] = "Mediterranean California" 
df = df[df$peak_quantile>.25 & df$drying_rate>=0,]
#Rename event_id  (Somethign is weird here...)
df<-df %>% 
  mutate(event_id = seq(1, nrow(df)))

df = df %>% group_by(gage) %>% count() %>% left_join(df,.,by="gage")
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Step 2: Estimate events per year ---------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Create fun to estimate number of drying events in the same meterologic year per event
fun<-function(n){
  
  #Libraries of interest
  library(dplyr)
  
  #isolate event of interest
  event <- df[n,]
  
  #count number of events in same year and at same gage
  count<- df %>% 
    filter(meteorologic_year == event$meteorologic_year) %>% 
    filter(gage == event$gage) %>% 
    nrow() 
  
  #Export info
  tibble(
    event_id = event$event_id,
    freq_local = count
  )
}

#run function
n.cores <- detectCores() - 1
cl <- makeCluster(n.cores)
clusterExport(cl, "df")
output<-parLapply(cl, seq(1,nrow(df)), fun)
remove(cl)

#add results to df
output<-bind_rows(output)
df<-left_join(df,output)

### Make rel_freq metric
df$rel_freq = df$freq_local/df$n

dat.scale <- df %>% 
  #Select vars of interest
  select("peak2zero","drying_rate", 
         "dry_date_start", "dry_dur",
         "peak_quantile", "rel_freq") %>% 
  #scale vars
  scale()

dat.scale <- df %>% 
  #Select vars of interest
  select("peak2zero","drying_rate"
         , "dry_dur",
         "peak_quantile", "rel_freq") %>% 
  #scale vars
  scale()


# NbClust(x, diss="NULL", distance = "euclidean", min.nc=2, max.nc=10, 
#         method = "ward", index = "dindex", alphaBeale = 0.1)
############## PCA ############
PCA = prcomp(dat.scale)
autoplot(PCA,loadings=T,loadings.label=T)



# Visualize eigenvalues/variances
fviz_screeplot(PCA, addlabels = TRUE, ylim = c(0, 50))

# var$contribution

################### K-Means ######################
fviz_nbclust(dat.scale, kmeans, method = "silhouette") + theme_classic()

wcke<-eclust(dat.scale, "kmeans", hc_metric="euclidean",k=5)
fviz_cluster(wcke, geom = "point", ellipse.type = "norm", ggtheme = theme_minimal())


sile<-silhouette(wcke$cluster, dist(dat.scale))
fviz_silhouette(sile)

################ Gaussian mixture model ################

## https://rdrr.io/cran/ClusterR/man/predict_GMM.html 
# http://mlampros.github.io/2016/09/12/clusterR_package/


opt_gmm = Optimal_Clusters_GMM(dat.scale, max_clusters = 10, criterion = "BIC", 
                               
                               dist_mode = "maha_dist", seed_mode = "random_subset",
                               
                               km_iter = 10, em_iter = 10, var_floor = 1e-10, 
                               
                               plot_data = T)


gmm = GMM(dat.scale, 5, dist_mode = "maha_dist", seed_mode = "random_subset", km_iter = 10,
          
          em_iter = 10, verbose = F)          

pr = predict_GMM(dat.scale, gmm$centroids, gmm$covariance_matrices, gmm$weights) 

###################### Hierch Cluster ######################

# Create distance matrix with scaled vars
d <- dat.scale %>%
  vegdist(., method = 'euclidean')

#Use heirchal clustering
fit <- hclust(d, method = "ward")

#After visual inspection, select where to cut the tree
df<- df %>% 
  mutate(
    clust_4 = cutree(fit, k=4))


plot(fit, 
     sub="Sampling Site", 
     hang=-0.5, 
     main = NULL,
     labels = FALSE,
     ylab="Height") 
title("Cluster Analysis: Ward's Mimium Variance (Euclidean Distance)", line = 3, cex =2)
title("4 Groups", line = 2)
rect.hclust(fit, k=4)

##################### Density Based Clustering #########################
library(fpc)
library(dbscan)
library(factoextra)

kNNdistplot(dat.scale,k = log(length(dat.scale)))

db = fpc::dbscan(dat.scale,eps=.5,MinPts = log(length(dat.scale)))

fviz_cluster(db,dat.scale, stand = FALSE, ellipse = FALSE, geom = "point",show.clust.cent = T,pointsize = .5)


######### Merge Clustering Data ###########################

clust = df %>% select(gage,Name,CLASS,dec_lat_va,dec_long_va,clust_4,peak2zero,drying_rate,dry_date_start, dry_dur,peak_quantile, rel_freq)

clust = cbind(clust,pr$cluster_labels+1,wcke$cluster,db$cluster+1) 

colnames(clust) = c("gage","Name","CLASS","dec_lat_va","dec_long_va","hier.4.clust","peak2zero","drying_rate", 
                    "dry_date_start", "dry_dur",
                    "peak_quantile", "rel_freq","gmm.clust","kmeans.clust","dbscan.cluster")

# write.csv(clust,"../data/clustering_results_allevents.csv")
######## Plots ##########
#  The plotting data is in the process of being moved to 
#  dryingMetrics_clusterPlot.r
# 
# 

# Color scale
# cols <- 
#   c("1" = "#4477AA",
#     "2" = "#66CCEE",
#     "3" = "#228833",
#     "4" = "#CCBB44",
#     "5" = "#EE6677",
#     "6" = "#AA3377",
#     "7" = "#BBBBBB",
#     "8" = "#999944",
#     "9" = "#332288")
# 
# ## PCA Plot
# 
# PCA = prcomp(dat.scale)
# var <- get_pca_var(PCA)
# corrplot(var$cos2, is.corr=FALSE)
# 
# df_out <- as.data.frame(PCA$x)
# df_out$group <- sapply( strsplit(as.character(row.names(df)), "_"), "[[", 1 )
# head(df_out)
# 
# k_means <- ggplot(df_out,aes(x=PC1,y=PC2,col=factor(clust$kmeans.clust)))+
#   geom_point(size=3,alpha=0.5)+
#   scale_color_manual(name = "K-means Cluster",values = cols)+
#   theme_classic()
# 
# gmm.clust <- ggplot(df_out,aes(x=PC1,y=PC2,col=factor(clust$gmm.clust)))+
#   geom_point(size=3,alpha=0.5)+
#   scale_color_manual(name = "GMM Cluster",values = cols)+
#   theme_classic()
# 
# hier <- ggplot(df_out,aes(x=PC1,y=PC2,col=factor(clust$hier.4.clust)))+
#   geom_point(size=3,alpha=0.5)+
#   scale_color_manual(name = "Hierarchical Cluster",values = cols)+
#   theme_classic()
# 
# dbscan.clust <- ggplot(df_out,aes(x=PC1,y=PC2,col=factor(clust$dbscan.cluster)))+
#   geom_point(size=3,alpha=0.5)+
#   scale_color_manual(name = "DBSCAN Cluster",values = cols)+
#   theme_classic()
# 
# 
# # autoplot(PCA,colour = as.factor(clust$hier.4.clust),scale = 0,loadings=T)
# # autoplot(PCA,colour = as.factor(clust$gmm.clust),scale = 0,loadings=T)
# # autoplot(PCA,colour = as.factor(clust$kmeans.clust),scale = 0,loadings=T)
# # autoplot(PCA,colour = as.factor(clust$dbscan.cluster),scale = 0,loadings=T)
# 
# # Stats plot
# dat.metrics = df %>% select("peak2zero","drying_rate", 
#                             "dry_date_start", "dry_dur",
#                             "peak_quantile", "rel_freq")
# 
# h  = ggpairs(dat.metrics,
#              aes(color = as.factor(clust$hier.4.clust)))
# for(i in 1:h$nrow) {
#   for(j in 1:h$ncol){
#     h[i,j] <- h[i,j] + 
#       scale_fill_manual(values=cols) +
#       scale_color_manual(values=cols)  
#   }
# }
# 
# g = ggpairs(dat.metrics,
#         aes(color = as.factor(clust$gmm.clust)))
# for(i in 1:g$nrow) {
#   for(j in 1:g$ncol){
#     g[i,j] <- g[i,j] + 
#       scale_fill_manual(values=cols) +
#       scale_color_manual(values=cols)  
#   }
# }
# 
# k = ggpairs(dat.metrics,
#         aes(color = as.factor(clust$kmeans.clust)))
# for(i in 1:k$nrow) {
#   for(j in 1:k$ncol){
#     k[i,j] <- k[i,j] + 
#       scale_fill_manual(values=cols) +
#       scale_color_manual(values=cols)  
#   }
# }
# 
# d = ggpairs(dat.metrics,
#         aes(color = as.factor(clust$dbscan.clust)))
# for(i in 1:d$nrow) {
#   for(j in 1:d$ncol){
#     d[i,j] <- d[i,j] + 
#       scale_fill_manual(values=cols) +
#       scale_color_manual(values=cols)  
#   }
# }
# 
# h
# g
# k
# d
# 
# 
# tt = boxplot(dat.metrics$peak2zero~clust$dbscan.clust, outline=F, ylab = "Peak to Zero [days]", xlab=NULL)
# 
# 
# ############### CONUS plots ####################
# 
# 
# mode <- function(x) {
#   ux <- unique(x)
#   ux[which.max(tabulate(match(x, ux)))]
# }
# 
# clust = na.omit(clust)
# k.means  = clust  %>% group_by(gage) %>% summarise(kmeans.mode = mode(kmeans.clust))
# hier  = clust %>% group_by(gage) %>% summarise(hier.4.mode = mode(hier.4.clust))
# gmm  = clust %>% group_by(gage) %>% summarise(gmm.mode = mode(gmm.clust))
# 
# spat.clust = clust %>% left_join(.,k.means,by="gage")%>% left_join(.,hier,by="gage")%>% left_join(.,gmm,by="gage")
# 
# states <- map_data("state")
# 
# kmean_CLUST <- ggplot(data = states) + 
#   geom_polygon(aes(x = long, y = lat, group = group), fill = "gray", color = "black") + 
#   coord_fixed(1.3, xlim = c(-124.25,-70), ylim = c(26,48.5)) +
#   theme_linedraw() + 
#   geom_point(data=spat.clust, aes(x=dec_long_va, y=dec_lat_va, shape = factor(CLASS), colour = kmeans.mode), size = 2, alpha=1)+
#   scale_color_viridis(option = "A",name="Cluster Membership Mode")+
#   ggtitle("k-means")
# 
# kmean_CLUST
# 
# 
# hier_CLUST <- ggplot(data = states) + 
#   geom_polygon(aes(x = long, y = lat, group = group), fill = "gray", color = "black") + 
#   coord_fixed(1.3, xlim = c(-124.25,-70), ylim = c(26,48.5)) +
#   theme_linedraw() + 
#   geom_point(data=spat.clust, aes(x=dec_long_va, y=dec_lat_va, shape = factor(CLASS), colour = hier.4.mode), size = 2, alpha=1)+
#   scale_color_viridis(option = "A",name="Cluster Membership Mode")+
#   ggtitle("Hierarchical")
# 
# # hier_CLUST + facet_wrap(~factor(hier.4.clust))
# hier_CLUST
# 
# gmm_CLUST <- ggplot(data = states) + 
#   geom_polygon(aes(x = long, y = lat, group = group), fill = "gray", color = "black") + 
#   coord_fixed(1.3, xlim = c(-124.25,-70), ylim = c(26,48.5)) +
#   theme_linedraw() + 
#   geom_point(data=spat.clust, aes(x=dec_long_va, y=dec_lat_va, shape = factor(CLASS), colour = gmm.mode), size = 2, alpha=1)+
#   scale_color_viridis(option = "A",name="Cluster Membership Mode")+
#   ggtitle("GMM")
# 
# # gmm_CLUST + facet_wrap(~factor(gmm.clust))
# gmm_CLUST
# 
# 
# ###################### Centroids in 2d bins ###################
# 
# dat = read.csv('../data/metrics_by_event_combined.csv')
# 
# ### Assign Ignore to Mediterranean Cali
# 
# dat$Name[dat$Name == "Ignore"] = "Mediterranean California" 
# 
# dat.mean = read.csv('../data/metrics_by_event_mean.csv')
# 
# dat.mean$Name[dat.mean$Name == "Ignore"] = "Mediterranean California" 
# 
# 
# pal_regions <- 
#   c("Eastern Forests" = "#009E73",
#     "Mediterranean California" = "#F0E442",
#     "Northern Great Plains" = "#0072B2",
#     "Southern Great Plains" = "#E69F00",
#     "Western Deserts" = "#D55E00",
#     "Western Mountains" = "#56B4E9")
# 
# ########## Number of events 
# dat = dat %>% group_by(gage) %>% count() %>% left_join(dat,.,by="gage")
# 
# region.mean  = dat.mean %>% group_by(Name,CLASS)%>%
#   select(Name,CLASS,mean_dry_date_start,mean_drying_rate,mean_peak2zero,mean_dry_dur,n_events) %>%
#   summarise(.,mean.dry.date = mean(mean_dry_date_start),
#             mean.drying.rate = mean(mean_drying_rate),
#             mean.peak2zero = mean(mean_peak2zero),
#             mean.dry_dur = mean(mean_dry_dur),
#             mean.n_events = mean(n_events)
#   )
# 
# kmean.mean = clust %>% group_by(kmeans.clust) %>%
#   summarise(.,mean.dry.date = mean(dry_date_start),
#             mean.drying.rate = mean(drying_rate),
#             mean.peak2zero = mean(peak2zero),
#             mean.dry_dur = mean(dry_dur),
#             mean.rel_freq = mean(rel_freq))
# 
# drying_rate = ggplot(dat,aes(x=dry_date_start,y = drying_rate))+
#   geom_bin2d(binwidth = c(5, 0.15))+
#   xlab("Dry Start Date (day)")+
#   ylab("Drying Rate\n(1/day)") +
#   scale_fill_viridis(limit=c(0,120))
# # scale_fill_gradientn(trans = 'log',labels=trans_format("identity", function(x) round(x,2)),limits = c(1,300),colors = viridis(10),breaks =c(1,10,50,100,200))
# drying_rate = drying_rate + 
#   geom_point(data=region.mean,aes(x=mean.dry.date,y=mean.drying.rate,group=paste(Name,CLASS),shape = factor(CLASS)),colour="white",size = 5, alpha=.75)
# drying_rate = drying_rate + geom_point(data=region.mean,aes(x=mean.dry.date,y=mean.drying.rate,group=paste(Name,CLASS),color=Name,shape = factor(CLASS)),size = 4, alpha=.25)
# 
# drying_rate = drying_rate + 
#   geom_point(data=kmean.mean,aes(x=mean.dry.date,y=mean.drying.rate,color=factor(kmeans.clust)),shape=15,size = 4)+
#   scale_color_manual(values = c(pal_regions,cols))
# 
# peak2zero = ggplot(dat,aes(x=dry_date_start,y = peak2zero))+
#   geom_bin2d(binwidth = c(5,5)) +
#   ylim(0,365)+
#   xlab("Dry Start Date (day)")+
#   ylab("Peak2Zero\n(day)") +
#   scale_fill_viridis(limit=c(0,120))
# # scale_fill_gradientn(trans = 'log',labels=trans_format("identity", function(x) round(x,2)),limits = c(1,300),colors = viridis(10),breaks =c(1,10,50,100,200))
# 
# peak2zero = peak2zero + 
#   geom_point(data=region.mean,aes(x=mean.dry.date,y=mean.peak2zero,group=paste(Name,CLASS),shape = factor(CLASS)),colour="white",size = 5, alpha=.25)
# peak2zero = peak2zero + geom_point(data=region.mean,aes(x=mean.dry.date,y=mean.peak2zero,group=paste(Name,CLASS),color=Name,shape = factor(CLASS)),size = 4, alpha=.25)+
#   scale_color_manual(values = pal_regions)
# 
# peak2zero = peak2zero + 
#   geom_point(data=kmean.mean,aes(x=mean.dry.date,y=mean.peak2zero,color=factor(kmeans.clust)),shape=15,size = 4)+
#   scale_color_manual(values = c(pal_regions,cols))
# 
# dry_dur = ggplot(dat,aes(x=dry_date_start,y = dry_dur)) +
#   geom_bin2d(binwidth = c(5,5)) +
#   ylim(0,365) +
#   xlab("Dry Start Date (day)")+
#   ylab("Drying Duration\n(day)") +
#   scale_fill_viridis(limit=c(0,120))
# # scale_fill_gradientn(trans = 'log',labels=trans_format("identity", function(x) round(x,2)),limits = c(1,300),colors = viridis(10),breaks =c(1,10,50,100,200))
# 
# dry_dur = dry_dur + 
#   geom_point(data=region.mean,aes(x=mean.dry.date,y=mean.dry_dur,group=paste(Name,CLASS),shape = factor(CLASS)),colour="white",size = 5, alpha=.25)
# 
# dry_dur = dry_dur + geom_point(data=region.mean,aes(x=mean.dry.date,y=mean.dry_dur,group=paste(Name,CLASS),color=Name,shape = factor(CLASS)),size = 4, alpha=.25)+
#   scale_color_manual(values = pal_regions)
# 
# dry_dur = dry_dur + 
#   geom_point(data=kmean.mean,aes(x=mean.dry.date,y=mean.dry_dur,color=factor(kmeans.clust)),shape=15,size = 4)+
#   scale_color_manual(values = c(pal_regions,cols))
# 
# 
# n_events = ggplot(dat,aes(x=dry_date_start,y = n)) +
#   geom_bin2d(binwidth = c(5,7)) +
#   # ylim(0,365) +
#   xlab("Dry Start Date (day)")+
#   ylab("Number of Events") +
#   scale_fill_viridis(limit=c(0,120))
# # scale_fill_gradientn(trans = 'log',labels=trans_format("identity", function(x) round(x,2)),limits = c(1,300),colors = viridis(10),breaks =c(1,10,50,100,200))
# 
# n_events = n_events + 
#   geom_point(data=region.mean,aes(x=mean.dry.date,y=mean.n_events,group=paste(Name,CLASS),shape = factor(CLASS)),colour="white",size = 5, alpha=.25)
# n_events = n_events+geom_point(data=region.mean,aes(x=mean.dry.date,y=mean.n_events,group=paste(Name,CLASS),color=Name,shape = factor(CLASS)),size = 4, alpha=.25)+
#   scale_color_manual(values = pal_regions)
# 
# binned  = drying_rate + peak2zero   + dry_dur + plot_layout(ncol = 3, guides = "collect") & theme(legend.position = 'right') & ggtitle("k-means")
# 
# binned
