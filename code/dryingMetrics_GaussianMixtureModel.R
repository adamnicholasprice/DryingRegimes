#####################################################################
##
## Script name: 
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


############################# Code ################################

### Load Data
dat = read.csv("../data/metrics_by_event_combined.csv")

dat$Name[dat$Name == "Ignore"] = "Mediterranean California" 
dat = dat[dat$peak_quantile>.25,]
# dat[dat$peak2zero>365,]
# # tt = dat[dat$dry_dur>1000,]

dat = dat %>% group_by(gage) %>% count() %>% left_join(dat,.,by="gage")

###################### Analysis #########################

dat.metrics = dat %>% select(gage,peak2zero,drying_rate,dry_date_start,n)
# dat.metrics$dry_date_start = sin(2*pi*(dat.metrics$dry_date_start/365))
dat.scale = scale(dat.metrics[,-1])

dat.scale <- df %>% 
  #Select vars of interest
  select("peak2zero","drying_rate", 
         "dry_date_start", "dry_dur",
         "peak_quantile", "sev") %>% 
  #scale vars
  scale()


############## PCA ############
PCA = prcomp(dat.scale)
autoplot(PCA,loadings=T,loadings.label=T)



# Visualize eigenvalues/variances
fviz_screeplot(PCA, addlabels = TRUE, ylim = c(0, 50))

################### K-Means ######################
fviz_nbclust(dat.scale, kmeans, method = "silhouette") + theme_classic()

wcke<-eclust(dat.scale, "kmeans", hc_metric="euclidean",k=4)
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


gmm = GMM(dat2, 4, dist_mode = "maha_dist", seed_mode = "random_subset", km_iter = 10,
          
          em_iter = 10, verbose = F)          

pr = predict_GMM(dat2, gmm$centroids, gmm$covariance_matrices, gmm$weights) 

###################### Hierch Cluster ######################

# Create distance matrix with scaled vars
df$sev = df$freq_local/df$n

d <- df %>% 
  #Select vars of interest
  select("peak2zero","drying_rate", 
         "dry_date_start", "dry_dur",
         "peak_quantile", "sev") %>% 
  #scale vars
  scale() %>% 
  #Create distance matrix
  vegdist(., method = 'euclidean')

#Use heirchal clustering
fit <- hclust(d, method = "ward")

#After visual inspection, select where to cut the tree
df<- df %>% 
  mutate(
    clust_4 = cutree(fit, k=4))


##################### Density Based Clustering #########################
library(fpc)
library(dbscan)
library(factoextra)

kNNdistplot(dat.scale,k = log(length(dat.scale)))

db = fpc::dbscan(dat.scale,eps=.5,MinPts = log(length(dat.scale)))


fviz_cluster(db,dat.scale, stand = FALSE, ellipse = FALSE, geom = "point",show.clust.cent = T,pointsize = .5)


######### Merge Clustering Data ###########################

clust = df %>% select(gage,Name,CLASS,dec_lat_va,dec_long_va,clust_4)

clust = cbind(clust,pr$cluster_labels+1,wcke$cluster,db$cluster+1) 

colnames(clust) = c("gage","Name","CLASS","dec_lat_va","dec_long_va","hier.4.clust","gmm.clust","kmeans.clust","dbscan.cluster")
######## Plots ##########
# Color scale
cols <- 
  c("1" = "#cf597e",
    "2" = "#eeb479",
    "3" = "#e9e29c",
    "4" = "#39b185",
    "5" = "#009392")


# Cluster plot

## PCA Plot
autoplot(PCA,colour = as.factor(clust$hier.4.clust),scale = 0,loadings=T)
autoplot(PCA,colour = as.factor(clust$gmm.clust),scale = 0,loadings=T)
autoplot(PCA,colour = as.factor(clust$kmeans.clust),scale = 0,loadings=T)
autoplot(PCA,colour = as.factor(clust$dbscan.cluster),scale = 0,loadings=T)

ggplot(PCA,aes(x=PC1,y=PC2, colour = as.factor(pr$cluster_labels)))+
  geom_point()+
  stat_ellipse()

# Stats plot
dat.metrics = df %>% select("peak2zero","drying_rate", 
                            "dry_date_start", "dry_dur",
                            "peak_quantile", "sev")
ggpairs(dat.metrics,
        aes(color = as.factor(clust$hier.4.clust)))

ggpairs(dat.metrics,
        aes(color = as.factor(clust$gmm.clust)))

ggpairs(dat.metrics,
        aes(color = as.factor(clust$kmeans.clust)))

ggpairs(dat.metrics,
        aes(color = as.factor(clust$dbscan.clust)))



tt = boxplot(dat.metrics$peak2zero~clust$dbscan.clust, outline=F, ylab = "Peak to Zero [days]", xlab=NULL)



# CONUS plot
states <- map_data("state")

CONUS_CLUST <- ggplot(data = states) + 
  geom_polygon(aes(x = long, y = lat, group = group), fill = "gray", color = "black") + 
  coord_fixed(1.3, xlim = c(-124.25,-70), ylim = c(26,48.5)) +
  theme_linedraw() + 
  geom_point(data=clust, aes(x=dec_long_va, y=dec_lat_va, shape = factor(CLASS), colour = kmeans.clust), size = 2, alpha=1) +
  scale_color_viridis_c(breaks=c(0,1,2,3,4,5,6,7,8))

ggplotly(CONUS_CLUST)




tt = cbind(dat,pr$cluster_labels)

counts = tt %>% group_by(gage,pr$cluster_labels) %>% count()
