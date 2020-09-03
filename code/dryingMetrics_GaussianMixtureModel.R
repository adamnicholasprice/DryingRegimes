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
library(rwrfhydro)
library(ggfortify)
library(plotly)
library(ClusterR)
library(cluster)
library(factoextra)


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


############# PAM clustering ##################
# fviz_nbclust(dat.scale, pam, method = "silhouette") + theme_classic()
# pam.res <- eclust(dat.scale, "pam", k = 4, hc_metric="euclidean")
# 
# fviz_cluster(pam.res, geom = "point", ellipse.type = "norm", ggtheme = theme_minimal())
# 
################ Gaussian mixture model ################

## https://rdrr.io/cran/ClusterR/man/predict_GMM.html 
# http://mlampros.github.io/2016/09/12/clusterR_package/


opt_gmm = Optimal_Clusters_GMM(dat.metrics[,-1], max_clusters = 10, criterion = "BIC", 
                               
                               dist_mode = "maha_dist", seed_mode = "random_subset",
                               
                               km_iter = 10, em_iter = 10, var_floor = 1e-10, 
                               
                               plot_data = T)

dat2 = center_scale(dat.metrics[,-1], mean_center = T, sd_scale = T)  # centering and scaling the data

gmm = GMM(dat2, 4, dist_mode = "maha_dist", seed_mode = "random_subset", km_iter = 10,
          
          em_iter = 10, verbose = F)          

pr = predict_GMM(dat2, gmm$centroids, gmm$covariance_matrices, gmm$weights) 


# mb = Mclust(dry_mean[,-1])
# mb$G\
# 
# summary(mb)
# 
# plot(mb,what=c("classification"))
# 
# mb
# mb$classification
######### Merge Data ###########################
# PCA_df = as.data.frame(PCA$x)
# g2 = gages2Attr %>% mutate_at(vars(STAID),as.numeric)
# 
# dat = left_join(dat.mean,y = g2,by = c("dat.mean$gage"="STAID")) %>% cbind(.,mb$classification,PCA_df)


######## Plots ##########
# Color scale
cols = RColorBrewer::brewer.pal(n = length(unique(t$`mb$classification`)),"Set2")

cols <- 
  c("1" = "#cf597e",
    "2" = "#eeb479",
    "3" = "#e9e29c",
    "4" = "#39b185",
    "5" = "#009392")


# Cluster plot

## PCA Plot
autoplot(PCA,colour = as.factor(pr$cluster_labels+1),scale = 0,loadings=T)

ggplot(PCA,aes(x=PC1,y=PC2, colour = as.factor(pr$cluster_labels)))+
  geom_point()+
  stat_ellipse()

  # scale_color_manual(values = cols)


##################### Agglomerative Hierarchical Clustering

dat.scale = scale(dat.metrics[,-1])

# d <- dist(dat.scale, method = "euclidean")
# 
# hc1 <- hclust(d, method = "complete" )
# 
# # Plot the obtained dendrogram
# plot(hc1, cex = 0.6, hang = -1)

m <- c( "average", "single", "complete", "ward")
names(m) <- c( "average", "single", "complete", "ward")

# function to compute coefficient
ac <- function(x) {
  agnes(dat.scale, method = x)$ac
}

map_dbl(m, ac)

##################### Density Based Clustering #########################
library(fpc)
library(dbscan)
library(factoextra)

kNNdistplot(dat.scale,k = log(length(dat.scale)))

db = fpc::dbscan(dat.scale,eps=.5,MinPts = log(length(dat.scale)))


fviz_cluster(db,dat.scale, stand = FALSE, ellipse = FALSE, geom = "point",show.clust.cent = T,pointsize = .5)

tt = cbind(as.data.frame(dat.scale),array(db$cluster))

ggplot(PCA,aes(x=PC1,y=PC2, colour = as.factor(tt$`array(db$cluster)`)))+
  geom_point()+
  stat_ellipse()

# Stats plot

# CONUS plot
states <- map_data("state")

CONUS_CLUST <- ggplot(data = states) + 
  geom_polygon(aes(x = long, y = lat, group = group), fill = "gray", color = "black") + 
  coord_fixed(1.3, xlim = c(-124.25,-70), ylim = c(26,48.5)) +
  theme_linedraw() + 
  geom_point(data=t, aes(x=LNG_GAGE, y=LAT_GAGE, shape = factor(CLASS), colour = as.factor(mb$classification)), size = 2, alpha=1) 
  scale_color_manual(values = cols)

CONUS_CLUST


# ggplotly(CONUS_CLUST)





tt = cbind(dat,pr$cluster_labels)

counts = tt %>% group_by(gage,pr$cluster_labels) %>% count()
