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


############################# Code ################################

# dat = read.csv('../data/annual_no_flow_and_climate_metrics_climatic_year_050820.csv')
# 
# sites = unique(dat$sitewith0)
# 
# # Load ecoregion data
# 
# region_dat = read.csv('../data/mean_annual_no_flow_climate_watershed_EPA1_032920.csv') 
# 
# region = region_dat %>% select(gage_ID,Aggregated_region,Class,lat_cent,long_cent)

# Load drying data


dat = read.csv("../data/metrics_by_event.csv") %>% na.omit()


## Get Mean values for the data and event counts
dat.mean = dat %>% group_by(gage) %>% summarise_all(funs(mean),na.rm=TRUE)
counts = dat %>% group_by(gage) %>% count()
metric.index = c(1,5,6,7,8,10)
metric.index.gage = c(1,5,6,7,8,10)

dat.mean = dat.mean[,metric.index.gage] %>% inner_join(.,counts,by="gage") %>% na.omit()
dat.mean$dry_date_mean <- NULL


## omit NA
dat = dat[,metric.index]

###################### Analysis #########################

# PCA
PCA = prcomp(dat.mean[,-1],scale.=TRUE)
autoplot(PCA,loadings=T,loadings.label=T)


dat.scale = scale(dat[,-1])
dat2 = dat[,-1]
PCA = prcomp(dat)
autoplot(PCA,loadings=T,loadings.label=T)


res.pca <- prcomp(dat.scale,  graph = FALSE)
# Visualize eigenvalues/variances
fviz_screeplot(res.pca, addlabels = TRUE, ylim = c(0, 50))
screeplot(res.pca)


#Gaussian mixture model

## https://rdrr.io/cran/ClusterR/man/predict_GMM.html 
# http://mlampros.github.io/2016/09/12/clusterR_package/


opt_gmm = Optimal_Clusters_GMM(dat[,-2], max_clusters = 10, criterion = "BIC", 
                               
                               dist_mode = "maha_dist", seed_mode = "random_subset",
                               
                               km_iter = 10, em_iter = 10, var_floor = 1e-10, 
                               
                               plot_data = T)

dat2 = center_scale(dat[,-2], mean_center = T, sd_scale = T)  # centering and scaling the data

gmm = GMM(dat2, 6, dist_mode = "maha_dist", seed_mode = "random_subset", km_iter = 10,
          
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

dat.scale = scale(dat[,-1])

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
