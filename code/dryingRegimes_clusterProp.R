#####################################################################
##
## Script name: dryingMetrics_clusterProp.r
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
############################# Packages #############################

library(ggplot2)
library(tidyverse)

############################# Code ################################


clust = read.csv('data/kmeans.csv')
clust$gage = as.numeric(clust$gage)


tt = clust %>% 
  select(gage,kmeans) %>% 
  group_by(gage,kmeans)%>% 
  unique() %>%
  mutate(n = seq(1)) %>%
  spread(.,key='kmeans',
         value = 'n')


tt[is.na(tt)]=0
tt =as.data.frame(tt)
tt$c = rowSums(tt[,-1])
colnames(tt) = c('gage','c1','c2','c3','c4','c')

a = tt[tt$c>0,'gage']
a = as.data.frame(a)
colnames(a) = 'gage'
p = a %>% inner_join(.,clust,by='gage')


# Some notes: 79 gages only have 1 event in these clusters
# Cluster 1: 0
# Cluster 2: 1
# Cluster 3: 71
# Cluster 4: 7
# 
# 
# 
# Ratio of unique gages per cluster to total gages
# Cluster 1: 157/894
# Cluster 2: 251/894
# Cluster 3: 841/894
# Cluster 4: 669/894
# 
# 
# 
# -222 (24.8%) gages belong to 1 unique cluster. Of those the breakdown of cluster membership is as follows
# Cluster 1:0
# Cluster 2:1 (This is also the same gage that only has 1 event in c2)
# Cluster 3: 204
# Cluster 4: 17
# which makes up 925/25207 events
# (3.7%)
# 
# -386 gages belong to at least 2 clusters
# which makes up 6973/25207 events
# (27.7%)
# 
# -220 gages belong to at least 3 clusters
# 11415/25207 
# (45.3%)
# 
# -66 gages belong to all 4 clusters
# 5894/25207
# (23.4%)

