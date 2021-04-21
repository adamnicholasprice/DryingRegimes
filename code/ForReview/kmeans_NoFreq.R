#####################################################################
##
## Script name:
##
## Author: Adam N. Price
##
## Date Created: 2021-04-20
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
library(NbClust)
library(tidyverse)
library(parallel)
library(vegan)
library(clValid)
library(ggplot2)
library(cluster)
library(factoextra)
############################# Code ################################


#####################################################################
######### Load Data #################################################
#####################################################################


df = read.csv('data/dryingRegimes_data_RF.csv')


# df = df %>% group_by(gage) %>% count() %>% left_join(df,.,by="gage")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Step 3: Select and scale variables -------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
dat.scale <- df %>% 
  #Select vars of interest
  select("peak2zero","drying_rate"
         , "dry_dur",
         "peak_quantile") %>%
  mutate(peak2zero = (peak2zero-mean(peak2zero))/sd(peak2zero),
         drying_rate = (drying_rate-mean(drying_rate))/sd(drying_rate),
         dry_dur = (dry_dur-mean(dry_dur))/sd(dry_dur),
         peak_quantile = (peak_quantile-mean(peak_quantile))/sd(peak_quantile))


#####################################################################
######### NbClust ###################################################
#####################################################################

for (i in 1){
  set.seed(11)
  samp = sample(1:nrow(df),1000)
  # sub = as.matrix(dat.scale[samp,])
  # 
  # rownames(sub) = df$gage[samp]
  
  
  ideal_ward2 <- NbClust(data = dat.scale[samp,], method = "kmeans")
  # factoextra::fviz_nbclust(ideal_ward2) + theme_minimal() + ggtitle("NbClust's optimal number of clusters")
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Other popular clustering indecies
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Elbow method
fviz_nbclust(dat.scale, kmeans, method = "wss") +
  geom_vline(xintercept = 4, linetype = 2)+
  labs(subtitle = "Elbow method")
# Silhouette method
fviz_nbclust(dat.scale, kmeans, method = "silhouette")+
  labs(subtitle = "Silhouette method")
# Gap statistic
# nboot = 50 to keep the function speedy. 
# recommended value: nboot= 500 for your analysis.
# Use verbose = FALSE to hide computing progression.
set.seed(123)
fviz_nbclust(dat.scale, kmeans, nstart = 25,  method = "gap_stat", nboot = 50)+
  labs(subtitle = "Gap statistic method")


#####################################################################
#################### K-means clustering #############################
#####################################################################

wcke<-eclust(dat.scale, "kmeans", hc_metric="euclidean",k=4)
fviz_gap_stat(wcke$gap_stat)
fviz_cluster(wcke, geom = "point", ellipse.type = "norm", ggtheme = theme_minimal())



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# PCA -------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
tt = prcomp(dat.scale,center=T,scale=T)
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

comps = as.data.frame(tt$x)
loads =  data.frame(Variables = rownames(tt$rotation), tt$rotation)


newdf <- cbind(wcke$cluster,comps[,c(1,2)]) %>%
  setNames(c("cluster","PC1","PC2"))

p = ggplot() +
  geom_point(data = newdf, aes(x=PC1, y=PC2, fill = factor(cluster)),shape=21, col="black")+
  scale_fill_manual(values = cols)

p = p + geom_segment(data = loads, 
                     aes(x=0,y=0,xend=PC1*20,yend=PC2*20),
                     arrow = arrow(length = unit(1/2, "picas")))+ 
  annotate("text", x = (loads$PC1*15), y = (loads$PC2*15),
           label = loads$Variables)+
  theme_minimal()


pdf("docs/response_plots/PCA.pdf")
p
dev.off()


