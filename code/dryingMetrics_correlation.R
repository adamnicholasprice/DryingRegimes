#####################################################################
##
## Script name: 
##
## Author: Adam N. Price
##
## Date Created: 2020-08-13
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

library(tidyverse)
library(ggplot2)
library(patchwork)
library(reshape2)
library(viridis)

############################# Code ################################

dat = read.csv("../data/metrics_by_event.csv")


## Get Mean values for the data and event counts
dat.mean = dat %>% group_by(gage) %>% summarise_all(funs(mean),na.rm=TRUE)
counts = dat %>% group_by(gage) %>% count()
metric.index = c(5,6,7,8,9,10)
metric.index.gage = c(1,5,6,7,8,9,10)

dat.mean = dat.mean[,metric.index.gage] %>% inner_join(.,counts,by="gage")


### Quick correlation plot


get_lower_tri<-function(cormat){
  cormat[upper.tri(cormat)] <- NA
  return(cormat)
}
# Get upper triangle of the correlation matrix
get_upper_tri <- function(cormat){
  cormat[lower.tri(cormat)]<- NA
  return(cormat)
}

cor.mat = round(cor(dat.mean[,-1],use="complete.obs"),2)

cor.mat = melt(get_lower_tri(cor.mat))

cor.plt <- ggplot(data = cor.mat, aes(x=Var1, y=Var2, fill=value)) + 
                    geom_tile()+
                    scale_fill_viridis(option = 'C')+
                    geom_text(aes(Var2, Var1, label = value), color = "black", size = 4)


cor.plt


############################################################################


dat.mean = dat %>% group_by(gage) %>% summarise_all(funs(mean),na.rm=TRUE)
counts = dat %>% group_by(gage) %>% count()
metric.index.gage = c(1,5,6,7,8,10)


dat.mean = dat.mean[,metric.index.gage] %>% inner_join(.,counts,by="gage") %>% na.omit()
dat.mean$dry_date_mean <- NULL


dat.mean

get_lower_tri<-function(cormat){
  cormat[upper.tri(cormat)] <- NA
  return(cormat)
}
# Get upper triangle of the correlation matrix
get_upper_tri <- function(cormat){
  cormat[lower.tri(cormat)]<- NA
  return(cormat)
}

cormat <- round(cor(dat.mean[,-1]),2)

upper_tri <- get_upper_tri(cormat)

library(reshape2)
melted_cormat <- melt(upper_tri, na.rm = TRUE)
# Heatmap
library(ggplot2)
ggheatmap <- ggplot(melted_cormat, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal()+ # minimal theme
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1))+
  coord_fixed()

ggheatmap + 
  geom_text(aes(Var2, Var1, label = value), color = "black", size = 4) +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    axis.ticks = element_blank(),
    legend.justification = c(1, 0),
    legend.position = c(0.6, 0.7),
    legend.direction = "horizontal")+
  guides(fill = guide_colorbar(barwidth = 7, barheight = 1,
                               title.position = "top", title.hjust = 0.5))
