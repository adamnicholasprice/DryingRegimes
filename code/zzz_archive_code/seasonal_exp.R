#####################################################################
##
## Script name: 
##
## Author: Adam N. Price
##
## Date Created: 2020-04-16
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
library(GGally)
library(lubridate)
library(tidyverse)

#install.packages('caret')
library(caret)
#install.packages('randomForest')
library(randomForest)
#install.packages('RcolorBrewer')
library(RColorBrewer)
#install.packages('rsample')
library(rsample)
#install.packages('foreign')
library(foreign)
#install.packages('rgdal')
library(rgdal)
#install.packages('Metrics')
library(Metrics)
#install.packages('plm')
library(plm)
library(viridis)
############################# Functions ################################
# Multiplot function
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

############################# Code ################################

data = read.csv('../data/meanannualnf_climate_peak2zero.csv')

p2z_data = read.csv('../data/p2z_seasonal.csv')[2:26]

reg_data = read.csv('../data/regression_Seasonal.csv')[2:29]

geo_data = read.csv('../data/mean_annual_no_flow_climate_watershed_EPA1_032920.csv')

# Subset data based on reference gages
temp = which(geo_data$Class == 'Ref')
sub = geo_data[temp,]

refs = sub$gage_ID

# Subset JH data
ref_data =  data.frame(matrix(NA, nrow = length(refs), ncol = ncol(data)))

for (i in seq(refs)){
ref_data[i,] = data[data$site==as.integer(refs[i]),]
}

colnames(ref_data) = colnames(data)

# Subset p2z data
p2z_df =  data.frame(matrix(NA, nrow = length(refs), ncol = ncol(p2z_data)))

for (i in seq(refs)){
  p2z_df[i,] = p2z_data[p2z_data$site_no==as.integer(refs[i]),]
}

colnames(p2z_df) = colnames(p2z_data)

# # Subset NJ data
# reg_df =  data.frame(matrix(NA, nrow = length(refs), ncol = ncol(reg_data)))
# 
# for (i in seq(refs)){
#   if (reg_data$site_no==refs[i],] == NA{
#     print('no way')
#   }
#   else{
#     reg_df = reg_data[reg_data$site_no==refs[i],]
#   }
# }



full_data = cbind(ref_data,p2z_df,sub)

coords = cbind(full_data$dec_long_va,full_data$dec_lat_va)
colnames(coords) = c('dec_long_va','dec_lat_va')

fd_nonTSmetrics = 

#Subset data by season
winter = grepl('djf', colnames(full_data))
w_ref = cbind(full_data$site,full_data[,winter],coords)

spring = grepl('mam', colnames(full_data))
s_ref = cbind(full_data$site,full_data[,spring],coords)

summer = grepl('jja', colnames(full_data))
su_ref = cbind(full_data$site,full_data[,summer],coords)

fall = grepl('son', colnames(full_data))
f_ref = cbind(full_data$site,full_data[,fall],coords)

# Remove duplicate columns
full_data = full_data[!duplicated(as.list(full_data))]



# Seasonal p2z length cv

summary(f_ref$p2z_cv_son)
mid <- mean(full_data$p2z_cv)
breaks = c(.1,.2,.5,1,2,4,8)

states <- map_data("state")

p1 = ggplot(data = states) + 
  geom_polygon(aes(x = long, y = lat, group = group), fill = "grey", color = "white") + 
  theme_linedraw() +
  geom_point(data=w_ref, aes(x=dec_long_va, y=dec_lat_va,  colour = p2z_cv_djf), size = 2, shape=20, alpha=1) +
  scale_color_viridis(breaks = breaks, limits = c(0,5))


p2 = ggplot(data = states) + 
  geom_polygon(aes(x = long, y = lat, group = group), fill = "grey", color = "white") + 
  theme_linedraw() +
  geom_point(data=s_ref, aes(x=dec_long_va, y=dec_lat_va,  colour = p2z_cv_mam), size = 2, shape=20, alpha=1) +
  scale_color_viridis(breaks = breaks, limits = c(0,5))

p3 = ggplot(data = states) + 
  geom_polygon(aes(x = long, y = lat, group = group), fill = "grey", color = "white") + 
  theme_linedraw() +
  geom_point(data=s_ref, aes(x=dec_long_va, y=dec_lat_va,  colour = p2z_cv_mam), size = 2, shape=20, alpha=1) +
  scale_color_viridis(breaks = breaks, limits = c(0,5))

p4 = ggplot(data = states) + 
  geom_polygon(aes(x = long, y = lat, group = group), fill = "grey", color = "white") + 
  theme_linedraw() +
  geom_point(data=f_ref, aes(x=dec_long_va, y=dec_lat_va,  colour = p2z_cv_son), size = 2, shape=20, alpha=1) +
  scale_color_viridis(breaks = breaks, limits = c(0,5))


multiplot(p1,p2,p3,p4)

# p2z max seasonal plots
s
summary(full_data$p2z_max_jja)
breaks = c(1,2,3,4,5,10,20,40,80,160)
breaks = c(1,50,100)
limits = c(1,100)


p5 = ggplot(data = states) + 
  geom_polygon(aes(x = long, y = lat, group = group), fill = "grey", color = "white") + 
  theme_linedraw() +
  geom_point(data=w_ref, aes(x=dec_long_va, y=dec_lat_va,  colour = p2z_max_djf), size = 2, shape=20, alpha=1) +
  scale_color_viridis(breaks = breaks, limits = limits)


p6 = ggplot(data = states) + 
  geom_polygon(aes(x = long, y = lat, group = group), fill = "grey", color = "white") + 
  theme_linedraw() +
  geom_point(data=s_ref, aes(x=dec_long_va, y=dec_lat_va,  colour = p2z_max_mam), size = 2, shape=20, alpha=1) +
  scale_color_viridis(breaks = breaks, limits = limits)

p7 = ggplot(data = states) + 
  geom_polygon(aes(x = long, y = lat, group = group), fill = "grey", color = "white") + 
  theme_linedraw() +
  geom_point(data=su_ref, aes(x=dec_long_va, y=dec_lat_va,  colour = p2z_max_jja), size = 2, shape=20, alpha=1) +
  scale_color_viridis(breaks = breaks, limits = limits)

p8 = ggplot(data = states) + 
  geom_polygon(aes(x = long, y = lat, group = group), fill = "grey", color = "white") + 
  theme_linedraw() +
  geom_point(data=f_ref, aes(x=dec_long_va, y=dec_lat_va,  colour = p2z_max_son), size = 2, shape=20, alpha=1) +
  scale_color_viridis(breaks = breaks, limits = limits)

multiplot(p5,p6,p7,p8)

# p2z minimum

summary(full_data$p2z_min_jja)
breaks = c(1,2,3,4,5,10,20,40,80,160)
limits = c(1,max(full_data$p2z_min_jja))

p9 = ggplot(data = states) + 
  geom_polygon(aes(x = long, y = lat, group = group), fill = "grey", color = "white") + 
  theme_linedraw() +
  geom_point(data=w_ref, aes(x=dec_long_va, y=dec_lat_va,  colour = p2z_min_djf), size = 2, shape=20, alpha=1) +
  scale_color_gradientn(colours = viridis(10),limit = c(1,10))


p10 = ggplot(data = states) + 
  geom_polygon(aes(x = long, y = lat, group = group), fill = "grey", color = "white") + 
  theme_linedraw() +
  geom_point(data=s_ref, aes(x=dec_long_va, y=dec_lat_va,  colour = p2z_min_mam), size = 2, shape=20, alpha=1) +
  scale_color_gradientn(colours = viridis(10),limit = c(1,10))

p11 = ggplot(data = states) + 
  geom_polygon(aes(x = long, y = lat, group = group), fill = "grey", color = "white") + 
  theme_linedraw() +
  geom_point(data=su_ref, aes(x=dec_long_va, y=dec_lat_va,  colour = p2z_min_jja), size = 2, shape=20, alpha=1) +
  scale_color_gradientn(colours = viridis(10),limit = c(1,10))

p12 = ggplot(data = states) + 
  geom_polygon(aes(x = long, y = lat, group = group), fill = "grey", color = "white") + 
  theme_linedraw() +
  geom_point(data=f_ref, aes(x=dec_long_va, y=dec_lat_va,  colour = p2z_min_son), size = 2, shape=20, alpha=1) +
  scale_color_gradientn(colours = viridis(10),limit = c(1,10))

multiplot(p9,p10,p11,p12)
