#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Author: Nate Jones
# Tilte: Multivariate Analysis
# Date: 9/2/2020
# Description:  Examine drying metrics in ordination space and with clustering
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Next Steps --
#   (1) Develop frequency metric for each event (i.e., how many other events occured in an atmospheric year)
#   (2) Redo Cluster (create fun to explore different groups of clusters)
#         #http://www.sthda.com/english/wiki/beautiful-dendrogram-visualizations-in-r-5-must-known-methods-unsupervised-machine-learning
#   (3) Computer MRPP on clustered groups
#   (4) Create plot over time that looks at some aspect of space-time dist of clustering

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Step 1: Setup workspace ------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Clear memory
remove(list=ls())

#Libraries (for Windows OS)
library(parallel)
library(clustsig)
library(vegan)
library(lubridate)
library(tidyverse)


#Load metrics into R env
df<-read_csv(paste0('./data/metrics_by_event.csv'))

#Filter
df<-df %>% 
  filter(p_value<0.1) %>% 
  filter(drying_rate>0)

#Rename event_id  (Somethign is weird here...)
df<-df %>% 
  mutate(event_id = seq(1, nrow(df)))

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

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Step 3: Cluster analysis -----------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Create distance matrix with scaled vars
d <- df %>% 
  #Select vars of interest
  select("peak2zero","drying_rate", 
         "dry_date_start", "dry_dur",
         "peak_quantile", "freq_local") %>% 
  #scale vars
  scale() %>% 
  #Create distance matrix
  vegdist(., method = 'euclidean')

#Use heirchal clustering
fit <- hclust(d, method = "ward")

#After visual inspection, select where to cut the tree
df<- df %>% 
  mutate(
    clust_3 = cutree(fit, k=3), 
    clust_6 = cutree(fit, k=6))

#Explore Cluster Analysis ------------------------------------------------------
plot(fit, 
     sub="Sampling Site", 
     hang=-0.5, 
     main = NULL,
     labels = FALSE,
     ylab="Height") 
title("Cluster Analysis: Ward's Mimium Variance (Euclidean Distance)", line = 3, cex =2)
title("3 Groups & 6 Groups", line = 2)
rect.hclust(fit, k=3)
rect.hclust(fit, k=6)

#Explore 3 group clustering-----------------------------------------------------
#Box plots
#Graphing parameters (cause I wanna be fancy)
par(mfrow=c(2,3))
par(mar = c(2,4,0,0)+0.25)
par(ps=12)
par(cex.lab=14/12)
par(cex.axis = 10/12)

#Box plots
# plot(fit, 
#      sub="Sampling Site", 
#      hang=-0.5, 
#      main = NULL,
#      labels = FALSE,
#      ylab="Height") 
# rect.hclust(fit, k=3, border= c("#e41a1c","#377eb8", "#4daf4a"))
boxplot(peak2zero~clust_3, col=c("#e41a1c","#377eb8", "#4daf4a"), data=df, outline=F, ylab = "Peak to Zero [days]", xlab=NULL)
boxplot(drying_rate~clust_3,col=c("#e41a1c","#377eb8", "#4daf4a"), data=df, outline=F, ylab = "Drying Rate [day^-1]", xlab=NULL)
boxplot(dry_date_start~clust_3,col=c("#e41a1c","#377eb8", "#4daf4a"), data=df, outline=F, ylab = "Drying Date [julian day]", xlab=NULL)
boxplot(dry_dur~clust_3, data=df,col=c("#e41a1c","#377eb8", "#4daf4a"), outline=F, ylab = "Dry Duration [days]", xlab=NULL)
boxplot(peak_quantile~clust_3, data=df, col=c("#e41a1c","#377eb8", "#4daf4a"),outline=F, ylab = "Peak Flow [%]", xlab=NULL)
boxplot(freq_local~clust_3, col=c("#e41a1c","#377eb8", "#4daf4a"), data=df, outline=F, ylab = "Drying Events", xlab=NULL)


#Explore 6 group clustering-----------------------------------------------------
#Box plots
#Graphing parameters (cause I wanna be fancy)
par(mfrow=c(2,3))
par(mar = c(2,4,0,0)+0.25)
par(ps=12)
par(cex.lab=14/12)
par(cex.axis = 10/12)

#Define cols
cols<-c("#1b9e77","#d95f02","#7570b3","#e7298a","#66a61e","#e6ab02")

#Box plots
plot(fit, 
     sub="Sampling Site", 
     hang=-0.5, 
     main = NULL,
     labels = FALSE,
     ylab="Height") 
rect.hclust(fit, k=6, border= cols)
boxplot(peak2zero~clust_6, col=cols, data=df, outline=F, ylab = "Peak to Zero [days]", xlab=NULL)
boxplot(drying_rate~clust_6,col=cols, data=df, outline=F, ylab = "Drying Rate [day^-1]", xlab=NULL)
boxplot(dry_date_start~clust_6,col=cols, data=df, outline=F, ylab = "Drying Date [julian day]", xlab=NULL)
boxplot(dry_dur~clust_6, data=df,col=cols, outline=F, ylab = "Dry Duration [days]", xlab=NULL)
boxplot(peak_quantile~clust_6, data=df, col=cols,outline=F, ylab = "Peak Flow [%]", xlab=NULL)
