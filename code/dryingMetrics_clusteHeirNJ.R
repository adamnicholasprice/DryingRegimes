#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Author: Nate Jones
# Tilte: Multivariate Analysis
# Date: 9/2/2020
# Description:  Examine drying metrics in ordination space and with clustering
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Next Steps --
#    (3) Computer MRPP on clustered groups
#   (4) plot by region

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
freq_fun<-function(n){
  
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
output<-parLapply(cl, seq(1,nrow(df)), freq_fun)
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
rect.hclust(fit, k=3, border=c("#e41a1c","#377eb8", "#4daf4a"))

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

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Step 4: Space-time plots -------------------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Create function to export tibble with gage, jday, drying type 
dur_fun<-function(n){
  
  #Load libraries of interest
  library(tidyverse)
  
  #Isolate event of interest
  m<-df[n,]
  
  #Select cols of interest
  m<-m %>% select(gage, peak_date, peak2zero, dry_dur, clust_3)
  
  #Create output tibble
  output<-tibble(
    gage = m$gage,
    jday = seq(from = m$peak_date, by = 1, to = (m$peak_date + m$peak2zero + m$dry_dur)),
    type = m$clust_3
  )
  
  #Deal with jday>365
  output<-output %>% 
    mutate(jday = if_else(jday>365, jday%%365, jday))
  
  #Export
  return(output)
}

#Run function
n.cores <- detectCores() - 1
cl <- makeCluster(n.cores)
clusterExport(cl, "df")
output<-parLapply(cl, seq(1,nrow(df)), dur_fun)
remove(cl)

#add results to df
dur<-bind_rows(output)

#Summarise by jday and type
dur<-dur %>% 
  group_by(jday, type) %>% 
  summarise(n = n()) %>% 
  pivot_wider(values_from = n, names_from = type)

#Convert to prop
dur<-dur %>% 
  mutate(
    tot = `1` + `2` +  `3`,
    Type_1   = `1`/tot, 
    Type_2   = `2`/tot,
    Type_3   = `3`/tot) %>% 
  select(jday, Type_1, Type_2, Type_3) %>% 
  pivot_longer(-jday)

#Plot
dur %>% 
  ggplot(aes(x = jday, y = value, fill = name)) +
    geom_area() + 
    scale_fill_manual(
      values = c("#e41a1c","#377eb8", "#4daf4a"), 
      name = "Drying Event Cluster"
    ) + 
  theme_bw() + 
  ylab('Proportion of Drying Events') +
  xlab('Julian Date') +
  #Axes Options
  theme(
    axis.title = element_text(size=14),
    axis.text  = element_text(size = 10)) +
  #Legend Options
  theme(legend.position = "bottom", 
        legend.title = element_text(size=14), 
        legend.text = element_text(size=10)) 
