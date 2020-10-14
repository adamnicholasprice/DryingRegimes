library(NbClust)
library(tidyverse)
library(parallel)
library(vegan)
library(clValid)
library(ggplot2)
library(cluster)
library(factoextra)



#####################################################################
######### Load Data #################################################
#####################################################################


df = read.csv('../data/metrics_by_event_combined_raw.csv')

df$Name[df$Name == "Ignore"] = "Mediterranean California" 
df = df[df$peak_quantile>.25 & df$drying_rate>0,]
#Rename event_id  (Somethign is weird here...)
df<-df %>% 
  mutate(event_id = seq(1, nrow(df)))

df = df %>% group_by(gage) %>% count() %>% left_join(df,.,by="gage")


#####################################################################
########## Estimate events per year #################################
#####################################################################
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

stopCluster(cl)

#add results to df
output<-bind_rows(output)
df<-left_join(df,output)



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Step 3: Select and scale variables -------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
dat.scale <- df %>% 
  #Select vars of interest
  select("peak2zero","drying_rate"
         , "dry_dur",
         "peak_quantile", "freq_local") %>% 
  #scale vars
  scale()

#####################################################################
######### NbClust ###################################################
#####################################################################

for (i in 1:2){
set.seed(i)
samp = sample(1:nrow(df),1000)
sub = as.matrix(dat.scale[samp,])

rownames(sub) = df$gage[samp]


ideal_ward2 <- NbClust(data = sub, method = "ward.D2")
factoextra::fviz_nbclust(ideal_ward2) + theme_minimal() + ggtitle("NbClust's optimal number of clusters")
}


#####################################################################
#################### K-means clustering #############################
#####################################################################

fviz_nbclust(dat.scale, kmeans, method = "silhouette") + theme_classic()

wcke<-eclust(dat.scale, "kmeans", hc_metric="euclidean",k=4)
fviz_gap_stat(wcke$gap_stat)
fviz_cluster(wcke, geom = "point", ellipse.type = "norm", ggtheme = theme_minimal())





#####################################################################
#########Multiple Responce Permutation Procedure#####################
#####################################################################
#Create dataframe containing data for MRPP
n.cores <- detectCores() - 1
cl <- makeCluster(n.cores)

df = na.omit(df)

mrpp.2group<-mrpp(dat.scale, wcke$cluster, permutations = 5,distance = "euclidean",parallel  = getOption("n.cores"))

remove(cl)


#display the p-values and A of both tests
mrpp.2group$Pvalue

mrpp.2group$A

mrpp.2group$boot.deltas


# Export cluster data
df$kmeans = wcke$cluster

write.csv(df,'../data/kmeans.csv')

