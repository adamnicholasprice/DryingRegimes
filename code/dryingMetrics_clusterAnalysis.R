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


df = read.csv('data/metrics_by_event_combined_raw.csv')

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

df_sub <- df %>% 
  #Select vars of interest
  select("peak2zero","drying_rate"
         , "dry_dur",
         "peak_quantile", "freq_local")
#####################################################################
######### NbClust ###################################################
#####################################################################

for (i in 1){
set.seed(110)
samp = sample(1:nrow(df),1000)
sub = as.matrix(dat.scale[samp,])

rownames(sub) = df$gage[samp]


ideal_ward2 <- NbClust(data = sub, method = "kmeans")
# factoextra::fviz_nbclust(ideal_ward2) + theme_minimal() + ggtitle("NbClust's optimal number of clusters")
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Other popular clsutering indecies
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Elbow method
fviz_nbclust(dat.scale, kmeans, method = "wss") +
  geom_vline(xintercept = 4, linetype = 2)+
  labs(subtitle = "Elbow method")
# Silhouette method
fviz_nbclust(df, kmeans, method = "silhouette")+
  labs(subtitle = "Silhouette method")
# Gap statistic
# nboot = 50 to keep the function speedy. 
# recommended value: nboot= 500 for your analysis.
# Use verbose = FALSE to hide computing progression.
set.seed(123)
fviz_nbclust(df, kmeans, nstart = 25,  method = "gap_stat", nboot = 50)+
  labs(subtitle = "Gap statistic method")


#####################################################################
#################### K-means clustering #############################
#####################################################################

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

# Plot

tt = prcomp(df_sub,center=T,scale=T)
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


pdf("docs/PCA.pdf")
p
dev.off()
