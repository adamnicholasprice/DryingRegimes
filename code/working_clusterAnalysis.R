library(NbClust)
library(tidyverse)
library(parallel)
library(vegan)
library(clValid)

df = read.csv('../data/clustering_results_allevents.csv')


dat.scale <- df %>% 
  #Select vars of interest
  select("peak2zero","drying_rate"
         , "dry_dur",
         "peak_quantile", "rel_freq") %>% 
  #scale vars
  scale()

#####################################################################
######### NbClust ###################################################
#####################################################################
for (i in 1:10){
set.seed(i)
samp = sample(1:nrow(df),1000)
sub = as.matrix(dat.scale[samp,])

rownames(sub) = df$gage[samp]


intern <- clValid(sub, nClust = 2:15, 
                  clMethods = c("hierarchical", "kmeans"), validation = "internal",)



y
summary(intern)
}

# ideal_ward2 <- NbClust(data = dat.scale, method = "ward.D2")

# factoextra::fviz_nbclust(ideal_kmeans) + theme_minimal() + ggtitle("NbClust's optimal number of clusters")

#####################################################################
#########Multiple Responce Permutation Procedure#####################
#####################################################################
#Create dataframe containing data for MRPP
n.cores <- detectCores() - 1
cl <- makeCluster(n.cores)

mrpp.2group<-mrpp(dat.scale, df$hier.4.clust, permutations = 5,distance = "euclidean",parallel  = getOption("n.cores"))

remove(cl)


#display the p-values and A of both tests
mrpp.2group$Pvalue

mrpp.2group$A
