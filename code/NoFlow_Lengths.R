######################################################
# Author: Adam N. Price
#
# Description: This piece of code reads in stream flow data from a file, 
# counts the number of repititions of values below a certain threshold and length of occurance, 
# and replaces them with a 1 = flow ,  0 = No-flow
#
######################################################
library(data.table)
library(here)
library(viridis)


files = list.files(here("data/reference/"),full.names = TRUE,pattern = '*csv')

metric = 'Q_cfs'
threshold = 0.01
nfPeriods = c(3,5,7,14,30,60,90,180)

############## Functions #########################
#
#
#
#
######################################################

writeNF_feather <-function(files){
  
  ap_nf_length <- function(files,metric,threshold,nflength){
    dat = fread(files,sep = ",", select = c(metric))
    dat[which(dat<=threshold)]=-999
    nf_period = rle(dat[[metric]])
    
    nf_len = rep(nf_period$lengths, nf_period$lengths)
    nf_len[which(nf_len < nflength)] = 1
    nf_len[which(nf_len >=nflength)] = 0
    
    return(nf_len)
  }
  

  root = strsplit(files,'/')[[1]][10]
  metric = 'Q_cfs'
  threshold = 0.01
  nfPeriods = c(3,5,7,14,30,60,90,180)
  
  nf_len = list()
  for (i in 1:length(nfPeriods)){
    nf_len[[i]] = ap_nf_length(files,metric,threshold,nfPeriods[i])
  }
  
  names(nf_len) = paste0("NF",nfPeriods)
  write.csv(cbind(read.csv(files),nf_len),paste0('data/reference/nfdata/',root))
  
  rm(nf_len,root)
  
}

######################## Write reference feather in parallel ####################

## Get list of files
files = list.files(here::here("data/reference/"),full.names = TRUE,pattern = '*csv')

# get number of cores, start cluster and load packages on cores.
cores = parallel::detectCores()-2
cl <- parallel::makeCluster(cores)
parallel::clusterEvalQ(cl,.libPaths())
parallel::clusterEvalQ(cl,
                       {library(here)
                         library(feather)
                         library(data.table)
                       })

# Use mpapply to exicute function
parallel::parLapply(cl,files,writeNF_feather)


# Stop the cluster
parallel::stopCluster(cl)

