library(tidyverse)
library(ggplot2)
library(lubridate)
library(Hmisc)

############################# Code ################################

## Load data

clust = read.csv('data/kmeans.csv')

find.event <- function(df,cluster,ci){
  cit = clust[clust$kmeans==cluster,]  %>% 
    select(peak_date,peak_quantile,peak2zero,drying_rate,dry_date_start,dry_dur,freq_local) %>%
    summarise_all(smean.cl.boot)
  
  out = df[df$kmeans==cluster,] %>%
    select(peak_date,peak_quantile,peak2zero,drying_rate,dry_date_start,dry_dur,freq_local) %>%
    mutate(peak_quantile = abs(peak_quantile-cit$peak_quantile[[ci]]),
           peak2zero = abs(peak2zero-cit$peak2zero[[ci]]),
           drying_rate = abs(drying_rate-cit$drying_rate[[ci]]),
           dry_dur = abs(dry_dur-cit$dry_dur[[ci]]),
           freq_local = abs(freq_local-cit$freq_local[[ci]])) %>%
    scale()
  ind = which(rowSums(out)==min(rowSums(out)))
  out = df[df$kmeans==cluster,]
  tt = out[ind,]
  return(out)
}

out=as.data.frame(NA)
for (j in 1:4){
  print(j)
  for (i in 1:3){
    evt = find.event(df=clust,
           cluster = j,
           ci=i)
    out = cbind(out,t(evt))
  }
}

out = t(out)






