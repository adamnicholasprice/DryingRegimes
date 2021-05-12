#####################################################################
##
## Script name:
##
## Author: Adam N. Price
##
## Date Created: 2021-04-30
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
##
library(tidyverse)
##   
##
############################# Code ################################

df = read.csv("data/kmeans_NoFreq.csv")


df %>% group_by(kmeans) %>% count()
# 1  4428
# 2     2  1127
# 3     3  9878
# 4     4  9774

df %>% group_by(kmeans,gage)%>% unique() %>% count()

c1 = df[df$kmeans==1,'gage']
length(unique(c1))
#437 
c2 = df[df$kmeans==2,'gage']
length(unique(c2))
#246
c3 = df[df$kmeans==3,'gage']
length(unique(c3))
#812
c4 = df[df$kmeans==4,'gage']
length(unique(c4))
#668

tt = df %>% select(gage,kmeans) %>% 
  unique() %>% 
  mutate(k.ones = 1) %>% 
  pivot_wider(names_from = kmeans,values_from = k.ones,values_fill=0) %>%
  setNames(c("gage","c1",'c2','c3','c4')) %>%
  rowwise(gage)%>%
  mutate(k.sums = sum(c(c1,c2,c3,c4)))


k.sums = tt$k.sums

length(k.sums[k.sums==1])
# [1] 205
length(k.sums[k.sums==2])
# [1] 246
length(k.sums[k.sums==3])
# [1] 306
length(k.sums[k.sums==4])
# [1] 137


#########  Stats
cv <- function(x){
  mean(x)/sd(x)
}


c1 = df[df$kmeans==1,]%>%
  select(peak2zero,drying_rate,dry_dur,peak_quantile,dry_date_start)%>%
  summarise_all(c(mean,cv))
#   peak2zero_fn1 drying_rate_fn1 dry_dur_fn1 peak_quantile_fn1 dry_date_start_fn1 peak2zero_fn2 drying_rate_fn2 dry_dur_fn2 peak_quantile_fn2 dry_date_start_fn2
# 1      10.32091        1.453408    18.66057         0.8547657            209.227      1.264945        2.876609   0.6277466          5.623965           2.518482

c2 = df[df$kmeans==2,]%>%
  select(peak2zero,drying_rate,dry_dur,peak_quantile,dry_date_start)%>%
  summarise_all(c(mean,cv))
# peak2zero_fn1 drying_rate_fn1 dry_dur_fn1 peak_quantile_fn1 dry_date_start_fn1 peak2zero_fn2 drying_rate_fn2 dry_dur_fn2 peak_quantile_fn2 dry_date_start_fn2
# 1      27.03106       0.5747709    298.8066         0.7792343           184.6043     0.5385429        1.520157    1.423991          4.039784           2.634493

c3  = df[df$kmeans==3,]%>%
  select(peak2zero,drying_rate,dry_dur,peak_quantile,dry_date_start)%>%
  summarise_all(c(mean,cv))
# peak2zero_fn1 drying_rate_fn1 dry_dur_fn1 peak_quantile_fn1 dry_date_start_fn1 peak2zero_fn2 drying_rate_fn2 dry_dur_fn2 peak_quantile_fn2 dry_date_start_fn2
# 1      29.21128       0.4873903    26.60468         0.4301595           207.0104     0.8490997        1.663513   0.7604751          3.506946           2.741581

c4 = df[df$kmeans==4,]%>%
  select(peak2zero,drying_rate,dry_dur,peak_quantile,dry_date_start)%>%
  summarise_all(c(mean,cv))
# peak2zero_fn1 drying_rate_fn1 dry_dur_fn1 peak_quantile_fn1 dry_date_start_fn1 peak2zero_fn2 drying_rate_fn2 dry_dur_fn2 peak_quantile_fn2 dry_date_start_fn2
# 1      14.42183       0.6463607    23.49192         0.8453678           192.8761     0.9758967        2.370853   0.7234324          7.398362           2.241442


all = rbind(c1,c2,c3,c4)

write.csv(all,'data/clusterStats.csv')

