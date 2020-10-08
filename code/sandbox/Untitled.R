library(zoo)

yy = tt %>% group_by(dry_date_start,kmeans.clust)%>%
  mutate(mov.avg = zoo::rollmean(percentage,5,na.pad = T))

pp = tt

pp$k.clust = paste0("kmeans.",pp$kmeans.clust)

pp$kmeans.clust = NULL

yy = pp %>% 
  select(dry_date_start,k.clust,percentage) %>% 
  spread(k.clust,percentage)

kk = yy%>%select(dry_date_start,s=kmeans.1) %>% mutate(mov.avg = zoo::rollmean(s,k=2,fill=NA,align="right"))
