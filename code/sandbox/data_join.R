all = read.csv("data/kmeans.csv")

clust = read.csv("data/dryingRegimes_data_RF.csv")

tt= all %>%  select(gage,event_id,peak_date,peak_value,peak_quantile,
                peak2zero,drying_rate,p_value,calendar_year,season,
                meteorologic_year,dry_date_start,dry_date_mean,dry_dur,Name,TOPWET) %>%
  left_join(.,clust,by=c('gage','TOPWET'))

sub

pred = read.csv("data/all_rf_data.csv")

abs(pred$lulc_forest_prc) - abs(sub$lulc_forest_prc)


tt = all %>% inner_join(.,fit_data_i,by="event_id")
