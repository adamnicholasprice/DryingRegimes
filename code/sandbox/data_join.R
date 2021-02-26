all = read.csv("data/kmeans.csv")


tt= all %>%  select(gage,X,dec_lat_va,dec_long_va,peak_date,peak_value,peak_quantile,
                peak2zero,drying_rate,p_value,calendar_year,season,
                meteorologic_year,dry_date_start,dry_date_mean,dry_dur,Name,TOPWET)
colnames(tt) = c('gage','event_id','dec_lat_va','dec_long_va','peak_date','peak_value','peak_quantile',
                      'peak2zero','drying_rate','p_value','calendar_year','season',
                      'meteorologic_year','dry_date_start','dry_date_mean','dry_dur','AggEcoregion','TOPWET')

pred = read.csv("data/all_rf_data.csv") %>% select(X,gage,.pred_class)

### load sub from RF

data= tt %>% 
  left_join(.,sub,by=c("event_id"="X","gage")) %>% 
  left_join(.,pred,by=c("event_id"="X","gage"))

write.csv(data,"data/dryingRegimes_data_RF.csv")
