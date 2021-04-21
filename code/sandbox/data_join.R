############################# Code ################################

clust = read.csv('data/kmeans.csv')
clust$gage = as.numeric(clust$gage)

dat = read.csv("data/metrics_by_event_combined.csv")
dat$gage = as.numeric(dat$gage)
dat$Name[dat$Name == "Ignore"] = "Mediterranean California" 
dat = dat[dat$peak_quantile>.25 & dat$drying_rate>=0,]

ant.cond = read.csv("data/metrics_by_event_withAntCondNoFlow.csv")
ant.cond = ant.cond[ant.cond$peak_quantile>.25 & ant.cond$drying_rate>=0,]
ant.cond$Date = as.Date(ant.cond$Date)

df = dat %>% left_join(.,ant.cond,by=c("gage","event_id"))

### Timeseries LULC
lulc = read.csv("data/TS_lulc_clust.csv")

# This is the only way I could get the data to join
df = cbind(df,clust,lulc)

######### Select variables and clean up for rf

df = df %>% subset(., select=which(!duplicated(names(.))))

df$kmeans = as.factor(df$kmeans)




sub = df %>%  select(X,gage,
                     kmeans,
                     lulc_water_prc,lulc_dev_prc,lulc_forest_prc,lulc_barren_prc,lulc_grass_prc,lulc_ag_prc,lulc_wetland_prc,
                     DRAIN_SQKM,SNOW_PCT_PRECIP,GEOL_REEDBUSH_DOM,FRESHW_WITHDRAWAL,
                     AWCAVE,PERMAVE,CLAYAVE,SILTAVE,SANDAVE,
                     TOPWET,ELEV_MEAN_M_BASIN,
                     porosity,storage_m,
                     P_mm,PET_mm,SWE_mm,melt_mm,Tmax_C,
                     P_90,PET_90,Tmax_90,melt_90,
                     freq_local
)%>%
  mutate(P.PET = P_mm/PET_mm,
         P.PET90 = P_90/PET_90
  )

#### Clean up data
rm(ant.cond,clust,dat,lulc,df)


############ Load Clustering Data#############
all = read.csv("data/kmeans.csv")

tt= all %>%  select(gage,X,dec_lat_va,dec_long_va,peak_date,peak_value,peak_quantile,
                peak2zero,drying_rate,p_value,calendar_year,season,
                meteorologic_year,dry_date_start,dry_date_mean,dry_dur,Name,TOPWET)
colnames(tt) = c('gage','event_id','dec_lat_va','dec_long_va','peak_date','peak_value','peak_quantile',
                      'peak2zero','drying_rate','p_value','calendar_year','season',
                      'meteorologic_year','dry_date_start','dry_date_mean','dry_dur','AggEcoregion','TOPWET')


############ Load Random Forest Predicted Data ###########
pred = read.csv("data/all_rf_data.csv") %>% select(X,gage,.pred_class)

############ Join them all together ###########

data= tt %>% 
  left_join(.,sub,by=c("event_id"="X","gage")) %>% 
  left_join(.,pred,by=c("event_id"="X","gage"))

############# Save the data #############

write.csv(data,"data/dryingRegimes_data_RF.csv")
