library(tidymodels)
library(ranger)

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


bu = df

sub = df %>%  select(gage,dec_lat_va,dec_long_va,
                     kmeans,
                     lulc_water_prc,lulc_dev_prc,lulc_forest_prc,lulc_barren_prc,lulc_grass_prc,lulc_ag_prc,lulc_wetland_prc,
                     DRAIN_SQKM,SNOW_PCT_PRECIP,GEOL_REEDBUSH_DOM,FRESHW_WITHDRAWAL,
                     AWCAVE,PERMAVE,CLAYAVE,SILTAVE,SANDAVE,
                     TOPWET,ELEV_MEAN_M_BASIN,
                     porosity,storage_m,
                     P_mm,PET_mm,SWE_mm,melt_mm,Tmax_C,
                     P_90,PET_90,Tmax_90,melt_90
)%>%
  mutate(P.PET = P_mm/PET_mm,
         P.PET90 = P_90/PET_90
  )


sub[is.na(sub$P.PET),'P.PET']=0
# sub[is.na(sub$P.PET7),'P.PET7']=0

sub = sub %>% filter_all(all_vars(!is.infinite(.)))
sub = sub[complete.cases(sub[,-c(1:3)]),]



tt = read.csv('data/all_rf_data.csv') %>% 
  select(.pred_class) %>%
  set_names(c("pred_cluster"))

final = cbind(sub,tt)

# write.csv(final,file = "data/dryingRegimes_data_RF.csv")

tt = final %>% 
  select(gage,kmeans)%>%
  group_by(gage,kmeans)%>%
  count()

clust.act = tt %>%
  group_by(gage)%>%
  mutate(nn=sum(n),
         prop = n/nn)

clust.act = clust.act [clust.act $kmeans==1,]


tt = final %>% 
  select(gage,pred_cluster)%>%
  group_by(gage,pred_cluster)%>%
  count()

clust.pred = tt %>%
  group_by(gage)%>%
  mutate(nn=sum(n),
         prop.pred = n/nn)

clust.pred = clust.pred [clust.pred $pred_cluster==1,]


dat = clust.act %>%
  left_join(.,clust.pred,by=c("gage"))

dat[is.na(dat$prop.pred),'prop.pred']=0


ggplot(dat)+
  geom_point(aes(x=prop.pred,y=prop))+
  geom_abline(slope=1)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Step -------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
######## Plots ##########
# Color scales
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



pal_regions <- 
  c("Eastern Forests" = "#009E73",
    "Mediterranean California" = "#F0E442",
    "Northern Great Plains" = "#0072B2",
    "Southern Great Plains" = "#E69F00",
    "Western Deserts" = "#D55E00",
    "Western Mountains" = "#56B4E9",
    "Ignore"="lightgrey")


# Calculate mode and cluster membership proportion

## Plot

bu = final
mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

n = final %>% select(gage,pred_cluster)%>%
  group_by(gage,pred_cluster) %>% count()

n.clust = final %>% select(gage) %>%
  group_by(gage)%>%
  count()


final = final %>% left_join(.,n,by=c("gage","pred_cluster"))
final = final %>% left_join(.,n.clust,by=c("gage"))

tt = final %>% 
  group_by(gage)%>%
  mutate(prop = n.x/n.y)


### Load map data


states <- st_as_sf(map("state", plot = FALSE, fill = TRUE))


agg_eco = sf::st_read('data/dissolved_ecoregions_060220/dissolved_ecoregions_060220.shp')
agg_eco$NA_L1CODE = as.numeric(agg_eco$NA_L1CODE)
newData <- sf::st_transform(agg_eco, CRS=4326)
states <- sf::st_transform(states, CRS=4326)


counts = final %>% group_by(pred_cluster) %>% count()
labs = c("1" = paste0("Cluster 1\nn=",counts$n[1]),
         "2" = paste0("Cluster 2\nn=",counts$n[2]),
         "3" = paste0("Cluster 3\nn=",counts$n[3]),
         "4" = paste0("Cluster 4\nn=",counts$n[4]))

# map <- ggplot(data = newData) +
#   geom_sf(aes(fill=Name),color=NA,alpha=.4)+
#   scale_fill_manual(values = pal_regions)

map = ggplot(data=states)+
  geom_sf(alpha=0,lwd=.2)

map = map + 
  geom_sf(data=newData,aes(fill=Name),color=NA,alpha=.4)+
  scale_fill_manual(values = pal_regions)

map = map + 
  geom_point(data=tt, aes(x=dec_long_va, y=dec_lat_va,colour = factor(pred_cluster), alpha = prop))+
  scale_color_manual(values = cols)+
  # scale_radius(trans = 'sqrt',breaks = c(0,.2,.4,.6,.8,1),name = "Proportion of Events in Cluster")+
  scale_shape(name="Gage Type")+
  facet_wrap(~pred_cluster)+
  theme_void()

pdf("docs/spatialCluster_RF.pdf")
map
dev.off()

