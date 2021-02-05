#####################################################################
##
## Script name: 
##
## Author: Adam N. Price
##
## Date Created: 2020-11-23
##
## Copyright (c) Adam N. Price, 2020
## Email: adnprice@ucsc.edu
##
############################# Description ##########################
##
## Find the median hydrograph for k-means clusters
##   
##
############################# Packages #############################

library(tidyverse)
library(ggplot2)
library(lubridate)

############################# Code ################################

## Load data

clust = read.csv('data/kmeans.csv')

clust.metrics = clust %>%
  select(gage,peak_date,peak_quantile,peak2zero,drying_rate,dry_date_start,dry_dur,freq_local)

clust.median = clust %>% 
  select(peak_date,peak_quantile,peak2zero,drying_rate,dry_date_start,dry_dur,freq_local) %>%
  mutate(peak_quantile = abs(peak_quantile-median(peak_quantile)),
         peak2zero = abs(peak2zero-median(peak2zero)),
         drying_rate = abs(drying_rate-median(drying_rate)),
         dry_dur = abs(dry_dur-median(dry_dur)),
         freq_local = abs(freq_local-median(freq_local))
  ) %>%
  scale()

c1 = clust[clust$kmeans==1,]  %>% 
  select(peak_date,peak_quantile,peak2zero,drying_rate,dry_date_start,dry_dur,freq_local) %>%
  mutate(peak_quantile = abs(peak_quantile-median(peak_quantile)),
         peak2zero = abs(peak2zero-median(peak2zero)),
         drying_rate = abs(drying_rate-median(drying_rate)),
         dry_dur = abs(dry_dur-median(dry_dur)),
         freq_local = abs(freq_local-median(freq_local))
  ) %>%
  scale()

c2 = clust[clust$kmeans==2,]  %>% 
  select(peak_date,peak_quantile,peak2zero,drying_rate,dry_date_start,dry_dur,freq_local) %>%
  mutate(peak_quantile = abs(peak_quantile-median(peak_quantile)),
         peak2zero = abs(peak2zero-median(peak2zero)),
         drying_rate = abs(drying_rate-median(drying_rate)),
         dry_dur = abs(dry_dur-median(dry_dur)),
         freq_local = abs(freq_local-median(freq_local))
  ) %>%
  scale()

c3 = clust[clust$kmeans==3,]  %>% 
  select(peak_date,peak_quantile,peak2zero,drying_rate,dry_date_start,dry_dur,freq_local) %>%
  mutate(peak_quantile = abs(peak_quantile-median(peak_quantile)),
         peak2zero = abs(peak2zero-median(peak2zero)),
         drying_rate = abs(drying_rate-median(drying_rate)),
         dry_dur = abs(dry_dur-median(dry_dur)),
         freq_local = abs(freq_local-median(freq_local))
  ) %>%
  scale()

c4 = clust[clust$kmeans==4,]  %>% 
  select(peak_date,peak_quantile,peak2zero,drying_rate,dry_date_start,dry_dur,freq_local) %>%
  mutate(peak_quantile = abs(peak_quantile-median(peak_quantile)),
         peak2zero = abs(peak2zero-median(peak2zero)),
         drying_rate = abs(drying_rate-median(drying_rate)),
         dry_dur = abs(dry_dur-median(dry_dur)),
         freq_local = abs(freq_local-median(freq_local))
         ) %>%
  scale()


c.ind = which(rowSums(clust.median)==min(rowSums(clust.median)))
c1.ind = which(rowSums(c1)==min(rowSums(c1)))
c2.ind = which(rowSums(c2)==min(rowSums(c2)))
c3.ind = which(rowSums(c3)==min(rowSums(c3)))
c4.ind = which(rowSums(c4)==min(rowSums(c4)))


c1 = clust[clust$kmeans==1,] 
c2 = clust[clust$kmeans==2,] 
c3 = clust[clust$kmeans==3,] 
c4 = clust[clust$kmeans==4,] 

c = clust[c.ind,]
c1 = c1[c1.ind,]
c2 = c2[c2.ind,]
c3 = c3[c3.ind,]
c4 = c4[c4.ind,]

c.med = rbind(c,c1,c2,c3,c4)
c.med = transform(c.med, peak_date_filt = as.Date(paste0(calendar_year, "-1-1")) + peak_date)
c.med = transform(c.med, dry_date_filt = as.Date(paste0(calendar_year, "-1-1")) + dry_date_start)
c.med$gage = sprintf("%08d",c.med$gage)

median.peak = c(median(clust$dry_date_start),203,180,215,209)


out = as.data.frame(NA)
ind.out = as.data.frame(NA)
for (i in 1:5){
  print(i)
  file = read.csv(file = paste0('data/daily_data_with_ climate_and_PET/csv/',c.med$gage[i],'.csv'))
  file$Date = ymd(file$Date)
  peak.ind = which(file$Date==c.med$peak_date_filt[i])-1
  # dry_date.ind = which(file$Date==c.med$dry_date_filt[i])+(round(c.med$dry_dur[i]+(.5*c.med$dry_dur[i]),0))
  dry_date.ind = which(file$Date==c.med$dry_date_filt[i])+(c.med$dry_dur[i])
  dat = file[peak.ind:dry_date.ind,5]
  dat[length(dat)] = 5
  length(dat) = 1000
  ind = seq(median.peak[i]-1,(median.peak[i]-1)+999,1)-c.med$peak2zero[i]
  out = cbind(out,dat)
  ind.out = cbind(ind.out,ind)
}

dat = out[,-1]
colnames(dat) = c.med$gage

dat.ind = ind.out[-1]
colnames(dat.ind) = c.med$gage


dat= dat %>% gather() 
dat.ind = dat.ind %>% gather()
dat.ind$key = NULL

all =cbind(dat,dat.ind)

colnames(all) = c('gage','q','peak_day')
all = na.omit(all)



##### Plot Data

cols <- 
  c("08072730" = "#4477AA",
    "11085000" = "#66CCEE",
    "02304500" = "#228833",
    "05595730" = "#CCBB44",
    "11147500" = "#000000")

cluster.labels <-
  c("08072730" = 'Cluster 1',
    "11085000" = 'Cluster 2',
    "02304500" = 'Cluster 3',
    "05595730" = 'Cluster 4',
    "11147500" = "Drying Event Average")


hydro = ggplot(all,aes(x=peak_day,y=round(q,0),color = gage)) +
  geom_line(size=.5,alpha=.9)+
  scale_color_manual(name = "Cluster Membership",values = cols,labels=cluster.labels)+
  # xlim(170,230)+
  xlab("Day of Year")+
  ylab('Discharge (cfs)')+
  theme_minimal()

hydro  + theme(legend.position = "none")

pdf("docs//synthHydro.pdf", width = 12, height=12)
hydro+ theme(legend.position = "none")
dev.off()




######## Where the median events are located ########
# c.med$kmeans[1] = 5
# 
# states <- map_data("state")
# 
# kmean_CLUST <- ggplot(data = states) + 
#   geom_polygon(aes(x = long, y = lat, group = group), fill = "lightgray", color = "black") + 
#   coord_fixed(1.3, xlim = c(-124.25,-70), ylim = c(26,48.5)) +
#   theme_linedraw() + 
#   geom_point(data=c.med, aes(x=dec_long_va, y=dec_lat_va, shape = factor(CLASS),color = factor(kmeans)),size = 4,alpha=.8)+
#   scale_color_manual(name = "Cluster Membership",values = c.cols, labels = cluster.labels)+
#   scale_shape(name="Gage Type")
####### Make a table with stats #############
# 
# clust.metrics = clust %>%
#   select(kmeans,peak_date,peak_quantile,peak2zero,drying_rate,dry_date_start,dry_dur,freq_local)
# 
# clust.metrics %>% group_by(kmeans) %>% summarise_