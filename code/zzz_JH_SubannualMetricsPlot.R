#####################################################################
##
## Script name: 
##
## Author: John Hammond
## Co-author: Adam N. Price
##
## Date Created: 2020-07-15
##
## Copyright (c) Adam N. Price, 2020
## Email: adnprice@ucsc.edu
##
############################# Description ##########################
## CONUS plot of drying metrics 
##  
##   
##
############################# Packages #############################

library(ggplot2)
library(ggmap)
library(maps)
library(mapdata)
library(dplyr)
library(scales)
library(maptools)
library(dataRetrieval)
library(viridis)
library(gridExtra)

############################# Code ################################

# plots of subannual metrics

states <- map_data("state")


######################### Mean number of drying events ####################### 
lmax = round(max(c(winter$n_event_mean,spring$n_event_mean,summer$n_event_mean,fall$n_event_mean),na.rm = T),1)
lmin = round(min(c(winter$n_event_mean,spring$n_event_mean,summer$n_event_mean,fall$n_event_mean),na.rm = T),1)

de_wi <- ggplot(data = states) + 
  geom_polygon(aes(x = long, y = lat, group = group), fill = "gray", color = "black") + 
  ylab('Mean Number of Drying Events\n(days)\nlat') +
  coord_fixed(1.3, xlim = c(-124.25,-70), ylim = c(26,48.5)) +
  theme_linedraw() + 
  geom_point(data=winter[!is.na(winter$n_event_mean),], aes(x=long_cent, y=lat_cent, shape = factor(Class), colour = n_event_mean), size = 2, alpha=1) +
  # geom_text(data=winter[!is.na(winter$n_event_mean),], aes(x=long_cent, y=lat_cent,label=round(n_event_mean,3)))+
  # scale_color_gradientn(name = "Winter Mean Number Drying Events", colours = viridis(10),labels=trans_format("identity", function(x) round(x,2)),breaks= scale_breaks,values = rescale(scale_breaks))+
  # scale_color_viridis(breaks= scale_breaks,values = rescale(scale_breaks))+
  scale_color_gradientn(trans = 'log',labels=trans_format("identity", function(x) round(x,2)),limits = c(0.01,lmax),colors = viridis(10))+
  scale_shape_manual(values=c(20, 17))+guides(shape = FALSE)

de_sp <- ggplot(data = states) + 
  geom_polygon(aes(x = long, y = lat, group = group), fill = "gray", color = "black") + 
  coord_fixed(1.3, xlim = c(-124.25,-70), ylim = c(26,48.5)) +
  theme_linedraw() + 
  geom_point(data=spring[!is.na(spring$n_event_mean),], aes(x=long_cent, y=lat_cent, shape = factor(Class), colour = n_event_mean), size = 2, alpha=1) +
  # scale_color_gradientn(name = "Spring Mean Number Drying Events", colours = plasma(100),labels=trans_format("identity", function(x) round(x,3)), limits = c(lmin,lmax))+
  # geom_text(data=spring[!is.na(spring$n_event_mean),], aes(x=long_cent, y=lat_cent,label=round(n_event_mean,3)))+
  scale_color_gradientn(trans = 'log',labels=trans_format("identity", function(x) round(x,2)),limits = c(0.01,lmax),colors = viridis(10))+
  scale_shape_manual(values=c(20, 17))+guides(shape = FALSE)

de_su <- ggplot(data = states) + 
  geom_polygon(aes(x = long, y = lat, group = group), fill = "gray", color = "black") + 
  coord_fixed(1.3, xlim = c(-124.25,-70), ylim = c(26,48.5)) +
  theme_linedraw() + 
  geom_point(data=summer[!is.na(summer$n_event_mean),], aes(x=long_cent, y=lat_cent, shape = factor(Class), colour = n_event_mean), size = 2, alpha=1) +
  # scale_color_gradientn(name = "Summer Mean Number Drying Events", colours = plasma(100),labels=trans_format("identity", function(x) round(x,3)), limits = c(lmin,lmax))+
  scale_color_gradientn(trans = 'log',labels=trans_format("identity", function(x) round(x,2)),limits = c(0.01,lmax),colors = viridis(10))+
  scale_shape_manual(values=c(20, 17))+guides(shape = FALSE)

de_fall <- ggplot(data = states) + 
  geom_polygon(aes(x = long, y = lat, group = group), fill = "gray", color = "black") + 
  coord_fixed(1.3, xlim = c(-124.25,-70), ylim = c(26,48.5)) +
  theme_linedraw() + 
  geom_point(data=fall[!is.na(fall$n_event_mean),], aes(x=long_cent, y=lat_cent, shape = factor(Class), colour = n_event_mean), size = 2, alpha=1) +
  # scale_color_gradientn(name = "Fall Mean Number Drying Events", colours = plasma(100),labels=trans_format("identity", function(x) round(x,3)), limits = c(lmin,lmax))+
  scale_color_gradientn(trans = 'log',labels=trans_format("identity", function(x) round(x,2)),limits = c(0.01,lmax),colors = viridis(10))+
  scale_shape_manual(values=c(20, 17))+guides(shape = FALSE)

de_all <- ggplot(data = states) +
  geom_polygon(aes(x = long, y = lat, group = group), fill = "gray", color = "black") + 
  coord_fixed(1.3, xlim = c(-124.25,-70), ylim = c(26,48.5)) +
  theme_linedraw() + 
  geom_point(data=all_season[!is.na(all_season$n_event_mean),], aes(x=long_cent, y=lat_cent, shape = factor(Class), colour = n_event_mean), size = 2, alpha=1) +
  # scale_color_gradientn(name = "Fall Mean Number Drying Events", colours = plasma(100),labels=trans_format("identity", function(x) round(x,3)), limits = c(lmin,lmax))+
  scale_color_gradientn(trans = 'log',labels=trans_format("identity", function(x) round(x,2)),limits = c(0.01,lmax),colors = viridis(10))+
  scale_shape_manual(values=c(20, 17))+guides(shape = FALSE)


 # p  = de_wi + de_sp + de_su + de_fall + plot_layout(ncol = 4, guides = "collect") & theme(legend.position = 'right') 
 # 
 # p

 # ggsave(filename="seasonal_no_flow_fractions.pdf", plot = plotnoflowfracs, width = 20, height = 30, units = "cm")
######################### Mean peak 2 zero length #######################
 lmax = round(max(c(winter$peak2zero_mean,spring$peak2zero_mean,summer$peak2zero_mean,fall$peak2zero_mean),na.rm = T),1)/4
 lmin = round(min(c(winter$peak2zero_mean,spring$peak2zero_mean,summer$peak2zero_mean,fall$peak2zero_mean),na.rm = T),1)
 
 p2z_wi <- ggplot(data = states) + 
   geom_polygon(aes(x = long, y = lat, group = group), fill = "gray", color = "black") + 
   coord_fixed(1.3, xlim = c(-124.25,-70), ylim = c(26,48.5)) +
   ylab('Mean Peak 2 Zero Length\n(day)\nlat')+
   theme_linedraw() + 
   geom_point(data=winter[!is.na(winter$peak2zero_mean),], aes(x=long_cent, y=lat_cent, shape = factor(Class), colour = peak2zero_mean), size = 2, alpha=1) +
   # scale_color_gradientn(name = "Spring Mean Number Drying Events", colours = plasma(100),labels=trans_format("identity", function(x) round(x,3)), limits = c(lmin,lmax))+
   scale_color_gradientn(trans = 'log',labels=trans_format("identity", function(x) round(x,2)),limits = c(lmin,lmax),colors = viridis(10),oob=squish)+
   scale_shape_manual(values=c(20, 17))+guides(shape = FALSE)
 
 p2z_sp <- ggplot(data = states) + 
   geom_polygon(aes(x = long, y = lat, group = group), fill = "gray", color = "black") + 
   coord_fixed(1.3, xlim = c(-124.25,-70), ylim = c(26,48.5)) +
   theme_linedraw() + 
   geom_point(data=spring[!is.na(spring$peak2zero_mean),], aes(x=long_cent, y=lat_cent, shape = factor(Class), colour = peak2zero_mean), size = 2, alpha=1) +
   # scale_color_gradientn(name = "Winter Mean Number Drying Events",colours = plasma(100),labels=format(breaks), breaks = breaks)+
   # scale_color_gradientn(name = "Spring Mean Number Drying Events", colours = plasma(100),labels=trans_format("identity", function(x) round(x,3)), limits = c(lmin,lmax))+
   scale_color_gradientn(trans = 'log',labels=trans_format("identity", function(x) round(x,2)),limits = c(lmin,lmax),colors = viridis(10),oob=squish)+
   scale_shape_manual(values=c(20, 17))+guides(shape = FALSE)
 
 p2z_su <- ggplot(data = states) + 
   geom_polygon(aes(x = long, y = lat, group = group), fill = "gray", color = "black") + 
   coord_fixed(1.3, xlim = c(-124.25,-70), ylim = c(26,48.5)) +
   theme_linedraw() + 
   geom_point(data=summer[!is.na(summer$peak2zero_mean),], aes(x=long_cent, y=lat_cent, shape = factor(Class), colour = peak2zero_mean), size = 2, alpha=1) +
   # scale_color_gradientn(name = "Spring Mean Number Drying Events", colours = plasma(100),labels=trans_format("identity", function(x) round(x,3)), limits = c(lmin,lmax))+
   scale_color_gradientn(trans = 'log',labels=trans_format("identity", function(x) round(x,2)),limits = c(lmin,lmax),colors = viridis(10),oob=squish)+
   scale_shape_manual(values=c(20, 17))+guides(shape = FALSE)
 
 p2z_fall <- ggplot(data = states) + 
   geom_polygon(aes(x = long, y = lat, group = group), fill = "gray", color = "black") + 
   coord_fixed(1.3, xlim = c(-124.25,-70), ylim = c(26,48.5)) +
   theme_linedraw() + 
   geom_point(data=fall[!is.na(fall$peak2zero_mean),], aes(x=long_cent, y=lat_cent, shape = factor(Class), colour = peak2zero_mean), size = 2, alpha=1) +
   # scale_color_gradientn(name = "Spring Mean Number Drying Events", colours = plasma(100),labels=trans_format("identity", function(x) round(x,3)), limits = c(lmin,lmax))+
   scale_color_gradientn(trans = 'log',labels=trans_format("identity", function(x) round(x,2)),limits = c(lmin,lmax),colors = viridis(10),oob=squish)+
   scale_shape_manual(values=c(20, 17))+guides(shape = FALSE)
 
p2z_all <- ggplot(data = states) + 
   geom_polygon(aes(x = long, y = lat, group = group), fill = "gray", color = "black") + 
   coord_fixed(1.3, xlim = c(-124.25,-70), ylim = c(26,48.5)) +
   theme_linedraw() + 
   geom_point(data=all_season[!is.na(all_season$peak2zero_mean),], aes(x=long_cent, y=lat_cent, shape = factor(Class), colour = peak2zero_mean), size = 2, alpha=1) +
   scale_color_gradientn(trans = 'log',labels=trans_format("identity", function(x) round(x,2)),limits = c(lmin,lmax),colors = viridis(10),oob=squish)+
   scale_shape_manual(values=c(20, 17))+guides(shape = FALSE)
 
 
 # p  = p2z_wi + p2z_sp + p2z_su + p2z_fall + plot_layout(ncol = 4, guides = "collect") & theme(legend.position = 'right') 
 # 
 # p

######################### Mean drying rate #######################
 lmax = round(max(c(winter$drying_rate_mean,spring$drying_rate_mean,summer$drying_rate_mean,fall$drying_rate_mean),na.rm = T),1)
 lmin = round(min(c(winter$drying_rate_mean,spring$drying_rate_mean,summer$drying_rate_mean,fall$drying_rate_mean),na.rm = T),1)

 dr_wi <- ggplot(data = states) + 
   geom_polygon(aes(x = long, y = lat, group = group), fill = "gray", color = "black") + 
   coord_fixed(1.3, xlim = c(-124.25,-70), ylim = c(26,48.5)) +
   ylab('Drying Rate\n(1/day)\nlat')+
   xlab('long\nWinter') +
   theme_linedraw() + 
   geom_point(data=winter[!is.na(winter$drying_rate_mean),], aes(x=long_cent, y=lat_cent, shape = factor(Class), colour = drying_rate_mean), size = 2, alpha=1) +
   scale_color_gradientn(trans = 'log',labels=trans_format("identity", function(x) round(x,2)),limits = c(0.01,lmax),colors = viridis(10),oob=squish)+
   scale_shape_manual(values=c(20, 17))+guides(shape = FALSE)
 
 dr_sp <- ggplot(data = states) + 
   geom_polygon(aes(x = long, y = lat, group = group), fill = "gray", color = "black") + 
   coord_fixed(1.3, xlim = c(-124.25,-70), ylim = c(26,48.5)) +
   xlab('long\nSpring') +
   theme_linedraw() + 
   geom_point(data=spring[!is.na(spring$drying_rate_mean),], aes(x=long_cent, y=lat_cent, shape = factor(Class), colour = drying_rate_mean), size = 2, alpha=1) +
   scale_color_gradientn(trans = 'log',labels=trans_format("identity", function(x) round(x,2)),limits = c(0.01,lmax),colors = viridis(10),oob=squish)+
   scale_shape_manual(values=c(20, 17))+guides(shape = FALSE)
 
 dr_su <- ggplot(data = states) + 
   geom_polygon(aes(x = long, y = lat, group = group), fill = "gray", color = "black") + 
   coord_fixed(1.3, xlim = c(-124.25,-70), ylim = c(26,48.5)) +
   xlab('long\nSummer') +
   theme_linedraw() + 
   geom_point(data=summer[!is.na(summer$drying_rate_mean),], aes(x=long_cent, y=lat_cent, shape = factor(Class), colour = drying_rate_mean), size = 2, alpha=1) +
   scale_color_gradientn(trans = 'log',labels=trans_format("identity", function(x) round(x,2)),limits = c(0.01,lmax),colors = viridis(10),oob=squish)+
   scale_shape_manual(values=c(20, 17))+guides(shape = FALSE)
 
 dr_fall <- ggplot(data = states) + 
   geom_polygon(aes(x = long, y = lat, group = group), fill = "gray", color = "black") + 
   coord_fixed(1.3, xlim = c(-124.25,-70), ylim = c(26,48.5)) +
   xlab('long\nFall') +
   theme_linedraw() + 
   geom_point(data=fall[!is.na(fall$drying_rate_mean),], aes(x=long_cent, y=lat_cent, shape = factor(Class), colour = drying_rate_mean), size = 2, alpha=1) +
   scale_color_gradientn(trans = 'log',labels=trans_format("identity", function(x) round(x,2)),limits = c(0.01,lmax),colors = viridis(10),oob=squish)+
   scale_shape_manual(values=c(20, 17))+guides(shape = FALSE)
 
 dr_all <- ggplot(data = states) + 
   geom_polygon(aes(x = long, y = lat, group = group), fill = "gray", color = "black") + 
   coord_fixed(1.3, xlim = c(-124.25,-70), ylim = c(26,48.5)) +
   xlab('long\nAnnual') +
   theme_linedraw() + 
   geom_point(data=all_season[!is.na(all_season$drying_rate_mean),], aes(x=long_cent, y=lat_cent, shape = factor(Class), colour = drying_rate_mean), size = 2, alpha=1) +
   scale_color_gradientn(trans = 'log',labels=trans_format("identity", function(x) round(x,2)),limits = c(0.01,lmax),colors = viridis(10),oob=squish)+
   scale_shape_manual(values=c(20, 17))+guides(shape = FALSE)
 
 
 # p  = de_wi + de_sp + de_su + de_fall + plot_layout(ncol = 4, guides = "collect") & theme(legend.position = 'right') 
 # 
 # p

######################### Zero flow centriod date #######################
 lmax = round(max(c(winter$zeroflowcentroiddatejfm,spring$zeroflowcentroiddateamj,summer$zeroflowcentroiddatejas,fall$zeroflowcentroiddateond),na.rm = T),1)
 lmin = round(min(c(winter$zeroflowcentroiddatejfm,spring$zeroflowcentroiddateamj,summer$zeroflowcentroiddatejas,fall$zeroflowcentroiddateond),na.rm = T),1)
 
 cen_wi <- ggplot(data = states) + 
   geom_polygon(aes(x = long, y = lat, group = group), fill = "gray", color = "black") + 
   coord_fixed(1.3, xlim = c(-124.25,-70), ylim = c(26,48.5)) +
   ylab('No Flow Centroid Day\n(day of season)\nlat')+
   theme_linedraw() + 
   geom_point(data=winter[!is.na(winter$zeroflowcentroiddatejfm),], aes(x=long_cent, y=lat_cent, shape = factor(Class), colour = zeroflowcentroiddatejfm), size = 2, alpha=1) +
   scale_color_gradientn(name = "Winter Centroid", colours = viridis(10),labels=trans_format("identity", function(x) round(((x)/(lmax-lmin)),2)), limits = c(0,lmax),oob=squish)+
   # scale_color_gradientn(trans = 'identity',labels=trans_format("identity", function(x) round(x,2)),limits = c(0,120),colors = viridis(10),oob=squish)+
   scale_shape_manual(values=c(20, 17))+guides(shape = FALSE)
 
 cen_sp <- ggplot(data = states) + 
   geom_polygon(aes(x = long, y = lat, group = group), fill = "gray", color = "black") + 
   coord_fixed(1.3, xlim = c(-124.25,-70), ylim = c(26,48.5)) +
   theme_linedraw() + 
   geom_point(data=spring[!is.na(spring$zeroflowcentroiddateamj),], aes(x=long_cent, y=lat_cent, shape = factor(Class), colour = zeroflowcentroiddateamj), size = 2, alpha=1) +
   scale_color_gradientn(colours = viridis(10),labels=trans_format("identity", function(x) round(((x)/(lmax-lmin)),2)), limits = c(0,lmax),oob=squish)+
   scale_shape_manual(values=c(20, 17))+guides(shape = FALSE)
 
 cen_su <- ggplot(data = states) + 
   geom_polygon(aes(x = long, y = lat, group = group), fill = "gray", color = "black") + 
   coord_fixed(1.3, xlim = c(-124.25,-70), ylim = c(26,48.5)) +
   theme_linedraw() + 
   geom_point(data=summer[!is.na(summer$zeroflowcentroiddatejas),], aes(x=long_cent, y=lat_cent, shape = factor(Class), colour = zeroflowcentroiddatejas), size = 2, alpha=1) +
   scale_color_gradientn( colours = viridis(10),labels=trans_format("identity", function(x) round(((x)/(lmax-lmin)),2)), limits = c(0,lmax),oob=squish)+
   scale_shape_manual(values=c(20, 17))+guides(shape = FALSE)
 
 
 cen_fall <- ggplot(data = states) + 
   geom_polygon(aes(x = long, y = lat, group = group), fill = "gray", color = "black") + 
   coord_fixed(1.3, xlim = c(-124.25,-70), ylim = c(26,48.5)) +
   theme_linedraw() + 
   geom_point(data=fall[!is.na(fall$zeroflowcentroiddateond),], aes(x=long_cent, y=lat_cent, shape = factor(Class), colour = zeroflowcentroiddateond), size = 2, alpha=1) +
   scale_color_gradientn(colours = viridis(10),labels=trans_format("identity", function(x) round(((x)/(lmax-lmin)),2)), limits = c(0,lmax),oob=squish)+
   scale_shape_manual(values=c(20, 17))+guides(shape = FALSE)
 
 lmin = min(all_season$zeroflowcentroiddate,na.rm = T)
 lmax = max(all_season$zeroflowcentroiddate,na.rm = T)
 
 
 cen_all <- ggplot(data = states) + 
   geom_polygon(aes(x = long, y = lat, group = group), fill = "gray", color = "black") + 
   coord_fixed(1.3, xlim = c(-124.25,-70), ylim = c(26,48.5)) +
   theme_linedraw() + 
   geom_point(data=all_season[!is.na(all_season$zeroflowcentroiddate),], aes(x=long_cent, y=lat_cent, shape = factor(Class), colour = zeroflowcentroiddate), size = 2, alpha=1) +
   scale_color_gradientn(colours = viridis(10),labels=trans_format("identity", function(x) round(((x)/(lmax-lmin)),2)), limits = c(0,lmax),oob=squish)+
   scale_shape_manual(values=c(20, 17))+guides(shape = FALSE)
 
 
 
 # p  = cen_wi + cen_sp + cen_su + cen_fall + plot_layout(ncol = 4, guides = "collect") & theme(legend.position = 'right') 
 # 
 # p
 

######################### No-flow fraction #######################
 # lmax = round(max(c(winter$drying_rate_mean,spring$drying_rate_mean,summer$drying_rate_mean,fall$drying_rate_mean),na.rm = T),1)
 # lmin = round(min(c(winter$drying_rate_mean,spring$drying_rate_mean,summer$drying_rate_mean,fall$drying_rate_mean),na.rm = T),1)


 
##################### All plots together ########################
 
 # all = cen_wi + cen_sp + cen_su + cen_fall +
 #   p2z_wi+ p2z_sp + p2z_su + p2z_fall +
 #   de_wi + de_sp + de_su + de_fall + 
 #   dr_wi + dr_sp + dr_su + dr_fall + 
 #   plot_layout(ncol = 4, guides = "collect") & theme(legend.position = 'right') 
 # 
 # all

 
 
 
 all = cen_wi + cen_sp+ cen_su + cen_fall + cen_all +
   p2z_wi + p2z_sp + p2z_su + p2z_fall+ p2z_all +
   de_wi + de_sp + de_su + de_fall + de_all +
   dr_wi + dr_sp + dr_su + dr_fall + dr_all +
   plot_layout(ncol = 5, guides = "collect") & theme(legend.position = 'bottom',legend.direction = 'vertical') 
 
 all
