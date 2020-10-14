library(cowplot)

states <- map_data("state")




kmean_CLUST <- ggplot(data = states) + 
  geom_polygon(aes(x = long, y = lat, group = group), fill = "lightgrey", color = "black") + 
  coord_fixed(1.3, xlim = c(-121,-115), ylim = c(32.5,35)) +
  theme_linedraw() + 
  # geom_point(data=k.means, aes(x=dec_long_va, y=dec_lat_va, shape = factor(CLASS),size = proportion),alpha=1)+
  geom_point(data=k.means, aes(x=dec_long_va, y=dec_lat_va, shape = factor(CLASS),colour = factor(mode), size = proportion),alpha=1)+
  scale_color_manual(name = "Cluster Membership Mode",values = cols)+
  # scale_radius(trans='sqrt',breaks = c(.2,.4,.6,.8,1),labels = c(5,4,3,2,1),name = "Number of Cluster Changes") +
  scale_shape(name="Gage Type")+
  theme(legend.position = "left")

kmean_CLUST

conus <- ggplot(data = states) + 
  geom_polygon(aes(x = long, y = lat, group = group), fill = "grey", color = "black")+
  geom_rect(xmin = -121, xmax = -115, ymin = 32.5, ymax = 35, 
            fill = NA, colour = "red", size = .5) +
  theme_nothing()


gg_inset_map1 = ggdraw() +
  draw_plot(kmean_CLUST) +
  draw_plot(conus, x = .7, y = .7, width = 0.3, height = 0.3)

gg_inset_map1


kmean_CLUST <- ggplot(data = states) + 
  geom_polygon(aes(x = long, y = lat, group = group), fill = "gray", color = "black") + 
  coord_fixed(1.3, xlim = c(-121,-115), ylim = c(32.5,35)) +
  theme_linedraw() + 
  geom_point(data=k.means, aes(x=dec_long_va, y=dec_lat_va, shape = factor(CLASS),size = proportion+0.01),alpha=.25)+
  scale_radius(trans = 'sqrt',limits = c(.1,.5),name = "Proportion of Events in Cluster")+
  facet_wrap(~cluster)


kmean_CLUST +   geom_point(data=k.means, aes(x=dec_long_va, y=dec_lat_va, shape = factor(CLASS),colour = factor(cluster), size = proportion),alpha=.7)+
  scale_color_manual(values = cols)+
  # scale_radius(trans='sqrt',breaks = c(.2,.4,.6,.8,1),labels = c(5,4,3,2,1),name = "Number of Cluster Changes") +
  scale_radius(trans = 'sqrt',limits = c(.1,.5),name = "Proportion of Events in Cluster")+
  scale_shape(name="Gage Type")+
  ggtitle("k-means")+
  facet_wrap(~cluster)
