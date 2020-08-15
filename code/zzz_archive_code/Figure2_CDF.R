

zScore <- function(x, na.rm = FALSE) (x - mean(x, na.rm = TRUE)) / sd(x, na.rm=TRUE)

# names(spring)[names(spring)=="Aggregated_region"]  = 'Test'


######################### Number of drying events######################### 


# de_winter<- ggplot()+
#   stat_ecdf(data=winter, aes(x = n_event_mean, group=Aggregated_region,color = Aggregated_region)) +
#   scale_color_manual(values = pal_regions)+
#   ylab('# of\nNo Flow Events')+
#   theme(
#     # get rid of panel grids
#     panel.grid.major = element_blank(),
#     panel.grid.minor = element_blank(),
#     # Change plot and panel background
#     plot.background = element_rect(fill = back_col[1]),
#     panel.background = element_rect(fill = back_col[1]),
#     axis.title.x = element_blank())
# 
# de_spring <- ggplot()+
#   stat_ecdf(data=spring, aes(x = n_event_mean, group=Aggregated_region,color = Aggregated_region)) +
#   scale_color_manual(values = pal_regions)+
#   theme(
#     # get rid of panel grids
#     panel.grid.major = element_blank(),
#     panel.grid.minor = element_blank(),
#     # Change plot and panel background
#     plot.background = element_rect(fill = back_col[1]),
#     panel.background = element_rect(fill = back_col[1]),
#     axis.title.x = element_blank(),
#     axis.title.y = element_blank())
# 
# de_summer <- ggplot()+
#   stat_ecdf(data=summer, aes(x = n_event_mean, group=Aggregated_region,color = Aggregated_region)) +
#   scale_color_manual(values = pal_regions)+
#   theme(
#     # get rid of panel grids
#     panel.grid.major = element_blank(),
#     panel.grid.minor = element_blank(),
#     # Change plot and panel background
#     plot.background = element_rect(fill = back_col[1]),
#     panel.background = element_rect(fill = back_col[1]),
#     axis.title.x = element_blank(),
#     axis.title.y = element_blank())
# 
# 
# de_fall <- ggplot()+
#   stat_ecdf(data=fall, aes(x = n_event_mean, group=Aggregated_region,color = Aggregated_region)) +
#   scale_color_manual(values = pal_regions)+
#   theme(
#     # get rid of panel grids
#     panel.grid.major = element_blank(),
#     panel.grid.minor = element_blank(),
#     # Change plot and panel background
#     plot.background = element_rect(fill = back_col[1]),
#     panel.background = element_rect(fill = back_col[1]),
#     axis.title.x = element_blank(),
#     axis.title.y = element_blank())
# 
# de_all <- ggplot()+
#   stat_ecdf(data=all_season, aes(x = n_event_mean, group=Aggregated_region,color = Aggregated_region)) +
#   scale_color_manual(values = pal_regions)+
#   theme(
#     # get rid of panel grids
#     panel.grid.major = element_blank(),
#     panel.grid.minor = element_blank(),
#     # Change plot and panel background
#     plot.background = element_rect(fill = back_col[1]),
#     panel.background = element_rect(fill = back_col[1]),
#     axis.title.x = element_blank(),
#     axis.title.y = element_blank())


######################### Mean duration of drying events######################### 
dedur_winter<- ggplot()+
  stat_ecdf(data=winter, aes(x = zScore(dry_dur_mean), group=Aggregated_region, color = Aggregated_region)) +
  scale_color_manual(values = pal_regions)+
  ylab('Mean Duration\nof No Flow Events')+
  theme(
    # get rid of panel grids
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    # Change plot and panel background
    plot.background = element_rect(fill = back_col[2]),
    panel.background = element_rect(fill = back_col[2]),
    axis.title.x = element_blank())

dedur_spring <- ggplot()+
  stat_ecdf(data=spring, aes(x = zScore(dry_dur_mean), group=Aggregated_region,color = Aggregated_region)) +
  scale_color_manual(values = pal_regions)+
  theme(
    # get rid of panel grids
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    # Change plot and panel background
    plot.background = element_rect(fill = back_col[2]),
    panel.background = element_rect(fill = back_col[2]),
    axis.title.x = element_blank(),
    axis.title.y = element_blank())

dedur_summer <- ggplot()+
  stat_ecdf(data=summer, aes(x = zScore(dry_dur_mean), group=Aggregated_region,color = Aggregated_region)) +
  scale_color_manual(values = pal_regions)+
  theme(
    # get rid of panel grids
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    # Change plot and panel background
    plot.background = element_rect(fill = back_col[2]),
    panel.background = element_rect(fill = back_col[2]),
    axis.title.x = element_blank(),
    axis.title.y = element_blank())

dedur_fall <- ggplot()+
  stat_ecdf(data=fall, aes(x = zScore(n_event_mean), group=Aggregated_region,color = Aggregated_region)) +
  scale_color_manual(values = pal_regions)+
  theme(
    # get rid of panel grids
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    # Change plot and panel background
    plot.background = element_rect(fill = back_col[2]),
    panel.background = element_rect(fill = back_col[2]),
    axis.title.x = element_blank(),
    axis.title.y = element_blank())

dedur_all <- ggplot()+
  stat_ecdf(data=all_season, aes(x = zScore(n_event_mean), group=Aggregated_region,color = Aggregated_region)) +
  scale_color_manual(values = pal_regions)+
  theme(
    # get rid of panel grids
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    # Change plot and panel background
    plot.background = element_rect(fill = back_col[2]),
    panel.background = element_rect(fill = back_col[2]),
    axis.title.x = element_blank(),
    axis.title.y = element_blank())

dedur_winter + dedur_spring + dedur_summer +plot_layout(ncol = 3, guides = "collect")  & theme(legend.position = 'bottom') 
######################### Mean Peak2Zero length######################### 

p2z_winter<- ggplot()+
  stat_ecdf(data=winter, aes(x = peak2zero_mean, group=Aggregated_region, color = Aggregated_region)) +
  scale_color_manual(values = pal_regions)+
  ylab('Mean\nPeak2Zero Length')+
  theme(
    # get rid of panel grids
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    # Change plot and panel background
    plot.background = element_rect(fill = back_col[1]),
    panel.background = element_rect(fill = back_col[1]),
    axis.title.x = element_blank())

p2z_spring <- ggplot()+
  stat_ecdf(data=spring, aes(x = peak2zero_mean, group=Aggregated_region,color = Aggregated_region)) +
  scale_color_manual(values = pal_regions)+
  theme(
    # get rid of panel grids
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    # Change plot and panel background
    plot.background = element_rect(fill = back_col[1]),
    panel.background = element_rect(fill = back_col[1]),
    axis.title.x = element_blank(),
    axis.title.y = element_blank())

p2z_summer <- ggplot()+
  stat_ecdf(data=summer, aes(x = peak2zero_mean, group=Aggregated_region,color = Aggregated_region)) +
  scale_color_manual(values = pal_regions)+
  theme(
    # get rid of panel grids
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    # Change plot and panel background
    plot.background = element_rect(fill = back_col[1]),
    panel.background = element_rect(fill = back_col[1]),
    axis.title.x = element_blank(),
    axis.title.y = element_blank())

p2z_fall <- ggplot()+
  stat_ecdf(data=fall, aes(x = peak2zero_mean, group=Aggregated_region,color = Aggregated_region)) +
  scale_color_manual(values = pal_regions)+
  theme(
    # get rid of panel grids
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    # Change plot and panel background
    plot.background = element_rect(fill = back_col[1]),
    panel.background = element_rect(fill = back_col[1]),
    axis.title.x = element_blank(),
    axis.title.y = element_blank())

p2z_all <- ggplot()+
  stat_ecdf(data=all_season, aes(x = peak2zero_mean, group=Aggregated_region,color = Aggregated_region)) +
  scale_color_manual(values = pal_regions)+
  theme(
    # get rid of panel grids
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    # Change plot and panel background
    plot.background = element_rect(fill = back_col[1]),
    panel.background = element_rect(fill = back_col[1]),
    axis.title.x = element_blank(),
    axis.title.y = element_blank())

######################### Mean drying rate######################### 

dr_winter<- ggplot()+
  stat_ecdf(data=winter, aes(x = drying_rate_mean, group=Aggregated_region, color = Aggregated_region)) +
  scale_color_manual(values = pal_regions)+
  ylab('Mean\nDrying Rate')+
  theme(
    # get rid of panel grids
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    # Change plot and panel background
    plot.background = element_rect(fill = back_col[2]),
    panel.background = element_rect(fill = back_col[2]),
    axis.title.x = element_blank())

dr_spring <- ggplot()+
  stat_ecdf(data=spring, aes(x = drying_rate_mean, group=Aggregated_region,color = Aggregated_region)) +
  scale_color_manual(values = pal_regions)+
  theme(
    # get rid of panel grids
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    # Change plot and panel background
    plot.background = element_rect(fill = back_col[2]),
    panel.background = element_rect(fill = back_col[2]),
    axis.title.x = element_blank(),
    axis.title.y = element_blank())

dr_summer <- ggplot()+
  stat_ecdf(data=summer, aes(x = drying_rate_mean, group=Aggregated_region,color = Aggregated_region)) +
  scale_color_manual(values = pal_regions)+
  theme(
    # get rid of panel grids
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    # Change plot and panel background
    plot.background = element_rect(fill = back_col[2]),
    panel.background = element_rect(fill = back_col[2]),
    axis.title.x = element_blank(),
    axis.title.y = element_blank())

dr_fall <- ggplot()+
  stat_ecdf(data=fall, aes(x = drying_rate_mean, group=Aggregated_region,color = Aggregated_region)) +
  scale_color_manual(values = pal_regions)+
  theme(
    # get rid of panel grids
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    # Change plot and panel background
    plot.background = element_rect(fill = back_col[2]),
    panel.background = element_rect(fill = back_col[2]),
    axis.title.x = element_blank(),
    axis.title.y = element_blank())

dr_all <- ggplot()+
  stat_ecdf(data=all_season, aes(x = drying_rate_mean, group=Aggregated_region,color = Aggregated_region)) +
  scale_color_manual(values = pal_regions)+
  theme(
    # get rid of panel grids
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    # Change plot and panel background
    plot.background = element_rect(fill = back_col[2]),
    panel.background = element_rect(fill = back_col[2]),
    axis.title.x = element_blank(),
    axis.title.y = element_blank())

######################### Centroid date######################### 

cd_winter <- ggplot()+
  stat_ecdf(data=winter, aes(x = zeroflowcentroiddatejfm, group=Aggregated_region,color = Aggregated_region)) +
  scale_color_manual(values = pal_regions)+
  ylab('Zero Flow\nCentroid Date')+
  theme(
    # get rid of panel grids
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    # Change plot and panel background
    plot.background = element_rect(fill = back_col[1]),
    panel.background = element_rect(fill = back_col[1]),
    axis.title.x = element_blank())


cd_spring <- ggplot()+
  stat_ecdf(data=spring, aes(x = zeroflowcentroiddateamj, group=Aggregated_region,color = Aggregated_region)) +
  scale_color_manual(values = pal_regions)+
  theme(
    # get rid of panel grids
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    # Change plot and panel background
    plot.background = element_rect(fill = back_col[1]),
    panel.background = element_rect(fill = back_col[1]),
    axis.title.x = element_blank(),
    axis.title.y = element_blank())

cd_summer <- ggplot()+
  stat_ecdf(data=summer, aes(x = zeroflowcentroiddatejas, group=Aggregated_region,color = Aggregated_region)) +
  scale_color_manual(values = pal_regions)+
  theme(
    # get rid of panel grids
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    # Change plot and panel background
    plot.background = element_rect(fill = back_col[1]),
    panel.background = element_rect(fill = back_col[1]),
    axis.title.x = element_blank(),
    axis.title.y = element_blank())

cd_fall <- ggplot()+
  stat_ecdf(data=fall, aes(x = zeroflowcentroiddateond, group=Aggregated_region,color = Aggregated_region)) +
  scale_color_manual(values = pal_regions)+
  theme(
    # get rid of panel grids
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    # Change plot and panel background
    plot.background = element_rect(fill = back_col[1]),
    panel.background = element_rect(fill = back_col[1]),
    axis.title.x = element_blank(),
    axis.title.y = element_blank())

cd_all <- ggplot()+
  stat_ecdf(data=all_season, aes(x = zeroflowcentroiddate, group=Aggregated_region,color = Aggregated_region)) +
  scale_color_manual(values = pal_regions)+
  theme(
    # get rid of panel grids
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    # Change plot and panel background
    plot.background = element_rect(fill = back_col[1]),
    panel.background = element_rect(fill = back_col[1]),
    axis.title.x = element_blank(),
    axis.title.y = element_blank())


######################### Fraction of noflow ######################### 

nff_winter <- ggplot()+
  stat_ecdf(data=winter, aes(x = jfmfractionnoflow, group=Aggregated_region,color = Aggregated_region)) +
  scale_color_manual(values = pal_regions)+
  ylab('No Flow\nFraction') +
  xlab('Winter')+
  theme(
    # get rid of panel grids
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    # Change plot and panel background
    plot.background = element_rect(fill = back_col[2]),
    panel.background = element_rect(fill = back_col[2]))


nff_spring <- ggplot()+
  stat_ecdf(data=spring, aes(x = amjfractionnoflow, group=Aggregated_region,color = Aggregated_region)) +
  scale_color_manual(values = pal_regions)+
  xlab('Spring')+
  theme(
    # get rid of panel grids
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    # Change plot and panel background
    plot.background = element_rect(fill = back_col[2]),
    panel.background = element_rect(fill = back_col[2]),
    axis.title.y = element_blank())


nff_summer <- ggplot()+
  stat_ecdf(data=summer, aes(x = jasfractionnoflow, group=Aggregated_region,color = Aggregated_region)) +
  scale_color_manual(values = pal_regions)+
  xlab('Summer')+
  theme(
    # get rid of panel grids
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    # Change plot and panel background
    plot.background = element_rect(fill = back_col[2]),
    panel.background = element_rect(fill = back_col[2]),
    axis.title.y = element_blank())


nff_fall <- ggplot()+
  stat_ecdf(data=fall, aes(x = ondfractionnoflow, group=Aggregated_region,color = Aggregated_region)) +
  scale_color_manual(values = pal_regions)+
  xlab('Fall')+
  theme(
    # get rid of panel grids
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    # Change plot and panel background
    plot.background = element_rect(fill = back_col[2]),
    panel.background = element_rect(fill = back_col[2]),
    axis.title.y = element_blank()) 

nff_all <- ggplot()+
  stat_ecdf(data=all_season, aes(x = annualfractionnoflow, group=Aggregated_region,color = Aggregated_region)) +
  scale_color_manual(values = pal_regions)+
  xlab('Annual')+
  theme(
    # get rid of panel grids
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    # Change plot and panel background
    plot.background = element_rect(fill = back_col[2]),
    panel.background = element_rect(fill = back_col[2]),
    axis.title.y = element_blank()) 

######################### Combine all plots######################### 
# t <-ggarrange(cd_winter, cd_spring, cd_summer, cd_fall, nff_winter, nff_spring, nff_summer, nff_fall,nrow=2,ncol=4,common.legend = TRUE,legend = "right")


patched <- 
  # de_winter + de_spring + de_summer + de_fall +
  dedur_winter + dedur_spring + dedur_summer + dedur_fall + dedur_all +
  p2z_winter + p2z_spring + p2z_summer + p2z_fall + p2z_all +
  dr_winter + dr_spring + dr_summer + dr_fall + dr_all +
  cd_winter + cd_spring + cd_summer + cd_fall + cd_all +
  nff_winter + nff_spring + nff_summer + nff_fall + nff_all +
  plot_layout(ncol = 5, guides = "collect")  & theme(legend.position = 'right') 

patched = patched + plot_annotation(
  title = "Figure 2: Distributions of Drying Metrics by Season and Ecoregion"
)


patched

