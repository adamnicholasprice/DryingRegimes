# This is an edit

library(tidyverse)

dat = read.csv("data/dryingRegimes_data_RF.csv")

ecoDrain = dat %>% select(AggEcoregion,DRAIN_SQKM) %>%
  group_by(AggEcoregion)


pal_regions <- 
  c("Eastern Forests" = "#009E73",
    "Mediterranean California" = "#F0E442",
    "Northern Great Plains" = "#0072B2",
    "Southern Great Plains" = "#E69F00",
    "Western Deserts" = "#D55E00",
    "Western Mountains" = "#56B4E9")


ggplot(data=ecoDrain)+
  geom_boxplot(aes(x=factor(AggEcoregion),y=DRAIN_SQKM,group=factor(AggEcoregion),fill = factor(AggEcoregion)),outlier.colour = NA)+
  geom_abline(slope=0,intercept =500)+
  scale_fill_manual(values = pal_regions,"EPA L1 aggregated ecoregion")+
  ylim(0,7000)+
  theme_light()


v = ggplot(data=ecoDrain)+
  geom_violin(aes(x=factor(AggEcoregion),y=DRAIN_SQKM,group=factor(AggEcoregion),fill = factor(AggEcoregion)),draw_quantiles = c(0.5))+
  geom_abline(slope=0,intercept = log10(500))+
  scale_fill_manual(values = pal_regions,"EPA L1 aggregated ecoregion")+
  # ylim(0,7000)+
  scale_y_log10()+
  xlab("EPA L1 aggregated ecoregion")+
  ylab("log10(Drainage area (km2)")+
  theme_light()+
  annotation_logticks() 

pdf("docs/drainageArea.pdf")
v
dev.off()


