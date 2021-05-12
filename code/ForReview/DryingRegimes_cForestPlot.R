#####################################################################
##
## Script name: DryingRegimes_cForestPlot.R
##
## Author: Adam N. Price
##
## Date Created: 2021-05-09
##
## Copyright (c) Adam N. Price, 2021
## Email: adnprice@ucsc.edu
##
############################# Description ##########################
##
## 
##   
##
############################# Packages #############################
##
library(tidyverse)
library(ggplot2)
############################# Code ################################
a.df =  read.csv('data/cforest_Importance.csv') %>% 
  select(predictor,ImpCondPerm)%>% 
  top_n(10) %>%
  mutate(scaledImp = ImpCondPerm/max(ImpCondPerm))

all.df =  read.csv('data/cforest_Importance.csv') %>% 
  select(predictor,ImpCondPerm)%>% 
  mutate(scaledImp = ImpCondPerm/max(ImpCondPerm))




lulc.pred = c('lulc_water_prc','lulc_dev_prc','lulc_forest_prc','lulc_barren_prc','lulc_grass_prc','lulc_ag_prc','lulc_wetland_prc')

phys.pred = c('DRAIN_SQKM','GEOL_REEDBUSH_DOM','FRESHW_WITHDRAWAL',
              'AWCAVE','PERMAVE','CLAYAVE','SILTAVE','SANDAVE',
              'TOPWET','ELEV_MEAN_M_BASIN','porosity','storage_m')
clim.pred = c('P_mm','PET_mm','SWE_mm','melt_mm','Tmax_C',
              'P_90','PET_90','Tmax_90','melt_90',
              'P.PET','P.PET90','SNOW_PCT_PRECIP')

all.pred = c(lulc.pred,phys.pred,clim.pred)

a.df$Category = NA
a.df$Category[which(a.df$predictor %in% lulc.pred)] <- "Land use"
a.df$Category[which(a.df$predictor %in% clim.pred)] <- "Climate"
a.df$Category[which(a.df$predictor %in% phys.pred)] <- "Physiography"


all.df$Category = NA
all.df$Category[which(all.df$predictor %in% lulc.pred)] <- "Land use"
all.df$Category[which(all.df$predictor %in% clim.pred)] <- "Climate"
all.df$Category[which(all.df$predictor %in% phys.pred)] <- "Physiography"


cols = c("Climate" = "#e6194b",
         "Physiography"= "#0082c8",
         "Land use" = "#3cb44b")

fullname = 
  c('lulc_water_prc'	=	'Water',
    'lulc_dev_prc'	=	'Developed',
    'lulc_forest_prc'	=	'Forest',
    'lulc_barren_prc'	=	'Barren',
    'lulc_grass_prc'	=	'Grass',
    'lulc_ag_prc'	=	'Agriculture',
    'lulc_wetland_prc'	=	'Wetland',
    'DRAIN_SQKM'	=	'Drainage Area',
    'GEOL_REEDBUSH_DOM'	=	'Geology',
    'FRESHW_WITHDRAWAL'	=	'Freshwater Withdrawal',
    'AWCAVE'	=	'Aval. Water',
    'PERMAVE'	=	'Soil Perm.',
    'CLAYAVE'	=	'Soil Clay',
    'SILTAVE'	=	'Soil Silt',
    'SANDAVE'	=	'Soil Sand',
    'TOPWET'	=	'Topo. Wetness',
    'ELEV_MEAN_M_BASIN'	=	'Mean Basin Elev.',
    'porosity'	=	'Porosity',
    'storage_m'	=	'Storage',
    'P_mm'	=	'P',
    'PET_mm'	=	'PET',
    'SWE_mm'	=	'SWE',
    'melt_mm'	=	'Melt',
    'Tmax_C'	=	'Tmax',
    'P_90'	=	'P(90)',
    'PET_90'	=	'PET(90)',
    'Tmax_90'	=	'Tmax(90)',
    'melt_90'	=	'Melt(90)',
    'P.PET'	=	'P/PET',
    'P.PET90'	=	'P/PET(90)',
    'SNOW_PCT_PRECIP'	=	'Percent Snow')

pa = ggplot(a.df,aes(x = reorder(predictor,scaledImp),y = scaledImp,fill=Category)) +
  geom_bar(stat="identity", position="dodge")+
  coord_flip()+
  scale_fill_manual(values=cols) +
  ylab("Scaled variable importance")+
  xlab("Predictor variable")+
  scale_x_discrete(labels=fullname,expand = c(0,0))+
  scale_y_continuous(expand = c(0,0))+
  theme_classic()


pdf("docs/response_plots/cForest_VarImp.pdf")
pa
dev.off()



p.all = ggplot(all.df,aes(x = reorder(predictor,scaledImp),y = scaledImp,fill=Category)) +
  geom_bar(stat="identity", position="dodge")+
  coord_flip()+
  scale_fill_manual(values=cols) +
  ylab("Scaled variable importance")+
  xlab("Predictor variable")+
  scale_x_discrete(labels=fullname,expand = c(0,0))+
  scale_y_continuous(expand = c(0,0))+
  theme_classic()


pdf("docs/response_plots/cForest_VarImp_ALL.pdf")
p.all
dev.off()

