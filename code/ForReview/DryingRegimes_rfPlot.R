#####################################################################
##
## Script name: DryingRegimes_rfPlot.r
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
library(tidyverse)
library(ggplot2)
library(patchwork)
############################# Code ################################

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Step: Variable Importance
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

a.df =  read.csv('data/all_rf_importance.csv') %>% 
  select(predictor,MeanDecreaseAccuracy)%>% 
  top_n(10) %>%
  mutate(scaledImp = MeanDecreaseAccuracy/max(MeanDecreaseAccuracy))

all.df =  read.csv('data/all_rf_importance.csv') %>% 
  select(predictor,MeanDecreaseAccuracy)%>% 
  mutate(scaledImp = MeanDecreaseAccuracy/max(MeanDecreaseAccuracy))


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

pdf("docs/response_plots/randomForest_VarImp.pdf")
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

pdf("docs/response_plots/randomForest_VarImp_ALL.pdf")
p.all
dev.off()



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Step: Confusion Matrix
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

dat = read.csv('data/all_confusionMetrics.csv')

dat = dat %>%
  group_by(Reference) %>%
  mutate(ref.Tot= sum(Freq)) %>%
  mutate(freq.Perc = Freq/ref.Tot) %>%
  group_by(Prediction) %>%
  mutate(pred.Tot= sum(Freq),
         pred.Perc = Freq/pred.Tot)


mets = dat %>%
  group_by(Reference) %>%
  mutate(Sensitivity = max(Freq)/ref.Tot) %>%
  group_by(Prediction) %>%
  mutate(Specificity= Freq/sum(Freq))

mets = 
  dat[dat[,"Prediction"]==dat[,"Reference"],]


confusion.mat = ggplot(data=dat)+
  geom_tile(aes(x= Reference,y = Prediction,fill=freq.Perc))+
  # scale_fill_gradient(low="#0082c8",
  #                     high="#e6194b")+
  scale_fill_viridis_c(option = "viridis")+
  geom_text(aes(x= Reference,y = Prediction,label=Freq), color="white") +
  geom_text(data=mets,aes(x= Reference,y = Prediction+.1,label=paste0("(",round(freq.Perc,2)," / ",round(pred.Perc,2),")")), color="white")+
  scale_y_continuous(trans = "reverse",name = "Predicted cluster",expand=c(0,0),label=c("Clust 1","Clust 2","Clust 3","Clust 4"),breaks=c(1,2,3,4))+
  scale_x_continuous(position = "top",name = "Actual cluster",expand=c(0,0),label=c("Clust 1","Clust 2","Clust 3","Clust 4"),breaks=c(1,2,3,4))+
  theme(panel.background = element_rect(
    fill = NA,
    colour = "black",
    size = 1),
    axis.line = element_line(colour = "black"),
    text=element_text(size=12),
    axis.title = element_text(size = 14))



pdf("docs/response_plots/randomForest_confusionMatrixViridis.pdf")
confusion.mat
dev.off()

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Step Partial dependance plots
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


df = read.csv('data/all_PDP.csv')
top10 = unique(a.df$predictor)

df = df[which(df$predictor %in% top10),]
a.df$rank = rank(a.df$scaledImp)
df = df %>% left_join(.,a.df,by="predictor")


cols <- 
  c("1" = "#4477AA",
    "2" = "#66CCEE",
    "3" = "#228833",
    "4" = "#CCBB44")


pdp = ggplot(df)+
  geom_line(aes(x=value,y=yhat,color=factor(class)))+
  # geom_point(aes(x=value,y=yhat,color=factor(class)))+
  theme_minimal()+
  scale_color_manual(
    values=cols, 
    name = "Cluster") +
  facet_wrap(~-rank+predictor,scales="free",nrow=2,
             labeller = labeller(rank=NULL,predictor=fullname))


pdf("docs/response_plots/PDP_allTop10.pdf")
pdp
dev.off()

### all PDP
df = read.csv('data/all_PDP.csv')

all.df$rank = rank(all.df$scaledImp)
df = df %>% left_join(.,all.df,by="predictor")


cols <- 
  c("1" = "#4477AA",
    "2" = "#66CCEE",
    "3" = "#228833",
    "4" = "#CCBB44")


pdp = ggplot(df)+
  geom_line(aes(x=value,y=yhat,color=factor(class)))+
  geom_point(aes(x=value,y=yhat,color=factor(class)))+
  # annotate("rect",xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf, fill = df$Category,alpha=.2)+
  theme_minimal()+
  scale_color_manual(
    values=cols, 
    name = "Cluster") +
  facet_wrap(~Category+predictor+(-round(scaledImp,2)),scales="free",nrow=5,
             labeller = labeller(rank=NULL,predictor=fullname))

pdf("docs/response_plots/PDP_all.pdf")
pdp
dev.off()
# https://bgreenwell.github.io/pdp/articles/pdp.html