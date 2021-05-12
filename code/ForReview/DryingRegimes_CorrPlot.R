#####################################################################
##
## Script name: DryingRegimes_CorrPlot.R
##
## Author: Adam N. Price
##
## Date Created: 2020-10-02
##
## Copyright (c) Adam N. Price, 2020
## Email: adnprice@ucsc.edu
##
############################# Description ##########################
##
## 
##   
##
############################# Packages #############################
library(corrplot)
library(ggplot2)
library(tidyverse)
library(viridis)
############################# Code #################################

# Load Data

df = read.csv("data/kmeans_NoFreq.csv")



df = df %>%  select( lulc_water_prc,lulc_dev_prc,lulc_forest_prc,lulc_barren_prc,lulc_grass_prc,lulc_ag_prc,lulc_wetland_prc,
                     DRAIN_SQKM,SNOW_PCT_PRECIP,FRESHW_WITHDRAWAL,
                     AWCAVE,PERMAVE,CLAYAVE,SILTAVE,SANDAVE,
                     TOPWET.x,ELEV_MEAN_M_BASIN,
                     porosity,storage_m,
                     P_mm,PET_mm,SWE_mm,melt_mm,Tmax_C,
                     P_90,PET_90,Tmax_90,melt_90,
                     P.PET,P.PET90
)%>% 
  na.omit()

df  = df[is.finite(df$P.PET),]
df$PERMAVE = NULL
df$SANDAVE = NULL
df$P_mm = NULL
df$P_90 = NULL
df$PET_mm = NULL
df$PET_90 = NULL
df$melt_mm = NULL


get_lower_tri<-function(cormat){
  cormat[upper.tri(cormat)] <- NA
  return(cormat)
}
# Get upper triangle of the correlation matrix
get_upper_tri <- function(cormat){
  cormat[lower.tri(cormat)]<- NA
  return(cormat)
}

cormat <- round(cor(df),2)

upper_tri <- get_upper_tri(cormat)

########### Correlogram



cols = viridis(20, option = "magma")

t= corrplot(cormat, method="circle",type="upper",tl.col="black")

pdf('docs/response_plots/corrPlot.pdf')
t = corrplot(cormat, method="circle",type="upper",tl.col="black")
dev.off()

