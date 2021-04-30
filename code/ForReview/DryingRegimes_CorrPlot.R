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
# yy = cormat

cormat = yy
cormat[abs(cormat)>=0.7] = 1


cols = viridis(20, option = "magma")

cols = c(cols,"1"="#0000FF")
# t = corrplot(cormat, method="circle",type="upper",tl.col="black")
t = corrplot(cormat, method="square",type="upper",tl.col="black",col=cols)

pdf('docs/corrPlot.pdf')
t
dev.off()





# # Heatmap

# melted_cormat <- melt(upper_tri, na.rm = TRUE)
# ggheatmap <- ggplot(melted_cormat, aes(Var2, Var1, fill = value))+
#   geom_tile(color = "white")+
#   scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
#                        midpoint = 0, limit = c(-1,1), space = "Lab", 
#                        name="Pearson\nCorrelation") +
#   theme_minimal()+ # minimal theme
#   theme(axis.text.x = element_text(angle = 45, vjust = 1, 
#                                    size = 12, hjust = 1))+
#   coord_fixed()
# 
# ggheatmap = ggheatmap + 
#   geom_text(aes(Var2, Var1, label = value), color = "black", size = 1) +
#   theme(
#     axis.title.x = element_blank(),
#     axis.title.y = element_blank(),
#     panel.grid.major = element_blank(),
#     panel.border = element_blank(),
#     panel.background = element_blank(),
#     axis.ticks = element_blank(),
#     legend.justification = c(1, 0),
#     legend.position = c(0.6, 0.7),
#     legend.direction = "horizontal")+
#   guides(fill = guide_colorbar(barwidth = 7, barheight = 1,
#                                title.position = "top", title.hjust = 0.5))
# 
# pdf('docs/corrPlot.pdf')
# ggheatmap
# dev.off()
