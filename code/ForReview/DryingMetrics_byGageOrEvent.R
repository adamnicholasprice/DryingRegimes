#####################################################################
##
## Script name: DryingMetrics_byGageOrEvent.R
##
## Author: Adam N. Price
##
## Date Created: 2021-02-09
##
## Copyright (c) Adam N. Price, 2021
## Email: adnprice@ucsc.edu
##
############################# Description ##########################
##
## Grouping and plotting to see if random forest is predicting clusters
##   by event or by gage
##
############################# Packages #############################

library(tidyverse)
library(ggplot2)
library(sf)
library(maps)

############################# Code ################################

dat = read.csv("data/all_rf_data.csv") %>% 
  select(gage,kmeans,pred_class)%>%
  setNames(c("gage",'DryingCluster','PredDryingCluster'))

tt = dat %>% select(gage,DryingCluster,PredDryingCluster) %>% na.omit()

total = tt %>% 
  select(gage,DryingCluster,PredDryingCluster) %>%
  group_by(gage)%>%
  count() %>%
  setNames(c("gage","total"))

true.total = tt %>% 
  select(gage,DryingCluster,PredDryingCluster) %>%
  group_by(gage,DryingCluster)%>%
  count() %>%
  setNames(c("gage","DryingCluster","true.total")) %>%
  pivot_wider(names_from = DryingCluster,values_from=true.total)
# setNames(c("gage","true1","true2","true3",'true4'))

pred.total = tt %>% 
  select(gage,DryingCluster,PredDryingCluster) %>%
  group_by(gage,PredDryingCluster)%>%
  count() %>%
  setNames(c("gage","PredDryingCluster","pred.total"))%>%
  pivot_wider(names_from = PredDryingCluster,values_from=pred.total)

true = total %>%
  left_join(.,true.total,by="gage") %>%
  mutate(`1` = `1`/total,
         `2` = `2`/total,
         `3` = `3`/total,
         `4` = `4`/total
  )%>%
  select(gage,`1`,`2`,`3`,`4`)%>%
  pivot_longer(!gage,names_to = "cluster", values_to="true.prop")

pred = total %>%
  left_join(.,pred.total,by="gage") %>%
  mutate(`1` = `1`/total,
         `2` = `2`/total,
         `3` = `3`/total,
         `4` = `4`/total
  )%>%
  select(gage,`1`,`2`,`3`,`4`)%>%
  pivot_longer(!gage,names_to = "cluster", values_to="pred.prop")

prop = true %>%
  left_join(.,pred,by=c('gage','cluster'))%>%
  na.omit()

########## Plots
cols <- 
  c("1" = "#4477AA",
    "2" = "#66CCEE",
    "3" = "#228833",
    "4" = "#CCBB44")

# Scatter

scat = ggplot(prop,aes(x=true.prop,y=pred.prop,color=factor(cluster)),alpha=.7)+
  geom_point()+
  # geom_smooth(method = "lm", fill = NA,alpha=.5)+
  geom_abline(slope = 1,alpha=.5,intercept = 0)+
  scale_color_manual(values = cols)+
  xlab("True cluster membership proportion")+
  ylab("Predicted cluster membership proportion")+
  theme_bw()

pdf("docs/response_plots/trueVSpred.pdf")
scat
dev.off()
