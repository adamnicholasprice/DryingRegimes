#####################################################################
##
## Script name:
##
## Author: Adam N. Price
##
## Date Created: 2021-05-03
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
## 
library(tidyverse)
library(ggplot2)
##
############################# Code ################################

df = read.csv("data/kmeans_NoFreq.csv")

df_class = read.csv("data/kmeans.csv") %>%
  select("gage","CLASS") %>%
  unique()

# df = df %>% select(gage,kmeans) %>% 
#   unique() %>% 
#   mutate(k.ones = 1) %>% 
#   pivot_wider(names_from = kmeans,values_from = k.ones,values_fill=0) %>%
#   setNames(c("gage","c1",'c2','c3','c4')) %>%
#   rowwise(gage)%>%
#   mutate(k.sums = sum(c(c1,c2,c3,c4)))%>% 
#   left_join(.,df_class,by = "gage")%>%
#   select(k.sums,CLASS)

df = df %>% select(gage,kmeans) %>% 
    unique() %>%
    left_join(.,df_class,by = "gage")

df$kmeans = as.factor(df$kmeans)
df$gage = NULL

tt = rwrfhydro::gages2Attr %>% select(CLASS)
tt$kmeans = "GAGES-II"

df = rbind(df,tt) 

yy <- df %>%
  group_by(kmeans, CLASS) %>%
  summarise(count = n()) %>%
  group_by(kmeans) %>%
  mutate(per=count/sum(count)) %>% 
  ungroup()

### Plot
cols <- 
  c("1" = "#4477AA",
    "2" = "#66CCEE",
    "3" = "#228833",
    "4" = "#CCBB44",
    "GAGES-II" = "#EE6677")

test  = ggplot(yy)+
  geom_bar(aes(x = kmeans,y = per,fill=paste(kmeans),alpha = factor(CLASS)),position = "fill",stat = "identity")+
  scale_fill_manual(values=cols)+
  theme_minimal()

pdf("docs/response_plots/gageProp2.pdf")
test
dev.off()
