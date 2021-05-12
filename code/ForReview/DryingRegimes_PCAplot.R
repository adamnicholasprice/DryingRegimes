#####################################################################
##
## Script name: DryingRegimes_PCAplot.R
##
## Author: Adam N. Price
##
## Date Created: 2021-05-10
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
##   
##
############################# Code ################################

df = read.csv("data/kmeans_NoFreq.csv")

dat.scale <- df %>% 
  #Select vars of interest
  select("peak2zero","drying_rate"
         , "dry_dur",
         "peak_quantile") %>%
  mutate(peak2zero = (peak2zero-mean(peak2zero))/sd(peak2zero),
         drying_rate = (drying_rate-mean(drying_rate))/sd(drying_rate),
         dry_dur = (dry_dur-mean(dry_dur))/sd(dry_dur),
         peak_quantile = (peak_quantile-mean(peak_quantile))/sd(peak_quantile))


tt = prcomp(dat.scale,center=T,scale=T)
cols <- 
  c("1" = "#4477AA",
    "2" = "#66CCEE",
    "3" = "#228833",
    "4" = "#CCBB44")

comps = as.data.frame(tt$x)
loads =  data.frame(Variables = rownames(tt$rotation), tt$rotation)


newdf <- cbind(df$kmeans,comps[,c(1,2)]) %>%
  setNames(c("cluster","PC1","PC2"))

p = ggplot() +
  geom_point(data = newdf, aes(x=PC1, y=PC2, fill = factor(cluster)),shape=21, col="black")+
  scale_fill_manual(values = cols)

p = p + geom_segment(data = loads, 
                     aes(x=0,y=0,xend=PC1*20,yend=PC2*20),
                     arrow = arrow(length = unit(1/2, "picas")))+ 
  annotate("text", x = (loads$PC1*15), y = (loads$PC2*15),
           label = loads$Variables)+
  theme_bw()


pdf("docs/response_plots/PCA.pdf")
p
dev.off()
