library(corrplot)
library(factoextra)

dat = read.csv('data/kmeans.csv') %>%
  select("peak2zero","drying_rate", 
         "dry_dur","peak_quantile", 
         "freq_local") %>% 
  #scale vars
  scale()



tt = prcomp(dat)
tt = get_pca_var(tt)

corrplot::corrplot(tt$cos2, is.corr=FALSE)

plot(tt)
