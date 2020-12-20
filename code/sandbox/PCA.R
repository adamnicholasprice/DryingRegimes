library(corrplot)
library(prcomp)


tt = prcomp(dat.scale)
tt = get_pca_var(tt)

corrplot::corrplot(tt$cos2, is.corr=FALSE)
