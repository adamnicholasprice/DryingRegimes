install.packages('randomForestExplainer')

library(randomForestExplainer)
# https://cran.r-project.org/web/packages/randomForestExplainer/vignettes/randomForestExplainer.html
measure_importance(f_output[[1]]$rf)




w_Imp = w_output %>% sapply("[[", "rf_importance") %>% rowMeans() %>%as.data.frame()
s_Imp = s_output %>% sapply("[[", "rf_importance") %>% rowMeans()%>% as.data.frame()
su_Imp = su_output %>% sapply("[[", "rf_importance") %>% rowMeans()%>% as.data.frame()
f_Imp = f_output %>% sapply("[[", "rf_importance") %>% rowMeans()%>% as.data.frame()

tt = t(cbind(w_Imp,s_Imp,su_Imp,f_Imp))


rownames(tt) <- c('Winter','Spring','Summer','Fall')

tt = tt[ ,order(colMeans(tt)) ]

library(reshape2)
library(ggplot2)
library(tidyverse)

vImp <- ggplot(melt(tt), aes(x=value, y=variable)) + geom_point()




vImp <- ggplot(melt(tt), aes(x=Var1, y=Var2,fill = value)) + 
  geom_raster()


vImp <- vImp + scale_fill_viridis(discrete = FALSE,name="Variable\n Importance") + xlab('Season') + ylab('Predictor Variable') + ggtitle('No Flow Fraction Variable Importance')


vImp

f_output[[1]]$rf$forest$nodepred
