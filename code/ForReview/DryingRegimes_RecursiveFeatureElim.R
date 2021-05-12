#####################################################################
##
## Script name: DryingRegimes_RecursiveFeatureElim.R
##
## Author: Adam N. Price
##
## Date Created: 2021-05-04
##
## Copyright (c) Adam N. Price, 2021
## Email: adnprice@ucsc.edu
##
############################# Description ##########################
##
## 
##   https://towardsdatascience.com/effective-feature-selection-recursive-feature-elimination-using-r-148ff998e4f7
##
############################# Packages #############################
library(tidyverse)
library(caret) 
library(randomForest)   
library(tidymodels)
library(patchwork)
##
############################# Code ################################
df = read.csv("data/kmeans_NoFreq.csv")


#######################################################
################# Step: set up predictiors      
#######################################################


metrics <- "kmeans"

# all possible predictors
lulc.pred = c('lulc_water_prc','lulc_dev_prc','lulc_forest_prc','lulc_barren_prc','lulc_grass_prc','lulc_ag_prc','lulc_wetland_prc')

phys.pred = c('DRAIN_SQKM','GEOL_REEDBUSH_DOM','FRESHW_WITHDRAWAL',
              'AWCAVE','CLAYAVE','SILTAVE',
              'TOPWET','ELEV_MEAN_M_BASIN','porosity','storage_m')
clim.pred = c('SWE_mm','melt_mm','Tmax_C',
              'Tmax_90','melt_90',
              'P.PET','P.PET90','SNOW_PCT_PRECIP')

predictors_all = c(lulc.pred,phys.pred,clim.pred)


#######################################################
################# Step: Select all predictors and filter data
#######################################################

df = df %>% 
  rename(TOPWET = TOPWET.x,
         index = X) %>% 
  select("index","gage","kmeans",all_of(predictors_all)) %>%
  subset(complete.cases(.)) %>%
  subset(is.finite(P.PET))

df$GEOL_REEDBUSH_DOM = as.factor(df$GEOL_REEDBUSH_DOM)
df$kmeans = as.factor(df$kmeans)



#######################################################
################# Step: Scale data
#######################################################

data <- df %>% select(-index,-gage) %>%
  # Center and scale numeric features
  mutate_if(is.numeric, scale)


#######################################################
################# Step: Split data
#######################################################

x <- data %>%
  select(-kmeans) %>%
  as.data.frame()

# Target variable
y <- data$kmeans

# Training: 80%; Test: 20%
set.seed(42)
inTrain <- createDataPartition(y, p = .80, list = FALSE)[,1]

x_train <- x[ inTrain, ]
x_test  <- x[-inTrain, ]

y_train <- y[ inTrain]
y_test  <- y[-inTrain]

#######################################################
################# Step: Run RFE
#######################################################
library(doParallel) 
cl <- makeCluster(detectCores()-1, type='PSOCK',outfile="")
registerDoParallel(cl)

# Define the control using a random forest selection function
control <- rfeControl(functions = rfFuncs, # random forest
                      method = "repeatedcv", # repeated cv
                      repeats = 3, # number of repeats
                      number = 10,
                      allowParallel = TRUE) # number of folds
subsets = c(1:25)

result_rfe1 <- rfe(x = x_train, 
                   y = y_train, 
                   sizes = subsets,
                   rfeControl = control)

# Print the results
result_rfe1 

save(result_rfe1,file = "data/RFE.RData")

# Print the selected features
predictors(result_rfe1) %>%
  as.data.frame() %>%
  write_csv("data/rfe_predictors.csv")

kill
# Print the results visually
alp = ggplot(data = result_rfe1, metric = "Accuracy") + theme_bw()
kap = ggplot(data = result_rfe1, metric = "Kappa") + theme_bw()

num = alp + kap

pdf("docs/response_plots/rfe_AlphaKappa.pdf")
num
dev.off()

varimp_data <- data.frame(predictor = row.names(varImp(result_rfe1))[1:24],
                          importance = varImp(result_rfe1)[1:24, 1])


#### Make color palettes
varimp_data$Category = NA
varimp_data$Category[which(varimp_data$predictor %in% lulc.pred)] <- "Land use"
varimp_data$Category[which(varimp_data$predictor %in% clim.pred)] <- "Climate"
varimp_data$Category[which(varimp_data$predictor %in% phys.pred)] <- "Physiography"

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

impPlot = ggplot(data = varimp_data, 
       aes(x = reorder(predictor, importance), y = importance, fill = Category)) +
  geom_bar(stat="identity") + 
  coord_flip()+
  labs(x = "Predictors", y = "Variable Importance") + 
  scale_x_discrete(labels=fullname,expand = c(0,0))+
  scale_fill_manual(values=cols) +
  # geom_text(aes(label = round(importance, 2)), vjust=1.6, color="white", size=4) + 
  theme_bw() + 
  theme(legend.position = "none")

pdf("docs/response_plots/rfe_ImpPlot.pdf")
impPlot
dev.off()
