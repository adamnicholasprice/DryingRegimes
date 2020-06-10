#===================================================================================#
#-----------------------------------------------------------------------------------#
# This script is for preparing data to be modeled using a random forest technique   #
# an accompanying word document that details the approach is saved in               #
# 03_DenitMapping_PNAS/03_OutlineDrafts/RF Approach. The data used for the model    #
# training and testing are a combination of data from various infiltration          #
# experiments done in the lab, field and under MAR operations from several different# 
# field sites. This script is used to develop a model for nitrate removal during    #
# infiltration at these sites. For this script the input data are restricted to     #
# "untreated" infiltration i.e. native soil treatments in which no carbon           #
# amendment was added.                                                              #
#-----------------------------------------------------------------------------------#
# GG
# 11/7/2019
#-----------------------------------------------------------------------------------#
#===================================================================================#

#===================================================================================#
#####INSTALL PACKAGES#####
#install.packages('tidyverse')
library(tidyverse)
#install.packages('caret')
library(caret)
#install.packages('randomForest')
library(randomForest)
#install.packages('RcolorBrewer')
library(RColorBrewer)
#install.packages('rsample')
library(rsample)
#install.packages('foreign')
library(foreign)
#install.packages('rgdal')
library(rgdal)
#install.packages('Metrics')
library(Metrics)
#install.packages('plm')
library(plm)
#####
#===================================================================================#

#===================================================================================#
#####READ IN THE DATA#####
setwd('/Volumes/GoogleDrive/My Drive/UCSC Google Drive/Projects/DenitMapping/')
setwd('J://My Drive/UCSC Google Drive/Projects/DenitMapping/')
denit.data <- read.csv('FluidChem/AllData.csv', header = T, 
                       stringsAsFactors = F) %>%
  as_tibble()

#select the data you want and clean it up
denit.data <- denit.data[,c('Location','Experiment','IR.mday','Surface.NO3','Depth.NO3','Surface.DOC','Depth.DOC','Depth.cm','C','N','Pct.Clay','Pct.Silt','Pct.Sand')]

#calculate the amount removed and the fraction removed
denit.data$NO3.Amt.Rmv <- denit.data$Surface.NO3 - denit.data$Depth.NO3
denit.data$NO3.Frac.Rmv <- denit.data$NO3.Amt.Rmv/denit.data$Surface.NO3

#clean up nitrate additions because our response varible is
#log(no3 removed) so any additions are recorded as near-zero removals
denit.data <- denit.data[!is.na(denit.data$NO3.Frac.Rmv),]
#denit.data[denit.data$NO3.Frac.Rmv <= 0, ]$NO3.Amt.Rmv <- 0.00000001

#some data have no Surface.DOC data, for those observations we will use
#the depth DOC data as a proxy for surface
denit.data[is.na(denit.data$Surface.DOC),'Surface.DOC'] <- denit.data[is.na(denit.data$Surface.DOC),'Depth.DOC']

#some of the observations don't have either one so they must be removed
denit.data <- denit.data[!is.na(denit.data$Surface.DOC),]

#remove any data with a negative concentration
denit.data <- denit.data[denit.data$Surface.NO3 > 0,]

#Add soil RT as a predictor variabla
denit.data$Soil.RT <- (denit.data$Depth.cm/10)/denit.data$IR.mday

#add type to the data frame
denit.data$type <- denit.data$Experiment
denit.data[grepl('Perc',denit.data$type),'type'] <- 'Perc'
denit.data[grepl('MAR',denit.data$type),'type'] <- 'MAR'
denit.data <- denit.data[!is.na(denit.data$type),]
denit.data$type <- as.factor(denit.data$type)



#####
#===================================================================================#
#####VARIABLE SELECTION NOTE
# What I found in doing variable selection is that the dataset is not as sensitive to the
# training and testing split as I had thought. Most of this analysis was done in the RFApproachNSData_191107.R
# file. So instead of going through a loop and aggregating over the variable importance data, I am just going to 
# grow the model and move on from there. See the word doc RF Approach.doc for explanation on final variable selection
#===================================================================================#
#####SPLIT DATA INTO CALIB AND VALID SETS AND PLOT#####
#set seed for reproducibility
set.seed(125)
denit.data$index <- seq(1:nrow(denit.data))
training <- as.data.frame(denit.data[sample(1:nrow(denit.data),132, replace = F),])
testing <- as.data.frame(denit.data[!denit.data$index %in% training$index,])
#visulaize the split
par(mfrow = c(2,2))
hist(denit.data$NO3.Amt.Rmv, ylim = c(0,70), las = 1, breaks = c(seq(-5,9,0.5)), col = '#fc8d62', xlab = 'NO3 Removed (mg/L)', main = '', ylab = '# of Observations', xlim = c(-1,4))
par(new = T)
hist(training$NO3.Amt.Rmv, ylim = c(0,70), xlab = '', ylab = '', axes = F, breaks = c(seq(-5,9,0.5)), col = '#8da0cb', main = '', xlim = c(-1,4))
legend('topright', col = c('#8da0cb', '#fc8d62'), pch = 15, legend = c('Training (n = 132)','Testing (n = 45)'), bty = 'n', cex = 1.5)

hist(denit.data$Soil.RT, ylim = c(0,70), las = 1, breaks = c(seq(0,90,5)), col = '#fc8d62', xlab = 'Soil RT (days)', main = '', ylab = '# of Observations', xlim = c(0,70))
par(new = T)
hist(training$Soil.RT, ylim = c(0,70), xlab = '', ylab = '', axes = F, breaks = c(seq(0,90,5)), col = '#8da0cb', main = '', xlim = c(0,70))
legend('topright', col = c('#8da0cb', '#fc8d62'), pch = 15, legend = c('Training (n = 132)','Testing (n = 45)'), bty = 'n', cex = 1.5)

#####
#===================================================================================#

#===================================================================================#
#####CHOOSE METAPARAMETERS WITH CROSSFOLD VALIDATION THEN GROW RF#####
trainControl <- trainControl(method = "oob", number = 1, returnResamp = 'all', verboseIter=F)
# use cross validation to determine what the best value for mtry is 
# lm.n.depth.rf <- train(NO3.Amt.Rmv ~ Surface.NO3+C+Soil.RT+Pct.Clay, data = training,
#                        method = "rf",
#                        trControl = trainControl, tuneLength = 10
# )
# lm.n.depth.rf

#build the model
rf <- randomForest(NO3.Amt.Rmv ~ Surface.NO3+C+Soil.RT+Pct.Clay+Pct.Silt+Pct.Sand+type, data = training, mtry = 3,importance = T)

#predict the result of the model
testing$NO3.Amt.Rmv.rf <- predict(rf, testing)
training$NO3.Amt.Rmv.rf <- predict(rf, training)
#get error estimates
#rmse
rmse.test.rf <- round(rmse(testing$NO3.Amt.Rmv, testing$NO3.Amt.Rmv.rf), digits = 2)
rmse.train.rf <- round(rmse(training$NO3.Amt.Rmv, training$NO3.Amt.Rmv.rf), digits = 2)
rmse.oob.rf <- round(rmse(rf$predicted, training$NO3.Amt.Rmv), digits = 2)
#rsquared
lm.test <- lm(testing$NO3.Amt.Rmv.rf~testing$NO3.Amt.Rmv) %>% summary()
lm.test.r2 <- lm.test$r.squared
lm.train <- lm(training$NO3.Amt.Rmv.rf~training$NO3.Amt.Rmv) %>% summary()
lm.train.r2 <- lm.train$r.squared
lm.oob <- lm(rf$predicted~training$NO3.Amt.Rmv) %>% summary()
lm.oob.r2 <- lm.oob$r.squared
#fill the error matrix
error <- c(rmse.train.rf,rmse.oob.rf,rmse.test.rf,lm.train.r2,lm.oob.r2,lm.test.r2)
#####
#===================================================================================#

#===================================================================================#
#####PLOT THE RESULTS#####
#this plots the training data against the out of bag perdictions for the training data
plot(rf$y, rf$predicted, 
     ylim = c(-1,4), xlim = c(-1,4), pch = 21, bg = 'white', col = 'white', typ = 'n',
     ylab = 'Modeled N Removed (mg/L)', xlab = 'Measured N Removed (mg/L)', las = 1, main = 'Calibration/Validataion')
abline(0,1, col = 'red', lty = 2)
#training measured data vs. oob predictions
points(training$NO3.Amt.Rmv, rf$predicted, pch = 21, bg = '#8da0cb', col = 'black', cex = 1.4)
#training measured data vs. modeled training data
points(training$NO3.Amt.Rmv, training$NO3.Amt.Rmv.rf, pch = 21, bg = '#66c2a5', col = 'black', cex = 1.4)
#testing measured data vs. modeled testing data
points(testing$NO3.Amt.Rmv, testing$NO3.Amt.Rmv.rf, pch = 21, bg = '#fc8d62', col = 'black', cex = 1.4)
legend('topleft', text.col = c('#7570b3','#1b9e77','#d95f02'), legend = c(paste('oob prediction',rmse.oob.rf), paste('training',rmse.train.rf), paste('testing',rmse.test.rf)), bty = 'n',title = 'RMSE')

#plot the residuals
plot(rf$y, rf$y-rf$predicted, ylim = c(-2,2), xlim = c(-1,4), pch = 21, bg = 'white', col = 'white', typ = 'n',
     ylab = 'Measured - Modeled N Removed (mg/L)', xlab = 'Measured N Removed (mg/L)', las = 1, main = 'Residual Plot')
abline(0,0, col = 'red', lty = 2)
#training measured data vs. measured - oob modeled
points(rf$y, rf$y-rf$predicted, pch = 21, bg = '#8da0cb', col = 'black', cex = 1.4)
#training measured data vs. measured-modeled
points(rf$y, rf$y-training$NO3.Amt.Rmv.rf, pch = 21, bg = '#66c2a5', col = 'black', cex = 1.4)
#testing training vs training measured - training modeled
points(testing$NO3.Amt.Rmv, testing$NO3.Amt.Rmv-testing$NO3.Amt.Rmv.rf, pch = 21, bg = '#fc8d62', col = 'black', cex = 1.4)
legend('topleft', text.col = c('#7570b3','#1b9e77','#d95f02'), legend = c(paste('oob prediction',rmse.oob.rf), paste('training',rmse.train.rf), paste('testing',rmse.test.rf)), bty = 'n',title = 'RMSE')

#make the variable importance plot
varImpPlot(rf)
#look at the values of variable importance
importance(rf)


#look at the variable importance for the linear and the log transformed data


#make a boxplot for both log and linear
#linear
m <- rbind(c(1,2), c(1,3))
layout(m)
par(mar = c(6,4,4,4), omi = c(0,0.5,0,0))
boxplot(v.imp.lin[8,]^0.5, v.imp.lin[2,]^0.5,v.imp.lin[3,]^0.5, v.imp.lin[1,]^0.5, v.imp.lin[6,]^0.5, v.imp.lin[4,]^0.5, v.imp.lin[7,]^0.5, v.imp.lin[5,]^0.5,  horizontal = T,
        names = c('Exp Type','Surface.DOC','C','Surface.NO3','Pct.Clay','Soil.RT','Pct.Silt','Pct.Sand'), las = 1,
        xlab = 'Average Variable Importance 1000 iterations \n Increase in RMSE', main = 'Random Forest Variable Importance Plot')
#boxplot of errors
boxplot(error.lin[,1:3], col = c('#1b9e77','#7570b3','#d95f02'), names = c('Train','OOB','Test'), main = 'RMSE \n 1000 Iterations', las = 2, ylab = 'RMSE')
boxplot(error.lin[,4:6], col = c('#1b9e77','#7570b3','#d95f02'), names = c('Train','OOB','Test'), main = 'R2 \n 1000 Iterations', las = 2, ylab = 'R2')


