#######################################################
# Author: Adam Price
# Date: 2020-01-24
# 
# Description: Parallel process to find first zero day and peak 
# of stream flow directly before
#
#
#
#######################################################
# https://cran.r-project.org/web/packages/foreach/vignettes/foreach.pdf
# https://stackoverflow.com/questions/29828710/parallel-processing-in-r-for-a-data-frame
#
#
#
#######################################################

# Define fucntions

parTest <- function(files) {
  peak2zero <- function(files){
    
    library(feather)
    library(tidyr)
    library(here)
    library(tidyverse)
    gauge = as.character(tools::file_path_sans_ext(basename(files)))
    data = read.csv(files)
    
    date = which(colnames(data)=='Date')
    q = which(colnames(data)=='X_00060_00003')
    
    
    
    ## remove NAs
    data = na.omit(data[,c(date,q)])
    data$Date = lubridate::as_date(data$Date)
    data$num.date = as.numeric(data$Date)
    
    
    ## Find the first occurance of a no-flow period
    nf_start = rle(data$X_00060_00003)
    
    end = cumsum(nf_start$lengths)
    start = c(1, lag(end)[-1] + 1)
    
    nf_start = start[which(nf_start$values==0)]
    
    
    
    ##-create an empty column for the slope forward and backward
    data$slp.b = rep(NA, length.out = nrow(data))
    data$slp.f = rep(NA, length.out = nrow(data))
    
    for(i in 2:(nrow(data)-1)){
      ##-calculate the slope back one day
      data$slp.b[i] = (data$X_00060_00003[i]-data$X_00060_00003[(i-1)])/(data$num.date[i]-data$num.date[(i-1)])
      ##-calculate the slope forward one day
      data$slp.f[i] = (data$X_00060_00003[(i+1)]-data$X_00060_00003[i])/(data$num.date[(i+1)]-data$num.date[i])
    }
    
    
    ##-make a column for the peak of each event flagged by a change in derivative
    data$peak.flag = rep(NA, length.out = nrow(data))
    
    
    ##-now flag those derivative changes
    for(i in 2:(nrow(data)-1)){
      ##-if the slope back is greater than some threshold and the slope forward is negative, flag it as a peak
      if(data$slp.b[i]>0.0001 & data$slp.f[i]<0){
        data$peak.flag[i] = 1 }
      else{
        ##-otherwise don't
        data$peak.flag[i] = -9999}
    }
    
    # Mark locations of start of no-flow events
    data$peak.flag[nf_start] = -1
    
    
    peak2zero = rle(data$peak.flag)
    
    end = cumsum(peak2zero$lengths)
    start = c(1, lag(end)[-1] + 1)
    
    peakstart = start[which(peak2zero$values==1)]
    
    
    
    tt= sort(append(nf_start,peakstart))
    
    peak2z = rep(NA, length.out = length(nf_start))
    
    for (i in 1:length(nf_start)){
      if (length(nf_start[i] - tt[which(tt==nf_start[i])-1])==0){
        next
      }
      else{
        peak2z[i] = nf_start[i] - tt[which(tt==nf_start[i])-1]
      }
    }
    peak2z = na.omit(peak2z)
    cv = sd(peak2z)/mean(peak2z)
    sd = sd(peak2z)
    mean = mean(peak2z)
    median = median(peak2z)
    num_period = length(peak2z)
    # out = c(gauge,mean,median,sd,cv)
    out = data.frame('gauge' = gauge,'num_period'=num_period,'mean'=mean,'median'=median,'sd'=sd,'cv'=cv)
    return(out)
  }
  
  
  
  
  out <- foreach(i = 1:length(files)) %dopar% {
    temp = peak2zero(files[i])
  }
  do.call('rbind', out)
}

#####################################
# Execute and write
library(doParallel)
library(foreach)

cl <- makeCluster(6)
registerDoParallel(cl)


files = list.files('../data/daily_data_with_ climate_and_PET/csv',full.names = TRUE)
p2z = parTest(files)
write.csv(p2z,'../data/peak2zero.csv')




