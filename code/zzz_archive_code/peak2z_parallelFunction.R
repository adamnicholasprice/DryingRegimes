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
    
    peak.threshold <- quantile(data$X_00060_00003, 0.5) # define the peak threshold to avoid regarding small rises during low flows as peaks.
    
    for(i in 2:(nrow(data)-1)){
      ##-if the slope back is greater than some threshold and the slope forward is negative, flag it as a peak
      if(data$slp.b[i]>0.0001 & data$slp.f[i]<0 & data$X_00060_00003[i] >= peak.threshold){
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
      if(which(tt==nf_start[i]) == 1){
          next
      }
      if ((length(nf_start[i] - tt[which(tt==nf_start[i])-1]) > 0) & (tt[which(tt==nf_start[i])-1] %in% peakstart)){
          peak2z[i] = nf_start[i] - tt[which(tt==nf_start[i])-1]
      }
    }
    
    nf_start = nf_start[!is.na(peak2z)]
    peak2z = na.omit(peak2z)
    
    total = cbind(data[nf_start,c('Date','X_00060_00003')],peak2z)
    colnames(total) = c('date','X_00060_00003','peak2z_length')
    
    write_csv(total,paste0('../data/peak2z/',gauge,'_peak2z.csv'))
  }

#####################################
# Execute and write
library(doParallel)
library(foreach)

## Get list of files

files = list.files('../data/daily_data_with_ climate_and_PET/csv',full.names = TRUE)

# get number of cores, start cluster and load packages on cores.
cores = parallel::detectCores()-1
cl <- parallel::makeCluster(cores)


# Use mpapply to exicute function
parallel::parLapply(cl,files,peak2zero)


# Stop the cluster
parallel::stopCluster(cl)



