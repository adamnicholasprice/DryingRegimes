#####################################################################
##
## Script name: 
##
## Author: Adam N. Price
##
## Date Created: 2020-04-17
##
## Copyright (c) Adam N. Price, 2020
## Email: adnprice@ucsc.edu
##
############################# Description ##########################
##
## Devide pead2zero metric into seasons.
##   
##
############################# Packages #############################

library(lubridate)
library(dplyr)
library(doParallel)
library(foreach)

############################# Code ################################

# Functions

statCalc <- function(x){
  output = c(
    mean(x,na.rm = TRUE),
    length(x),
    max(x),
    min(x),
    sd(x,na.rm = TRUE)/mean(x,na.rm = TRUE),
    sd(x,na.rm = TRUE)
  )
  return(output)
}

readP2Z <- function(file){
  library(lubridate)
  library(dplyr)
  statCalc <- function(x){
    output = c(
      mean(x,na.rm = TRUE),
      length(x),
      max(x),
      min(x),
      sd(x,na.rm = TRUE)/mean(x,na.rm = TRUE),
      sd(x,na.rm = TRUE)
    )
    return(output)
  }
  data = read.csv(file)
  site  = strsplit(file,split = '/|_')[[1]][4]
  
  winter = c(data[between(lubridate::month(data$date),12,13),]$peak2z_length,data[between(lubridate::month(data$date),1,2),]$peak2z_length)
  spring = data[between(lubridate::month(data$date),3,5),]$peak2z_length
  summer = data[between(lubridate::month(data$date),6,8),]$peak2z_length
  fall = data[between(lubridate::month(data$date),9,11),]$peak2z_length
  
  temp = data.frame(c(site,statCalc(winter),statCalc(spring),statCalc(summer),statCalc(fall)))
  return(temp)
}

##### Run Functions

files = list.files('../data/peak2z',full.names = TRUE,pattern = '*csv')


# get number of cores, start cluster and load packages on cores.
cores = parallel::detectCores()-1
cl <- parallel::makeCluster(cores)

output  = data.frame(matrix(NA,nrow = length(files),ncol=26))

# Use mpapply to exicute function
output = foreach(i = 1:length(files), .combine=rbind) %dopar%
  as.data.frame(t(readP2Z(files[i])))


colnames(output) <- c("site_no"
                  ,'p2z_mean_djf','p2z_count_djf','p2z_max_djf','p2z_min_djf','p2z_cv_djf','p2z_sd_djf'
                  ,'p2z_mean_mam','p2z_count_mam','p2z_max_mam','p2z_min_mam','p2z_cv_mam','p2z_sd_mam'
                  ,'p2z_mean_jja','p2z_count_jja','p2z_max_jja','p2z_min_jja','p2z_cv_jja','p2z_sd_jja'
                  ,'p2z_mean_son','p2z_count_son','p2z_max_son','p2z_min_son','p2z_cv_son','p2z_sd_son')


# Stop the cluster
parallel::stopCluster(cl)

# Clean up the data
output[output == "NaN"] <- NA
output[output == "-Inf"] <- NA
output[output == "Inf"] <- NA

write.csv(output,file = '../data/p2z_seasonal.csv')

