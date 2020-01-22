library(here)
library(lubridate)
library(feather)
library(tools)





################## Functions##################################

splitData <- function(files){
  
  
  writeAnnual <- function(data,start,end,guage){
    tt <- data[ymd(data$Date) >= start & ymd(data$Date) <= end,]
    WY_day = seq(nrow(tt))
    wy = format(ymd(tt$Date[nrow(tt)]),"%Y")
    print(wy)
    write_feather(cbind(WY_day,tt),here::here(paste0(newpath,'/WY',wy)))
    rm(tt,WY_day)
  }
  
  
  # Find the gauge and make directory
  gauge = strsplit(files,split='_')[[1]][1]
  dir.create(paste0(path,"annual/",gauge))
  
  newpath = paste0(path,"annual/",gauge)
  
  print(gauge)
  # Load the data
  dat = as.data.frame(read.csv(here::here(paste0(path,files))))
  dat$Date = ymd(dat$Date)
  
  # Find all the WY and make a sequence of cutoff dates
  beginYear  = unique(format(as.Date(dat$Date),"%Y"))[1]
  
  endYear = unique(format(as.Date(dat$Date),"%Y"))[length(unique(format(as.Date(dat$Date),"%Y")))]
  
  Bcutoffs = seq(from=as.Date(paste0(beginYear,"-10-01")), 
                 to=as.Date(paste0(endYear,"-10-01")), 
                 by="year")
  
  Ecutoffs = ymd(seq(from=as.Date(paste0(beginYear,"-09-30")), 
                     to=as.Date(paste0(endYear,"-09-30")), 
                     by="year"))+years(1)
  
  # Split all the data
  for(i in 1:(length(Bcutoffs)-2)){
    writeAnnual(dat,Bcutoffs[i],Ecutoffs[i],gauge)
  }
  rm(dat)
}




######################## Write reference feather in parallel ####################

## Get list of files
path = 'data/reference/'
files = list.files(path,pattern = '*csv')
 
## Make annual file
dir.create(paste0(path,"annual/"))

# get number of cores, start cluster and load packages on cores.
cores = parallel::detectCores()-2
cl <- parallel::makeCluster(cores)
parallel::clusterEvalQ(cl,.libPaths())
parallel::clusterEvalQ(cl,
                       {library(here)
                         library(feather)
                         library(tools)
                         library(lubridate)})
parallel::clusterEvalQ(cl,{path = 'data/reference/'})

# Use mpapply to exicute function
parallel::parLapply(cl,files,splitData)


# Stop the cluster
parallel::stopCluster(cl)



################### Write non-reference feather in parallel######################

## Get list of files
path = 'data/non_reference/'
files = list.files(path)

# get number of cores, start cluster and load packages on cores.
cores = parallel::detectCores()-2
cl <- parallel::makeCluster(cores)
parallel::clusterEvalQ(cl,.libPaths())
parallel::clusterEvalQ(cl,
                       {library(here)
                         library(feather)
                         library(tools)
                         library(lubridate)})
parallel::clusterEvalQ(cl,{path = 'data/reference/'})

# Use mpapply to exicute function
parallel::parLapply(cl,files,splitData)


# Stop the cluster
parallel::stopCluster(cl)

