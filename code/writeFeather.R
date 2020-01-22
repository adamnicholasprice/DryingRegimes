### Adam N. Price
#
#
#
#
#



library(here)
here::here()
library(parallel)
library(feather)
library(tools)

## Get list of files
files = list.files(here('daily_data_with_climate_and_PET/'))

## Loop through files and write as feather in parallel

# Write a quick function to iterate
ap_writeFeather <- function(files){
      root <- tools::file_path_sans_ext(files)
      dat <- read.csv(here::here('daily_data_with_climate_and_PET/',files))
      feather::write_feather(x = dat,path =paste0('daily_feather/',root))
}


# get number of cores, start cluster and load packages on cores.
cores = parallel::detectCores()-2
cl <- parallel::makeCluster(cores)
parallel::clusterEvalQ(cl,.libPaths())
parallel::clusterEvalQ(cl,
                      {library(here)
                      library(feather)
                      library(tools)})

# Use mpapply to exicute function
parallel::parLapply(cl,files,ap_writeFeather)
  
  
# Stop the cluster
parallel::stopCluster(cl)


