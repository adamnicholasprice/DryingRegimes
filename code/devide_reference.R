### Adam N. Price
#
#
#
#
#
#
#

library(feather)
library(maptools)
library(rgdal)
library(mapview)
library(raster)
library(sp)
library(plyr)
library(here)
library(filesstrings)

# Read in the data from files
ref_data = feather::read_feather(here('data//mean_annual_no_flow_and_climate_metric_means_for_no_flow_sites_082819_with_info.feather'))
ref_data = as.data.frame(ref_data)

# Sort by refence guage
ref_data$CLASS <- revalue(ref_data$CLASS,c("Ref"=1))
ref_data$CLASS <- revalue(ref_data$CLASS,c("Non-ref"=0))

# Split files into reference and non-referene sites
ref = ref_data$site[ref_data$CLASS == 1]
non_ref = ref_data$site[ref_data$CLASS == 0]

# make dir and write

dir.create('data/reference')
dir.create('data/non_reference')

for (i in 1:length(ref)){
  ref_files = list.files('data/daily_flow_climate_and_antecedent_p_pet_melt_110119/', pattern = paste0('*',ref[i]))
  file.move(here(paste0('data/daily_flow_climate_and_antecedent_p_pet_melt_110119/',ref_files)),here('data/reference/'))
  }

for (i in 1:length(non_ref)){
  ref_files = list.files('data/daily_flow_climate_and_antecedent_p_pet_melt_110119/', pattern = paste0('*',non_ref[i]))
  file.move(here(paste0('data/daily_flow_climate_and_antecedent_p_pet_melt_110119/',ref_files)),here('data/non_reference/'))
  print(i)
}
