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

# Read in the data from files
ref_data = feather::read_feather(here('data//mean_annual_no_flow_and_climate_metric_means_for_no_flow_sites_082819_with_info.feather'))
ref_data = as.data.frame(ref_data)

# Sort by refence guage
ref_data$CLASS <- revalue(ref_data$CLASS,c("Ref"=1))
ref_data$CLASS <- revalue(ref_data$CLASS,c("Non-ref"=0))

# Remove Sites with no coords
ref_data <- ref_data[complete.cases(ref_data[ ,which(colnames(ref_data)=="dec_lat_va")]),]

ref = as.data.frame(ref_data$CLASS)
sp_data = sp::SpatialPointsDataFrame(cbind(ref_data$dec_long_va,ref_data$dec_lat_va),
                                     proj4string=CRS('+init=epsg:4269'),
                                     dat=ref_data)

# Save out reference spatial points data frame

reference_gauges = ref_data[ref==1,]
sp_data = sp::SpatialPointsDataFrame(cbind(reference_gauges$dec_long_va,reference_gauges$dec_lat_va),
                                     proj4string=CRS('+init=epsg:4269'),
                                     dat=reference_gauges)


## Plot the stations
pal = mapviewPalette("mapviewRasterColors")

sites = ref_data$site



m <- mapview::mapview(sp_data,
                 zcol =c("cvlengthflow"),
                 col.regions=pal(12),
                 at = seq(0,5,.25),
                 pch=5,
                label = sites)

# Read in data with reference 

mapshot(m,url="test.html")
