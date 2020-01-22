### Adam N. Price
#
#
#
#
#
#
#

library(dataRetrieval)
library(feather)

files <- list.files(here('daily_feather//'))
site <- data$site_no
lat <- data$dec_lat_va
long <- data$dec_long_va
datum <-as.character(data$dec_coord_datum_cd)

coord_list = data.frame(site,lat,long,datum,stringsAsFactors = FALSE)

for (i in 1:length(files))
{
  data = dataRetrieval::readNWISsite(files[i])
  if (data$agency_cd == 'USGS')
  {
  site[[i]] <- data$site_no
  lat[[i]] <- data$dec_lat_va
  long[[i]] <- data$dec_long_va
  datum[[i]] <-as.character(data$dec_coord_datum_cd)
  }
  else{
    return
  }
}

coord_list = na.omit(data.frame(site,lat,long,datum,stringsAsFactors = FALSE))

feather::write_feather(coord_list,'SiteCoordinates.feather')

