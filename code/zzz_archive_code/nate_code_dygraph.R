library(lubridate)
library(dygraphs)
library(dplyr)
library(xts)
data = read.csv('../data/daily_data_with_ climate_and_PET/csv/01101000.csv')

data$Date <- ymd(data$Date)
 
begin = ymd('2016-06-01')

end = ymd('2016-09-30')

sub = data[between(data$Date,begin,end),c("Date","X_00060_00003","P_mm")]
discharge = sub$X_00060_00003
round_discharge = round(sub$X_00060_00003,1)
precip = sub$P_mm
date = sub$Date
sub2 = xts(x = cbind(discharge,round_discharge,precip),order.by = date)

dygraph(sub)



dygraph(sub2, main = "Gage 01101000")%>%
  dyAxis("y", label = "discharge", valueRange = c(-1, 10), independentTicks = TRUE)%>%
  dyAxis("y2", label = "precip ", valueRange = c(-1, 10), independentTicks = TRUE) %>%
  dySeries("precip", axis=('y2'))

p = dygraph(sub2, main = "Gage 01101000")%>%
  dyAxis("y", label = "discharge (cfs)" , independentTicks = TRUE)%>%
  dyAxis("y2", label = "precip (mm)", independentTicks = TRUE) %>%
  dySeries("precip", axis=('y2'))

htmlwidgets::saveWidget(p,'../plots/test.html')

dygraphs::dygraphOutput('')