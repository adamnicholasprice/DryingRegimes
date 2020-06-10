## Adam N. Price
#
#
#
#
#
#
#
#
#
#
#
library(data.table)

ap_cv <-function(data){
  sd(data,na.rm=TRUE)/mean(data,na.rm=TRUE)
}


dat = read.csv('data/non_reference/01097300_daily_flow_and_climate.csv_and_API.csv')

files = list.files('data/reference/')

dat= fread('data/non_reference/01097300_daily_flow_and_climate.csv_and_API.csv',sep = ",", select = c("Q_cfs"))

# Find lengths of no-flow days

dat$Q_cfs[is.na(dat$Q_cfs)]=0


nf_period = rle(dat$Q_cfs)
nf_period = nf_period$lengths[nf_period$values<=0]


tt = round(cv(nf_period,na.rm = TRUE),2)

