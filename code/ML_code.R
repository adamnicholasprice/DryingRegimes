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

library(lubridate)
library(here)
#####GALEN PACKAGES#####
library(tidyverse)
library(caret)
library(randomForest)
library(RColorBrewer)
library(rsample)
library(foreign)
library(rgdal)
library(Metrics)
library(plm)
#####

files = list.files(here::here("data/reference/"),full.names = TRUE)


dat = read.csv(files[1]) %>%
  as_tibble()

vars = dat[!is.na(dat$Q_cfs),c('Date','Q_cfs','P_mm','PET_mm','Tmin_C','Tmax_C','SWE_mm')]

tt  = split(vars, c(format(as.Date(vars$Date), "%Y"),format(as.Date(vars$Date), "%Y")))

years = unique(format(as.Date(vars$Date),"%Y"))


breaks <- seq(from=as.Date("1999-10-01"), to=as.Date("2015-10-01"), by="year")  
years.breaks = as.numeric(format(breaks,"%Y"))
labels.wy = years.breaks[2:length(breaks)]
test <- cut(as.Date(dat$Date), breaks,labels=labels.wy)
