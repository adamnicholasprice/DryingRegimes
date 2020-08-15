#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Author: Adam Price 
# Co-authors: John Hammond and Nate Jones
# Tilte: Event Scale Analysis
# Date: 8/1/2020
# Description: Parallel process to examine individual storm events in USGS IRES 
#              gage data
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Resources~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# https://cran.r-project.org/web/packages/foreach/vignettes/foreach.pdf
# https://stackoverflow.com/questions/29828710/parallel-processing-in-r-for-a-data-frame

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Step 1: Setup workspace ------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Clear memory
remove(list=ls())

#Libraries (for Windows OS)
library(parallel)
library(lubridate)
library(tidyverse)

# Get list of files
files <- list.files('./data/daily_data_with_ climate_and_PET/csv',full.names = TRUE)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Step 2: Create Function ------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#For testing
#file<-files[str_detect(files,'02277600')]

# Create peak 2 zero function
metrics_fun <- function(n){

  #For testing
  #n<-which(str_detect(files, '14034500'))

  
  #Setup workspace~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #Download libraries of interest
  library(lubridate)
  library(tidyverse)
  
  #Define gage
  gage <- as.character(tools::file_path_sans_ext(basename(files)))[n]
  
  #Download data and clean
  df <- read_csv(file = files[n], 
                 col_types = 'dDdddddddd') %>% 
    mutate(date=as_date(Date), 
           num_date = as.numeric(Date), 
           q = X_00060_00003) %>% 
    select(date, num_date, q) %>% 
    na.omit() 
  
  #Identify inidividual drying events~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # #Filter flow data 
  # df<-df %>%
  #   #Round to nearest tenth
  #   mutate(q = round(q, 1)) %>%
  #   #25% quantile thresholds
  #   mutate(q_peak = if_else(q>quantile(q,0.25),  q, 0))
  
  #Define peaks
  df<-df %>% 
    #Define forward and backward slope at each point
    mutate(
      slp_b = (q-lag(q))/(num_date-lag(num_date)), 
      slp_f = (lead(q)-q)/(lead(num_date)-num_date), 
    ) %>% 
    #now flag those derivative changes
    mutate(peak_flag = if_else(slp_b>0.0001 & slp_f<0, 1,0),
           peak_flag = if_else(is.na(peak_flag), 0, peak_flag)) %>% 
    #Define individual storm events
    mutate(event_id = cumsum(peak_flag)+1) %>% 
    #Flag initiation of no flow
    mutate(nf_start = if_else(q == 0 & lag(q)!=0, 1, 0)) 
  
  #Recession metrics~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  recession_fun<-function(m){
    #Isolate indivdual recession events
    t<-df %>% filter(event_id == m) 
    
    #Convert NA to zero
    t<-t %>% replace_na(list(nf_start=0))
    
    #If drying event occurs
    if(sum(t$nf_start, na.rm=T)!=0 &        #If nf_start is na (very intiial value)
       t$q[1]!=0 & t$q[2]!=0 & t$q[3]!=0    #there is a zero in first three days
       ){
      #Isolate recession
      clip<-t$num_date[t$nf_start==1] %>% first() 
      t<-t %>% filter(num_date<=clip)
      
      #Define event_id
      event_id <- t$event_id[1]
      
      #Define Peak Data
      peak_date <- as.POSIXlt(t$date[1], "%Y-%m-%d")$yday[1]
      peak_value <- t$q[1]
      peak_quantile <- ecdf(df$q)(peak_value)
      
      #Define Peak to zero metric
      peak2zero <- nrow(t)
      
      #Drying rate
      t<- t %>% mutate(dQ = lag(q) - q) %>% filter(dQ>0)
      model<-lm(log10(dQ+0.1)~log10(q+0.1), data=t)
      drying_rate <- model$coefficients[2]
      p_value <- summary(model)$coefficients[2,4]
     
      #Create output tibble
      output<-tibble(event_id, peak_date, peak_value, peak_quantile, peak2zero, drying_rate, p_value)
      
    }else{
      output<-tibble(
        event_id = t$event_id[1],
        peak_date = as.POSIXlt(t$date[1], "%Y-%m-%d")$yday[1],
        peak_value = NA,
        peak_quantile = NA,
        peak2zero = NA,
        drying_rate = NA,
        p_value = NA
      )
    }
    
    #Export 
    output
  }
  
  #Dry metrics~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  dry_fun<-function(m){
    #Isolate indivdual recession events
    t<-df %>% filter(event_id == m) 
    
    #If drying event occurs
    if(sum(t$nf_start, na.rm=T)!=0){
      #Isolate dry period
      t<-t %>% 
        #Define no flow events [there may be mulitiple]
        mutate(nf_event = cumsum(nf_start)) %>% 
        #filter to first no flow event
        filter(nf_event == 1, q == 0)
      
      #Create output
      output<- tibble(
        #event_id
        event_id = t$event_id[1],
        #Define Year 
        calendar_year = year(t$date[1]), 
        #Define season
        season = if_else(month(t$date[1])<=3, "Winter", 
                         if_else(month(t$date[1])>3 & month(t$date[1])<=6, "Spring", 
                                 if_else(month(t$date[1])>6 & month(t$date[1])<=9, "Summer", 
                                         "Fall"))), 
        #Define meterological year
        meteorologic_year = if_else(season == 'Winter', 
                                    calendar_year -1,
                                    calendar_year),
        #define dry date
        dry_date_start = as.POSIXlt(t$date, "%Y-%m-%d")$yday[1],
        #Define mean dry date
        dry_date_mean = mean(as.POSIXlt(t$date, "%Y-%m-%d")$yday, na.rm = T),
        #Estiamte dry duration
        dry_dur = nrow(t)) 
    }else{
      output<-tibble(
        event_id = t$event_id[1],
        calendar_year = NA, 
        season = NA,
        meteorologic_year = NA, 
        dry_date_start = NA,
        dry_date_mean = NA,
        dry_dur = NA
      )
    }
    
    #Export 
    output
  }
  
  #Run functions~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  metrics<-lapply(
    X = seq(1,max(df$event_id, na.rm=T)), 
    FUN = function(m){full_join(recession_fun(m), dry_fun(m))}
  ) %>% 
    bind_rows() %>% 
    mutate(gage = gage) %>% 
    drop_na()
  
  #Export metrics
  metrics
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Step 3: Execute and write-----------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Start timer
t0<-Sys.time()

#Create error handling function
execute<-function(a){
  tryCatch(metrics_fun(a), error=function(e){
    tibble(
      event_id = NA, 
      peak_date = NA,
      peak2zero = NA, 
      drying_rate = NA, 
      calendar_year = NA, 
      season = NA, 
      meteorologic_year = NA, 
      dry_date_start = NA, 
      dry_date_mean = NA, 
      dry_dur = NA, 
      gage = tools::file_path_sans_ext(basename(files))[a])}
  )
}

# get number of cores
n.cores <- detectCores()

#start cluster
cl <-  makePSOCKcluster(n.cores)

#Export file list to cluster
clusterExport(cl, c('files', 'metrics_fun'), env=.GlobalEnv)

# Use mpapply to exicute function
x<-parLapply(cl,seq(1, length(files)),execute) #length(files)

# Stop the cluster
stopCluster(cl)

#gather output
output<-bind_rows(x)

#Capture finishing time
tf<-Sys.time()
tf-t0

#Write output
output<-output %>% select(gage, calendar_year, meteorologic_year, season,
                          peak_date, peak2zero, drying_rate, 
                          dry_date_start, dry_date_mean, dry_dur)
write_csv(output,paste0('./data/metrics_by_event.csv'))
