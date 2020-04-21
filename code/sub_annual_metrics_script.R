#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Author: Adam Price 
# Co-authors: John Hammond and Nate Jones
# Tilte: Event Scale Analysis
# Date: 4/17/2020
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
# Create peak 2 zero function
metrics_fun <- function(n){
  
  #Setup workspace~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #Download libraries of interest
  library(lubridate)
  library(tidyverse)
  
  #Define gage
  gage <- as.character(tools::file_path_sans_ext(basename(files)))[n]
  
  #Download data and clean
  data <- read_csv(files[n]) %>% 
    mutate(date=as_date(Date), 
           num_date = as.numeric(Date), 
           q = X_00060_00003) %>% 
    na.omit() %>% 
    select(date, num_date, q)

  #Identify Individual Drying Events~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #Define when no flow start
  data<-data %>% 
    mutate(nf_start = if_else(q == 0 & lag(q)!=0, 1, 0)) 

  #Define forward and backward slope at each point
  data<-data %>% 
    mutate(
      slp_b = (q-lag(q))/(num_date-lag(num_date)), 
      slp_f = (lead(q)-q)/(lead(num_date)-num_date)
    )
  
  #now flag those derivative changes
  data <- data %>% 
    mutate(peak_flag = if_else(slp_b>0.0001 & slp_f<0, 1,0),
           peak_flag = if_else(is.na(peak_flag), 0, peak_flag))

  #Define individual storm events
  data<-data %>% 
    mutate(event_id = cumsum(peak_flag)+1)
  
  #Create function to estimate metrics by storm event~~~~~~~~~~~~~~~~~~~~~~~~~~~
  event_fun<-function(m){
    #Isolate indivdual recession events`````````````````````````````````````````
    df<-data %>% filter(event_id == m) 
    
    #Estimate Recesion metrics``````````````````````````````````````````````````
    #Isolate recession
    r<-df %>% 
      #Define recession points (i.e., where dQ < 0)
      filter(slp_f<0) %>% 
      #Rename slope for analysis
      mutate(dQ=-1*slp_f)
    
    #Do not estimate metrics if <5 records
    if(nrow(r)>=5){ 
  
      #Create linear model in log-log space
      model<-lm(log(dQ)~log(q+0.001), data=r)
      
      #Create output
      rec_output<-tibble(
        event_id = r$event_id[1],
        rec_dur = nrow(r)+1,
        Q_mean = mean(r$q),
        log_a = model$coefficients[1],
        b = model$coefficients[2],
        rsq = summary(model)$r.squared)
    }else{
      #Output if recession too short
      rec_output<-tibble(
        event_id = r$event_id[1],
        rec_dur = nrow(r),
        Q_mean = mean(r$q),
        log_a = NA,
        b = NA,
        rsq = NA)
    }
      
  #Estimate Drying Metrics``````````````````````````````````````````````````````
  dry<-df %>% filter(q==0)
  
  #Estimate zero metrics
  if(nrow(dry)>0){ #Do not estimate metrics with 0 records    
     dry_output<-tibble(
       event_id = dry$event_id[1],
       year = year(dry$date)[1], 
       dry_dur = nrow(dry), 
       dry_date_start= as.POSIXlt(dry$date, "%Y-%m-%d")$yday[1],
       dry_date_mean = mean(as.POSIXlt(dry$date, "%Y-%m-%d")$yday, na.rm = T)
     )
  }else{
    dry_output<-tibble(
      event_id = dry$event_id,
      year = NA,
      dry_dur = NA, 
      dry_date_start= NA, 
      dry_date_mean = NA)
  }
  
  #Combine Output'''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
  #Join
  output<-left_join(rec_output, dry_output)
  
  #Export
  output
  }
  
  #Execute Event Function~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  metrics<-lapply(seq(1,max(data$event_id, na.rm=T)), event_fun) %>% 
    bind_rows() %>% 
    drop_na() 

  #Normalize intercept (log_a)~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #Estimate median b
  b_m<-median(metrics$b)
  
  #Create function to estimate standardized log_a````````````````````````````````
  correction_fun<-function(m){
    #Isolate indivdual recession events
    df<-data %>% filter(event_id == m) 
    
    #Isolate recession
    r<-df %>% 
      #Define recession points (i.e., where dQ < 0)
      filter(slp_f<0) %>% 
      #Rename slope for analysis
      mutate(dQ=-1*slp_f)
    
    #Estimate Recesion metrics
    if(nrow(r)>=5){ #Do not estimate metrics if <5 records
      
      #Create linear model in log-log space (hold intercept constant at b_m)
      model<-lm(log(dQ)~0+log(q+0.001), offset = rep(b_m, nrow(r)), data=r)
      
      #Create output
      inner_output<-tibble(
        event_id = r$event_id[1],
        log_a_norm = model$coefficients[1])
    }else{
      inner_output<-tibble(
        event_id = r$event_id[1],
        log_a_norm = NA)
    }
    
    #Return output
    inner_output
  }
  
  #Run function'''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
  log_a_norm<-lapply(seq(1,max(data$event_id, na.rm=T)), correction_fun) %>% 
    bind_rows() %>% 
    drop_na() 
  
  #Export output~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  total<-left_join(metrics, log_a_norm) %>% mutate(gage=gage)
  write_csv(total,paste0('./data/sub_annual_metrics/',gage,'.csv'))
  total
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Step 3: Execute and write-----------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Create error handling function
execute<-function(a){
  tryCatch(metrics_fun(a), error=function(e){
    tibble(
      event_id = NA, 
      rec_dur	= NA, 
      Q_mean	= NA, 
      log_a	= NA, 
      b	= NA, 
      rsq	= NA, 
      year	= NA, 
      dry_dur	= NA, 
      dry_date_start	= NA, 
      dry_date_mean	= NA, 
      log_a_norm	= NA, 
      gage = a)}
    )
}
  
# get number of cores
n.cores <- detectCores()-1

#start cluster
cl <-  makePSOCKcluster(n.cores)

#Export file list to cluster
clusterExport(cl, c('files', 'metrics_fun'), env=.GlobalEnv)

# Use mpapply to exicute function
x<-parLapply(cl,seq(1, length(files)),execute)

# Stop the cluster
stopCluster(cl)

#gather output
output<-bind_rows(x)

#Write output
write_csv(output,paste0('./data/metrics_by_event.csv'))

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Step 4: summarise by gage-----------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#bulk stats~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
bulk<-output %>% 
  #group by gage
  group_by(gage) %>% 
  #Summarise
  summarise(
    b_median = median(b, na.rm=T),
    b_mean = mean(b, na.rm = T),
    b_sd = sd(b, na.rm=T), 
    log_a_norm_median = median(log_a_norm, na.rm=T),
    log_a_norm_mean = mean(log_a_norm, na.rm=T), 
    log_a_norm_sd = sd(log_a_norm, na.rm = T), 
    peak2zero_median = median(rec_dur, na.rm = T),
    peak2zero_mean = mean(rec_dur, na.rm = T),
    peak2zero_sd = sd(rec_dur, na.rm = T),
    dry_dur_event_median = median(dry_dur, na.rm=T),
    dry_dur_event_mean = mean(dry_dur, na.rm=T), 
    dry_dur_event_sd = sd(dry_dur, na.rm = T), 
  )

#annual duration stats~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
dur_annual<-output %>% 
  #gruop gage and year
  group_by(gage, year) %>% 
  #Summarise
  summarise(dry_dur_annual = sum(dry_dur)) %>% 
  #make wide format
  pivot_wider(names_from = gage, 
              values_from = dry_dur_annual) %>% 
  #Pivot back to long format
  pivot_longer(-year, 
               names_to = 'gage', 
               values_to = 'dry_dur_annual') %>% 
  #Convert NA to zero
  mutate(dry_dur_annual = replace_na(dry_dur_annual, 0)) %>% 
  #Group by year
  group_by(gage) %>% 
  #Summarise by year
  summarise(
    dry_dur_annual_mean = mean(dry_dur_annual, na.rm=T),
    dry_dur_annual_median = median(dry_dur_annual, na.rm =T), 
    dry_dur_annual_sd = sd(dry_dur_annual, na.rm=T)
  )
  
#n_events per year~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
n_events_annual<-output %>% 
  #gruop gage and year
  group_by(gage, year) %>% 
  #Summarise
  summarise(n_event_annual = nrow(.)) %>% 
  #make wide format
  pivot_wider(names_from = gage, 
              values_from = n_event_annual) %>% 
  #Pivot back to long format
  pivot_longer(-year, 
               names_to = 'gage', 
               values_to = 'n_event_annual') %>% 
  #Convert NA to zero
  mutate(n_event_annual = replace_na(n_event_annual, 0)) %>% 
  #Group by year
  group_by(gage) %>% 
  #Summarise by year
  summarise(
    n_event_annual_mean = mean(n_event_annual, na.rm=T),
    n_event_annual_median = median(n_event_annual, na.rm =T), 
    n_event_annual_sd = sd(n_event_annual, na.rm=T)
  )

#initial dry day~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
initial_dry_day_annual<-output %>% 
  #group gage and year
  group_by(gage, year) %>% 
  summarise(
    initial_dry_day = min(dry_date_start)
  ) %>% 
  #Now group by gage
  group_by(gage) %>% 
  summarise(
    initial_dd_annual_mean = mean(initial_dry_day, na.rm=T), 
    initial_dd_annual_median = median(initial_dry_day, na.rm=T), 
    initial_dd_sd = sd(initial_dry_day, na.rm=T)
  )
  
#Merge and export~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
export<-left_join(bulk, dur_annual) %>% 
  left_join(., n_events_annual) %>% 
  left_join(., initial_dry_day_annual)

#Write output
write_csv(output,paste0('./data/metrics_by_year.csv'))

