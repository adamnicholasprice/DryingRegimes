#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Title: Peak threshold sensitivity analysis
# Initial Peak 2 Zero Sensitivity Analysis
# Date: 5/22/2020
# Description: Examine the impact of peak threshold on drying metrics
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Step 1: Setup workspace ------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Clear memory
remove(list=ls())

#Libraries (for Windows OS)
library(patchwork)
library(lubridate)
library(tidyverse)

# Get list of files
files <- list.files('data/daily_data_with_ climate_and_PET/csv',pattern = "*.csv",full.names = TRUE)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Step 2: Create function ------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Metrics fun (slight modification from sub_annual_maetrics_script.R)
metrics_fun <- function(n, quant){
  
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
  #Create new collumn with flow data 25% quantile for peak id
  df<-df %>%
    #Round to nearest tenth
    mutate(q = round(q, 1)) %>%
    #quantile thresholds
    mutate(q_peak = if_else(q>quantile(q,quant),  q, 0))
  
  #Define peaks using slope break method
  df<-df %>% 
    #Define forward and backward slope at each point
    mutate(
      slp_b = (q_peak-lag(q_peak))/(num_date-lag(num_date)), 
      slp_f = (lead(q_peak)-q_peak)/(lead(num_date)-num_date), 
      slp_f = (lead(q_peak)-q_peak)/(lead(num_date)-num_date),  
    ) %>% 
    #now flag those derivative changes
    mutate(peak_flag = if_else(slp_b>0.0001 & slp_f<0, 1,0),
           peak_flag = if_else(is.na(peak_flag), 0, peak_flag)) 
  
  #Define initiation of no flow
  df<-df %>%   
    #Define individual storm events
    mutate(event_id = cumsum(peak_flag)+1) %>% 
    #Flag initiation of no flow
    mutate(nf_start = if_else(q == 0 & lag(q) != 0, 1, 0)) 
  
  #Recession metrics~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  recession_fun<-function(m){
    #Isolate indivdual recession events
    t<-df %>% filter(event_id == m) 
    
    #Convert NA to zero
    t<-t %>% replace_na(list(nf_start=0)) 
    
    #Compute drying regime stats if the following conditions exist
    if(sum(t$nf_start, na.rm=T)!=0 & #there is a dry period in this event
       t$q[1]!=0 &                   #the event dosn't start with q=0
       sum(t$q_peak)!=0){            #there is no peak value     
      
      #Define recession event as the length of time between peak and longest dry 
      #    event before the next peak. 
      #Define all drying event
      t<-t %>% 
        #Number drying events
        mutate(dry_event_id = cumsum(nf_start)) %>% 
        #Remove id number when > 0
        mutate(dry_event_id = if_else(q>0, 0, dry_event_id)) 
      
      #Define dry date as the start of the longest drying event
      dry_date <- t %>% 
        #Count length of indivdiual drying events
        filter(dry_event_id>0) %>% 
        group_by(dry_event_id) %>% 
        summarise(
          n = n(),
          date = min(date)) %>% 
        #filter to max
        arrange(-n, date) %>% 
        filter(row_number()==1) %>% 
        #isolate just the date
        select(date)
      
      #Dry Date
      t<-t %>% filter(date<=dry_date$date)
      
      #Define event_id
      event_id <- t$event_id[1]
      
      #Define Peak Data
      peak_date <- as.POSIXlt(t$date[1], "%Y-%m-%d")$yday[1]
      peak_value <- t$q[1]
      peak_quantile <- ecdf(df$q)(peak_value)
      
      #Define Peak to zero metric
      peak2zero <- nrow(t)
      
      #Create linear model of dQ vs q
      t<- t %>% mutate(dQ = lag(q) - q) %>% filter(dQ>=0)
      model<-lm(log10(dQ+0.1)~log10(q+0.1), data=t)
      
      #Estimate drying rate [note the error catch for low slopes]
      drying_rate <- tryCatch(model$coefficients[2], error = function(e) NA)
      p_value <- tryCatch(summary(model)$coefficients[2,4], error = function(e) NA)
      
      #Create output tibble
      output<-tibble(event_id, peak_date, peak_value, peak_quantile, peak2zero, drying_rate, p_value)
      
    }else{
      output<-tibble(
        event_id = t$event_id[1],
        peak_date = NA,
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
    
    #Convert NA to zero
    t<-t %>% replace_na(list(nf_start=0)) 
    
    #If drying event occurs
    if(sum(t$nf_start, na.rm=T)!=0){
      #Define recession event as the length of time between peak and longest dry event before the next peak. 
      #Define all drying events
      t<-t %>% 
        #Number drying events
        mutate(dry_event_id = cumsum(nf_start)) %>% 
        #Remove id number when > 0
        mutate(dry_event_id = if_else(q>0, 0, dry_event_id)) 
      
      #Define longest dry event
      dry_event <- t %>% 
        #Count length of indivdiual drying events
        filter(dry_event_id>0) %>% 
        group_by(dry_event_id) %>% 
        summarise(
          n = n(),
          date = min(date)) %>% 
        #filter to max
        arrange(-n, date) %>% 
        filter(row_number()==1) %>% 
        #isolate just the date
        select(dry_event_id) %>% pull()
      
      #filter data frame to dry event
      t<-t %>% filter(dry_event_id==dry_event)
      
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
    drop_na(dry_dur)
  
  #Export metrics
  metrics
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Step 3: Sensitivity Analysis -------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Create list of n and quant
queue<-tibble(
    n=seq(1, length(files)), 
    q01 = 0.01,
    q05 = 0.05,
    q10 = 0.10,
    q15 = 0.15,
    q20 = 0.20, 
    q25 = 0.25, 
    q30 = 0.30, 
    q35 = 0.35,
    q40 = 0.40, 
    q45 = 0.45,
    q50 = 0.50) %>% 
  pivot_longer(-n) %>% 
  select(file = n, 
         quant = value)
    

#Create wrapper function
execute<-function(m){
  #Run fun
  output<-tryCatch(
    metrics_fun(queue$file[m],queue$quant[m]), 
      error=function(e){
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
          p_value = NA,
          gage = tools::file_path_sans_ext(basename(files))[queue$file[m]])}
      )
  
  #Add threshold info
  output$threshold<-queue$quant[m]
  
  #Export 
  output
}

#Start timer
t0<-Sys.time()

# get number of cores
n.cores <- detectCores()

#start cluster
cl <-  makePSOCKcluster(n.cores)

#Export file list to cluster
clusterExport(cl, c('queue','files', 'metrics_fun'), env=.GlobalEnv)

# Use mpapply to exicute function
x<-parLapply(cl,seq(1, nrow(queue)),execute) #nrow(queue)

#bind rows
df <-x %>% bind_rows(x) %>% filter(drying_rate>0)

# Stop the cluster
stopCluster(cl)

#Capture finishing time
tf<-Sys.time()
tf-t0

#Export csv
write_csv(df, "data/SI_sensitivity.csv")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Step 4: Plot -----------------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#4.1 Create individual plots ---------------------------------------------------
#If starting here -- read the csv
df<-read_csv('data/SI_sensitivity.csv')

#dry down duration
dry_dur<-df %>% 
  #Count events by gage and peak threshold
  # group_by(gage, meteorologic_year, threshold) %>% 
  # summarise(peak2zero  = median(peak2zero,na.rm=T)) %>% 
  #Estimate median and 25/75 quantile for each treshold
  group_by(threshold) %>% 
  summarise(
    median = median(peak2zero),
    low = quantile(peak2zero, 0.25), 
    high = quantile(peak2zero, 0.75)
  ) %>% 
  #Plot
  ggplot() +
  geom_errorbar(
    aes(x=threshold, ymin=low, ymax=high), 
    width = 0, 
    col="grey30")+
  geom_point(
    aes(x=threshold, y=median), 
    shape = 21,
    fill="steelblue4", 
    col="grey30", 
    size = 3) +
  theme_bw() + 
  labs(x="Peak Threshold [Quantile]", y = "Dyring Duration [Days]") +
  theme(
    axis.text  = element_text(size = 12),
    axis.title = element_text(size = 14)
  )

#drying rate
drying_rate<-df %>% 
  #Count events by gage and peak threshold
  # group_by(gage, meteorologic_year, threshold) %>% 
  # summarise(drying_rate   = median(drying_rate ,na.rm=T)) %>% 
  #Estimate median and 25/75 quantile for each treshold
  group_by(threshold) %>% 
  summarise(
    median = median(drying_rate ),
    low = quantile(drying_rate , 0.25), 
    high = quantile(drying_rate , 0.75)
  ) %>% 
  #Plot
  ggplot() +
  geom_errorbar(
    aes(x=threshold, ymin=low, ymax=high), 
    width = 0, 
    col="grey30")+
  geom_point(
    aes(x=threshold, y=median), 
    shape = 21,
    fill="steelblue4", 
    col="grey30", 
    size = 3) +
  theme_bw() + 
  labs(x="Peak Threshold [Quantile]", y = "Dyring Rate [1/Days]") +
  theme(
    axis.text  = element_text(size = 12),
    axis.title = element_text(size = 14)
  )

#no flow duration
dry_dur<-df %>% 
  #Count events by gage and peak threshold
  # group_by(gage, meteorologic_year, threshold) %>% 
  # summarise(dry_dur   = median(dry_dur ,na.rm=T)) %>% 
  #Estimate median and 25/75 quantile for each treshold
  group_by(threshold) %>% 
  summarise(
    median = median(dry_dur ),
    low = quantile(dry_dur , 0.25), 
    high = quantile(dry_dur , 0.75)
  ) %>% 
  #Plot
  ggplot() +
  geom_errorbar(
    aes(x=threshold, ymin=low, ymax=high), 
    width = 0, 
    col="grey30")+
  geom_point(
    aes(x=threshold, y=median), 
    shape = 21,
    fill="steelblue4", 
    col="grey30", 
    size = 3) +
  theme_bw() + 
  labs(x="Peak Threshold [Quantile]", y = "No Flow Duration [Days]") +
  theme(
    axis.text  = element_text(size = 12),
    axis.title = element_text(size = 14)
  )

#peak quantile
peak_quantile<-df %>% 
  #Count events by gage and peak threshold
  # group_by(gage, meteorologic_year, threshold) %>% 
  # summarise(peak_quantile   = median(peak_quantile ,na.rm=T)) %>% 
  #Estimate median and 25/75 quantile for each treshold
  group_by(threshold) %>% 
  summarise(
    median = median(peak_quantile ),
    low = quantile(peak_quantile , 0.25), 
    high = quantile(peak_quantile , 0.75)
  ) %>% 
  #Plot
  ggplot() +
  geom_errorbar(
    aes(x=threshold, ymin=low, ymax=high), 
    width = 0, 
    col="grey30")+
  geom_point(
    aes(x=threshold, y=median), 
    shape = 21,
    fill="steelblue4", 
    col="grey30", 
    size = 3) +
  theme_bw() + 
  labs(x="Peak Threshold [Quantile]", y = "Median Peak Quantile") +
  theme(
    axis.text  = element_text(size = 12),
    axis.title = element_text(size = 14)
  )

#n_events
n_events<-df %>% 
  #Count events by gage and peak threshold
  # group_by(gage, meteorologic_year, threshold) %>% 
  # summarise(n_events = n()) %>% 
  #Estimate median and 25/75 quantile for each treshold
  group_by(threshold) %>% 
  summarise(
    n_events = n(),
    median = median(n_events),
    low = quantile(n_events, 0.25), 
    high = quantile(n_events, 0.75)
  ) %>% 
  #Plot
  ggplot() +
    geom_errorbar(
      aes(x=threshold, ymin=low, ymax=high), 
      width = 0, 
      col="grey30")+
    geom_point(
      aes(x=threshold, y=median), 
      shape = 21,
      fill="steelblue4", 
      col="grey30", 
      size = 3) +
    theme_bw() + 
      labs(x="Peak Threshold [Quantile]", y = "Drying Events [#]") +
      #scale_y_log10(limits = c(1,100))+
      theme(
        axis.text  = element_text(size = 12),
        axis.title = element_text(size = 14)
      )
 
#peak quantile
dry_date_start <-df %>% 
  #Count events by gage and peak threshold
  # group_by(gage, meteorologic_year, threshold) %>% 
  # summarise(dry_date_start    = median(dry_date_start  ,na.rm=T)) %>% 
  #Estimate median and 25/75 quantile for each treshold
  group_by(threshold) %>% 
  summarise(
    median = median(dry_date_start),
    low = quantile(dry_date_start  , 0.25), 
    high = quantile(dry_date_start  , 0.75)
  ) %>% 
  #Plot
  ggplot() +
  geom_errorbar(
    aes(x=threshold, ymin=low, ymax=high), 
    width = 0, 
    col="grey30")+
  geom_point(
    aes(x=threshold, y=median), 
    shape = 21,
    fill="steelblue4", 
    col="grey30", 
    size = 3) +
  theme_bw() + 
  labs(x="Peak Threshold [Quantile]", y = "Dry Event Start [Julian Day]") +
  theme(
    axis.text  = element_text(size = 12),
    axis.title = element_text(size = 14)
  )  
  
#4.2 Combine Plots   -----------------------------------------------------------
png("docs/SI_sensitivity.png", width=7, height = 8, units="in", res=300)
dry_dur + drying_rate + dry_dur + peak_quantile + n_events + dry_date_start +
  plot_layout(ncol=2)
dev.off()  
