#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Tile: Stacked Cluster Plot TS
#Coder: Nate Jones (cnjones7@ua.edu)
#Date: 11/20/2020
#Purpose: Produce plot of cluster proportions over time
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Step 1: Setup Workspace -------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Clear memory
remove(list=ls())

#Load packages of interest
library(tidyverse)

#Load data of interest
df <- read.csv("data/kmeans.csv") %>% as_tibble(.)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Step 2: Plot ------------------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#2.1 Tidy data -----------------------------------------------------------------
#Create function to print days of drying event (drying + dry)
fun<-function(n){

  #isolate event of interest
  event<-df[n,]
  
  #Select cols of interest
  event<-event %>% select(event_id, peak_date,peak2zero, dry_dur, kmeans)
  
  #Creat tibble of event julian days
  output<-tibble(j_day = seq(event$peak_date, event$peak_date+ event$peak2zero + event$dry_dur))
  
  #Deal with vals over 365
  while(max(output$j_day, na.rm=T)>365){
    output<-output %>% 
      mutate(j_day = ifelse(j_day>365, 
                             j_day-365, 
                             j_day))
  }
  
  #Add relevant info to output
  output<-output %>% 
    mutate(
      event_id = event$event_id,
      kmeans   =event$kmeans)
  
  #Export output
  output
}

#Apply function to df
ts<-lapply(
  X=seq(1,nrow(df)),
  FUN = fun
)

#bind lists 
ts<-ts %>% bind_rows()

#Tidy
ts<-ts %>% 
  #Tally events by j_day and kmeans
  group_by(j_day, kmeans) %>% 
  summarise(totals=n()) %>% 
  #Pivot Wider
  pivot_wider(names_from = kmeans, 
              values_from=totals, 
              values_fill=0) %>% 
  #Tidy a bit
  filter(j_day!=0) %>% 
  rename(
    one=   '1',
    two=   '2',
    three= '3',
    four = '4') %>% 
  #Estimate Proportions
  mutate(
    one_prop   = one/(one+two+three+four),
    two_prop   = two/(one+two+three+four),
    three_prop = three/(one+two+three+four),
    four_prop  = four/(one+two+three+four)) %>% 
  #Tidy and pivot longer
  select(-one,-two,-three,-four) %>% 
  rename(one = one_prop,
         two = two_prop, 
         three = three_prop, 
         four = four_prop) %>% 
  pivot_longer(-j_day)
  
#2.2 Plot ----------------------------------------------------------------------
#Create factor for ordering
ts$name<-factor(ts$name, levels=c('one','two','three','four'))

#Atmospheric Date
ts<-ts %>% 
  mutate(a_day = if_else(j_day>61, j_day-61, j_day+305))

#Plot
ts_plot<-ggplot(ts, aes(x=a_day, y=value, fill=name)) + 
  geom_area() +
  scale_fill_manual(
    values=c("#4477AA","#66CCEE","#228833","#CCBB44"), 
    name = "Cluster") + 
  theme_bw() + 
  ylab('Proportion of Gages') +
  xlab('Atmospheric Year Day') +
  #Axes Options
  theme(
    axis.title = element_text(size=14),
    axis.text  = element_text(size = 10),
    legend.position = 'none') 
  # #Legend Options
  # theme(legend.position = "bottom", 
  #       legend.title = element_text(size=14), 
  #       legend.text = element_text(size=10))

#Print 
jpeg("docs//stackedClusterPlots.jpg", width = 7, height=4, units = "in", res=300)
ts_plot
dev.off()
