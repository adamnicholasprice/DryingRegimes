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
library(lubridate)
library(scales)

#Load data of interest
df <- read.csv("data/kmeans_NoFreq.csv") %>% as_tibble(.)

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
  # output<-tibble(j_day = seq(event$peak_date, event$peak_date+ event$peak2zero))
  
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
ts.count<-ts %>% 
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
  pivot_longer(-j_day)


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

## Spoof the dates to make the plot labels work.....
ts$j.date = lubridate::as_date(ts.count$j_day)

ts<-ts%>% 
  mutate(j.date = if_else(j.date<"1970-03-01", j.date+365, j.date+0))



cols <- 
  c("one" = "#4477AA",
    "two" = "#66CCEE",
    "three" = "#228833",
    "four" = "#CCBB44")

ts$name2  <- factor(ts$name, sort(unique(ts$name), decreasing = TRUE))

#Plot
ts_plot <-
  ggplot(ts, aes(x=j.date, y=value, fill=name2)) + 
  geom_area() +
  scale_fill_manual(
    values=cols, 
    name = "Cluster") +
  scale_x_date(labels = date_format("%b"),expand = c(0,0),date_breaks = "months")+
  theme_bw() + 
  ylab('Proportion of drying events') +
  xlab('Month') +
  #Axes Options
  theme(
    axis.title = element_text(size=14),
    axis.text  = element_text(size = 10),
    legend.position = 'none') +
  scale_y_continuous(expand = c(0,0))


ts_plot = ts_plot + 
  annotate("text", x = as.Date("1970-09-01"), 
           y = c(0.05,.25,.5,.85), 
           label = c("Cluster 1","Cluster 2","Cluster 3","Cluster 4"),
           color = "Black")



### Plot Number of events

ts.count$name<-factor(ts.count$name, levels=c('one','two','three','four'))

ts.count<-ts.count %>% 
  mutate(a_day = if_else(j_day>61, j_day-61, j_day+305))

## Spoof the dates to make the plot labels work.....
ts.count$j.date = lubridate::as_date(ts.count$j_day)

ts.count<-ts.count %>% 
  mutate(j.date = if_else(j.date<"1970-03-01", j.date+365, j.date+0))

ts.count.plot = ggplot(ts.count) +
  geom_line(aes(x=j.date,y=value,color=name))+
  scale_color_manual(
    values=cols, 
    name = "Cluster")+
  scale_x_date(labels = date_format("%B"),expand = c(0,0),date_breaks = "months")+
  theme_bw() + 
  ylab('Number of drying events') +
  xlab('Month') +
  #Axes Options
  theme(axis.title = element_text(size=14),
        axis.text  = element_text(size = 10),
        legend.position = 'none',
        axis.text.x = element_blank(),
        axis.title.x = element_blank())+
  scale_y_continuous(expand = c(0,0))

ts.count.plot = ts.count.plot + annotate("text", x = as.Date("1970-09-01"), 
                                         y = c(500,1300,2800,2100), 
                                         label = c("Cluster 1","Cluster 2","Cluster 3","Cluster 4"),
                                         color = c("#4477AA","#66CCEE","#228833","#CCBB44"))


library(patchwork)

out = ts.count.plot / ts_plot 
out

pdf("docs/response_plots/stackedClusterPlotsALT.pdf", width = 10, height=6)
out
dev.off()


## for john

tt = ts.count %>% 
  select(a_day,value,j.date) %>% 
  group_by(a_day) %>%
  mutate(value= sum(value)) %>%
  select(a_day,value,j.date) %>%
  distinct()

tt$name = "all"



ts.count.plot = ggplot(ts.count) +
  geom_line(aes(x=j.date,y=value,color=name))+
  geom_area(data =tt,aes(x=j.date,y= value),fill="grey",alpha=.25)+
  scale_color_manual(
    values=cols,
    name = "Cluster")+
  scale_x_date(labels = date_format("%B"),expand = c(0,0),date_breaks = "months",minor_breaks = NULL)+
  theme_bw() + 
  ylab('Number of drying events') +
  xlab('Month') +
  #Axes Options
  theme(axis.title = element_text(size=14),
        axis.text  = element_text(size = 10),
        legend.position = 'none',
        axis.text.x = element_blank(),
        axis.title.x = element_blank())+
  scale_y_continuous(limits = c(0,7000),expand = c(0,0))

ts.count.plot = ts.count.plot + annotate("text", x = as.Date("1970-09-01"), 
                                         y = c(500,1300,2800,2100,5000), 
                                         label = c("Cluster 1","Cluster 2","Cluster 3","Cluster 4","All Events"),
                                         color = c("#4477AA","#66CCEE","#228833","#CCBB44","#000000"))

out = ts.count.plot / ts_plot 
out

pdf("docs//stackedClusterPlots_ALT2.pdf")
out
dev.off()
