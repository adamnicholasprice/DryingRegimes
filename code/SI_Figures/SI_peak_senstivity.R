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

#Read sub annual metric data
df<-read_csv('data/metrics_by_event.csv')
  
#apply filters used for anlaysis
df<-df %>% filter(drying_rate>0)  

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Step 2: Create function ------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Create function
fun<-function(quant){
  #Filter df
  temp<-df %>% filter(peak_quantile>quant)
  
  #summarise
  temp<-temp %>% 
    group_by(gage) %>% 
    summarise(
      n_events=n(),
      peak2zero = median(peak2zero), 
      dry_dur = median(dry_dur)) %>% 
    mutate(quant=quant)
}

#Run function
events<-lapply(X=seq(0,0.5,0.05), FUN = fun) %>% bind_rows()

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Step 3: Plot ---------- ------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Create boxplots
n_events<-events %>% 
  mutate(quant = as.factor(quant)) %>% 
  ggplot() +
    geom_boxplot(
      aes(x=quant, y=n_events), 
      outlier.shape = NA, 
      fill="steelblue4", 
      alpha=0.7) +
    theme_bw() + 
      labs(x="Peak Threshold (Quantile)", y = "Number of Drying Events") +
      scale_y_log10(limits=c(1,100)) +
      theme(
        axis.text  = element_text(size = 12),
        axis.title = element_text(size = 14)
      )

# Peak 2 Zero
peak2zero<-events %>% 
  mutate(quant = as.factor(quant)) %>% 
  ggplot() +
  geom_boxplot(
    aes(x=quant, y=peak2zero), 
    outlier.shape = NA, 
    fill="orange3", 
    alpha=0.7) +
  theme_bw() + 
  labs(x="Peak Threshold [Quantile]", y = "Peak2Zero [Days]") +
  scale_y_log10(limits=c(1,100)) +
  theme(
    axis.text  = element_text(size = 12),
    axis.title = element_text(size = 14)
  )

# Dry Duration
dry<-events %>% 
  mutate(quant = as.factor(quant)) %>% 
  ggplot() +
  geom_boxplot(
    aes(x=quant, y=dry_dur), 
    outlier.shape = NA, 
    fill="orange3", 
    alpha=0.7) +
  theme_bw() + 
  labs(x="Peak Threshold [Quantile]", y = "Dry Duration [Days]") +
  scale_y_log10(limits=c(1,100)) +
  theme(
    axis.text  = element_text(size = 12),
    axis.title = element_text(size = 14)
  )  
  
  
  
  
  
  
  
  
