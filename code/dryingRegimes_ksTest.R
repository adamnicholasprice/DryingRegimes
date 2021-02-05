#####################################################################
##
## Script name: dryingRegimes_ksTest.r
##
## Author: Adam N. Price
##
## Date Created: 2020-12-07
##
## Copyright (c) Adam N. Price, 2020
## Email: adnprice@ucsc.edu
##
############################# Description ##########################
##
## 
##   
##
############################# Packages #############################

library(kableExtra)
library(tidyverse)

############################# Code ################################



df = read.csv("data/kmeans.csv")

  

getKS <-function(region,metric){
  R = df[which(df$Name==region & df$CLASS=="Ref"),]
  NR = df[which(df$Name==region & df$CLASS=="Non-ref"),]
  
  ind = which(colnames(df)==metric)
  return(ks.test(R[[ind]],NR[[ind]]))
}

metrics = c("peak2zero","drying_rate","dry_dur","peak_quantile","freq_local","dry_date_start")
form_met = c("Dry-down duration",'Drying Rate','No Flow Duration','Antecedent peak quanle','Annual event frequency','No flow start date')
eco  = unique(df$Name)
D = data.frame(NA)
p = data.frame(NA)
lab = data.frame(NA)
Metric=data.frame(NA)
o=0

for (j in metrics){
  o=o+1
  for (i in eco){
    ks = getKS(i,j)
    D = cbind(D,ks[1])
    p = cbind(p,ks[2])
    lab = cbind(lab,paste(i,j))
    Metric = cbind(Metric,form_met[o])
  }
}

p = t(p)
lab = t(lab)
D  = t(D) 
Metric= t(Metric) %>% na.omit()

rownames(D) = lab
rownames(p) = lab

ks = as.data.frame(cbind(D,p)) %>% na.omit()

colnames(ks) = c('D-statistic','p-value')

ks$Name = rownames(ks)
ks$Metric = Metric
ks$Ecoregion = eco

ks$`D-statistic` = round(ks$`D-statistic`,4)
rownames(ks) = NULL

table = ks %>% 
  select('Metric',"Ecoregion",'D-statistic','p-value')

print(table)


### By ecoregion

# Matrix to store the result
groups = unique(df$Name)
d.stat <- matrix(NA, nc=length(groups), nr=length(groups))
p.val<- matrix(NA, nc=length(groups), nr=length(groups))
colnames(d.stat) <- rownames(d.stat) <- groups
colnames(p.val) <- rownames(p.val) <- groups

d.mat = list()
p.mat = list()
# Loop
for (m in 1:length(metrics)){
  eco_df = df %>%
    select(Name,metrics[m])%>%
    set_names(.,c("Name","value"))
  for( g1 in groups ) {
    for( g2 in groups ) {
      d.stat[ g1, g2 ] <- ks.test( 
        eco_df$value[eco_df$Name == g1], 
        eco_df$value[ eco_df$Name == g2 ]
      )$statistic
      p.val[ g1, g2 ] <- ks.test( 
        eco_df$value[eco_df$Name == g1], 
        eco_df$value[ eco_df$Name == g2 ]
      )$p.value
    }
  }
  d.mat[[m]] <- d.stat
  p.mat[[m]] <- p.val
}
  

d.mat = data.frame(purrr::map_df(d.mat, tibble::as_tibble)) 
colnames(d.mat) = groups
rownames(d.mat) = apply(expand.grid(groups, metrics), 1, paste, collapse=".")

p.mat = data.frame(purrr::map_df(p.mat, tibble::as_tibble))
colnames(p.mat) = groups
rownames(p.mat) = apply(expand.grid(groups, metrics), 1, paste, collapse=".")

p.mat[p.mat<0.05]=NA
p.mat[p.mat==1]=NA


### By cluster
groups = sort(unique(df$kmeans))
d.stat <- matrix(NA, nc=length(groups), nr=length(groups))
p.val<- matrix(NA, nc=length(groups), nr=length(groups))
colnames(d.stat) <- rownames(d.stat) <- groups
colnames(p.val) <- rownames(p.val) <- groups

d.mat = list()
p.mat = list()
# Loop
for (m in 1:length(metrics)){
  eco_df = df %>%
    select(kmeans,metrics[m])%>%
    set_names(.,c("Name","value"))
  for( g1 in groups ) {
    for( g2 in groups ) {
      d.stat[ g1, g2 ] <- ks.test( 
        eco_df$value[eco_df$Name == g1], 
        eco_df$value[ eco_df$Name == g2 ]
      )$statistic
      p.val[ g1, g2 ] <- ks.test( 
        eco_df$value[eco_df$Name == g1], 
        eco_df$value[ eco_df$Name == g2 ]
      )$p.value
    }
  }
  d.mat[[m]] <- d.stat
  p.mat[[m]] <- p.val
}

d.mat = data.frame(purrr::map_df(d.mat, tibble::as_tibble)) 
colnames(d.mat) = c("c1","c2","c3","c4")
rownames(d.mat) = apply(expand.grid(groups, metrics), 1, paste, collapse=".")

p.mat = data.frame(purrr::map_df(p.mat, tibble::as_tibble))
colnames(p.mat) = c("c1","c2","c3","c4")
rownames(p.mat) = apply(expand.grid(groups, metrics), 1, paste, collapse=".")
