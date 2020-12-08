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

library(gt)

############################# Code ################################



df = read.csv("data/kmeans.csv")

  

getKS <-function(region,metric){
  R = df[which(df$Name==region & df$CLASS=="Ref"),]
  NR = df[which(df$Name==region & df$CLASS=="Non-ref"),]
  
  ind = which(colnames(df)==metric)
  return(ks.test(R[[ind]],NR[[ind]]))
}

metrics = c("peak2zero","drying_rate","dry_dur","peak_quantile","freq_local")
eco  = unique(df$Name)
D = data.frame(NA)
p = data.frame(NA)
lab = data.frame(NA)

for (j in metrics){
  for (i in eco){
    ks = getKS(i,j)
    D = cbind(D,ks[1])
    p = cbind(p,ks[2])
    lab = cbind(lab,paste(i,j))
  }
}

p = t(p)
lab = t(lab)
D  = t(D) 

rownames(D) = lab
rownames(p) = lab

ks = as.data.frame(cbind(D,p)) %>% na.omit()

colnames(ks) = c('D-statistic','p-value')

ks$Name = rownames(ks)
ks$Metric = gsub("^.*\\.","", ks$Name) 
ks$Metric = c("Peak to No Flow",'Drying Rate','No Flow Duration','Peak Quanitle','Event Frequency')
ks$Ecoregion = eco

ks$`D-statistic` = round(ks$`D-statistic`,3)

table = ks %>% 
  select('Metric',"Ecoregion",'D-statistic','p-value') %>%
  gt(groupname_col = 'Metric') %>%
  tab_header(title = md("K-S Test Results"))%>%
  tab_options(row_group.background.color = "#123453") %>%
  fmt_scientific(columns = vars('p-value'),decimals = 2) 
  # tab_style(
  #   style = list(
  #     cell_fill(color="#FFCCCB")),
  #   locations = cells_body(
  #     columns = vars('p-value'),
  #     rows = which("p-value" >= 0.1))
  # )
