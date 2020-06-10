# logistic regression models predicing whether there will be 0 flow or not based on individual variables and combinations of variables

library(dataRetrieval)
library(aod)
library(ggplot2)
library(pscl)
setwd("C:\\Users\\jhammond\\Desktop\\DRRCN_drying_regimes\\daily_flow_climate_and_antecedent_p_pet_melt_110119")

# start just for reference sites, so read in list of ref and non-ref sites

ref_output <- data.frame(matrix(NA, nrow = 219, ncol = 6))
colnames(ref_output) <- c("site_no","all","just_p","just_pet","just_t","just_melt")

refs <- c("01195100","01594950","01613050","01643700","01658500",
"01661050","01669000","01669520","02051000","02053200","02077200","02079640","02081500",
"02084160","02084557","02096846","02125000","02128000","02147500","02192500","02193340",
"02196000","02202600","02212600","02215100","02216180","02228500","02231342","02236500",
"02266200","02297155","02297310","02298608","02300700","02310947","02312200","02314500",
"02326000","03159540","03237280","03238500","03281500","03282500","03285000","03291780",
"03300400","03302680","03346000","03357350","03364500","03368000","03384450","04015330",
"04185440","04199155","05057000","05057200","05120500","05123400","05293000","05454000",
"05488200","05489000","05495500","05503800","05507600","05508805","05591550","05592050",
"05592575","05593575","05593900","05595730","06332515","06339100","06339500","06344600",
"06350000","06352000","06353000","06354000","06360500","06404000","06406000","06440200",
"06441500","06447000","06452000","06453600","06468170","06468250","06470800","06471200",
"06477500","06479215","06479438","06784000","06814000","06846500","06847900","06853800",
"06879650","06888500","06889200","06903400","06910800","06911900","06917000","06919500",
"06921200","07075300","07105945","07142300","07144780","07145700","07149000","07167500",
"07180500","07184000","07195800","07196900","07226500","07249985","07252000","07261000",
"07301500","07315200","07315700","07335700","07346045","07351500","07362100","07366200",
"08023080","08023400","08025500","08050800","08066200","08068780","08079600","08082700",
"08086212","08086290","08101000","08103900","08104900","08109700","08128400","08150800",
"08152900","08155200","08158700","08158810","08164000","08164300","08164600","08171300",
"08175000","08176900","08178880","08189500","08190500","08194200","08195000","08198000",
"08198500","08200000","08201500","08202700","08400000","08401200","09066300","09312600",
"09378170","09378630","09386900","09423350","09430600","09480000","09484000","09494000",
"09498503","09505350","09508300","09510200","09512280","09513780","09535100","09537200",
"10257600","10258000","10258500","10259200","10336676","11015000","11098000","11111500",
"11120500","11124500","11138500","11148900","11151300","11162570","11176400","11180500",
"11180960","11224500","11237500","11253310","11274500","11274630","11284400","11299600",
"11315000","11451100","11480390","12115700","14362250","208111310")

# get plotting info

ref_info <- readNWISsite(refs)

# for i in seq_along sites

for(i in seq_along(refs)){
# i = 1
current <- read.csv(paste(refs[i],"_daily_flow_and_climate.csv_and_API.csv",sep = ""))
  
current$flow_0 <- ifelse(current$Q_cfs==0, 1,0)  
current <- current[-1:-30,]
current <- current[,c("flow_0","Tmin_30","API_40","AMELTI_40","APETI_40")]
# model just using antecedent P of 30 days
just_p <- glm(flow_0~API_40, data = current, family = "binomial")
just_p <- pR2(just_p)
just_p <- as.numeric(just_p[6])
# model just using antecedent T of 30 days
just_t <- glm(flow_0~Tmin_30, data = current, family = "binomial")
just_t <- pR2(just_t)
just_t <- as.numeric(just_t[6])
# model just using antecedent PET of 30 days
just_pet <- glm(flow_0~APETI_40, data = current, family = "binomial")
just_pet <- pR2(just_pet)
just_pet <- as.numeric(just_pet[6])
# model just using antecedent melt of 30 days
just_melt <- glm(flow_0~AMELTI_40, data = current, family = "binomial")
just_melt <- pR2(just_melt)
just_melt <- as.numeric(just_melt[6])
# using all four
just_all <- glm(flow_0~AMELTI_40+API_40+Tmin_30+APETI_40, data = current, family = "binomial")
just_all <- pR2(just_all)
just_all <- as.numeric(just_all[6])

ref_output[i,] <- c(refs[i],just_all,just_p,just_pet,just_t,just_melt)
  
}  
  
# merge to lat long for plotting  
  
summary(ref_output)
ref_output$all <- as.numeric(ref_output$all)
ref_output$just_p <- as.numeric(ref_output$just_p)
ref_output$just_pet <- as.numeric(ref_output$just_pet)
ref_output$just_t <- as.numeric(ref_output$just_t)
ref_output$just_melt <- as.numeric(ref_output$just_melt)

to_plot <- merge(ref_info,ref_output, by = "site_no")
# USE CART to see whether watershed properties or climate variables lead to better model performance

setwd("C:\\Users\\jhammond\\Desktop\\DRRCN_drying_regimes\\")

write.csv(ref_output, "day_30_pseudo_r2.csv")



ref_output <- data.frame(matrix(NA, nrow = 219, ncol = 6))
colnames(ref_output) <- c("site_no","all","just_p","just_pet","just_t","just_melt")

setwd("C:\\Users\\jhammond\\Desktop\\DRRCN_drying_regimes\\daily_flow_climate_and_antecedent_p_pet_melt_110119")

for(i in seq_along(refs)){
  # i = 1
  current <- read.csv(paste(refs[i],"_daily_flow_and_climate.csv_and_API.csv",sep = ""))
  
  current$flow_0 <- ifelse(current$Q_cfs==0, 1,0)  
  current <- current[-1:-30,]
  current <- current[,c("flow_0","Tmin_14","API_14","AMELTI_14","APETI_14")]
  # model just using antecedent P of 30 days
  just_p <- glm(flow_0~API_14, data = current, family = "binomial")
  just_p <- pR2(just_p)
  just_p <- as.numeric(just_p[6])
  # model just using antecedent T of 30 days
  just_t <- glm(flow_0~Tmin_14, data = current, family = "binomial")
  just_t <- pR2(just_t)
  just_t <- as.numeric(just_t[6])
  # model just using antecedent PET of 30 days
  just_pet <- glm(flow_0~APETI_14, data = current, family = "binomial")
  just_pet <- pR2(just_pet)
  just_pet <- as.numeric(just_pet[6])
  # model just using antecedent melt of 30 days
  just_melt <- glm(flow_0~AMELTI_14, data = current, family = "binomial")
  just_melt <- pR2(just_melt)
  just_melt <- as.numeric(just_melt[6])
  # using all four
  just_all <- glm(flow_0~AMELTI_14+API_14+Tmin_14+APETI_14, data = current, family = "binomial")
  just_all <- pR2(just_all)
  just_all <- as.numeric(just_all[6])
  
  ref_output[i,] <- c(refs[i],just_all,just_p,just_pet,just_t,just_melt)
  
}  


setwd("C:\\Users\\jhammond\\Desktop\\DRRCN_drying_regimes\\")

write.csv(ref_output, "day_14_pseudo_r2.csv")









ref_output <- data.frame(matrix(NA, nrow = 219, ncol = 6))
colnames(ref_output) <- c("site_no","all","just_p","just_pet","just_t","just_melt")

setwd("C:\\Users\\jhammond\\Desktop\\DRRCN_drying_regimes\\daily_flow_climate_and_antecedent_p_pet_melt_110119")

for(i in seq_along(refs)){
  # i = 1
  current <- read.csv(paste(refs[i],"_daily_flow_and_climate.csv_and_API.csv",sep = ""))
  
  current$flow_0 <- ifelse(current$Q_cfs==0, 1,0)  
  current <- current[-1:-30,]
  current <- current[,c("flow_0","Tmin_7","API_7","AMELTI_7","APETI_7")]
  # model just using antecedent P of 30 days
  just_p <- glm(flow_0~API_7, data = current, family = "binomial")
  just_p <- pR2(just_p)
  just_p <- as.numeric(just_p[6])
  # model just using antecedent T of 30 days
  just_t <- glm(flow_0~Tmin_7, data = current, family = "binomial")
  just_t <- pR2(just_t)
  just_t <- as.numeric(just_t[6])
  # model just using antecedent PET of 30 days
  just_pet <- glm(flow_0~APETI_7, data = current, family = "binomial")
  just_pet <- pR2(just_pet)
  just_pet <- as.numeric(just_pet[6])
  # model just using antecedent melt of 30 days
  just_melt <- glm(flow_0~AMELTI_7, data = current, family = "binomial")
  just_melt <- pR2(just_melt)
  just_melt <- as.numeric(just_melt[6])
  # using all four
  just_all <- glm(flow_0~AMELTI_7+API_7+Tmin_7+APETI_7, data = current, family = "binomial")
  just_all <- pR2(just_all)
  just_all <- as.numeric(just_all[6])
  
  ref_output[i,] <- c(refs[i],just_all,just_p,just_pet,just_t,just_melt)
  
}  

library(stringr)
setwd("C:\\Users\\jhammond\\Desktop\\DRRCN_drying_regimes\\")

day7 <- read.csv("day_7_pseudo_r2.csv")
day14 <- read.csv("day_14_pseudo_r2.csv")
day30 <- read.csv("day_30_pseudo_r2.csv")

day7$all <- as.numeric(day7$all)
day7$just_p <- as.numeric(day7$just_p)
day7$just_pet <- as.numeric(day7$just_pet)
day7$just_t <- as.numeric(day7$just_t)
day7$just_melt <- as.numeric(day7$just_melt)

day14$all <- as.numeric(day14$all)
day14$just_p <- as.numeric(day14$just_p)
day14$just_pet <- as.numeric(day14$just_pet)
day14$just_t <- as.numeric(day14$just_t)
day14$just_melt <- as.numeric(day14$just_melt)

day30$all <- as.numeric(day30$all)
day30$just_p <- as.numeric(day30$just_p)
day30$just_pet <- as.numeric(day30$just_pet)
day30$just_t <- as.numeric(day30$just_t)
day30$just_melt <- as.numeric(day30$just_melt)

day7$site_no <- str_pad(day7$site_no, 8, "left", pad = "0")
day14$site_no <- str_pad(day14$site_no, 8, "left", pad = "0")
day30$site_no <- str_pad(day30$site_no, 8, "left", pad = "0")

to_plot_7day <- merge(ref_info,day7, by = "site_no")
to_plot_14day <- merge(ref_info,day14, by = "site_no")
to_plot_30day <- merge(ref_info,day30, by = "site_no")



library(ggplot2)
library(ggmap)
library(maps)
library(mapdata)
library(dplyr)
library(scales)
library(maptools)
library(dataRetrieval)

states <- map_data("state")

ggplot(data = states) + 
  geom_polygon(aes(x = long, y = lat, group = group), fill = "black", color = "white") + 
  theme_linedraw() + 
  geom_point(data=to_plot_7day, aes(x=dec_long_va, y=dec_lat_va,  colour = all), size = 2, shape=20, alpha=1) +
  scale_color_gradientn(colors = rainbow(5), space ="Lab", breaks = seq(0,1,0.1), limits = c(0,1))

# just melt logistic model for 14 day period
ggplot(data = states) + 
  geom_polygon(aes(x = long, y = lat, group = group), fill = "black", color = "white") + 
  theme_linedraw() + 
  geom_point(data=to_plot_14day, aes(x=dec_long_va, y=dec_lat_va,  colour = just_melt), size = 2, shape=20, alpha=1) +
  scale_color_gradientn(colors = rainbow(5), space ="Lab", breaks = seq(0,1,0.1), limits = c(0,1))

# just precip logistic model for 14 day period

ggplot(data = states) + 
  geom_polygon(aes(x = long, y = lat, group = group), fill = "black", color = "white") + 
  theme_linedraw() + 
  geom_point(data=to_plot_14day, aes(x=dec_long_va, y=dec_lat_va,  colour = just_p), size = 2, shape=20, alpha=1) +
  scale_color_gradientn(colors = rainbow(5), space ="Lab", breaks = seq(0,1,0.1), limits = c(0,1))

# just PET logistic model for 14 day period

ggplot(data = states) + 
  geom_polygon(aes(x = long, y = lat, group = group), fill = "black", color = "white") + 
  theme_linedraw() + 
  geom_point(data=to_plot_14day, aes(x=dec_long_va, y=dec_lat_va,  colour = just_pet), size = 2, shape=20, alpha=1) +
  scale_color_gradientn(colors = rainbow(5), space ="Lab", breaks = seq(0,1,0.1), limits = c(0,1))

ggplot(data = states) + 
  geom_polygon(aes(x = long, y = lat, group = group), fill = "black", color = "white") + 
  theme_linedraw() + 
  geom_point(data=to_plot_14day, aes(x=dec_long_va, y=dec_lat_va,  colour = all), size = 2, shape=20, alpha=1) +
  scale_color_gradientn(colors = rainbow(5), space ="Lab", breaks = seq(0,1,0.1), limits = c(0,1))



ggplot(data = states) + 
  geom_polygon(aes(x = long, y = lat, group = group), fill = "black", color = "white") + 
  theme_linedraw() + 
  geom_point(data=to_plot_30day, aes(x=dec_long_va, y=dec_lat_va,  colour = all), size = 2, shape=20, alpha=1) +
  scale_color_gradientn(colors = rainbow(5), space ="Lab", breaks = seq(0,1,0.1), limits = c(0,1))

to_plot_30day$all_30_min_14 <- to_plot_30day$all-to_plot_14day$all
to_plot_30day$all_30_min_7 <- to_plot_30day$all-to_plot_7day$all
to_plot_30day$all_14_min_7 <- to_plot_14day$all-to_plot_7day$all


ggplot(data = states) + 
  geom_polygon(aes(x = long, y = lat, group = group), fill = "black", color = "white") + 
  theme_linedraw() + 
  geom_point(data=to_plot_30day, aes(x=dec_long_va, y=dec_lat_va,  colour = all_30_min_14), size = 2, shape=20, alpha=1) +
  scale_color_gradientn(colors = rainbow(5), space ="Lab", breaks = seq(0,1,0.1), limits = c(0,1))


ggplot(data = states) + 
  geom_polygon(aes(x = long, y = lat, group = group), fill = "black", color = "white") + 
  theme_linedraw() + 
  geom_point(data=to_plot_30day, aes(x=dec_long_va, y=dec_lat_va,  colour = all_30_min_7), size = 2, shape=20, alpha=1) +
  scale_color_gradientn(colors = rainbow(5), space ="Lab", breaks = seq(0,1,0.1), limits = c(0,1))


ggplot(data = states) + 
  geom_polygon(aes(x = long, y = lat, group = group), fill = "black", color = "white") + 
  theme_linedraw() + 
  geom_point(data=to_plot_30day, aes(x=dec_long_va, y=dec_lat_va,  colour = all_14_min_7), size = 2, shape=20, alpha=1) +
  scale_color_gradientn(colors = rainbow(5), space ="Lab", breaks = seq(0,1,0.1), limits = c(0,1))
