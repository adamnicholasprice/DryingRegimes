#===================================================================================#
#####INSTALL PACKAGES#####
library(dataRetrieval)
library(tidyverse)
library(magrittr)
library(Hmisc)
#####
#===================================================================================#

#===================================================================================#
#####QUERY NWIS#####
rr <- list()
sites <- list()
j = 1
v <- c('05482300','05482500','05484500')#,'05418110','05418400','05418720')#,'04080798','01493112','01646000')
# v = c('01195100','01594950')
for(i in 1:length(v)){
  site.data <- whatNWISdata(site = v[i], service = 'dv')
  if('00060' %in% site.data$parm_cd & '99133' %in% site.data$parm_cd){
    #print('Correct Parameters') 
    if('00003' %in% site.data[site.data$parm_cd == '00060','stat_cd'] & '00003' %in% site.data[site.data$parm_cd == '99133','stat_cd']){
      print('Correct Parameters/Correct Code')
      sites[[j]] <- whatNWISdata(site = v[i], service = 'dv')[1,]
      rr[[j]] <- readNWISdata(site = v[i], parameterCd = c('00060'),startDate="2012-03-01",endDate="2019-09-01", statCd = c('00003'))
      j <- j + 1
    }else{
      print('Correct Parameters/Incorrect Code')
      sites[[j]] <- whatNWISdata(site = v[i], service = 'dv')[1,]
      rr[[j]] <- readNWISdata(site = v[i], parameterCd = c('00060','99133'),startDate="2012-03-01",endDate="2019-09-01", statCd = c('00003','00008'))
      j <- j + 1
    }
  }else{
    print('Incorrect Parameters')
    next
  }
}
#####
#===================================================================================#

#####------------------------------------------------------------------------------------------#####

#===================================================================================#
#####PEAK PICKING ROUTINE#####

single.site.record <- list()
event.features <- list()
for(l in 1:length(rr)){
#  for(l in c(1,4)){
  #  l <- loc[h]
  ##-single site is one site's worth of data generated from the 
  ##-loop that checks the codes see above
  single.site <- rr[[l]]
  if(TRUE %in% grepl('Regression' ,colnames(single.site))){
    next
  }else{
    if(TRUE %in% grepl('X_99133_00008' ,colnames(single.site))){
      colnames(single.site)[6] <- 'X_99133_00003'
    }else{ }
    if(TRUE %nin% grepl('X_99133_00003' ,colnames(single.site))){
      next
    }else{
      ##-convert to a readable date time
      single.site$DateTime <- as.POSIXlt(single.site$dateTime, format = '%Y-%m-%d')
      single.site$Seconds <- as.numeric(single.site$DateTime)
      #plot(single.site$DateTime, single.site$X_00060_00003, typ = 'l', ylab = 'Discharge ft3', xlab = '', las = 1)
      ##-remove NAs
      ss <- single.site[!is.na(single.site$X_00060_00003),]
      single.site <- ss[!is.na(ss$X_99133_00003),]
      ##-make sure concentration and discharge are both greater than zero
      single.site <- single.site[single.site$X_00060_00003 > 0,]
      single.site <- single.site[single.site$X_99133_00003 > 0,]
      ##-create an empty column for the slope forward and backward
      single.site$slp.b <- rep(NA, length.out = nrow(single.site))
      single.site$slp.f <- rep(NA, length.out = nrow(single.site))
      
      for(i in 2:(nrow(single.site)-1)){
          ##-calculate the slope back one day
          single.site$slp.b[i] <- (single.site$X_00060_00003[i]-single.site$X_00060_00003[(i-1)])/(single.site$Seconds[i]-single.site$Seconds[(i-1)])
          ##-calculate the slope forward one day
          single.site$slp.f[i] <- (single.site$X_00060_00003[(i+1)]-single.site$X_00060_00003[i])/(single.site$Seconds[(i+1)]-single.site$Seconds[i])
        }
      
      ##-make a column for the peak of each event flagged by a change in derivative
      single.site$peak.flag <- rep(NA, length.out = nrow(single.site))
      ##-now flag those derivative changes
      for(i in 2:(nrow(single.site)-1)){
        ##-if the slope back is greater than some threshold and the slope forward is negative, flag it as a peak
        if(single.site$slp.b[i]>0.0001 & single.site$slp.f[i]<0){
          single.site$peak.flag[i] <- 1
        }else{
          ##-otherwise don't
          single.site$peak.flag[i] <- 0
        }
      }
      
      ##-Flag the changes in derivatitives, events is the row of single site which have events
      events <- which(single.site$peak.flag == 1)
      ##-if there are no events move on
      if(length(events) == 0){
        next
      }else{
        ##-within single site, make a column that will signal which observations belong to which peak
        ##-this will turn into the rising and falling limbs
        single.site$event.flag <- rep(NA, length.out = nrow(single.site))
        
        for(i in 1:length(events)){
          k <- events[i]
          ##-the while loop goes forward in time until the slope forward is no longer negative
          while(single.site$slp.f[k] <0){
            ##-and labels that with the event number (i) and makes it positive (+)
            single.site$event.flag[k] <- i
            k <- k +1
            
            ##-if the last row of single sites is an event peak then move on
            if(k == nrow(single.site)){
              break
            }else{
            }
          }
          
          ##-now step backward in time for the falling limb
          ##-starting with the point directly before the peak
          j <- events[i]-1
          ##-if it's the first two data points in the site don't bother
          if(j == 1|j == 0){
            next
          }else{
            ##-as you step backwards label the days with the event number (i) and make it negative (-)
            ##-this time to indicate it is the rising limb (before the peak)
            while(single.site$slp.b[j] > 0){
              single.site$event.flag[j] <- -1*i
              ##-label j-1 as part of the rising limb too because j-1 won't be grouped with the rising limb
              ##-if it has a negative slope on the next step going back
              single.site$event.flag[j-1] <- -1*i
              j <- j - 1
              ##-if i is 1,2 or 3 in the data frame forget about it
              if(j == 2| j == 1| j == 0){
                break
              }else{
              }
            }
          }
        }
        
        
        ##-The above algorithm basically flags any change in slope as a potential event, below we cull those events
        ##-to pull out the ones that we think are the most interesting/relevant/real
        
        #plot the events and see what they look like
        #plot(single.site$Seconds, single.site$X_00060_00003,typ = 'l', ylab = 'Discharge ft3', xlab = '')#, xlim = c(1.44e09,1.46e09))
        #points(single.site[single.site$peak.flag == 1,]$Seconds, single.site[single.site$peak.flag == 1,]$X_00060_00003, col = 'red', pch = 16)
        #points(single.site[!is.na(single.site$event.flag),]$Seconds, single.site[!is.na(single.site$event.flag),]$X_00060_00003, col = 'blue')
        #points(single.site[!is.na(single.site$event.flag)&single.site$event.flag<0,]$Seconds, single.site[!is.na(single.site$event.flag)&single.site$event.flag<0,]$X_00060_00003, col = 'green')
        #legend('topleft', col = c('red','green','blue'), legend = c('Peak','Rising Limb','Falling Limb'), bty = 'n', pch = c(16,1,1), title = 'Event')
        
        
        ##-save the single site record to a list
        single.site.record[[l]] <- single.site
        ##-for each event that passes the tests we will have a list of features describing it
        event.features[[l]] <- list()
        ##-normalize the concentration and the discharge
        single.site$q.norm <- (single.site$X_00060_00003-min(single.site$X_00060_00003, na.rm = T))/(max(single.site$X_00060_00003, na.rm = T)-min(single.site$X_00060_00003, na.rm = T))
        single.site$c.norm <- (single.site$X_99133_00003-min(single.site$X_99133_00003, na.rm = T))/(max(single.site$X_99133_00003, na.rm = T)-min(single.site$X_99133_00003, na.rm = T))
        ##-single load is the n load for each day in g/day (daily load)
        single.site$load.raw <- single.site$X_00060_00003*single.site$X_99133_00003*2446.58
        ##-this is the fraction of the total load (for the whole record) that is accounted for in each day
        single.site$load.frac <- (single.site$load.raw)/sum(single.site$load.raw, na.rm = T)
       
        #####---------------EVENT FEATURES---------------#####
        ##-start culling the events
        for(i in 1:length(events)){
          ##-select event i and remove any NAs that migth have popped up
          temp.event <- single.site[abs(single.site$event.flag) == i & !is.na(single.site$event.flag),]
          ##-if the rising limb or the falling limb is zero days long we aren't interested
          if(nrow(temp.event[temp.event$event.flag<0,])==0|nrow(temp.event[temp.event$event.flag>0,])==0){
            next
          }else{
            ##-look at the rate of rise on the rising limb and use that to cull some events that don't rise sharply
            ##-use the normalized discharge so it is comparable for multiple sites
            event.dq <- temp.event[temp.event$event.flag>0,]$q.norm[1] - temp.event[temp.event$event.flag<0,]$q.norm[1]
            ##-calculate dt in days, both dq and dt are from the start of the event to the peak
            event.dt <- (temp.event[temp.event$event.flag>0,]$Seconds[1]/86400) - (temp.event[temp.event$event.flag<0,]$Seconds[1]/86400)
            ##-0.015 was chosen from looking at the 1st quartile value for all sites
            if(event.dq/event.dt < 0.015){
              next
            }else{
            ##-if there are any NAs within the event we aren't interested
              if(TRUE %in% is.na(temp.event$X_99133_00003)){
                next
              }else{
                ##-if the max discharge is less that 10% of the max discharge of the total record we aren't interested
                if(max(temp.event$q.norm)<0.1){
                  next
                }else{
                  ##-if the event is shorter than four days we aren't interested
                  if(nrow(temp.event)<4){
                    next
                  }else{
                  
                  event.features[[l]][[i]] <- list()
                  ##-rising limb, made up of the rising limb and the peak value
                  temp.event.rising <- rbind(single.site[which(single.site$event.flag == (-1*i) & is.na(single.site$event.flag) == F),], single.site[which(single.site$event.flag == i),][1,])
                  ##-falling limb made up of the falling limb and the peak value
                  temp.event.falling <- single.site[which(single.site$event.flag == (i) & is.na(single.site$event.flag) == F),]
                  ##-full event
                  temp.event <- single.site[which(abs(single.site$event.flag) == i),]
                  
                  #assign values to list
                  event.features[[l]][[i]]$rising.limb <- temp.event.rising
                  event.features[[l]][[i]]$falling.limb <- temp.event.falling
                  event.features[[l]][[i]]$event <- temp.event
                  
                  q.o <- single.site[order(single.site$X_00060_00003),'X_00060_00003']
                  event.features[[l]][[i]]$max.flow <- round(which(q.o == max(temp.event$X_00060_00003))[1]/nrow(single.site), digits = 2)
                  event.features[[l]][[i]]$dq <- temp.event.falling$q.norm[1] - temp.event.rising$q.norm[1]
                  event.features[[l]][[i]]$dq.raw <- temp.event.falling$X_00060_00003[1] - temp.event.rising$X_00060_00003[1]
                  event.features[[l]][[i]]$dt <- temp.event.falling$Seconds[1] - temp.event.rising$Seconds[1]
                  event.features[[l]][[i]]$event.length <- temp.event$Seconds[nrow(temp.event)] - temp.event$Seconds[1]
                  ##-event cumulative flow
                  event.features[[l]][[i]]$cum.flow <- sum(temp.event$q.norm, na.rm = T)
                  event.features[[l]][[i]]$cum.flow.raw <- sum(temp.event$X_00060_00003, na.rm = T)
                  ##-event load
                  event.features[[l]][[i]]$sum.raw.load <- sum(temp.event$load.raw, na.rm = T)
                  event.features[[l]][[i]]$sum.frac.load <- sum(temp.event$load.frac, na.rm = T)
                  ##-rising limb load
                  event.features[[l]][[i]]$sum.raw.load.rising <- sum(temp.event.rising$load.raw, na.rm = T)
                  event.features[[l]][[i]]$sum.frac.load.rising <- sum(temp.event.rising$load.frac, na.rm = T)
                  ##-falling limb load
                  event.features[[l]][[i]]$sum.raw.load.falling <- sum(temp.event.falling$load.raw, na.rm = T)
                  event.features[[l]][[i]]$sum.frac.load.falling <- sum(temp.event.falling$load.frac, na.rm = T)
                  
                  Time <- temp.event.falling$Seconds
                  exp.mod <- lm(log(temp.event.falling$q.norm)~Time)
                  #time.values<- (seq(min(temp.event$Seconds),max(temp.event$Seconds),100000))
                  #pred <- exp(predict(exp.mod,list(Time=time.values)))
                  #lines(time.values, pred)
                  event.features[[l]][[i]]$recession.decay <- as.numeric(exp.mod$coefficients[2])
                  event.features[[l]][[i]]$month <- format(temp.event$DateTime[1], format = '%m')
                  #event.features[[l]][[i]]$bf.lag <- max(temp.event$bf.norm, na.rm = T)-max(temp.event$q.norm, na.rm = T)
                  
                  ##-calculate how long it was since the last event, although the last event will be the last
                  ##-inflection in slope not the last actual event
                  ##-THIS NEEDS REVISION-##
                  if(i !=1){
                    last <- single.site[which(abs(single.site$event.flag) == (i-1)),]
                    event.features[[l]][[i]]$prev.event <- temp.event$Seconds[1] - last$Seconds[nrow(last)]
                  }
                  
                  #try to classify CQ dynamics and hysteresis
                  event.features[[l]][[i]]$rising.cq <- round(as.numeric(coef(lm(log(temp.event.rising$X_99133_00003)~log(temp.event.rising$X_00060_00003)))[2]), digits = 2)
                  event.features[[l]][[i]]$rising.cq.r2 <- round(as.numeric(summary(lm(log(temp.event.rising$X_99133_00003)~log(temp.event.rising$X_00060_00003)))$r.squared), digits = 2)
                  event.features[[l]][[i]]$falling.cq <- round(as.numeric(coef(lm(log(temp.event.falling$X_99133_00003)~log(temp.event.falling$X_00060_00003)))[2]), digits = 2)
                  event.features[[l]][[i]]$falling.cq.r2 <- round(as.numeric(summary(lm(log(temp.event.falling$X_99133_00003)~log(temp.event.falling$X_00060_00003)))$r.squared), digits = 2)
                  event.features[[l]][[i]]$event.cq <- round(as.numeric(coef(lm(log(temp.event$X_99133_00003)~log(temp.event$X_00060_00003)))[2]), digits = 2)
                  event.features[[l]][[i]]$event.cq.r2 <- round(as.numeric(summary(lm(log(temp.event$X_99133_00003)~log(temp.event$X_00060_00003)))$r.squared), digits = 2)
                  
                  #classify hysteresis using Lawler 2006 first not log transformed the log transformed
                  qmid <- (max(temp.event$X_00060_00003, na.rm = T)-min(temp.event$X_00060_00003, na.rm = T))*0.5 + min(temp.event$X_00060_00003, na.rm = T)
                  #linear model of rising discharge
                  rising.discharge <- temp.event.rising$X_00060_00003
                  rising.lm <- lm(temp.event.rising$X_99133_00003~rising.discharge)
                  conc.mid.r <- predict(rising.lm, list(rising.discharge = qmid))
                  event.features[[l]][[i]]$r.cq.slp.lin.rsq <- summary(rising.lm)[[8]]
                  #linear model of falling discharge
                  falling.discharge <- temp.event.falling$X_00060_00003
                  falling.lm <- lm(temp.event.falling$X_99133_00003~falling.discharge)
                  conc.mid.f <- predict(falling.lm, list(falling.discharge = qmid))
                  event.features[[l]][[i]]$f.cq.slp.lin.rsq <- summary(falling.lm)[[8]]
                  #calculate h index
                  event.features[[l]][[i]]$hindex.lin <- (conc.mid.r/conc.mid.f)-1
                  
                  #now make the hysteresis classificiations when the data are log transformed
                  #first for the rising limb
                  rising.discharge <- log(temp.event.rising$X_00060_00003)
                  rising.lm.log <- lm(log(temp.event.rising$X_99133_00003)~rising.discharge)
                  conc.mid.r.log <- predict(rising.lm.log, list(rising.discharge = log(qmid)))
                  event.features[[l]][[i]]$r.cq.slp.log.rsq <- summary(rising.lm.log)[[8]]
                  #falling limb
                  falling.discharge <- log(temp.event.falling$X_00060_00003)
                  falling.lm.log <- lm(log(temp.event.falling$X_99133_00003)~falling.discharge)
                  conc.mid.f.log <- predict(falling.lm.log, list(falling.discharge = log(qmid)))
                  event.features[[l]][[i]]$f.cq.slp.log.rsq <- summary(falling.lm.log)[[8]]
                  
                  #calculate h index
                  event.features[[l]][[i]]$hindex.log <- (conc.mid.r.log/conc.mid.f.log)-1
                  
                  
                  #plotting the event
                  layout(matrix(c(1,1,2,3,4,5),3,2,byrow = TRUE), TRUE)
                  #par(mfrow = c(3,1), mar = c(4,2,2,1), mai = c(0,0.6,0,0.2), oma = c(3,1,3,1), mgp = c(3,0.5,0))
                  par(mar = c(4,2,2,1), mai = c(0.2,0.6,0.2,0.2), oma = c(3,1,3,1), mgp = c(3,0.5,0))
                  plot(single.site$DateTime, single.site$X_00060_00003, typ = 'l', ylab = 'Discharge ft3', xlab = '', las = 1)
                  lines(temp.event[temp.event$event.flag<0,'DateTime'],temp.event[temp.event$event.flag<0,'X_00060_00003'], col = 'red', lwd = 1.5)
                  lines(temp.event[temp.event$event.flag>0,'DateTime'],temp.event[temp.event$event.flag>0,'X_00060_00003'], col = 'blue', lwd = 1.5)
                  mtext(side = 3, line = 1.5, paste('Event',i,sites[[l]]$station_nm,sep = ' '))
                  
                  #Q
                  plot(temp.event$DateTime, temp.event$X_00060_00003, ylab = 'Discharge ft3', xlab = 'DateTime', typ = 'b', las = 1)#, ylim = c(0,30000))
                  points(temp.event[temp.event$event.flag<0,'DateTime'],temp.event[temp.event$event.flag<0,'X_00060_00003'], col = 'red', cex = 2)
                  points(temp.event[temp.event$event.flag>0,'DateTime'],temp.event[temp.event$event.flag>0,'X_00060_00003'], col = 'blue', cex = 2)
                  #C
                  plot(temp.event$DateTime, temp.event$X_99133_00003, ylab = 'Concentration', xlab = 'DateTime', typ = 'b', las = 1)
                  points(temp.event[temp.event$event.flag<0,'DateTime'],temp.event[temp.event$event.flag<0,'X_99133_00003'], col = 'red', cex = 2)
                  points(temp.event[temp.event$event.flag>0,'DateTime'],temp.event[temp.event$event.flag>0,'X_99133_00003'], col = 'blue', cex = 2)
                  #CQ
                  plot(log(temp.event$X_00060_00003), log(temp.event$X_99133_00003), ylab = 'logC', xlab = 'logQ', las = 1, col = 'white')
                  points(log(temp.event[temp.event$event.flag<0,]$X_00060_00003), log(temp.event[temp.event$event.flag<0,]$X_99133_00003), cex = 2, col = 'red')
                  points(log(temp.event[temp.event$event.flag>0,]$X_00060_00003), log(temp.event[temp.event$event.flag>0,]$X_99133_00003), ylab = 'logC', xlab = 'logQ', las = 1, cex = 2, col = 'blue')
                  mtext(side = 1, line = 1.5, 'logQ', cex = 0.8)
                  #Stats
                  q.o <- single.site[order(single.site$X_00060_00003),'X_00060_00003']
                  plot(log(temp.event$X_00060_00003), log(temp.event$X_99133_00003), ylab = '', xlab = '', las = 1, col = 'white', axes = F)
                  legend('top', pch = NA, bty = 'n', cex = 0.8, legend = c(paste('Event Duration ',difftime(temp.event$dateTime[nrow(temp.event)],temp.event$dateTime[1])),
                                                                           paste('Percentile Max Flow',round(which(q.o == max(temp.event$X_00060_00003))[1]/nrow(single.site), digits = 2)),
                                                                           paste('Rising CQ Slope',round(as.numeric(event.features[[l]][[i]]$rising.cq), digits = 2)),
                                                                           paste('Rising CQ Slope R2',round(event.features[[l]][[i]]$rising.cq.r2, digits = 2)),
                                                                           paste('Receeding CQ Slope', round(event.features[[l]][[i]]$falling.cq, digits = 2)),
                                                                           paste('Receeding CQ Slope R2',round(event.features[[l]][[i]]$falling.cq.r2, digits = 2)),
                                                                           paste('Event CQ Slope', round(event.features[[l]][[i]]$event.cq, digits = 2)),
                                                                           paste('Event CQ Slope R2',round(event.features[[l]][[i]]$event.cq.r2, digits = 2))
                                                                           #paste('Month', site.events[[i]]$month)#,
                                                                           #paste('Linear H index', round(site.events[[i]]$hindex.lin, digits = 2)),
                                                                           #paste('Log H index', round(site.events[[i]]$hindex.log, digits = 2))
                  ))
                  
                  
                  
                }
              }
            }
          }
        }
      }
        print(sites[[l]]$station_nm)
        #these are a couple of else brackets
      }}}
}

#####
#===================================================================================#

######################################################################################################################################################################################################
######################################################################################################################################################################################################
######################################################################################################################################################################################################
######################################################################################################################################################################################################

#this is a loop for going through the event.features structure and 
#pulling out the features into a dataframe for each site, placed within the list
#recent.events
recent.events <- list()
k <- list()
for(j in 1:length(event.features)){
  site.events <- event.features[[j]]
  
  
  
  #number of events
  event.number <- lapply(site.events, function(x) c(x$event.cq)) %>%
    unlist() %>%
    length()
  #get the stats you want
  a <- lapply(site.events, function(x) c(x$event.cq, x$event.cq.r2, x$month, x$max.flow, x$sum.raw.load, x$dq, x$dt, x$falling.limb$dateTime[1], x$rising.cq, x$falling.cq))
  #collapse into a dataframe
  site.df <- data.frame(matrix(unlist(a), nrow = event.number, byrow = T))
  colnames(site.df) <- c('event.cq', 'event.cq.r2', 'month','max.flow', 'sum.raw.load', 'dq','dt', 'dateTime', 'rising.cq', 'falling.cq')
  site.df$dateTime <- as.POSIXlt(as.numeric(as.character(site.df$dateTime)), origin = '1970-01-01')
  site.df$rising.cq <- as.numeric(as.character(site.df$rising.cq))
  site.df$falling.cq <- as.numeric(as.character(site.df$falling.cq))
  
  site.df$dq <- as.numeric(as.character(site.df$dq))
  site.df$dt <- as.numeric(as.character(site.df$dt))
  site.df$event.cq <- as.numeric(as.character(site.df$event.cq))
  site.df$event.cq.r2 <- as.numeric(as.character(site.df$event.cq.r2))
  site.df$max.flow <- as.numeric(as.character(site.df$max.flow))
  site.df$sum.raw.load <- as.numeric(as.character(site.df$sum.raw.load))
  site.df$season <- NA
  site.df[site.df$month %in% c('01','02','03'),'season'] <- '2-JFM'
  site.df[site.df$month %in% c('04','05','06'),'season'] <- '3-AMJ'
  site.df[site.df$month %in% c('07','08','09'),'season'] <- '4-JAS'
  site.df[site.df$month %in% c('10','11','12'),'season'] <- '1-OND'
  
  
  cvc.cvq <- vector()
  #calculate the event cv
  for(m in 1:length(site.events)){
    
    if(is.null(site.events[[m]])){
      next
    }else{
      
      sigma.q <- sd(site.events[[m]]$event$X_00060_00003)
      mu.q <- mean(site.events[[m]]$event$X_00060_00003)
      
      sigma.c <- sd(site.events[[m]]$event$X_99133_00003)
      mu.c <- mean(site.events[[m]]$event$X_99133_00003)
      
      temp <- (sigma.c/mu.c)/(sigma.q/mu.q)
      cvc.cvq <- c(temp,cvc.cvq)
    }}
  site.df$cvc.cvq <- cvc.cvq
  
  #cut the events so that we only have onces from 2016 on
  
  site.df <- site.df[site.df$dateTime > as.POSIXct('2016-01-01'),]
  
  #make cq slope into a factor depletion, enrichment and statis
  site.df$cq.regime <- NA
  site.df[site.df$event.cq > 0.2,'cq.regime'] <- 'enrichment'
  site.df[site.df$event.cq < -0.2,'cq.regime'] <- 'depletion'
  site.df[site.df$event.cq <= 0.2 & site.df$event.cq >= -0.2,'cq.regime'] <- 'statis'
  
  recent.events[[j]] <- site.df
  
  k[[j]] <- ggplot(data = site.df) +
    geom_point(aes(dq/dt, rising.cq, colour = season), size = 3) +
    scale_x_continuous(trans='log10') +
    ggtitle(sites[[j]]$station_nm) +
    #xlim(0,3)#+
    ylim(-1,1)#+
  #scale_color_continuous(low = 'blue', high = 'red')
  
}
#get the event cq for each site
lapply(recent.events, function(x) mean(x$event.cq))

#make a data frame is is amenable to plotting
for(i in 1:length(recent.events)){
recent.events[[i]]$site <- rep(cq.df[i,'site'], length.out = nrow(recent.events[[i]]))
recent.events[[i]]$crops <- rep(cq.df[i,'crops'], length.out = nrow(recent.events[[i]]))
recent.events[[i]]$crops100m <- rep(cq.df[i,'crops100m'], length.out = nrow(recent.events[[i]]))
recent.events[[i]]$crops1km <- rep(cq.df[i,'crops1km'], length.out = nrow(recent.events[[i]]))
recent.events[[i]]$cq.tot <- as.numeric(rep(cq.df[i,'cq.tot'], length.out = nrow(recent.events[[i]])))
}

all.recent.events <- recent.events[[1]][1,]
all.recent.events <- all.recent.events[-1,]
for(i in 1:length(recent.events)){
  all.recent.events <- rbind(all.recent.events,recent.events[[i]])
}

g1 <- ggplot(all.recent.events, aes(x = crops, y = event.cq, colour = site))+
  geom_boxplot()
g2 <- ggplot(all.recent.events, aes(x = crops100m, y = event.cq, colour = site))+
  geom_boxplot()
g3 <- ggplot(all.recent.events, aes(x = crops, y = cq.tot, colour = site), size = 3)+
  geom_point()
g4 <- ggplot(all.recent.events, aes(x = crops100m, y = cq.tot, colour = site), size = 3)+
  geom_point()

grid.arrange(g1,g2,g3,g4, nrow = 2)

boxplot(recent.events[[1]]$event.cq, recent.events[[2]]$event.cq, recent.events[[3]]$event.cq,recent.events[[4]]$event.cq, recent.events[[5]]$event.cq, recent.events[[6]]$event.cq, 
        at = c(cq.df$crops[1]*65, cq.df$crops[2]*65, cq.df$crops[3]*65,cq.df$crops[4]*65, cq.df$crops[5]*65, cq.df$crops[6]*65), 
        #at = c(1,3,9),
        xlim = c(30,65), 
        col = c('red','green','blue'))

#recent.events[[1]] <- cbind(recent.events[[1]],rep(cq.df[1,],length.out = )

######
#This is a section for using the selected events from event.features
#to select the "non-event" periods for analysis
#####

#make sure to select the event features after 2006
#temp.events is a list of the beginning and end time of each event, temporarily in a 
#list to be placed in a dataframe 
single.site.non.events <- list()
single.site.events <- list()
for(i in 1:length(event.features)){
temp.events <- list()

#for all the events that you've picked out
site.events <- event.features[[i]]
#get all the time stamps of the events
temp.events <- lapply(site.events, function(x) as.character(x$event$dateTime))

#put it in a structure called all events which is the datetime stamp of all events
be.events <- data.frame(matrix(unlist(temp.events)))
all.events <- strptime(as.POSIXct(be.events[,1], format = '%Y-%m-%d'), tz = 'GMT', format = '%Y-%m-%d')

#single.site.record is the full record at the site
#select the events and the non.events
single.site.non.events[[i]] <- single.site.record[[i]][!(single.site.record[[i]]$DateTime %in% all.events),]
single.site.events[[i]] <- single.site.record[[i]][(single.site.record[[i]]$DateTime %in% all.events),]

par(mfrow = c(4,1), mai = c(0,0.8,0,0.5), omi = c(0.8,0.8,0.6,0.2), mgp = c(4,1,0), xpd = F)
plot(single.site.record[[i]]$dateTime, single.site.record[[i]]$X_00060_00003, xlim = c(as.POSIXct('2016-01-01'), as.POSIXct('2017-01-01')), typ = 'b',
     las = 1, xlab = 'Date', ylab = 'Discharge (cfs)')
points(single.site.events[[i]]$dateTime, single.site.events[[i]]$X_00060_00003, col = 'red')
points(single.site.non.events[[i]]$dateTime, single.site.non.events[[i]]$X_00060_00003, col = 'blue')
legend('right', col = c('red','blue'), pch = 1, legend = c('Peak','Not Peak'), bty = 'n', title = '2016')
mtext(sites[[i]]$station_nm,side = 3, line = 2)
plot(single.site.record[[i]]$dateTime, single.site.record[[i]]$X_00060_00003, xlim = c(as.POSIXct('2017-01-01'), as.POSIXct('2018-01-01')), typ = 'b',
     las = 1, xlab = 'Date', ylab = 'Discharge (cfs)')
points(single.site.events[[i]]$dateTime, single.site.events[[i]]$X_00060_00003, col = 'red')
points(single.site.non.events[[i]]$dateTime, single.site.non.events[[i]]$X_00060_00003, col = 'blue')
legend('right', col = c('red','blue'), pch = 1, legend = c('Peak','Not Peak'), bty = 'n', title = '2017')
plot(single.site.record[[i]]$dateTime, single.site.record[[i]]$X_00060_00003, xlim = c(as.POSIXct('2018-01-01'), as.POSIXct('2019-01-01')), typ = 'b',
     las = 1, xlab = 'Date', ylab = 'Discharge (cfs)')
points(single.site.events[[i]]$dateTime, single.site.events[[i]]$X_00060_00003, col = 'red')
points(single.site.non.events[[i]]$dateTime, single.site.non.events[[i]]$X_00060_00003, col = 'blue')
legend('right', col = c('red','blue'), pch = 1, legend = c('Peak','Not Peak'), bty = 'n', title = '2018')
plot(single.site.record[[i]]$dateTime, single.site.record[[i]]$X_00060_00003, xlim = c(as.POSIXct('2019-01-01'), as.POSIXct('2020-01-01')), typ = 'b',
     las = 1, xlab = 'Date', ylab = 'Discharge (cfs)')
points(single.site.events[[i]]$dateTime, single.site.events[[i]]$X_00060_00003, col = 'red')
points(single.site.non.events[[i]]$dateTime, single.site.non.events[[i]]$X_00060_00003, col = 'blue')
legend('right', col = c('red','blue'), pch = 1, legend = c('Peak','Not Peak'), bty = 'n', title = '2019')



}

#now trim single.site.non.events to 2016 onward
recent.single.site.non.events <- list()
recent.single.site.events <- list()
for(i in 1:length(single.site.non.events)){
  recent.single.site.non.events[[i]] <- single.site.non.events[[i]][single.site.non.events[[i]]$dateTime > as.POSIXlt('2016-01-01'),]
  recent.single.site.events[[i]] <- single.site.events[[i]][single.site.events[[i]]$dateTime > as.POSIXlt('2016-01-01'),]
}

#clip out the everything below NO3 = 0.5 for jefferson as those data are erroneous

recent.single.site.non.events[[2]] <- recent.single.site.non.events[[2]][recent.single.site.non.events[[2]]$X_99133_00003 > 0.5,] 


#get the slopes of the events and the non events
#events
eventdays.cq <- unlist(lapply(recent.single.site.events, function(x) coef(lm(log(x$X_99133_00003)~log(x$X_00060_00003)))[2]))
#non-events
noneventdays.cq <- unlist(lapply(recent.single.site.non.events, function(x) coef(lm(log(x$X_99133_00003)~log(x$X_00060_00003)))[2]))
#get the slope of all the data
coef(lm(log(c(recent.single.site.events[[1]]$X_99133_00003,recent.single.site.non.events[[1]]$X_99133_00003))~log(c(recent.single.site.events[[1]]$X_00060_00003,recent.single.site.non.events[[1]]$X_00060_00003))))[2]
coef(lm(log(c(recent.single.site.events[[2]]$X_99133_00003,recent.single.site.non.events[[2]]$X_99133_00003))~log(c(recent.single.site.events[[2]]$X_00060_00003,recent.single.site.non.events[[2]]$X_00060_00003))))[2]
coef(lm(log(c(recent.single.site.events[[3]]$X_99133_00003,recent.single.site.non.events[[3]]$X_99133_00003))~log(c(recent.single.site.events[[3]]$X_00060_00003,recent.single.site.non.events[[3]]$X_00060_00003))))[2]

#make a threshold for the non-events such that if they are above that threshold they shouldn't be counted
recent.single.site.non.events.th <- lapply(recent.single.site.non.events, function(x) x[x$X_00060_00003 < 0.5*max(x$X_00060_00003),])

#recalculate the non.event cq with that threshold applied
unlist(lapply(recent.single.site.non.events.th, function(x) coef(lm(log(x$X_99133_00003)~log(x$X_00060_00003)))[2]))

#cq.df is a data frame with the cropage and cq event based data from LandUseBufferStats_191005.R
cq.df$eventdays.cq <- eventdays.cq
cq.df$noneventdays.cq <- noneventdays.cq
cq.df$site.pcode <- c('K1-Davis','K2-Dunns','K3-Shelby','R1-Sac City','R2-Jefferson','R3-Van Meter')
cq.df$cq.tot <- as.numeric(cq.df$cq.tot)

g1 <- ggplot(cq.df, aes(crops, cq.tot, colour = site.pcode))+
  geom_point(size = 3)+
  ylim(-0.2,1.3)
g2 <- ggplot(cq.df, aes(crops, cq.event, colour = site.pcode))+
  geom_point(size = 3)+
  geom_point(aes(crops, eventdays.cq, colour = site.pcode), size = 3, shape = 1)+
  ylim(-0.2,1.3)
g3 <- ggplot(cq.df, aes(crops, noneventdays.cq, colour = site.pcode))+
  geom_point(size = 3)+
  ylim(-0.2,1.3)
#100 m buffer
g4 <- ggplot(cq.df, aes(crops100m, cq.tot, colour = site.pcode))+
  geom_point(size = 3)+
  ylim(-0.2,1.3)
g5 <- ggplot(cq.df, aes(crops100m, cq.event, colour = site.pcode))+
  geom_point(size = 3)+
  geom_point(aes(crops100m, eventdays.cq, colour = site.pcode), size = 3, shape = 1)+
  ylim(-0.2,1.3)
g6 <- ggplot(cq.df, aes(crops100m, noneventdays.cq, colour = site.pcode))+
  geom_point(size = 3)+
  ylim(-0.2,1.3)
#1 km buffer
g7 <- ggplot(cq.df, aes(crops1km, cq.tot, colour = site.pcode))+
  geom_point(size = 3)+
  ylim(-0.2,1.3)
g8 <- ggplot(cq.df, aes(crops1km, cq.event, colour = site.pcode))+
  geom_point(size = 3)+
  geom_point(aes(crops1km, eventdays.cq, colour = site.pcode), size = 3, shape = 1)+
  ylim(-0.2,1.3)
g9 <- ggplot(cq.df, aes(crops1km, noneventdays.cq, colour = site.pcode))+
  geom_point(size = 3)+
  ylim(-0.2,1.3)

grid.arrange(g1,g2,g3,g4,g5,g6,g7,g8,g9, nrow = 3)

