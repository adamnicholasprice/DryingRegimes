tt = df %>% 
  select(kmeans,dry_date_start)

tt = tt %>% 
  mutate(a.day = if_else(dry_date_start>58, dry_date_start-61, dry_date_start+305)) %>%
  group_by(kmeans,a.day,dry_date_start)%>%
  count()%>% 
  group_by(kmeans) %>%
  mutate(event.cum = cumsum(n))

tt$j.date = lubridate::as_date(tt$dry_date_start)

tt<-tt %>% 
  mutate(j.date = if_else(j.date<"1970-03-01", j.date+365, j.date+0))

tt$name<-factor(ts$name, levels=c('one','two','three','four'))

ggplot(tt) +
  geom_line(aes(x=j.date,y=event.cum,color=factor(kmeans)))


cols <- 
  c("1" = "#4477AA",
    "2" = "#66CCEE",
    "3" = "#228833",
    "4" = "#CCBB44")


cumulative.plot = ggplot(tt) +
  geom_line(aes(x=j.date,y=event.cum,color=factor(kmeans)))+
  scale_color_manual(
    values=cols, 
    name = "Cluster")+
  scale_x_date(labels = date_format("%B"),expand = c(0,0))+
  theme_bw() + 
  ylab('Number of Events') +
  xlab('Month') +
  #Axes Options
  theme(axis.title = element_text(size=14),
        axis.text  = element_text(size = 10),
        legend.position = 'none',
        axis.text.x = element_blank(),
        axis.title.x = element_blank())+
  scale_y_continuous(expand = c(0,0))


cumulative.plot =  cumulative.plot + annotate("text", x = as.Date("1970-09-01"), 
                                         y = c(3000,1300,8000,5000), 
                                         label = c("Cluster 1","Cluster 2","Cluster 3","Cluster 4"),
                                         color = c("#4477AA","#66CCEE","#228833","#CCBB44"))



### Cdf
tt = df %>% 
  select(kmeans,dry_date_start) %>%
  mutate(a.day = if_else(dry_date_start>58, dry_date_start-61, dry_date_start+305))

tt$j.date = lubridate::as_date(tt$dry_date_start)
tt<-tt %>% 
  mutate(j.date = if_else(j.date<"1970-03-01", j.date+365, j.date+0))


CDF = ggplot(tt,aes(x=j.date,color=factor(kmeans)))+
  stat_ecdf(geom="smooth")+
  scale_color_manual(
    values=cols, 
    name = "Cluster")+
  scale_x_date(labels = date_format("%B"),expand = c(0,0))+
  theme_bw() + 
  ylab('Number of Events') +
  xlab('Month') +
  #Axes Options
  theme(axis.title = element_text(size=14),
        axis.text  = element_text(size = 10),
        legend.position = 'none',
        axis.text.x = element_blank(),
        axis.title.x = element_blank())+
  scale_y_continuous(expand = c(0,0))


