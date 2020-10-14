
dat = read.csv("../data/metrics_by_event_combined.csv")




quant = ggplot(dat,aes(x=dry_date_start,y = peak_quantile)) +
  geom_bin2d(binwidth = c(5,.01)) +
  # ylim(0,365) +
  xlab("Drying Event Start (day)")+
  ylab("Peak Quantile Value") +
  scale_fill_viridis()

quant


tt = ggplot(dat,aes(x=dry_date_start,y = dec_lat_va)) +
  geom_bin2d(binwidth = c(5,2)) +
  # ylim(0,365) +
  xlab("Drying Event Start (day)")+
  ylab("Peak Quantile Value") +
  scale_fill_viridis()
  
tt

tt = ggplot(dat,aes(x=dry_date_start,y = dec_long_va)) +
  geom_bin2d(binwidth = c(5,2)) +
  # ylim(0,365) +
  xlab("Drying Event Start (day)")+
  ylab("Peak Quantile Value") +
  scale_fill_viridis()

tt

tt = ggplot(dat,aes(x=dry_date_start,y = Name)) +
  geom_bin2d(binwidth = c(5,2)) +
  # ylim(0,365) +
  xlab("Drying Event Start (day)")+
  ylab("Aggregated Ecoregion") +
  scale_fill_viridis()

tt

tt = ggplot(dat,aes(x=dry_date_start,y = season)) +
  geom_bin2d(binwidth = c(5,2)) +
  # ylim(0,365) +
  xlab("Drying Event Start (day)")+
  ylab("Season") +
  scale_fill_viridis()

tt


tt = ggplot(dat,aes(x=dry_date_start,y = calendar_year)) +
  geom_bin2d(binwidth = c(5,1)) +
  # ylim(0,365) +
  xlab("Drying Event Start (day)")+
  ylab("Calendar Year") +
  scale_fill_viridis()

tt

tt = ggplot(dat,aes(x=peak_date,y = calendar_year)) +
  geom_bin2d(binwidth = c(5,1)) +
  # ylim(0,365) +
  xlab("Drying Event Start (day)")+
  ylab("Calendar Year") +
  scale_fill_viridis()

tt


tt = ggplot(dat,aes(x=dry_date_start,y = )) +
  geom_bin2d(binwidth = c(5,1)) +
  # ylim(0,365) +
  xlab("Drying Event Start (day)")+
  ylab("Calendar Year") +
  scale_fill_viridis()

tt

