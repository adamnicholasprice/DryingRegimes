bp <- function(metric){
  ggplot(data=df)+
    geom_violin(aes(x=paste('Cluster',factor(kmeans)),y=metric,group=kmeans,fill = factor(kmeans)))+
    scale_fill_manual(values = cols,"Cluster Membership")+
    theme_light()
}

peak2zero = bp(df$peak2zero)+
  ylim(c(0,70))+
  ylab("Dry-down duration\n(Days)")+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank())


drying_rate = bp(df$drying_rate) + 
  # ylim(0,2)+
  ylab("Drying rate\n(1/Days)")+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank())

dry_date_start = bp(df$dry_date_start)+
  ylab("Dry date start\n(Day of Year)")+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank())


dry_dur = bp(df$dry_dur) + 
  ylim(c(0,400))+
  ylab("No flow duration\n(Days)")+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank())

peakQ = bp(df$peak_quantile)+
  ylab("Antecedent peak quantile")+
  xlab(NULL)


ann_freq = bp(df$freq_local)+
  ylim(c(0,15))+
  ylab("Annual event requency")+
  xlab(NULL)


p = peak2zero  + drying_rate + dry_date_start + dry_dur + peakQ + ann_freq + 
  plot_layout(ncol = 2, guides = "collect") & theme(legend.position = "none")

p

pdf("docs//violinPlots.pdf")
p
dev.off()

