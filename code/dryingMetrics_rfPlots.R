
am.df = as.data.frame(h2o.varimp(am)) %>% top_n(15)

c1.df = as.data.frame(h2o.varimp(c1.m))%>% top_n(15)

c2.df = as.data.frame(h2o.varimp(c2.m))%>% top_n(15)

c3.df = as.data.frame(h2o.varimp(c3.m))%>% top_n(15)

c4.df = as.data.frame(h2o.varimp(c4.m))%>% top_n(15)


library(gt)

ws = c('DRAIN_SQKM','GEOL_REEDBUSH_DOM','depth_bedrock_m','porosity','storage_m','TOPWET','ELEV_MEAN_M_BASIN','AWCAVE','PERMAVE','CLAYAVE','SILTAVE','SANDAVE')
clim = c('P_mm','PET_mm','SWE_mm','melt_mm','Tmax_C','P_90','PET_90','Tmax_90','melt_90','P.PET90','P.PET','SNOW_PCT_PRECIP')
land = c('FRESHW_WITHDRAWAL','PCT_IRRIG_AG','DEVNLCD06','FORESTNLCD06','PLANTNLCD06','WATERNLCD06','SNOWICENLCD06','IMPNLCD06')

am.df$type[which(am.df$variable %in% land)] <- "land"
am.df$type[which(am.df$variable %in% clim)] <- "clim"
am.df$type[which(am.df$variable %in% ws)] <- "ws"

c1.df$type[which(c1.df$variable %in% land)] <- "land"
c1.df$type[which(c1.df$variable %in% clim)] <- "clim"
c1.df$type[which(c1.df$variable %in% ws)] <- "ws"

c2.df$type[which(c2.df$variable %in% land)] <- "land"
c2.df$type[which(c2.df$variable %in% clim)] <- "clim"
c2.df$type[which(c2.df$variable %in% ws)] <- "ws"

c3.df$type[which(c3.df$variable %in% land)] <- "land"
c3.df$type[which(c3.df$variable %in% clim)] <- "clim"
c3.df$type[which(c3.df$variable %in% ws)] <- "ws"

c4.df$type[which(c4.df$variable %in% land)] <- "land"
c4.df$type[which(c4.df$variable %in% clim)] <- "clim"
c4.df$type[which(c4.df$variable %in% ws)] <- "ws"



am.df %>% 
  select(variable,scaled_importance) %>%
  gt() %>%
  tab_header(title = md("Cluster Membership"))%>%
  tab_style(
    style = list(
      cell_fill(color="#7570b3")),
    locations = cells_body(
      rows = which(variable %in% clim)
    )) %>%
  tab_style(
    style = list(
      cell_fill(color="#1b9e77")),
    locations = cells_body(
      rows = which(variable %in% ws)
    )) %>%
  tab_style(
    style = list(
      cell_fill(color="#d95f02")),
    locations = cells_body(
      rows = which(variable %in% land)
    ))


c1.df %>% 
  select(variable,scaled_importance) %>%
  gt() %>%
  tab_header(title = md("Cluster 1"))%>%
  tab_style(
    style = list(
      cell_fill(color="#7570b3")),
    locations = cells_body(
      rows = which(variable %in% clim)
    )) %>%
  tab_style(
    style = list(
      cell_fill(color="#1b9e77")),
    locations = cells_body(
      rows = which(variable %in% ws)
    )) %>%
  tab_style(
    style = list(
      cell_fill(color="#d95f02")),
    locations = cells_body(
      rows = which(variable %in% land)
    ))


c2.df %>% 
  select(variable,scaled_importance) %>%
  gt() %>%
  tab_header(title = md("Cluster 2"))%>%
  tab_style(
    style = list(
      cell_fill(color="#7570b3")),
    locations = cells_body(
      rows = which(variable %in% clim)
    )) %>%
  tab_style(
    style = list(
      cell_fill(color="#1b9e77")),
    locations = cells_body(
      rows = which(variable %in% ws)
    )) %>%
  tab_style(
    style = list(
      cell_fill(color="#d95f02")),
    locations = cells_body(
      rows = which(variable %in% land)
    ))


c3.df %>% 
  select(variable,scaled_importance) %>%
  gt() %>%
  tab_header(title = md("Cluster 3"))%>%
  tab_style(
    style = list(
      cell_fill(color="#7570b3")),
    locations = cells_body(
      rows = which(variable %in% clim)
    )) %>%
  tab_style(
    style = list(
      cell_fill(color="#1b9e77")),
    locations = cells_body(
      rows = which(variable %in% ws)
    )) %>%
  tab_style(
    style = list(
      cell_fill(color="#d95f02")),
    locations = cells_body(
      rows = which(variable %in% land)
    ))


c4.df %>% 
  select(variable,scaled_importance) %>%
  gt() %>%
  tab_header(title = md("Cluster 4"))%>%
  tab_style(
    style = list(
      cell_fill(color="#7570b3")),
    locations = cells_body(
      rows = which(variable %in% clim)
    )) %>%
  tab_style(
    style = list(
      cell_fill(color="#1b9e77")),
    locations = cells_body(
      rows = which(variable %in% ws)
    )) %>%
  tab_style(
    style = list(
      cell_fill(color="#d95f02")),
    locations = cells_body(
      rows = which(variable %in% land)
    ))

cols = c("clim" = "#7570b3",
         "ws"= "#1b9e77",
         "land" = "#d95f02")

pa = ggplot(am.df,aes(x = reorder(variable,scaled_importance),y = scaled_importance,fill=type)) +
         geom_bar(stat="identity", position="dodge")+
  coord_flip()+
  scale_fill_manual(values=cols) +
  xlab(NULL)

p1= ggplot(c1.df,aes(x = reorder(variable,scaled_importance),y = scaled_importance,fill=type)) +
  geom_bar(stat="identity", position="dodge")+
  coord_flip()+
  scale_fill_manual(values=cols) +
  xlab(NULL)


p2 = ggplot(c2.df,aes(x = reorder(variable,scaled_importance),y = scaled_importance,fill=type)) +
  geom_bar(stat="identity", position="dodge")+
  coord_flip()+
  scale_fill_manual(values=cols) +
  xlab(NULL)

p3 = ggplot(c3.df,aes(x = reorder(variable,scaled_importance),y = scaled_importance,fill=type)) +
  geom_bar(stat="identity", position="dodge")+
  coord_flip()+
  scale_fill_manual(values=cols) +
  xlab(NULL)

p4 = ggplot(c4.df,aes(x = reorder(variable,scaled_importance),y = scaled_importance,fill=type)) +
  geom_bar(stat="identity", position="dodge")+
  coord_flip()+
  scale_fill_manual(values=cols) +
  xlab(NULL)
