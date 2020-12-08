library(tidyverse)
library(ranger)
library(h2o)
library(pdp)


clust = read.csv('data/kmeans.csv')
clust$gage = as.numeric(clust$gage)

dat = read.csv("data/metrics_by_event_combined.csv")
dat$gage = as.numeric(dat$gage)
dat$Name[dat$Name == "Ignore"] = "Mediterranean California" 
dat = dat[dat$peak_quantile>.25 & dat$drying_rate>=0,]

ant.cond = read.csv("data/metrics_by_event_withAntCondNoFlow.csv")
ant.cond = ant.cond[ant.cond$peak_quantile>.25 & ant.cond$drying_rate>=0,]
ant.cond$Date = as.Date(ant.cond$Date)

df = dat %>% left_join(.,ant.cond,by=c("gage","event_id"))

### Timeseries LULC
lulc = read.csv("data/TS_lulc_clust.csv")

# This is the only way I could get the data to join
df = cbind(df,clust,lulc)

######### Select variables and clean up for rf

df = df %>% subset(., select=which(!duplicated(names(.))))

df$kmeans = as.factor(df$kmeans)

df = df[!is.na(df$lulc_ag_prc),]
df = df[!is.na(df$dec_lat_va),]



sub = df %>%  select(
  kmeans,
  lulc_water_prc,lulc_dev_prc,lulc_forest_prc,lulc_barren_prc,lulc_grass_prc,lulc_ag_prc,lulc_wetland_prc,
  DRAIN_SQKM,SNOW_PCT_PRECIP,GEOL_REEDBUSH_DOM,FRESHW_WITHDRAWAL,
  AWCAVE,PERMAVE,CLAYAVE,SILTAVE,SANDAVE,
  TOPWET,ELEV_MEAN_M_BASIN,
  depth_bedrock_m,porosity,storage_m,
  P_mm,PET_mm,SWE_mm,melt_mm,Tmax_C,
  P_90,PET_90,Tmax_90,melt_90
)%>%
  mutate(P.PET = P_mm/PET_mm,
         P.PET90 = P_90/PET_90
  )


sub[is.na(sub$P.PET),'P.PET']=0
# sub[is.na(sub$P.PET7),'P.PET7']=0

sub = sub %>% filter_all(all_vars(!is.infinite(.)))
sub = sub[complete.cases(sub),]

#### Clean up data
# rm(ant.cond,clust,dat,lulc,df)


c1 = sub %>%
  mutate(kmeans = recode(kmeans,
                         '2'='0',
                         '3'='0',
                         '4'='0'))

c2 = sub %>%
  mutate(kmeans = recode(kmeans,
                         '1'='0',
                         '2' = '1',
                         '3'='0',
                         '4'='0'))
c3 = sub %>%
  mutate(kmeans = recode(kmeans,
                         '1'='0',
                         '2'='0',
                         '3' ='1',
                         '4'='0'))
c4 = sub %>%
  mutate(kmeans = recode(kmeans,
                         '1'='0',
                         '2'='0',
                         '3'='0',
                         '4'='1'))




# Set seed and split data
set.seed(42)
sub$index <- seq(1:nrow(sub))
training_size = round(nrow(sub)*0.7,0)
training <- as.data.frame(sub[sample(1:nrow(sub),training_size, replace = F),])
testing <- as.data.frame(sub[!sub$index %in% training$index,])

testing$index = NULL
training$index = NULL
sub$index = NULL

rf = ranger(formula = kmeans ~.,
            data = training,
            num.trees = 400,
            mtry = 10,
            max.depth = 10,
            sample.fraction = .8,
            importance = 'impurity',
            num.threads = 7,
            oob.error = T,
            classification = T)


head(sort(rf$variable.importance,decreasing = T),25)

vip::vip(rf)

pred_wrapper <- function(object, newdata) {
  p <- predict(object, data = newdata)$predictions[, 1L, drop = TRUE]
  c("avg" = mean(p), "avg-1sd" = mean(p) - sd(p), "avg+1sd" = mean(p) + sd(p))
}


pd1 <- pdp::partial(rf, pred.var = "P_90", pred.fun = pred_wrapper)



# create feature names
y <- "kmeans"
x <- setdiff(names(training), y)

# Initialize an h2o cluster
h2o.init(max_mem_size = "8g")


# turn training set into h2o object
train.h2o <- as.h2o(training)

# Setup your hyperparameter grid  
hyper_grid.h2o <- list(
  ntrees      = seq(100, 500, by = 100),
  mtries      = seq(10, 30, by = 10),
  max_depth   = seq(10, 40, by = 5),
  # min_rows    = seq(1, 5, by = 2),
  # nbins       = seq(10, 30, by = 5),
  sample_rate = c(.5, .75, .80)
)

# random grid search criteria
search_criteria <- list(
  strategy = "RandomDiscrete",
  stopping_metric = "logloss",
  stopping_tolerance = 0.05,
  stopping_rounds = 10,
  max_runtime_secs = 30*60
)

# build grid search 
random_grid <- h2o.grid(
  algorithm = "randomForest",
  grid_id = "rf_grid2",
  x = x, 
  y = y, 
  training_frame = train.h2o,
  hyper_params = hyper_grid.h2o,
  search_criteria = search_criteria
)

# collect the results and sort by our model performance metric of choice
grid_perf2 <- h2o.getGrid(
  grid_id = "rf_grid2",
  sort_by = "logloss",
  decreasing = F
)
print(grid_perf2)

best_model_id <- grid_perf2@model_ids[[1]]
bm <- h2o.getModel(best_model_id)

# h2o.saveGrid('data/',grid_perf2@grid_id)
h2o.saveModel(bm,'data/rf_model/')

h2o.saveGrid(grid_directory = 'data/rf_c1',random_grid@grid_id)

h2o.varimp_plot(bm)

h2o.shutdown(prompt = FALSE)


cluster <- parallel::makeCluster(parallel::detectCores() - 1) # convention to leave 1 core for OS
registerDoParallel(cluster)

partial(rf, 
        pred.var = c("P_90"), 
        mtry = tune_res$mtry[1], 
        min.node.size = tune_res$min_n[1],
        num.threads = (parallel::detectCores() - 1),
        parallel = T)
  
  
  rf = ranger(formula = kmeans ~.,
              data = training,
              num.trees = 400,
              mtry = 10,
              max.depth = 10,
              sample.fraction = .8,
              importance = 'impurity',
              num.threads = 7,
              oob.error = T,
              classification = T,
              probability = T)
  