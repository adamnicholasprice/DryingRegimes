c1.m


c1 = c1 %>% select(
  kmeans,
  DRAIN_SQKM,SNOW_PCT_PRECIP,GEOL_REEDBUSH_DOM,FRESHW_WITHDRAWAL,PCT_IRRIG_AG,
  FORESTNLCD06,PLANTNLCD06,WATERNLCD06,SNOWICENLCD06,IMPNLCD06,
  AWCAVE,PERMAVE,CLAYAVE,SILTAVE,SANDAVE,
  TOPWET,ELEV_MEAN_M_BASIN,
  depth_bedrock_m,porosity,storage_m,
  P_mm,PET_mm,SWE_mm,melt_mm,Tmax_C,
  P_90,PET_90,Tmax_90,melt_90, P.PET,P.PET90
  )

sub= c1
set.seed(1202)
seed = sample(1:10000,100)

# Set seed and select training data

i=10
print(i)
# # Set seed and split data
set.seed(seed[i])
print(seed[i])
sub$index <- seq(1:nrow(sub))
training_size = round(nrow(sub)*0.7,0)
training <- as.data.frame(sub[sample(1:nrow(sub),training_size, replace = F),])
testing <- as.data.frame(sub[!sub$index %in% training$index,])

testing$index = NULL
training$index = NULL

rf = ranger(formula = kmeans ~.,
           data = training,
           num.trees = 400,
           mtry = 10,
           max.depth = 10,
           sample.fraction = .8,
           importance = 'impurity',
           num.threads = 7,
           oob.error = T)

tt = as.data.frame(head(sort(rf$variable.importance,decreasing = T),15)) 

tt

p1 <- partial(rf, pred.var = "P_mm", plot = TRUE)

x_valid = training[,-1]

y_valid = training[,1]




pred <- function(model, newdata)  {
  results <- as.data.frame(h2o.predict(model, as.h2o(newdata)))
  return(results[[3L]])
}
pred(c4.m, x_valid) %>% head()


explainer_rf <- DALEX::explain(
  model = c4.m,
  data = x_valid,
  y = y_valid,
  predict_function = pred,
  label = "h2o rf"
)
