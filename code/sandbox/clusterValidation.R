# Hyper-Parameter Search Summary: ordered by increasing logloss
# max_depth mtries ntrees sample_rate         model_ids            logloss
# 1        10     10    300         0.8 rf_grid2_model_22 0.8169095954327732
# 2        15     20    300         0.5  rf_grid2_model_3  0.826188433891212
# 3        10     30    400         0.8 rf_grid2_model_13 0.8411700223952421
# 4        15     10    200         0.8  rf_grid2_model_2 0.8441924863771494
# 5        10     30    200        0.75 rf_grid2_model_21 0.8467675166195089




set.seed(123)

# function to compute total within-cluster sum of square 
wss <- function(k) {
  kmeans(dat.scale, k, nstart = 10 )$tot.withinss
}

# Compute and plot wss for k = 1 to k = 15
k.values <- 1:15

# extract wss for 2-15 clusters
wss_values <- map_dbl(k.values, wss)

plot(k.values, wss_values,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")
