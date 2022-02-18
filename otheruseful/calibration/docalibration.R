library(tidyverse)
library(deforecasteR)
library(parallel)
chop <- deforecasteR::chop
# we need the clusters, the lists of forest ids

actual <- read.csv("/home/chris/Documents/data/deforecast/calibration/actual.csv")[,c(2,8:10)]
clusters <- read.csv("/home/chris/Documents/data/deforecast/results/cluster/10kmeans")[,c(2,8)]
# sample_clusters <- clusters %>% group_by(cluster) %>% slice_sample(n=10)
# write.csv(sample_clusters,"/home/chris/Documents/data/deforecast/results/cluster/sampleclusters.csv")
sample_clusters <- unique(clusters[clusters$id %in% actual$id,])
calib_list <- read.csv("/home/chris/Documents/data/deforecast/results/cluster/sampleclusters.csv")
calib_list <- list.files("/home/chris/Documents/data/deforecast/processed/inputlists")
calib_list <- gsub(pattern = ".RData", "", calib_list)
calib_list <- calib_list[calib_list %in% sample_clusters$id]
calib_list <- split(calib_list, seq(length(calib_list)))

docalibration <- function(calibration_round, # integer
                          k, # numeric vector
                          r, # numeric vector
                          mat_max, # vector of character strings (Maturity/Max Age)
                          age_sd, # character string (age/sd)
                          dispersion, # numeric vector
                          travel, # numeric vector
                          mobility, # numeric vector
                          value,
                          ppl_scaling) # vector of values between 0 and 1 
                          {

dir.create(path = paste0("/home/chris/Documents/data/deforecast/calibration/", calibration_round))
dir.create(path = paste0("/home/chris/Documents/data/deforecast/calibration/", calibration_round, "/resultsbyforest"))
params <- expand.grid(k,r,mat_max,age_sd,dispersion,travel,mobility,value,ppl_scaling)

# this is where the work is done. Calibration is run on each of the forests in calib_list.
# this outputs the results by forest in a folder under ./calibration/##

calib_GO <- 
         mclapply(calib_list, 
         calibration,
         mc.cores = 8,
         params = params,
         years = 19,
         calibration_round = calibration_round)

results <- matrix(nrow = length(actual[,1]), ncol = 5)
results[,4] <- round(actual$t, 2)
i <- 1
while(i <= length(actual[,1])){
  id <- actual[i,1]
  f <- read.csv(paste0("./1/resultsbyforest/",id,".csv"))[2:10] 
  # gets calibration results outputed by 'calibration' 
  withactual <- merge(actual,f,by.x = "id", by.y = "wdpaid") %>% 
    # merges with actual deforestation for calibrated forests
    mutate(p_difference = protected_forest_growth - p,
           np_difference = unprotected_forest_growth - np,
           total_difference = total_forest_growth - t) %>%
    # calculates difference between model and actual for protected/unprotected/total
    mutate(
      rmse_p = sqrt(p_difference^2),
      rmse_np = sqrt(np_difference^2),
      rmse_t = sqrt(total_difference^2)
    )
  # calculates the RMSE for protected/unprotected/total
  bestparam <- which.min(withactual$rmse_t)
  # selects the parameter set with the best RMSE
  bestmodel <- withactual[bestparam,15]
  # selects the model prediction for the best performing RMSE score
  rmse_t <- min(withactual$rmse_t)
  # selects the best performing RMSE score
  results[i,1] <- id
  # gets the forest id
  results[i,2] <- bestparam
  results[i,3] <- round(bestmodel, 2)
  results[i,5] <- round(rmse_t, 2)
  i <- i + 1
}

colnames(results) <- c("id", "bestparam", "bestmodel", "actual", "rmse")
results <- unique(merge(results, clusters))
# finds clusters for each forest
write.csv(results, paste0("/home/chris/Documents/data/deforecast/calibration/", calibration_round, "/calibrationresults.csv"))
}

# iterations of this function for each round
# note for round 1 the model did not include the 'scaling' parameter

docalibration(calibration_round = 1,
              c("1", ".6"),
              c("0.01","0.02","0.03"),
              mat_max = "10/90",
              age_sd = "50/25",
              dispersion = "5",
              travel = c("50","150","300"),
              mobility = c("10", "30"),
              value = c("400", "500", "700", "1000")
)

docalibration(
  calibration_round = 2,
              k = c("1"),
              r = c("0.02"),
              mat_max = "10/90",
              age_sd = "50/25",
              dispersion = "5",
              travel = c("150"),
              mobility = c("10"),
              value = c("500"),
              ppl_scaling = c("0.1", "0.3", "0.4")
)
