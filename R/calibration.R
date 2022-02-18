# calibration
#' @title calibration
#' @param params a data frame of all combinations to calibrate model
#' @param x a 4 member list with ids, location of people, protected and unprotected trees

calibration <- function(x, params, years, calibration_round){

x <- readRDS(file = paste0("/home/chris/Documents/data/deforecast/processed/inputlists/",x,".RData"));
id <- x[[1]]
ppl_loc <- x[[2]]
ppl_n <- length(ppl_loc[,1])
p_loc <- x[[3]]
np_loc <- x[[4]]
results <- matrix(nrow = length(params[,1]), ncol = 9)
i <- 1;
while (i <= length(params[,1])){

res <- deforecast(
           xdim = NULL,
           ydim = NULL,
           explicit = TRUE, 
           ppl_loc = ppl_loc,
           ppl_scaling = as.numeric(params[i,9]),
           p_loc = p_loc, 
           np_loc = np_loc,
           mean_age = as.numeric(substr(params[i,4],1,2)),
           sd_age = as.numeric(substr(params[i,4],4,5)),
           p_n = NULL,
           np_n = NULL,
           ppl_n = NULL,
           travel_cost = as.numeric(params[i,6]),
           management_cost = 100,
           value = as.numeric(params[i,8]),
           mobility = as.numeric(params[i,7]),
           k = as.numeric(params[i,1]),
           r = as.numeric(params[i,2]), 
           maturity = as.numeric(substr(params[i,3], 1,2)), 
           max_age = as.numeric(substr(params[i,3], 4,5)),
           dispersion = as.numeric(params[i,5]),
           years = years,
           calibration_round)

# unprotected at start
p_s <- sum(res[[2]][[1]][,4]) # trees/year1/protected column == 1
np_s <- length(res[[2]][[1]][,4]) - sum(res[[2]][[1]][,4])
# protected at start 
if (length(res[[2]]) != (years + 1)) {
  p_e <- 0;
  np_e <- 0;
} else {
p_e <- sum(res[[2]][[years +1]][,4])
np_e <- length(res[[2]][[years +1]][,4]) - sum(res[[2]][[years +1]][,4])
}
# total deforestation
t <- ((p_e + np_e) - (p_s + np_s))/ (p_s + np_s)
# protected deforestation
p <- (p_e-p_s)/p_s
# unprotected deforestation
np <- (np_e-np_s)/np_s

results[i,] <- c(id,ppl_n,np_s,np_e,p_s,p_e,round(t, digits = 4), round(p, digits = 4), round(np, digits = 4))
colnames(results) <- c("wdpaid", 
                       "people", 
                       "unprotected_start", 
                       "unprotected_end", 
                       "protected_start", 
                       "protected_end", 
                       "total_forest_growth", 
                       "protected_forest_growth", 
                       "unprotected_forest_growth")
i <- i + 1;
}
write.csv(results, file = paste0("/home/chris/Documents/data/deforecast/calibration/",calibration_round,"/resultsbyforest/",id,".csv"))
}
