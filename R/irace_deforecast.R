#! /usr/bin/env Rscript

#' @title irace_tuning

args = commandArgs(trailingOnly=TRUE);
instance <- args[4];
params <- args[5:15];
irace_deforecast <- function(instance,params) {
params <- experiment$configuration;
actual <- read.csv("/home/chris/Documents/data/deforecast/calibration/actual.csv")[,c(1,10)] %>% drop_na();
actual <- filter(actual, X == basename(instance))[,2];
start <- readRDS(file = instance);
simulated <- deforecast(xdim = NULL,
                        ydim = NULL,
                        explicit = TRUE, 
                        p_n = NULL,
                        np_n = NULL,
                        ppl_n = NULL,
                        ppl_loc = start[[2]],
                        p_loc = start[[3]], 
                        np_loc = start[[4]],
                        ppl_scaling = params[[1]],
                        mean_age = params[[2]],
                        sd_age = params[[3]],
                        travel_cost = params[[4]],
                        value = as.numeric(params[[5]]),
                        mobility = as.numeric(params[[6]]),
                        k = params[[7]],
                        r = params[[8]], 
                        maturity = params[[9]], 
                        max_age = params[[10]],
                        dispersion = params[[11]],
                        years = 20,
                        management_cost = 100)

# results 
years = years
p_s <- sum(simulated[[2]][[1]][,4]) # trees/year1/protected column == 1
np_s <- length(simulated[[2]][[1]][,4]) - sum(simulated[[2]][[1]][,4])
# protected at start 
if (length(simulated[[2]]) != (years + 1)) {
  p_e <- 0;
  np_e <- 0;
} else {
  p_e <- sum(simulated[[2]][[years +1]][,4])
  np_e <- length(simulated[[2]][[years +1]][,4]) - sum(simulated[[2]][[years +1]][,4])
}
# total deforestation
t <- ((p_e + np_e) - (p_s + np_s))/ (p_s + np_s)

difference <- abs(diff(c(t,actual)))
return(difference)
}

