### First round of calibration

# carrying capacity
k <- c("1", ".6")
# growth rate
r <- c("0.01","0.02","0.03")
#maturity and max age
mat_max <- c("10/90")
# starting age and sd
age_sd <- c("50/25")
# how far seeds go
dispersion <- c("5")

# people variables

# cost to travel on landscape
travel <- c("50", "150", "300")
#travel at end of year
mobility <- c("10", "30")
# value of a tree
value <- c("400", "500", "700", "1000")

params1 <- expand.grid(k,r,mat_max,age_sd,dispersion,travel,mobility,value)
setwd("/home/chris/Documents/data/deforecast/processed/")
write.csv(params1, file = "./params/params1")
