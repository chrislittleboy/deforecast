# script for calling irace

library(irace)
setwd("./otheruseful/irace/tuning/")
i <- 1;
while (i <= 10) {
assign(x = paste0("scenario", i), 
       value = readScenario(paste0("scenario",i,".txt")))
i <- i + 1
}
parameters <- readParameters("parameters.txt")
irace(scenario1,parameters)
irace(scenario2,parameters)
irace(scenario3,parameters)
irace(scenario4,parameters)
irace(scenario5,parameters)
irace(scenario6,parameters)
irace(scenario7,parameters)
irace(scenario8,parameters)
irace(scenario9,parameters)
irace(scenario10,parameters)

getwd()
setwd("/home/chris/Documents/GitHub/deforecast/")
