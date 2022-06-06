irace_deforecast <- function(instance, params) {

actual <- read.csv("./actualdeforestation") %>% select(t) %>% filter(ID == instance)
simulated <- deforecast(instance, params)
difference <- actual-simulated
return(difference)
  }
  
  
}

getwd()
actual <- read.csv("/home/chris/Documents/data/deforecast/calibration/actual.csv")
(sum(actual$unprotected2020) - sum(actual$unprotected2001)) / sum(actual$unprotected2001)
(sum(actual$protected2020) - sum(actual$protected2001)) / sum(actual$protected2001)
