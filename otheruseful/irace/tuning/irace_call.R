# script for calling irace

library(irace)
setwd("./attempt2")
i <- 1;
while (i <= 10) {
assign(x = paste0("scenario", i), 
       value = readScenario(paste0("scenario",i,".txt")))
i <- i + 1
}
parameters <- readParameters("/home/chris/Documents/GitHub/deforecast/otheruseful/irace/tuning/parameters.txt")
setwd("/home/chris/Documents/GitHub/deforecast/otheruseful/irace/tuning/")
irace(scenario1,parameters)
irace(scenario10,parameters)
irace(scenario3,parameters)
irace(scenario4,parameters)
irace(scenario5,parameters)
irace(scenario6,parameters)
irace(scenario7,parameters)
irace(scenario8,parameters)
irace(scenario9,parameters)
irace(scenario10,parameters)
irace(scenario2,parameters)

setwd("/home/chris/Documents/GitHub/deforecast/otheruseful/irace/tuning/")

load("./attempt2/results/attempt2/C1/irace.Rdata")
c1_results <- iraceResults
load("./attempt2/results/attempt2/C2/irace.Rdata")
c2_results <- iraceResults
load("./attempt2/results/attempt2/C3/irace.Rdata")
c3_results <- iraceResults
load("./attempt2/results/attempt2/C4/irace.Rdata")
c4_results <- iraceResults
load("./attempt2/results/attempt2/C5/irace.Rdata")
c5_results <- iraceResults
load("./attempt2/results/attempt2/C6/irace.Rdata")
c6_results <- iraceResults
load("./attempt2/results/attempt2/C7/irace.Rdata")
c7_results <- iraceResults
load("./attempt2/results/attempt2/C8/irace.Rdata")
c8_results <- iraceResults
load("./attempt2/results/attempt2/C9/irace.Rdata")
c9_results <- iraceResults
load("./attempt2/results/attempt2/C10/irace.Rdata")
c10_results <- iraceResults

elites <- rbind(getFinalElites(c1_results),
                getFinalElites(c2_results),
                getFinalElites(c3_results),
                getFinalElites(c5_results),
                getFinalElites(c6_results),
                getFinalElites(c7_results),
                getFinalElites(c8_results),
                getFinalElites(c9_results),
                getFinalElites(c10_results))
elites <- cbind(c(rep(1, times = 5),
                  rep(2, times = 5),
                  rep(3, times = 5),
                  rep(5, times = 5),
                  rep(6, times = 5),
                  rep(7, times = 5),
                  rep(8, times = 5),
                  rep(9, times = 5),
                  rep(10, times = 5)),
                  elites)
colnames(elites)[1] <- "Cluster"
elite <- pivot_longer(data = elites, cols = 2:14)
elite$Cluster <- as.factor(elite$Cluster)
write_csv(x = elite, file = "./results/attempt2.csv")
p1_1 <- ggplot(filter(elite, name == "ppl_scaling")) +
  geom_point(aes(x = as.factor(Cluster), y = value, col = Cluster)) +
  ggtitle("ppl_scaling")
p2_1 <- ggplot(filter(elite, name == "mean_age")) +
  geom_point(aes(x = as.factor(Cluster), y = value, col = Cluster)) +
  ggtitle("mean_age")
p3_1 <- ggplot(filter(elite, name == "sd_age")) +
  geom_point(aes(x = as.factor(Cluster), y = value, col = Cluster)) +
  ggtitle("sd_age")
p4_1 <- ggplot(filter(elite, name == "travel_cost")) +
  geom_point(aes(x = as.factor(Cluster), y = value, col = Cluster)) +
  ggtitle("travel_cost")
p5_1 <- ggplot(filter(elite, name == "value")) +
  geom_point(aes(x = as.factor(Cluster), y = value, col = Cluster)) +
  ggtitle("value")
p6_1 <- ggplot(filter(elite, name == "mobility")) +
  geom_point(aes(x = as.factor(Cluster), y = value, col = Cluster)) +
  ggtitle("mobility")
p7_1 <- ggplot(filter(elite, name == "k")) +
  geom_point(aes(x = as.factor(Cluster), y = value, col = Cluster)) +
  ggtitle("k")
p8_1 <- ggplot(filter(elite, name == "r")) +
  geom_point(aes(x = as.factor(Cluster), y = value, col = Cluster)) +
  ggtitle("r")
p9_1 <- ggplot(filter(elite, name == "maturity")) +
  geom_point(aes(x = as.factor(Cluster), y = value, col = Cluster)) +
  ggtitle("maturity")
p10_1 <- ggplot(filter(elite, name == "max_age")) +
  geom_point(aes(x = as.factor(Cluster), y = value, col = Cluster)) +
  ggtitle("max_age")
p11_1 <- ggplot(filter(elite, name == "dispersion")) +
  geom_point(aes(x = as.factor(Cluster), y = value, col = Cluster)) +
  ggtitle("dispersion")


testing.main(logFile = "./attempt2/results/attempt2/C1/irace.Rdata")
testing.main(logFile = "./attempt2/results/attempt2/C2/irace.Rdata")
testing.main(logFile = "./attempt2/results/attempt2/C3/irace.Rdata")
testing.main(logFile = "./attempt2/results/attempt2/C4/irace.Rdata")
testing.main(logFile = "./attempt2/results/attempt2/C5/irace.Rdata")
testing.main(logFile = "./attempt2/results/attempt2/C6/irace.Rdata")
testing.main(logFile = "./attempt2/results/attempt2/C7/irace.Rdata")
testing.main(logFile = "./attempt2/results/attempt2/C8/irace.Rdata")
testing.main(logFile = "./attempt2/results/attempt2/C9/irace.Rdata")
testing.main(logFile = "./attempt2/results/attempt2/C10/irace.Rdata")

load("./attempt2/results/attempt2/C1/irace.Rdata")
c1_test <- iraceResults$testing$experiments
load("./attempt2/results/attempt2/C2/irace.Rdata")
c2_test <- iraceResults$testing$experiments
load("./attempt2/results/attempt2/C3/irace.Rdata")
c3_test <- iraceResults$testing$experiments
load("./attempt2/results/attempt2/C4/irace.Rdata")
c4_test <- iraceResults$testing$experiments
load("./attempt2/results/attempt2/C5/irace.Rdata")
c5_test <- iraceResults$testing$experiments
load("./attempt2/results/attempt2/C6/irace.Rdata")
c6_test <- iraceResults$testing$experiments
load("./attempt2/results/attempt2/C7/irace.Rdata")
c7_test <- iraceResults$testing$experiments
load("./attempt2/results/attempt2/C8/irace.Rdata")
c8_test <- iraceResults$testing$experiments
load("./attempt2/results/attempt2/C9/irace.Rdata")
c9_test <- iraceResults$testing$experiments
load("./attempt2/results/attempt2/C10/irace.Rdata")
c10_test <- iraceResults$testing$experiments
summary(c2_test)
summary(c3_test)
summary(c4_test)
summary(c5_test)
summary(c6_test)
summary(c7_test)
summary(c8_test)
summary(c9_test)
summary(c10_test)

install.packages("patchwork")
library(patchwork)

