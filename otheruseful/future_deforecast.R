rm(list= ls())
library(parallel)
library(devtools)
library(tidyverse)
library(deforecasteR)
library(RPushbullet)
library(irace)
# just brute force it

accuracy <- read.csv(file = "/home/chris/Documents/iracetestresults.csv")[,2:7]
actual <- read.csv("/home/chris/Documents/data/deforecast/calibration/actual.csv")
clusters <- read_csv("/home/chris/Documents/data/deforecast/results/cluster/10kmeans")

# loads results from irace
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

# gets elites from irace for each cluster
elites <- rbind(getFinalElites(c1_results),
                getFinalElites(c2_results),
                getFinalElites(c3_results),
                getFinalElites(c4_results),
                getFinalElites(c5_results),
                getFinalElites(c6_results),
                getFinalElites(c7_results),
                getFinalElites(c8_results),
                getFinalElites(c9_results),
                getFinalElites(c10_results))
elites <- cbind(c(rep(1, times = 5),
                  rep(2, times = 5),
                  rep(3, times = 5),
                  rep(4, times = 5),
                  rep(5, times = 5),
                  rep(6, times = 5),
                  rep(7, times = 5),
                  rep(8, times = 5),
                  rep(9, times = 5),
                  rep(10, times = 5)),
                elites)
colnames(elites)[1] <- "Cluster"
elites$Cluster <- as.factor(elites$Cluster)
elites$elite <- rep(1:5, times = 10)
elites$config <- paste0(elites$Cluster, "_", elites$elite)

# gets results from test for running elite configurations
setwd("/home/chris/Documents/data/deforecast/processed/inputlists/")
all_forests <- list.files()
allforestcharacteristics <- matrix(nrow = length(all_forests), ncol = 3)
i <- 1
while(i <= length(all_forests)) {
  
  forest <- readRDS(all_forests[i])
  allforestcharacteristics[i,1] <- forest[[1]]
  allforestcharacteristics[i,2] <- nrow(forest[[2]])
  allforestcharacteristics[i,3] <- nrow(forest[[3]]) + nrow(forest[[4]])
  i <- i + 1  
}
f <- data.frame(allforestcharacteristics)
colnames(f) <- c("id","ppl","trees")
f <- merge(f,clusters) %>% filter(id %in% accuracy$X1)
f <- merge(f, accuracy, by.x = "id", by.y = "X1")
f <- merge(f, actual)
f <- f %>% mutate(ppl = as.numeric(ppl), trees = as.numeric(trees))
# and finds the best fit out of the 5 elite configurations
bestmodel <- f %>%  
  group_by(X5,X6) %>% 
  summarise(performance = mean(X4)) %>%
  group_by(X5) %>% 
  mutate(best = min(performance)) %>%
  filter(best == performance) %>% mutate(config = paste0(X5,"_",X6)) %>%
  rename(Cluster = X5)

# and returns just the parameter values to go into the models
paramall <- elites[elites$config %in% bestmodel$config,3:13]

# expands the protected areas by a set amount

protected_adjust <- function(forest, pareaexp) {
  ppl <- forest[[2]]
  p <- forest[[3]]
  np <- forest[[4]]
  colnames(np) = colnames(p) = colnames(ppl) = c("x", "y", "value")
  xmax = max(c(max(np$x), max(p$x)), max(ppl$x))
  ymax = max(c(max(np$y), max(p$y)), max(ppl$y))
  np$dist <- rank(abs(np$x- xmax/2) + abs(np$y-ymax/2), ties.method = "first")
  n <- round(nrow(p) * (pareaexp -1), digits = 0) 
  p <- rbind(p, np[np$dist <= n,1:3])
  np <- np[np$dist > n, 1:3]
  forest[[3]] <- p
  forest[[4]] <- np
  return(forest)
}

# id, cluster, management_cost, parea, params
future_deforestation <- function(x, mcosts,pareas, cluster){
  params <- as.numeric(paramall[cluster,])
  results <- matrix(nrow = length(pareas),ncol = length(mcosts));
  i <- 1;
  j <- 1;
  while(i <= length(pareas)) {
    start <- readRDS(paste0("/home/chris/Documents/data/deforecast/processed/inputlists/",x, ".RData"))
    pareaexp <- pareas[i];
    start <- protected_adjust(start, pareaexp);
    j <- 1
    while(j <= length(mcosts)) {
      mcost <- mcosts[j]
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
                              years = 10,
                              management_cost = mcost)
      
      years = 10
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
      results[i,j] <- t
      j <- j+1;
    }
    i <- i+1;
  }
    return(results)
}


i <- 1
# just brute force it
while(i <= 10) {
  assign(value = c(list.files(paste0("/home/chris/Documents/data/deforecast/processed/clusters/test/c",i)),
                   list.files(paste0("/home/chris/Documents/data/deforecast/processed/clusters/train/c",i))), x = paste0("instance_", i))
  
  i <- i + 1
}

i <- 1

mcosts <- c(100,200, 400, 800, 1600)
pareas <- c(1,1.2, 1.5, 2,1000)

res1 <- mclapply(X = instance_1, FUN = future_deforestation, mcosts = mcosts, pareas = pareas, cluster = 1, mc.cores = 8)
ifelse(length(res1) == length(instance_1), "yay", "boo")
pbPost("note", "c1 done", .Last.value)

res2 <- mclapply(X = instance_2, FUN = future_deforestation, mcosts = mcosts, pareas = pareas, cluster = 2, mc.cores = 8)
ifelse(length(res2) == length(instance_2), "yay", "boo")
pbPost("note", "c2 done", .Last.value)

res3 <- mclapply(X = instance_3, FUN = future_deforestation, mcosts = mcosts, pareas = pareas, cluster = 3, mc.cores = 8)
ifelse(length(res3) == length(instance_3), "yay", "boo")
pbPost("note", "c3 done", .Last.value)

res4 <- mclapply(X = instance_4, FUN = future_deforestation, mcosts = mcosts, pareas = pareas, cluster = 4, mc.cores = 8)
ifelse(length(res4) == length(instance_4), "yay", "boo")
pbPost("note", "c4 done", .Last.value)

res5 <- mclapply(X = instance_5, FUN = future_deforestation, mcosts = mcosts, pareas = pareas, cluster = 5, mc.cores = 8)
ifelse(length(res5) == length(instance_5), "yay", "boo")
pbPost("note", "c5 done", .Last.value)

res6 <- mclapply(X = instance_6, FUN = future_deforestation, mcosts = mcosts, pareas = pareas, cluster = 6, mc.cores = 8)
ifelse(length(res6) == length(instance_6), "yay", "boo")
pbPost("note", "c6 done", .Last.value)

res7 <- mclapply(X = instance_7, FUN = future_deforestation, mcosts = mcosts, pareas = pareas, cluster = 7, mc.cores = 8)
ifelse(length(res7) == length(instance_7), "yay", "boo")
pbPost("note", "c7 done", .Last.value)

res8 <- mclapply(X = instance_8, FUN = future_deforestation, mcosts = mcosts, pareas = pareas, cluster = 8, mc.cores = 8)
ifelse(length(res8) == length(instance_8), "yay", "boo")
pbPost("note", "c8 done", .Last.value)

res9 <- mclapply(X = instance_9, FUN = future_deforestation, mcosts = mcosts, pareas = pareas, cluster = 9, mc.cores = 8)
ifelse(length(res9) == length(instance_9), "yay", "boo")
pbPost("note", "c9 done", .Last.value)

res10 <- mclapply(X = instance_10, FUN = future_deforestation, mcosts = mcosts, pareas = pareas, cluster = 10, mc.cores = 8)
ifelse(length(res10) == length(instance_10), "yay", "boo")
pbPost("note", "c10 done", .Last.value)

res <- c(res1,res2,res3,res4,res5,res6,res7,res8,res9,res10)
saveRDS(object = res, file = "/home/chris/Documents/GitHub/deforecast/otheruseful/irace/tuning/futureresults.RData")
results <- readRDS("/home/chris/Documents/GitHub/deforecast/otheruseful/irace/tuning/futureresults.RData")

i <- 1

result <- data.frame(matrix(ncol = 4, nrow = 0)) 
cn <- c("exp", "name", "value", "id")
colnames(result) <- cn
result1 <- result
result2 <- result
result3 <- result
result4 <- result
result5 <- result
result6 <- result
result7 <- result
result8 <- result
result9 <- result
result10 <- result


i <- 1
while(i <= length(res10)){
test <- data.frame(matrix(unlist(res10[i]), nrow = 5, ncol = 5))
colnames(test) <- c("mc_100", "mc_200", "mc_400", "mc_800", "mc_1600")
test$exp <- c("exp_1", "exp_1.2", "exp_1.5", "exp_2", "exp_1000")
test <- pivot_longer(test, cols = 1:5) 
test$name <- factor(test$name, ordered = T, levels = c("mc_100", "mc_200", "mc_400", "mc_800", "mc_1600"))
test$exp <- factor(test$exp, ordered = T, levels = c("exp_1", "exp_1.2", "exp_1.5", "exp_2", "exp_1000"))
test$id <- i
result10 <- rbind(result10,test)
i <- i + 1
}

result1$cluster <- 1
result2$cluster <- 2
result3$cluster <- 3
result4$cluster <- 4
result5$cluster <- 5
result6$cluster <- 6
result7$cluster <- 7
result8$cluster <- 8
result9$cluster <- 9
result10$cluster <- 10

allpreholsresults <- rbind(result1,result2,result3,result5,result6,result7,result8,result10)
write.csv(allpreholsresults, "/home/chris/Documents/GitHub/deforecast/otheruseful/irace/tuning/futureresults.csv")


all <- read.csv("/home/chris/Documents/GitHub/deforecast/otheruseful/irace/tuning/futureresults.csv")[,2:6] %>%
  mutate(value = as.numeric(value),
         name = factor(name, ordered = T, levels = c("mc_100", "mc_200", "mc_400", "mc_800", "mc_1600")),
         exp = factor(exp, ordered = T, levels = c("exp_1", "exp_1.2", "exp_1.5", "exp_2", "exp_1000"))) %>%
  drop_na() %>%
  group_by(cluster, name, exp) %>% 
  summarise(value = median(value)) %>%
  ggplot() +
  geom_tile(aes(x = name, y = exp, fill = value)) +
  facet_wrap(~cluster) +
    scale_fill_viridis(option = "inferno", labels = scales::percent) +
    geom_text(aes(x = name, y = exp, label = round(value*100, digits = 1))) +
  xlab("") +
  ylab("") +
  labs(caption = "Cluster 4 and Cluster 9 didn't run correctly, will try to work out why...") +
  ggtitle("Median future simulation results for each scenario and each cluster") +
  theme_classic() +   
  labs(fill = "Forest growth")

all
examples <- read.csv("/home/chris/Documents/GitHub/deforecast/otheruseful/irace/tuning/futureresults.csv")[,2:6] %>%
  filter(cluster == 1) %>% 
  mutate(value = as.numeric(value),
         name = factor(name, ordered = T, levels = c("mc_100", "mc_200", "mc_400", "mc_800", "mc_1600")),
         exp = factor(exp, ordered = T, levels = c("exp_1", "exp_1.2", "exp_1.5", "exp_2", "exp_1000"))) %>%
  drop_na()
set.seed(123)
ids <- sample(unique(examples$id), 24)
examples <- examples %>% filter(id %in% ids) %>%
  ggplot() +
  geom_tile(aes(x = name, y = exp, fill = value)) +
  facet_wrap(~id, nrow = 6, ncol = 4) +
  scale_fill_viridis(option = "inferno", labels = scales::percent) +
  geom_text(aes(x = name, y = exp, label = round(value*100, digits = 1))) +
  xlab("") +
  ylab("") +
  ggtitle("Future simulation results for a random sample of twenty-four cluster 1 forests") +
  theme_classic() +
  labs(fill = "Forest growth")
  examples 

setwd("/home/chris/Documents/GitHub/deforecast/otheruseful/manuscript/figures")
ggsave(plot = all, filename = "futureresults_bycluster.png", width = 210, height = 297, units = "mm")
ggsave(plot = examples, filename = "examplefutureresults.png", width = 210, height = 297, units = "mm")
getwd()

    
i <- 7
?scale_fill_viridis
heatmap(unlist(res1[1]))
unlist(res1[1])      
heatmap(matrix(unlist(res1[1]), nrow = 5, ncol = 5), Rowv = NA, Colv = NA)
?heatmap
