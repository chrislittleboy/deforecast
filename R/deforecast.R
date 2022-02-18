#' Run deforestation simulation
#' 
#' @useDynLib your-package-name
#' @importFrom Rcpp sourceCpp
#' 
#' @param explicit If FALSE, places people and trees randomly on the landscape. If TRUE takes input data.
#' 
#' @param ppl_loc Only if explicit_people is TRUE. A 4 column matrix with id, x/y locations and budget.
#' @param ppl_scaling Only if explicit_people is TRUE. A number between 0 and 1 to scale down the # of people on the map.
#' @param ppl_n Only if explicit_people is FALSE. The number of people on the landscape.
#'
#' @param xdim Size of landscape on x axis.
#' @param ydim Size of landscape on y axis
#' 
#' @param budget The starting resources of people on the landscape.
#' 
#' @param p_loc Only if explicit_trees is TRUE. A 4 column matrix with ID, x/y location and age of protected trees.
#' @param np_loc As p_loc, but for unprotected trees.
#' 
#' @param mean_age Mean of tree age.
#' @param sd_age Standard deviation of tree age.
#' 
#' @param p_n The number of protected trees on the landscape.
#' @param np_n The number of unprotected trees on the landscape.
#' 
#' @param travel_cost The cost for a person to move one grid cell to fell a tree.
#' @param management_cost The cost for a person to fell a tree in a protected area.
#'  
#' @param mobility The distance that a person moves towards the trees in each year.
#' 
#' @param value The value of a tree to a person.
#'
#' @param k Carrying capacity: forests can't grow beyond k times the size of the landscape. 
#' @param r The annual rate of growth of the forest.
#' @param maturity The age that trees start to produce seeds.
#' @param max_age The age that trees stop producing seeds.
#' @param dispersion The distance that seeds can travel from the seeding tree.
#' 
#' @param years The number of years for the simulation
#' @import Rcpp
#' 
# This is the main function for the simulation. 

deforecast <- function(
                       xdim = 1000,
                       ydim = 1000,
                       explicit = FALSE, 
                       ppl_loc = NULL, 
                       ppl_scaling = 1,
                       ppl_n = 1000,
                       budget = 1000,
                       p_loc = NULL, 
                       np_loc = NULL,
                       mean_age = 50,
                       sd_age = 25,
                       p_n = 5000,
                       np_n = 5000,
                       travel_cost = 100,
                       management_cost = 100,
                       value = 500,
                       mobility = 10,
                       k = 1,
                       r = 0.01, 
                       maturity = 10, 
                       max_age = 100,
                       dispersion = 5,
                       years = 10){

print("Starting simulation ...")

if(explicit == FALSE) {
    people <- make_people(
      xdim = xdim,
      ydim = ydim,
      ppl_n = ppl_n); # makes the people
} else { 
  people <- get_people(
    ppl_loc = ppl_loc, # or gets the people
    ppl_scaling = ppl_scaling)
  }
if(explicit == FALSE){
  trees <- make_trees(
      xdim = xdim,
      ydim = ydim,
      p_n = p_n,
      np_n = np_n,
      mean_age = mean_age,
      sd_age = sd_age); # makes the trees
} else {
  trees <- get_trees(
    p_loc = p_loc,
    np_loc = np_loc,
    mean_age = mean_age,
    sd_age = sd_age # or gets the trees
  )  
}

# creates two null list to store the results of the simulation for each year    

ppl_results <- vector(mode = "list", length = years + 1);
tree_results <- vector(mode = "list", length = years + 1);
  
# stores the initial conditions of the people and trees

ppl_results[[1]] <- people;
tree_results[[1]] <- trees;

t <- 1;

# creates global variables agebands

mature_age <- trees[,3];
mature_age <- unique(cbind(
  mature_age,
  cut(mature_age,
      breaks = c(quantile(mature_age,probs = seq(0,1, length = 11))), 
                                          labels = F,include.lowest = T)));
agebands <- c(maturity,aggregate(mature_age, by = list((mature_age[,2])), FUN = "max")[,2]);

while(t <= years){


felled <- felling(
      people = people, 
      trees = trees, 
      value = value,
      management_cost = management_cost,
      travel_cost = travel_cost,
      mobility = mobility); # removing trees from the landscape and adding value to people

people <- felled[[2]];
ppl_results[[t+1]] <- people; # stores results

trees <- felled[[1]];
if(length(matrix(trees, ncol = 4)[,1]) == 0){
  print("No more trees...");
  tree_results[[t+1]] <- NULL
break;
  } else {

trees <- growth(trees = trees,
      k = k,
      r = r,
      maturity = maturity,
      max_age = max_age,
      dispersion = dispersion,
      agebands = agebands); # grows the protected forest

tree_results[[t+1]] <- trees;
}
print(paste("Year", t, "done"))
t <- t + 1; # onto the next year!
}
return(list(ppl_results,tree_results));
}
