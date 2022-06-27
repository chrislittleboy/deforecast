#' Does the felling (peoples' budgets change and trees cease to exist)
#' @inheritParams deforecast
#' @param people A matrix containing the id, x/y location and budget of people.
#' @param trees_list A 2-member list with protected and unprotected trees.
#' @param target A matrix containing the cheapest tree to fell for each person.

felling <- function(people, trees, value, management_cost, travel_cost, mobility){

trees <-  cbind(
    1:length(trees[,1]), # new ids for trees
    matrix(trees[,1:4], ncol = 4)); # 1 (protected), 0 (not protected)

# does the logging and moving (see c++ function in src)  
  
  logged <- chop(x1 = people[,1], 
                 x2 = trees[,2],
                 y1 = people[,2],
                 y2 = trees[,3],
                 p = trees[,5],
                 value = value,
                 m_cost = management_cost,
                 t_cost = travel_cost,
                 mobility = mobility)

# new people
  newpeople <- matrix(cbind(logged[,3:4]), ncol = 2)
# gets ids of felled trees
  felled <- logged[logged[,2] > 0,1]

# removes felled trees from trees and remove ids column

  '%!in%' <- function(x,y)!('%in%'(x,y))
  trees <- trees[trees[,1] %!in% felled,2:5]

# creates list of 2 - listed trees, and people  
afterfelling <- list(trees,newpeople);
return(afterfelling);
}
