#include "Rcpp.h"
using namespace Rcpp;

// [[Rcpp::export]]
NumericMatrix 
  chop(
    NumericVector x1, // x coords of people 
    NumericVector x2, // x coords of trees
    NumericVector y1, // y coords of people
    NumericVector y2, // y coords of trees
    NumericVector p, // whether trees are protected
    double value, // value of tree to individual 
    double m_cost, // management cost
    double t_cost, // travel cost
    double mobility // distance that people move towards trees
  ){
  int ppl_n = x1.size(); // # of people
  int t_n = x2.size(); // # of trees

  // creates a copy of the x coordinate of each tree.
  // this is so the loop later can change make trees further 
  // without losing information on the original location
  
  NumericVector x2_copy(t_n); 
  for (int a = 0; a < t_n; ++a) {
    x2_copy[a] = x2[a]; 
  }
  
  NumericVector chop(ppl_n); // vector to store value in chopping trees
  NumericVector id(ppl_n); // ids of trees which are felled
  NumericVector z(t_n); // distance of trees to people
  NumericVector v(t_n); // value of trees to people

  for (int i = 0; i < ppl_n; ++i){ // for each person
    chop[i] = -1000000; // sets initial value of trees low
    for (int j = 0; j < t_n; ++j){
      z[j] = sqrt(pow((x1[i] - x2_copy[j]),2) + pow((y1[i] - y2[j]),2)); // distance to each tree
      v[j] = value- z[j]*t_cost - (p[j]*m_cost); // value of each tree

      // if value higher than last tree, new tree is more desirable.
      // iterates through all trees until most valuable tree is found.
        if(v[j] > chop[i]){
        chop[i] = v[j];
        id[i] = j;
      }
    }
// moves people 'mobility' cells towards their nearest tree in the x and y axes

        if (x1[i] - x2[id[i]] < 0) { // if tree to the west 
x1[i] = x1[i] + mobility;} else { // move person west, else ... 
x1[i] = x1[i] - mobility; // move east
}
        if (y1[i] - y2[id[i]] < 0) { // if tree to the north
y1[i] = y1[i] + mobility;} else { // move north, else...
y1[i] = y1[i] - mobility; // move south
  }

  if(chop[i] > 0) { // if valuable enough to be worth chopping ...
    // ...makes that tree far away so that it is not felled by the next person
    x2_copy[id[i]] = 1000000;
  }
  
// and does this for each person...

}

// reports the value and id of the MVT tree for each person

  NumericMatrix summary(ppl_n,4);
  for(int i = 0; i < ppl_n; ++i){
    summary(i,0) = id[i] + 1; // + 1 so it tallies to R indexing system
    summary(i,1) = chop[i];
    summary(i, 2) = x1[i];
    summary(i,3) = y1[i];
  }
  return summary;
}
