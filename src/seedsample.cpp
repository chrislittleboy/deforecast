#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericVector seedsample(NumericVector pr){
    int n = pr.size();
  NumericVector seed(n);
  NumericVector pr_copy(n);
  for(int k = 0; k < n; ++k) {
    pr_copy[k] = pr[k];
  }
  NumericVector x = {1,0}; // either seeds or doesn't seed
  int size = 1; // only takes one 
  bool rep = true; // replace is true
  NumericVector siz;
  NumericVector p;
  for(int i = 0; i < n; ++i){
    NumericVector p = {pr_copy[i], 1-pr_copy[i]};
    siz = sample(x, size, rep, p);
    seed[i] = siz[0];
  }
  return seed;
}