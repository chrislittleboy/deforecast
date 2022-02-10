#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]

NumericVector probseed(NumericVector ages, const NumericVector agebands, NumericVector probbands){
  int n = ages.size();
  NumericVector pnew(n);
  NumericVector a(n);
  
  // creates a copy of ages so as to not amend it in the loop
  
  for(int k = 0; k < n; k++){
    a[k] = ages[k];
  }
  
// works out the probability of seeding based on the age

  for(int i = 0; i < 11; ++i){
    for(int j = 0; j < n; ++j){
      if (a[j] < agebands[i]) {
        pnew[j] = probbands[i];
        a[j] = 1000;
      }
    }
  }
  return pnew;
}