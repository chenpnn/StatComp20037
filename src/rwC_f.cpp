#include <Rcpp.h>
#include<math.h> 
using namespace Rcpp;

//' @title A Gibbs sampler using Rcpp
//' @description A Gibbs sampler using Rcpp
//' @param sigma the standard error
//' @param x0 the start point
//' @param N the number of circles
//' @return a numeric matrix of results 
//' @export
// [[Rcpp::export]]
NumericVector rwC_f(double sigma, double x0, int N){
  NumericVector x(N+1);
  x[0] = x0;
  NumericVector u(N);
  for (int i = 0; i < N; i++){
    u[i] = runif(1)[0];
  }
  int k = 0;
  for (int i=1; i < N; i++){
    double y = rnorm(1, x[i-1], sigma)[0];
    if (u[i] <= (exp(-abs(y)) / exp(-abs(x[i-1])))){
      x[i] = y;
    }
    else {
      x[i] = x[i-1];
      k = k+1;
    }
  }
  x[N] = k;
  return(x);
}
