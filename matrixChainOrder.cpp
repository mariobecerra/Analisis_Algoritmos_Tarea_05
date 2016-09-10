#include <Rcpp.h>

using namespace Rcpp;

//  [[Rcpp::export]]
List matrixChainOrder(IntegerVector p){
  int n = p .size() - 1;
  IntegerMatrix m(n + 1, n + 1); 
  IntegerMatrix s(n + 1, n + 1);
  
  int j, q;
  //double inf = 1.0/0.0;
  //int inf = std::numeric_limits<int>::max();
  int inf = 2147483647;
  
  for(int i = 1; i <= n; i++){
    m(i, i) = 0;
  }
  
  for(int l = 2; l <= n; l++){ //tamaño de la cadena
    for(int i = 1; i <= n - l + 1; i++){
      j = i + l - 1;
      // m(i, j) = arma::math::inf();
      m(i, j) = inf;
      
      for(int k = i; k <= j - 1; k++){
        q = m(i, k) + m(k + 1, j) + p[i-1] * p[k] * p[j];
        if(q < m(i, j)){
          m(i, j) = q;
          s(i, j) = k;
        }
      }
    }
  }
  return List::create(
    Rcpp::Named("m") = m, 
    Rcpp::Named("s") = s);
}

