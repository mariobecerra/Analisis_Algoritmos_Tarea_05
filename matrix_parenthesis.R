library(Rcpp)

sourceCpp("matrixChainOrder.cpp")

orden_matrices <- function(p){
  res <- matrixChainOrder(p)
  m <- res$m
  s <- res$s
  m = m[2:nrow(m), 2:ncol(m)]
  s = s[2:nrow(s), 2:ncol(s)]
  cat("Ordenamiento óptimo: ")
  PrintAnswer(s, 1, ncol(s))
  return(list(m = m,
              s = s
  ))
}

PrintAnswer <- function(s, i, j) {
  if(i == j) {
    cat("A", i, sep = "")
  } else{
    cat("(")
    PrintAnswer(s, i, s[i, j])
    PrintAnswer(s, s[i, j] + 1, j)
    cat(")")
  }
}

#PrintAnswer(res$s, 1, ncol(res$s))
# res <- orden_matrices(c(30, 35, 15, 5, 10, 20, 25))

res <- orden_matrices(c(100, 4, 50, 20, 100))


