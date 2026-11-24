# Sandwich CR1 para WLS. Aproxima dependencia intrahogar; no replica todo el diseno EPH.
vcov_hogar <- function(modelo, hogar) {
  x <- model.matrix(modelo); w <- as.numeric(weights(modelo)); e <- residuals(modelo)
  n <- nrow(x); k <- ncol(x); g <- length(unique(hogar))
  stopifnot(modelo$rank == k, g > 1, n > k, length(hogar) == n)
  inversa <- solve(crossprod(x,x*w))
  aportes <- rowsum(x*(w*e),hogar,reorder=FALSE)
  g/(g-1)*(n-1)/(n-k)*inversa %*% crossprod(aportes) %*% inversa
}
