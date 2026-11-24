cuantil_ponderado <- function(x, w, p = .5) {
  ok <- is.finite(x) & is.finite(w) & w > 0
  x <- as.numeric(x[ok]); w <- as.numeric(w[ok])
  if (!length(x)) return(rep(NA_real_, length(p)))
  orden <- order(x); x <- x[orden]; w <- w[orden]
  acumulada <- cumsum(w) / sum(w)
  vapply(p, function(q) x[which(acumulada >= q)[1]], numeric(1))
}
