describir <- function(x, w) {
  ok <- is.finite(x) & is.finite(w) & w > 0
  x <- as.numeric(x[ok]); w <- as.numeric(w[ok])
  if (!length(x)) return(data.frame(n=0, media=NA, desvio=NA, p25=NA, mediana=NA, p75=NA))
  m <- weighted.mean(x,w); q <- cuantil_ponderado(x,w,c(.25,.5,.75))
  data.frame(n=length(x),media=m,desvio=sqrt(weighted.mean((x-m)^2,w)),
             p25=q[1],mediana=q[2],p75=q[3])
}
