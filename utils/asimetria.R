asimetria <- function(x,w) {
  centro <- weighted.mean(x,w);sigma <- sqrt(weighted.mean((x-centro)^2,w))
  weighted.mean(((x-centro)/sigma)^3,w)
}
