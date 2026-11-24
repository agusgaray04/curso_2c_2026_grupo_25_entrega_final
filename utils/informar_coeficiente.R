informar_coeficiente <- function(modelo, hogar, nombre, termino="titulo", escala=1e6) {
  v <- vcov_hogar(modelo,hogar); b <- unname(coef(modelo)[termino])
  ee <- sqrt(v[termino,termino]); gl <- length(unique(hogar))-1
  data.frame(modelo=nombre,n=nobs(modelo),hogares=gl+1,coeficiente=b*escala,
    error_estandar=ee*escala,ic_inferior=(b-qt(.975,gl)*ee)*escala,
    ic_superior=(b+qt(.975,gl)*ee)*escala,t=b/ee,gl=gl,
    p=2*pt(abs(b/ee),gl,lower.tail=FALSE),r2=summary(modelo)$r.squared)
}
