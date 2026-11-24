# Varianza poblacional ponderada y Theil T: descomposicion exacta anidada.
descomponer <- function(d, ingreso=d$ingreso) {
  w <- d$peso; y <- ingreso; total <- sum(w); media <- weighted.mean(y,w)
  puesto <- as.character(d$puesto); celda <- interaction(puesto,d$titulo,drop=TRUE)
  medias_p <- tapply(w*y,puesto,sum)/tapply(w,puesto,sum)
  medias_c <- tapply(w*y,celda,sum)/tapply(w,celda,sum)
  mp <- medias_p[puesto]; mc <- medias_c[as.character(celda)]
  varianzas <- c(sum(w*(mp-media)^2),sum(w*(mc-mp)^2),sum(w*(y-mc)^2))/total
  f <- function(a) { z <- a/media; ifelse(z>0,z*log(pmax(z,.Machine$double.xmin)),0) }
  t_p <- sum(w*f(mp))/total; t_c <- sum(w*f(mc))/total; t_y <- sum(w*f(y))/total
  theil <- c(t_p,t_c-t_p,t_y-t_c)
  stopifnot(abs(sum(varianzas)-weighted.mean((y-media)^2,w)) < 1e-6)
  data.frame(componente=c("Entre puestos","Entre titulo dentro del puesto","Dentro de puesto y titulo"),
    varianza_millones2=varianzas,porcentaje_varianza=100*varianzas/sum(varianzas),
    theil=theil,porcentaje_theil=100*theil/sum(theil))
}
