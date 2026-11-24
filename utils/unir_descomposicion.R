unir_descomposicion <- function(z,nombre,metodo) data.frame(soporte=nombre,componente=z$componente,
  porcentaje=if(metodo=="Varianza") z$porcentaje_varianza else z$porcentaje_theil,metodo=metodo)
