# Formato de presentacion, separado de los calculos y los archivos numericos.
formato_numero <- function(x, decimales=NULL) {
  if(!is.null(decimales)) x <- round(x,decimales)
  format(x,big.mark=".",decimal.mark=",",scientific=FALSE,trim=TRUE)
}
