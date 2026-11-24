# Salida de texto UTF-8 en Windows; no modifica los datos.
if (.Platform$OS.type == "windows") invisible(Sys.setlocale("LC_CTYPE", ".UTF-8"))
source("utils/guardar_tabla.R")
source("utils/cuantil_ponderado.R")
source("utils/descomponer.R")
d<-readRDS("input/muestras.rds")$modelo
principal<-descomponer(d);principal$escenario<-"Original"
acotado<-descomponer(d,pmin(d$ingreso,cuantil_ponderado(d$ingreso,d$peso,.99)))
acotado$escenario<-"Acotado al P99"
guardar_tabla(rbind(principal,acotado),"25_varianza_y_theil")
stopifnot(abs(principal$porcentaje_varianza[2]-14.4288700544)<1e-6,
          abs(sum(principal$theil)-.156996357178)<1e-9)
