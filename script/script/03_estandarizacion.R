# Salida de texto UTF-8 en Windows; no modifica los datos.
if (.Platform$OS.type == "windows") invisible(Sys.setlocale("LC_CTYPE", ".UTF-8"))
source("utils/guardar_tabla.R")
c <- readRDS("input/muestras.rds")$comparables
detalle <- do.call(rbind,lapply(split(c,c$puesto),function(z) {
  medias<-vapply(0:1,function(g) weighted.mean(as.numeric(z$p21[z$titulo==g]),as.numeric(z$pondiio[z$titulo==g])),numeric(1))
  kish<-vapply(0:1,function(g) {w<-as.numeric(z$pondiio[z$titulo==g]);sum(w)^2/sum(w^2)},numeric(1))
  data.frame(puesto=z$puesto[1],n_sin=sum(z$titulo==0),n_con=sum(z$titulo==1),
    media_sin=medias[1],media_con=medias[2],brecha_pct=100*(medias[2]/medias[1]-1),
    peso_comun=sum(as.numeric(z$pondiio))/sum(as.numeric(c$pondiio)),kish_sin=kish[1],kish_con=kish[2])
}))
general<-vapply(0:1,function(g) weighted.mean(as.numeric(c$p21[c$titulo==g]),as.numeric(c$pondiio[c$titulo==g])),numeric(1))
comun<-c(sum(detalle$media_sin*detalle$peso_comun),sum(detalle$media_con*detalle$peso_comun))
tabla<-data.frame(comparacion=c("Distribucion observada","Distribucion comun de puestos"),
  media_sin=c(general[1],comun[1]),media_con=c(general[2],comun[2]),
  brecha_pct=100*c(general[2]/general[1]-1,comun[2]/comun[1]-1),n=nrow(c))
guardar_tabla(detalle,"11_soporte_y_kish");guardar_tabla(tabla,"12_estandarizacion")
