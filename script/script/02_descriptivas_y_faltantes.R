# Salida de texto UTF-8 en Windows; no modifica los datos.
if (.Platform$OS.type == "windows") invisible(Sys.setlocale("LC_CTYPE", ".UTF-8"))
source("utils/guardar_tabla.R")
source("utils/cuantil_ponderado.R")
source("utils/describir.R")
universos <- readRDS("input/universos.rds")
muestras <- readRDS("input/muestras.rds")
s <- universos$asalariados; d <- muestras$modelo; c <- muestras$comparables
faltantes <- data.frame(variable=c("Ingreso P21","Titulo universitario","Ocupacion","Rama","Horas no utilizables","Horas cero"),
  n=c(sum(!s$ingreso_valido),sum(is.na(s$titulo)),sum(!s$ocupacion_valida),
      sum(!s$rama_valida),sum(is.na(s$pp3e_tot) | s$pp3e_tot>168 | s$pp3e_tot<0),sum(s$pp3e_tot==0,na.rm=TRUE)),
  motivo=c("P21 negativo/ausente o PONDIIO no positivo","Clasificacion educativa desconocida",
  "Codigo no clasificable","Codigo no clasificable","999, ausente o imposible","Ausencia semanal: no es faltante"))
faltantes$porcentaje <- 100*faltantes$n/nrow(s)
guardar_tabla(faltantes,"03_faltantes")
no_respuesta <- do.call(rbind,lapply(c(0,1,NA),function(g) {
  z <- if(is.na(g)) s[is.na(s$titulo),] else s[!is.na(s$titulo)&s$titulo==g,]
  data.frame(grupo=if(is.na(g)) "Sin clasificar" else if(g==1) "Con t\u00edtulo universitario" else "Sin t\u00edtulo universitario",
    n=nrow(z),no_utilizables=sum(!z$ingreso_valido),
    tasa_ponderada=100*weighted.mean(!z$ingreso_valido,as.numeric(z$pondera)))
}))
guardar_tabla(no_respuesta,"04_no_respuesta_educacion")
region <- do.call(rbind,lapply(split(s,s$region),function(z)
  data.frame(region=z$region[1],n=nrow(z),tasa=100*weighted.mean(!z$ingreso_valido,as.numeric(z$pondera)))))
guardar_tabla(region,"05_no_respuesta_region")
horas <- do.call(rbind,lapply(list(universo=s,comparables=c,modelo=d),function(z)
  data.frame(n=nrow(z),cero=sum(z$pp3e_tot==0,na.rm=TRUE),
    no_utilizables=sum(is.na(z$pp3e_tot)|z$pp3e_tot<0|z$pp3e_tot>168))))
horas$muestra <- rownames(horas); rownames(horas)<-NULL
guardar_tabla(horas,"06_horas_denominadores")
# Antes: toda informacion valida disponible para cada variable. No se promedian codigos -9/999.
# Despues: casos completos para el modelo. La diferencia tambien refleja seleccion de muestra.
etapas <- list("Asalariados: disponibles por variable"=s,
               "Cinco puestos antes de horas"=c,"Muestra modelo"=d)
descriptivas <- do.call(rbind,lapply(names(etapas),function(etapa) {
  z<-etapas[[etapa]]
  do.call(rbind,lapply(c("p21","ch06","pp3e_tot"),function(v) {
    ok <- if(v=="p21") z$ingreso_valido else if(v=="pp3e_tot") !is.na(z[[v]]) & z[[v]]>=0 & z[[v]]<=168 else !is.na(z[[v]])
    w <- if(v=="p21") z$pondiio else z$pondera
    a <- describir(z[[v]][ok],as.numeric(w[ok])); a$etapa<-etapa; a$variable<-v; a
  }))
}))
guardar_tabla(descriptivas,"07_antes_despues")
grupo <- do.call(rbind,lapply(split(d,d$titulo),function(z) {
  a<-describir(z$p21,z$peso);a$grupo<-if(z$titulo[1]==1) "Con t\u00edtulo universitario" else "Sin t\u00edtulo universitario";a
}))
guardar_tabla(grupo,"08_descriptivas_por_titulo")
categoricas<-do.call(rbind,lapply(c("sexo","puesto","educacion","sector"),function(v)
  do.call(rbind,lapply(split(d,d[[v]]),function(z) data.frame(variable=v,categoria=as.character(z[[v]][1]),
  n=nrow(z),porcentaje=100*sum(z$peso)/sum(d$peso))))))
guardar_tabla(categoricas,"09_categoricas_modelo")
# Continuidad con instancia 1: superior completo incluye terciario y universitario.
a <- universos$adultos
a$nivel <- ifelse(a$nivel_ed %in% c(1,2,3,7),1,ifelse(a$nivel_ed==4,2,
  ifelse(a$nivel_ed==5,3,ifelse(a$nivel_ed==6,4,NA))))
instancia1 <- do.call(rbind,lapply(1:4,function(g) {
  z<-subset(a,nivel==g); activos<-subset(z,estado %in% 1:2); ocu<-subset(z,estado==1)
  asal<-subset(ocu,cat_ocup==3 & pp07h %in% 1:2)
  inc<-subset(ocu,p21>0 & pondiio>0)
  data.frame(nivel=c("Hasta secundario incompleto","Secundario completo","Superior incompleto","Superior completo")[g],
    n=nrow(z),desocupacion=100*weighted.mean(activos$estado==2,as.numeric(activos$pondera)),
    registro=100*weighted.mean(asal$pp07h==1,as.numeric(asal$pondera)),
    cuentapropismo=100*weighted.mean(ocu$cat_ocup==2,as.numeric(ocu$pondera)),
    mediana_ingreso=cuantil_ponderado(inc$p21,inc$pondiio))
}))
guardar_tabla(instancia1,"10_continuidad_instancia1")
