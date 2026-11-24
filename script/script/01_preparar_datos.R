# Salida de texto UTF-8 en Windows; no modifica los datos.
if (.Platform$OS.type == "windows") invisible(Sys.setlocale("LC_CTYPE", ".UTF-8"))
# Ejecutar desde la raiz del proyecto. No requiere objetos de sesiones anteriores.
source("utils/guardar_tabla.R")
for (carpeta in c("input","output/tablas","output/graficos")) {
  dir.create(carpeta, recursive=TRUE, showWarnings=FALSE)
}
base <- read.table("raw/usu_individual_T126.txt",header=TRUE,sep=";",quote="\"",
  comment.char="",stringsAsFactors=FALSE,na.strings=c("","NA"))
names(base) <- tolower(names(base))
stopifnot(nrow(base)==43739, ncol(base)==234,
  all(base$ano4==2026 & base$trimestre==1),
  !anyDuplicated(base[c("codusu","nro_hogar","componente")]))
adultos <- subset(base,!is.na(ch06) & ch06>=18 & ch06<=65)
ocupados <- subset(adultos,estado==1)
asalariados <- subset(ocupados,cat_ocup==3)
asalariados$titulo <- NA_integer_
asalariados$titulo[asalariados$ch12 %in% c(1:6,9)] <- 0L
asalariados$titulo[asalariados$ch12 %in% 0 & asalariados$ch10 %in% 3] <- 0L
asalariados$titulo[asalariados$ch12 %in% 7 & asalariados$ch13 %in% 2] <- 0L
asalariados$titulo[(asalariados$ch12 %in% 7 & asalariados$ch13 %in% 1) |
                    asalariados$ch12 %in% 8] <- 1L
asalariados$ocupacion <- sprintf("%05d",as.integer(asalariados$pp04d_cod))
asalariados$rama <- sprintf("%04d",as.integer(asalariados$pp04b_cod))
asalariados$ocupacion_valida <- !is.na(asalariados$pp04d_cod) &
  asalariados$pp04d_cod>0 & substr(asalariados$ocupacion,5,5) %in% as.character(1:4)
asalariados$rama_valida <- !is.na(asalariados$pp04b_cod) & asalariados$pp04b_cod>0 &
  !asalariados$pp04b_cod %in% c(999,9999,99999)
asalariados$ingreso_valido <- !is.na(asalariados$p21) & asalariados$p21>=0 &
  !is.na(asalariados$pondiio) & asalariados$pondiio>0
asalariados$horas_validas <- !is.na(asalariados$pp3e_tot) &
  asalariados$pp3e_tot>0 & asalariados$pp3e_tot<=168
asalariados$puesto <- paste(asalariados$ocupacion,asalariados$rama,sep="_")
asalariados$hogar <- paste(asalariados$codusu,asalariados$nro_hogar,sep="_")
validos <- subset(asalariados,!is.na(titulo) & ingreso_valido & ocupacion_valida)
con_rama <- subset(validos,rama_valida)
conteos <- table(con_rama$puesto,factor(con_rama$titulo,levels=0:1))
claves <- rownames(conteos)[apply(conteos>=20,1,all)]
# El minimo de 20 se aplica antes del filtro de horas; Kish es alerta de fragilidad.
comparables <- subset(con_rama,puesto %in% claves)
modelo <- subset(comparables,horas_validas & ch04 %in% c(1,2) & !is.na(ch06))
modelo$ingreso <- as.numeric(modelo$p21)/1e6
modelo$peso <- as.numeric(modelo$pondiio)
modelo$horas <- as.numeric(modelo$pp3e_tot)
modelo$edad_centrada <- as.numeric(modelo$ch06)-40
modelo$sexo <- factor(modelo$ch04)
modelo$puesto <- factor(modelo$puesto)
modelo$region <- factor(modelo$region)
modelo$sector <- factor(modelo$pp04a)
modelo$educacion <- factor(ifelse(modelo$titulo==1,"Universitario completo",
  ifelse(modelo$ch12==6 & modelo$ch13==1,"Terciario completo","Resto")),
  levels=c("Terciario completo","Universitario completo","Resto"))
etiquetas <- read.csv("auxiliar/puestos.csv",stringsAsFactors=FALSE)
stopifnot(setequal(claves,etiquetas$puesto),nrow(validos)==11302,
          nrow(comparables)==1447,nrow(modelo)==1047)
flujo <- data.frame(etapa=c("Base original","18 a 65 a\u00f1os","Ocupados","Asalariados",
  "Titulo, ingreso y ocupaci\u00f3n validos","Con rama valida","Cinco puestos comparables","Muestra de regresion"),
  n=c(nrow(base),nrow(adultos),nrow(ocupados),nrow(asalariados),nrow(validos),nrow(con_rama),nrow(comparables),nrow(modelo)))
guardar_tabla(flujo,"01_flujo")
guardar_tabla(etiquetas,"02_puestos")
saveRDS(list(adultos=adultos,asalariados=asalariados),"input/universos.rds")
saveRDS(list(validos=validos,con_rama=con_rama,comparables=comparables,modelo=modelo),"input/muestras.rds")
cat("Preparacion: ",nrow(modelo)," personas; ",length(unique(modelo$hogar))," hogares.\n")
