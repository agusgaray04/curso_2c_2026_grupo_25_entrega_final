# Salida de texto UTF-8 en Windows; no modifica los datos.
if (.Platform$OS.type == "windows") invisible(Sys.setlocale("LC_CTYPE", ".UTF-8"))
library(ggplot2)
library(ragg)
# Muestra amplia: referencia principal para la entrega final, por devolucion docente.
# La estimacion en pesos se conserva para continuidad con Instancia 2.
source("utils/guardar_tabla.R")
source("utils/ajustar_puesto.R")
source("utils/descomponer.R")
source("utils/asimetria.R")
muestras<-readRDS("input/muestras.rds")
d<-subset(muestras$con_rama,horas_validas & ch04 %in% 1:2 & !is.na(ch06))
d$peso<-as.numeric(d$pondiio);d$ingreso<-as.numeric(d$p21)/1e6
d$horas<-as.numeric(d$pp3e_tot);d$edad_centrada<-as.numeric(d$ch06)-40
d$sexo<-factor(d$ch04);d$region<-factor(d$region);d$sector<-factor(d$pp04a)
conteo<-table(d$puesto,factor(d$titulo,levels=0:1))
ambos<-rownames(conteo)[rowSums(conteo>0)==2]
resultados<-do.call(rbind,lapply(3:4,function(j) {
  formula<-if(j==3) ingreso~titulo+horas+edad_centrada+I(edad_centrada^2)+sexo else
    ingreso~titulo+horas+edad_centrada+I(edad_centrada^2)+sexo+region+sector
  fit<-ajustar_puesto(d,formula)
  data.frame(modelo=paste0("M",j," soporte ampliado"),n=fit$n,hogares=fit$hogares,
    puestos=nrow(conteo),puestos_ambos=length(ambos),personas_ambos=sum(d$puesto %in% ambos),
    coeficiente=fit$b*1e6,ic_inferior=fit$inf*1e6,ic_superior=fit$sup*1e6,p=fit$p,r2=fit$r2)
}))
guardar_tabla(resultados,"27_soporte_ampliado")
# Puestos con un solo grupo aportan a los controles, no al contraste directo del titulo.
# Validacion de FWL contra el ajuste con indicadores explicitos en los cinco puestos.
z<-muestras$modelo;control<-ajustar_puesto(z,ingreso~titulo+horas+edad_centrada+I(edad_centrada^2)+sexo)
principal<-read.csv("output/tablas/13_modelos.csv")
stopifnot(abs(control$b*1e6-principal$coeficiente[3])<.001,nrow(d)==10156)


# Modelo principal: log del ingreso, con efectos fijos de ocupacion x rama.
# Se conservan todos los casos amplios en pesos; el log exige ingreso positivo.
dl <- subset(d, ingreso > 0)
dl$log_ingreso <- log(as.numeric(dl$p21))
formulas_log <- list(
  principal = log_ingreso ~ titulo + horas + edad_centrada + I(edad_centrada^2) + sexo,
  region_sector = log_ingreso ~ titulo + horas + edad_centrada + I(edad_centrada^2) + sexo + region + sector
)
ajustes_log <- lapply(formulas_log, function(f) ajustar_puesto(dl, f))
tabla_log <- do.call(rbind, lapply(names(ajustes_log), function(nombre) {
  z <- ajustes_log[[nombre]]
  data.frame(modelo=nombre,n=z$n,hogares=z$hogares,puestos=length(unique(dl$puesto)),
    coef_log=z$b,error_estandar=z$se,t=z$t,gl=z$gl,p=z$p,
    ic_log_inferior=z$inf,ic_log_superior=z$sup,
    diferencia_porcentual=100*expm1(z$b),ic_pct_inferior=100*expm1(z$inf),
    ic_pct_superior=100*expm1(z$sup),r2_ponderado=z$r2)
}))
guardar_tabla(tabla_log,"29_modelo_principal_log_amplio")
guardar_tabla(data.frame(etapa=c("Muestra amplia con controles validos",
  "Ingreso cero excluido solo para log","Muestra principal con ingreso positivo"),
  n=c(nrow(d),sum(d$ingreso==0),nrow(dl))),"30_flujo_log_amplio")

# Soporte de la comparacion: no todos los puestos tienen ambos grupos educativos.
ct <- table(dl$puesto,factor(dl$titulo,levels=0:1))
con_ambos <- rownames(ct)[rowSums(ct>0)==2]
guardar_tabla(data.frame(n=nrow(dl),n_sin_titulo=sum(dl$titulo==0),n_con_titulo=sum(dl$titulo==1),
  puestos=nrow(ct),puestos_ambos=length(con_ambos),personas_en_puestos_ambos=sum(dl$puesto %in% con_ambos)),
  "31_soporte_modelo_principal")

# Diagnosticos descriptivos comparables sobre los mismos casos positivos.
pesos_positivo <- ajustar_puesto(dl,ingreso~titulo+horas+edad_centrada+I(edad_centrada^2)+sexo)

diagnosticos <- do.call(rbind,lapply(c("pesos","log"),function(escala) {
  z <- if(escala=="pesos") pesos_positivo else ajustes_log$principal
  data.frame(escala=escala,n=nrow(dl),asimetria_residuos=asimetria(z$residuos,dl$peso),
    cor_abs_residuo_ajuste=cor(abs(z$residuos),z$ajustados))
}))
guardar_tabla(diagnosticos,"32_diagnosticos_modelo_amplio")
saveRDS(list(muestra=dl,ajustes=ajustes_log),"input/principal_amplio.rds")

# El grafico nuevo se agrega a los de Instancia 3; no reemplaza sus resultados.
g <- ggplot(tabla_log,aes(diferencia_porcentual,modelo))+
  geom_vline(xintercept=0,linetype=2,color="#77848B")+
  geom_segment(aes(x=ic_pct_inferior,xend=ic_pct_superior,yend=modelo),linewidth=1,color="#23777A")+
  geom_point(size=3.8,color="#23777A")+
  scale_y_discrete(labels=c(principal="Principal: puesto, horas, edad y sexo",region_sector="Agrega regi\u00f3n y sector"))+
  labs(title="Diferencia asociada al t\u00edtulo universitario en la muestra amplia",
    subtitle="9.968 ingresos positivos; efectos fijos de ocupaci\u00f3n x rama; IC individuales del 95 %",
    x="Diferencia porcentual condicional (%)",y=NULL,
    caption=paste("Fuente: elaboraci\u00f3n propia, EPH-INDEC, 1T 2026. PONDIIO; errores agrupados por hogar.",
      "Asociacion, no efecto causal. El log excluye ingresos cero."))+
  theme_minimal(base_size=13,base_family="Arial")+
  theme(panel.grid.minor=element_blank(),plot.caption=element_text(hjust=0,size=9))
ggsave("output/graficos/principal_log_amplio.png",g,width=12,height=4.8,dpi=200,device=ragg::agg_png,bg="white")
print(tabla_log,row.names=FALSE)
print(diagnosticos,row.names=FALSE)


# La participacion entre puestos depende de la agrupacion y del soporte elegido.
# La muestra ampliada de esta comparacion exige al menos diez casos totales por puesto.
n_puesto <- table(d$puesto)
dd <- subset(d,puesto %in% names(n_puesto)[n_puesto>=10])
partes_amplias <- descomponer(dd)
partes_amplias$n <- nrow(dd)
partes_amplias$puestos <- length(unique(dd$puesto))
partes_amplias$universo <- "Muestra amplia: puestos con al menos 10 casos totales"
guardar_tabla(partes_amplias,"33_descomposicion_soporte_ampliado")
print(partes_amplias,row.names=FALSE)


# Comparacion con el soporte descriptivo de la entrega anterior: sin filtro de horas.
# La descomposicion del ingreso no necesita controlar horas para ser calculable.
dg <- muestras$con_rama
dg$peso <- as.numeric(dg$pondiio)
dg$ingreso <- as.numeric(dg$p21)/1e6
ng <- table(dg$puesto)
dg <- subset(dg,puesto %in% names(ng)[ng>=10])
partes_sin_horas <- descomponer(dg)
partes_sin_horas$n <- nrow(dg)
partes_sin_horas$puestos <- length(unique(dg$puesto))
partes_sin_horas$universo <- "Descriptivo sin filtro de horas: al menos 10 casos totales por puesto"
guardar_tabla(rbind(partes_sin_horas,partes_amplias),"33_descomposicion_soporte_ampliado")
stopifnot(nrow(dg)==7823,abs(partes_sin_horas$porcentaje_varianza[1]-37.1714315364)<1e-7)

ggsave("output/graficos/principal_log_diapositiva.png",g+labs(title=NULL,subtitle=NULL,caption=NULL),
  width=13,height=5.8,dpi=180,device=ragg::agg_png,bg="white")
