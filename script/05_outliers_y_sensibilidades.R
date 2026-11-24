# Salida de texto UTF-8 en Windows; no modifica los datos.
if (.Platform$OS.type == "windows") invisible(Sys.setlocale("LC_CTYPE", ".UTF-8"))
source("utils/guardar_tabla.R")
source("utils/cuantil_ponderado.R")
source("utils/describir.R")
source("utils/vcov_hogar.R")
source("utils/informar_coeficiente.R")
muestras<-readRDS("input/muestras.rds"); d<-muestras$modelo
m3<-readRDS("input/modelos.rds")$modelos[[3]]
p99<-cuantil_ponderado(d$ingreso,d$peso,.99)
positivo<-d$ingreso>0
q<-cuantil_ponderado(log(d$ingreso[positivo]),d$peso[positivo],c(.25,.75))
limites<-exp(c(q[1]-1.5*diff(q),q[2]+1.5*diff(q)))
cook<-cooks.distance(m3);umbral<-4/nrow(d)
diagnosticos<-data.frame(ajustado=fitted(m3),residuo=residuals(m3),
  residuo_estandar=rstandard(m3),cook=cook,palanca=hatvalues(m3),titulo=d$titulo,
  puesto=d$puesto,peso=d$peso,ingreso=d$ingreso,horas=d$horas)
guardar_tabla(diagnosticos,"17_diagnosticos")
resumen<-data.frame(criterio=c("Ingreso cero","Cola baja: log-IQR","Cola alta: log-IQR","Superior al P99 ponderado","Cook > 4/n"),
  n=c(sum(!positivo),sum(d$ingreso>0 & d$ingreso<limites[1]),sum(d$ingreso>limites[2]),sum(d$ingreso>p99),sum(cook>umbral)),
  umbral=c(0,limites,p99,umbral))
guardar_tabla(resumen,"18_outliers")
perfiles<-do.call(rbind,lapply(list(cola_alta=d$ingreso>limites[2],
  cola_baja=d$ingreso>0 & d$ingreso<limites[1],superior_p99=d$ingreso>p99,
  influyentes=cook>umbral),function(i) {
    z<-d[i,];aggregate(list(n=rep(1,nrow(z))),list(titulo=z$titulo,puesto=z$puesto),sum)
  }))
perfiles$criterio<-sub("\\..*$","",rownames(perfiles));rownames(perfiles)<-NULL
guardar_tabla(perfiles,"19_perfil_extremos")
extremos<-subset(d,ingreso>p99,select=c(titulo,puesto,ingreso,horas,peso))
guardar_tabla(extremos,"20_superiores_p99")
acotado<-d;acotado$ingreso<-pmin(d$ingreso,p99)
sin_cook<-d[-which.max(cook),]
sin_fragil<-subset(d,puesto!="40332_8600");sin_fragil$puesto<-droplevels(sin_fragil$puesto)
escenarios<-list("Principal"=d,"Acotado al P99"=acotado,
                 "Sin caso de mayor Cook"=sin_cook,"Sin celda Kish menor a 10"=sin_fragil)
sensibilidad<-do.call(rbind,lapply(names(escenarios),function(nombre) {
  z<-escenarios[[nombre]];m<-lm(formula(m3),data=z,weights=peso)
  informar_coeficiente(m,z$hogar,nombre)
}))
log_datos<-d[positivo,];log_datos$ingreso<-log(log_datos$ingreso)
log_modelo<-lm(formula(m3),data=log_datos,weights=peso)
log_tabla<-informar_coeficiente(log_modelo,log_datos$hogar,"M3 logaritmico",escala=1)
log_tabla$porcentaje<-100*expm1(log_tabla$coeficiente)
log_tabla$ic_pct_inferior<-100*expm1(log_tabla$ic_inferior)
log_tabla$ic_pct_superior<-100*expm1(log_tabla$ic_superior)
guardar_tabla(log_tabla,"21_modelo_log")
guardar_tabla(sensibilidad,"22_sensibilidades")
# Sensibilidad a seleccion por horas: incluir ausencia semanal sin inventar jornadas.
con_ceros<-subset(muestras$comparables,!is.na(pp3e_tot) & pp3e_tot>=0 &
  pp3e_tot<=168 & ch04 %in% 1:2 & !is.na(ch06))
con_ceros$ingreso<-as.numeric(con_ceros$p21)/1e6
con_ceros$peso<-as.numeric(con_ceros$pondiio)
con_ceros$horas<-as.numeric(con_ceros$pp3e_tot)
con_ceros$ausencia<-as.integer(con_ceros$horas==0)
con_ceros$edad_centrada<-as.numeric(con_ceros$ch06)-40
con_ceros$sexo<-factor(con_ceros$ch04);con_ceros$puesto<-factor(con_ceros$puesto)
modelo_ceros<-lm(ingreso~titulo+puesto+horas+ausencia+edad_centrada+I(edad_centrada^2)+sexo,
  data=con_ceros,weights=peso)
guardar_tabla(informar_coeficiente(modelo_ceros,con_ceros$hogar,"Horas cero e indicador de ausencia"),
  "28_sensibilidad_horas_cero")
antes_despues<-rbind(cbind(escenario="Original",describir(d$ingreso*1e6,d$peso)),
                     cbind(escenario="Acotado al P99",describir(acotado$ingreso*1e6,acotado$peso)))
guardar_tabla(antes_despues,"23_descriptivas_tratamiento_extremos")
# La normalidad no se impone. La inferencia CR1 no exige homocedasticidad.
# Cook pertenece al WLS: es una alerta diagnostica y no prueba de error del registro.
supuestos<-data.frame(indicador=c("Correlaci\u00f3n absoluta residuo-ajustado","Cook > 4/n","Umbral Cook","Rango matriz / columnas"),
  valor=c(cor(abs(residuals(m3)),fitted(m3)),sum(cook>umbral),umbral,m3$rank/ncol(model.matrix(m3))))
guardar_tabla(supuestos,"24_supuestos")
saveRDS(list(diagnosticos=diagnosticos,p99=p99,limites=limites),"input/diagnosticos.rds")
