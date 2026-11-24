# Salida de texto UTF-8 en Windows; no modifica los datos.
if (.Platform$OS.type == "windows") invisible(Sys.setlocale("LC_CTYPE", ".UTF-8"))
# Completa el modelo amplio: descriptivas, extremos, influencia y sensibilidades.
library(ggplot2)
library(ragg)
source("utils/guardar_tabla.R")
source("utils/cuantil_ponderado.R")
source("utils/describir.R")
source("utils/ajustar_puesto.R")
source("utils/influencia_puesto.R")
source("utils/vcov_hogar.R")
source("utils/unir_descomposicion.R")
source("utils/formato_numero.R")
muestras <- readRDS("input/muestras.rds")
objeto <- readRDS("input/principal_amplio.rds")
d <- objeto$muestra
formula <- log_ingreso~titulo+horas+edad_centrada+I(edad_centrada^2)+sexo
principal <- objeto$ajustes$principal

# Validacion de la absorcion y CR1 contra indicadores explicitos en cinco puestos.
chica <- subset(muestras$modelo,ingreso>0)
chica$log_ingreso <- log(chica$p21)
directo <- lm(log_ingreso~titulo+puesto+horas+edad_centrada+I(edad_centrada^2)+sexo,
  data=chica,weights=peso)
absorbido <- ajustar_puesto(chica,formula)
vc <- vcov_hogar(directo,chica$hogar)
validacion <- data.frame(comprobacion=c("Coeficiente FWL versus indicadores","Error estandar CR1 versus indicadores"),
  diferencia=c(abs(coef(directo)["titulo"]-absorbido$b),
    abs(sqrt(vc["titulo","titulo"])-absorbido$se)))
stopifnot(all(validacion$diferencia<1e-8))

# Descriptivas antes y despues: siempre con datos validos, nunca codigos de no respuesta.
amplia <- subset(muestras$con_rama,horas_validas & ch04 %in% 1:2 & !is.na(ch06))
etapas <- list("Ingreso valido antes de filtro de horas"=muestras$con_rama,
  "Muestra amplia en pesos"=amplia,"Principal: ingreso positivo"=d)
desc <- do.call(rbind,lapply(names(etapas),function(etapa) {
  z <- etapas[[etapa]]
  do.call(rbind,lapply(c("p21","pp3e_tot","ch06"),function(v) {
    ok <- is.finite(z[[v]])
    if(v=="pp3e_tot") ok <- ok & z[[v]]>=0 & z[[v]]<=168
    a <- describir(z[[v]][ok],as.numeric(z$pondiio[ok]))
    cbind(etapa=etapa,variable=v,a)
  }))
}))
guardar_tabla(desc,"34_descriptivas_antes_despues_amplia")
por_grupo <- do.call(rbind,lapply(0:1,function(g) {
  z <- d[d$titulo==g,]
  do.call(rbind,lapply(c("p21","pp3e_tot","ch06"),function(v)
    cbind(grupo=if(g==1) "Con t\u00edtulo universitario" else "Sin t\u00edtulo universitario",variable=v,
      describir(z[[v]],z$peso))))
}))
guardar_tabla(por_grupo,"35_descriptivas_principal_por_titulo")
categoricas <- do.call(rbind,lapply(c("sexo","sector","educacion"),function(v) {
  if(v=="educacion") d[[v]] <- factor(ifelse(d$titulo==1,"Universitario completo",
    ifelse(d$ch12==6 & d$ch13==1,"Terciario completo","Resto sin t\u00edtulo universitario")))
  do.call(rbind,lapply(split(d,d[[v]]),function(z) data.frame(variable=v,
    categoria=as.character(z[[v]][1]),n=nrow(z),porcentaje_pondiio=100*sum(z$peso)/sum(d$peso))))
}))
guardar_tabla(categoricas,"36_categoricas_principal")

diag <- influencia_puesto(d,formula)
q <- cuantil_ponderado(d$log_ingreso,d$peso,c(.25,.75))
limites <- c(q[1]-1.5*diff(q),q[2]+1.5*diff(q))
p99 <- cuantil_ponderado(d$p21,d$peso,.99)
cola_baja <- d$log_ingreso<limites[1]
cola_alta <- d$log_ingreso>limites[2]
superior <- d$p21>p99
umbral <- 4/nrow(d)
resumen <- data.frame(criterio=c("Cola baja log-IQR","Cola alta log-IQR","Superior al P99 ponderado",
  "Cook mayor a 4/n, entre h menor a 1","Borrado de rango fijo no identificable: h=1"),
  n=c(sum(cola_baja),sum(cola_alta),sum(superior),sum(diag$cook>umbral,na.rm=TRUE),sum(!diag$borrado_identificable)),
  umbral=c(exp(limites),p99,umbral,1))
guardar_tabla(resumen,"37_outliers_modelo_principal")
perfiles <- do.call(rbind,lapply(list(cola_baja=cola_baja,cola_alta=cola_alta,superior_p99=superior),function(i) {
  z <- d[i,]
  if(!nrow(z)) return(NULL)
  data.frame(n=nrow(z),con_titulo=sum(z$titulo==1),horas_mediana=cuantil_ponderado(z$horas,z$peso),
    mediana_ingreso=cuantil_ponderado(z$p21,z$peso),p21_compatible_p47t=sum(z$p21<=z$p47t & z$p47t>=0,na.rm=TRUE),
    p47t_valido=sum(is.finite(z$p47t)&z$p47t>=0))
}))
perfiles$criterio <- rownames(perfiles);rownames(perfiles)<-NULL
guardar_tabla(perfiles,"38_perfil_extremos_principal")
acotado <- d;acotado$log_ingreso <- log(pmin(d$p21,p99))
indice <- which.max(abs(diag$delta_titulo))
sin_influyente <- d[-indice,]
chequeo_borrado <- ajustar_puesto(sin_influyente,formula)
stopifnot(abs((principal$b-diag$delta_titulo[indice])-chequeo_borrado$b)<1e-8)
validacion <- rbind(validacion,data.frame(comprobacion="Borrado exacto del coeficiente t\u00edtulo",
  diferencia=abs((principal$b-diag$delta_titulo[indice])-chequeo_borrado$b)))
guardar_tabla(validacion,"39_validacion_modelo_amplio")
perfil_caso <- data.frame(titulo=d$titulo[indice],puesto=d$puesto[indice],ingreso=d$p21[indice],
  horas=d$horas[indice],peso=d$peso[indice],cook=diag$cook[indice],
  delta_titulo_log=diag$delta_titulo[indice])
guardar_tabla(perfil_caso,"40_caso_influyente_principal")
solo_ambos <- subset(d,puesto %in% rownames(table(d$puesto,d$titulo))[rowSums(table(d$puesto,d$titulo)>0)==2])
escenarios <- list("Principal"=d,"P99 ponderado"=acotado,
  "Sin mayor influencia sobre t\u00edtulo"=sin_influyente,"Solo puestos con ambos grupos"=solo_ambos)
sens <- do.call(rbind,lapply(names(escenarios),function(nm) {
  z <- ajustar_puesto(escenarios[[nm]],formula)
  data.frame(escenario=nm,n=z$n,coef_log=z$b,diferencia_pct=100*expm1(z$b),
    ic_inferior_pct=100*expm1(z$inf),ic_superior_pct=100*expm1(z$sup),p=z$p)
}))
guardar_tabla(sens,"41_sensibilidades_principal_log")
antes_p99 <- rbind(cbind(escenario="Original positivo",describir(d$p21,d$peso)),
  cbind(escenario="Acotado al P99",describir(pmin(d$p21,p99),d$peso)))
guardar_tabla(antes_p99,"42_descriptivas_p99_principal")
saveRDS(list(diagnosticos=diag,p99=p99,limites_log=limites,indice_influyente=indice),"input/diagnosticos_principal.rds")

tema <- theme_minimal(base_size=15,base_family="Arial")+
  theme(panel.grid.minor=element_blank(),plot.title=element_text(face="bold",size=21),
    plot.caption=element_text(size=10,hjust=0),legend.position="bottom")
pie <- "EPH-INDEC, 1T 2026. PONDIIO. Modelo amplio: 9.968 personas."
rp <- read.csv("output/tablas/32_diagnosticos_modelo_amplio.csv")
diag_largo <- rbind(data.frame(escala="Ingreso en pesos",ajuste=ajustar_puesto(d,ingreso~titulo+horas+edad_centrada+I(edad_centrada^2)+sexo)$ajustados,
  residuo=ajustar_puesto(d,ingreso~titulo+horas+edad_centrada+I(edad_centrada^2)+sexo)$residuos),
  data.frame(escala="Log del ingreso",ajuste=diag$ajustado,residuo=diag$residuo))
g <- ggplot(diag_largo,aes(ajuste,residuo))+geom_point(alpha=.13,size=.6,color="#23777A")+
  geom_hline(yintercept=0,linetype=2,color="#C6533B")+facet_wrap(~escala,scales="free")+
  labs(x="Valor ajustado (escala de cada modelo)",y="Residuo",caption=pie)+tema
ggsave("output/graficos/diagnosticos_amplios.png",g,width=13,height=5.8,dpi=180,device=ragg::agg_png,bg="white")
g <- ggplot(subset(diag,is.finite(residuo_estandar)),aes(sample=residuo_estandar))+
  stat_qq(alpha=.3,size=.7,color="#23777A")+stat_qq_line(color="#C6533B")+
  labs(x="Cuantiles normales",y="Residuo estandarizado WLS",caption=paste(pie,"Se omiten h=1 solo en este diagnostico."))+tema
ggsave("output/graficos/qq_principal.png",g,width=10,height=5.8,dpi=180,device=ragg::agg_png,bg="white")
g <- ggplot(sens,aes(diferencia_pct,reorder(escenario,diferencia_pct)))+
  geom_vline(xintercept=0,linetype=2,color="#77848B")+
  geom_segment(aes(x=ic_inferior_pct,xend=ic_superior_pct,yend=reorder(escenario,diferencia_pct)),linewidth=1,color="#23777A")+
  geom_point(size=3.5,color="#C6533B")+
  labs(x="Diferencia multiplicativa condicional (%)",y=NULL,caption=paste(pie,"IC95 individuales, CR1 por hogar."))+tema
ggsave("output/graficos/sensibilidades_principal.png",g,width=13,height=5.5,dpi=180,device=ragg::agg_png,bg="white")

# Distribucion poblacional descriptiva; la masa de ingreso cero se informa aparte.
curva <- data.frame(ingreso=d$p21,grupo=factor(d$titulo,0:1,c("Sin t\u00edtulo universitario","Con t\u00edtulo universitario")),peso=d$peso)
g <- ggplot(curva,aes(ingreso,weight=peso,color=grupo))+geom_density(linewidth=1.1)+
  scale_x_log10(labels=function(x) format(x,big.mark=".",decimal.mark=",",scientific=FALSE,trim=TRUE))+
  scale_color_manual(values=c("#84929C","#23777A"))+
  labs(x="Ingreso mensual (pesos, escala logar\u00edtmica)",y="Densidad en escala log",color=NULL,caption=pie)+tema
ggsave("output/graficos/distribucion_principal.png",g,width=12,height=5.4,dpi=180,device=ragg::agg_png,bg="white")

# Comparacion explicita de soportes para las dos descomposiciones.
descomposicion_cinco <- subset(read.csv("output/tablas/25_varianza_y_theil.csv"),escenario=="Original")
descomposicion_amplia <- subset(read.csv("output/tablas/33_descomposicion_soporte_ampliado.csv"),n==7823)

partes <- rbind(unir_descomposicion(descomposicion_cinco,"Cinco puestos\nn = 1.047","Varianza"),unir_descomposicion(descomposicion_amplia,"170 puestos, m\u00ednimo 10\nn = 7.823","Varianza"),
  unir_descomposicion(descomposicion_cinco,"Cinco puestos\nn = 1.047","Theil T"),unir_descomposicion(descomposicion_amplia,"170 puestos, m\u00ednimo 10\nn = 7.823","Theil T"))
partes$componente <- factor(partes$componente,levels=unique(partes$componente),
 labels=c("Entre puestos","Entre t\u00edtulo dentro del puesto","Dentro de puesto y t\u00edtulo"))
partes$metodo <- factor(partes$metodo,levels=c("Varianza","Theil T"))
partes$soporte <- factor(partes$soporte,levels=c("Cinco puestos\nn = 1.047","170 puestos, m\u00ednimo 10\nn = 7.823"))
g <- ggplot(partes,aes(soporte,porcentaje,fill=componente))+geom_col(width=.55)+
  geom_text(aes(label=paste0(formato_numero(porcentaje,1)," %")),position=position_stack(vjust=.5),color="white",size=4.5)+
  facet_wrap(~metodo)+scale_fill_manual(values=c("#7A8A96","#C6533B","#23777A"))+
  labs(x=NULL,y="Participaci\u00f3n (%)",fill=NULL)+
  tema+theme(legend.text=element_text(size=10))
ggsave("output/graficos/descomposicion_soportes.png",g,width=13,height=6,dpi=180,device=ragg::agg_png,bg="white")

# Los intervalos del modelo por puesto son individuales; Holm se aplica a los p.
brechas_puesto <- read.csv("output/tablas/15_brechas_ajustadas_por_puesto.csv")
etiquetas <- read.csv("auxiliar/puestos.csv")
brechas_puesto$nombre <- etiquetas$nombre[match(brechas_puesto$puesto,etiquetas$puesto)]
for(par in list(c("Calificacion","Calificaci\u00f3n"),c("tecnica","t\u00e9cnica"),c("administracion publica","administraci\u00f3n p\u00fablica"),c("educacion","educaci\u00f3n"))) brechas_puesto$nombre<-gsub(par[1],par[2],brechas_puesto$nombre,fixed=TRUE)
brechas_puesto$significativo <- brechas_puesto$p_holm<.05
g <- ggplot(brechas_puesto,aes(coeficiente/1e6,reorder(nombre,coeficiente),color=significativo))+
  geom_vline(xintercept=0,linetype=2,color="#77848B")+
  geom_segment(aes(x=ic_inferior/1e6,xend=ic_superior/1e6,yend=reorder(nombre,coeficiente)),linewidth=1)+
  geom_point(size=3)+scale_color_manual(values=c("#84929C","#23777A"),labels=c("No rechaza tras Holm","Rechaza tras Holm"))+
  labs(x="Diferencia ajustada (millones de pesos)",y=NULL,color=NULL,
    caption="Detalle de cinco puestos, n = 1.047. IC95 individuales sin ajuste; p ajustados por Holm.")+tema
ggsave("output/graficos/brechas_puestos.png",g,width=13,height=5.7,dpi=180,device=ragg::agg_png,bg="white")
cat("Modelo principal y diagnosticos finales guardados.\n")


# Completar el diccionario con el chequeo auxiliar efectivamente usado.
dic <- read.csv("output/tablas/26_diccionario.csv",stringsAsFactors=FALSE)
dic <- dic[dic$variable!="P47T",]
ss <- readRDS("input/universos.rds")$asalariados
dic <- rbind(dic,data.frame(variable="P47T",descripcion="Ingreso total individual (pesos mensuales)",
  tipo="Num\u00e9rica",na_literal=sum(is.na(ss$p47t)),regla="Chequeo de plausibilidad, no control del modelo"))
guardar_tabla(dic,"26_diccionario")

# Influencia amplia: las observaciones h=1 permanecen en el ajuste, sin Cook de borrado.
di <- diag
di$indice <- seq_len(nrow(di))
g <- ggplot(subset(di,is.finite(cook)),aes(indice,cook))+
  geom_segment(aes(xend=indice,yend=0),color="#23777A",linewidth=.35)+
  geom_hline(yintercept=umbral,linetype=2,color="#C6533B")+
  scale_y_continuous(trans="sqrt",labels=scales::label_number(decimal.mark=","))+
  scale_x_continuous(labels=scales::label_number(big.mark=".",decimal.mark=","))+
  labs(x="Observaci\u00f3n (orden de la muestra)",y="Distancia de Cook (escala ra\u00edz cuadrada)")+tema
ggsave("output/graficos/cook_principal.png",g,width=13,height=5.6,dpi=180,device=ragg::agg_png,bg="white")


# Verificaciones solicitadas y estadisticas para la presentacion revisada.
composicion <- do.call(rbind,lapply(0:1,function(g) {
  zz <- d[d$titulo==g,]
  data.frame(titulo=g,n=nrow(zz),porcentaje_mujeres=100*weighted.mean(zz$ch04==2,zz$peso),
    porcentaje_estatal=100*weighted.mean(zz$pp04a==1,zz$peso),
    ingreso_medio=weighted.mean(zz$p21,zz$peso),
    ingreso_desvio=sqrt(weighted.mean((zz$p21-weighted.mean(zz$p21,zz$peso))^2,zz$peso)))
}))
guardar_tabla(composicion,"43_composicion_por_grupo_principal")
altos_iqr <- d$log_ingreso>limites[2]
altos_p99 <- d$p21>p99
verificacion_colas <- data.frame(umbral_iqr_pesos=exp(limites[2]),p99_ponderado=p99,
  n_altos_iqr=sum(altos_iqr),n_altos_p99=sum(altos_p99),
  mismos_casos=identical(altos_iqr,altos_p99),maximo_no_superior_p99=max(d$p21[!altos_p99]),
  minimo_superior_p99=min(d$p21[altos_p99]))
stopifnot(identical(altos_iqr,altos_p99))
guardar_tabla(verificacion_colas,"44_verificacion_iqr_y_p99")

# Brecha cruda aritmetica y secuencia comparable en log: no son equivalentes.
crudo <- lm(log_ingreso~titulo,data=d,weights=peso)
v_crudo <- vcov_hogar(crudo,d$hogar)
b_crudo <- unname(coef(crudo)["titulo"]);se_crudo<-sqrt(v_crudo["titulo","titulo"])
gl_crudo<-length(unique(d$hogar))-1
fijos <- ajustar_puesto(d,log_ingreso~titulo)
secuencia <- data.frame(modelo=c("Solo t\u00edtulo","Titulo y puesto","Puesto y controles"),
  n=nrow(d),coef_log=c(b_crudo,fijos$b,principal$b),
  error_estandar=c(se_crudo,fijos$se,principal$se),
  ic_log_inferior=c(b_crudo-qt(.975,gl_crudo)*se_crudo,fijos$inf,principal$inf),
  ic_log_superior=c(b_crudo+qt(.975,gl_crudo)*se_crudo,fijos$sup,principal$sup))
secuencia$diferencia_porcentual<-100*expm1(secuencia$coef_log)
secuencia$ic_pct_inferior<-100*expm1(secuencia$ic_log_inferior)
secuencia$ic_pct_superior<-100*expm1(secuencia$ic_log_superior)
secuencia$brecha_cruda_aritmetica_pct<-100*(composicion$ingreso_medio[2]/composicion$ingreso_medio[1]-1)
guardar_tabla(secuencia,"45_brecha_cruda_y_modelos_log")

# Comprobar si h=1 se debe a puestos con un unico registro en esta muestra.
sing <- as.numeric(table(d$puesto)[d$puesto])==1
es_h1 <- !diag$borrado_identificable
verificacion_h <- data.frame(n=nrow(d),h_igual_uno=sum(es_h1),personas_unicas_en_puesto=sum(sing),
  mismos_casos=all(es_h1==sing),h_uno_no_unicos=sum(es_h1 & !sing))
stopifnot(all(es_h1==sing))
guardar_tabla(verificacion_h,"46_verificacion_h_uno_y_singletons")

# NA literal y codigos de no respuesta se distinguen, con denominador explicito.
dic <- read.csv("output/tablas/26_diccionario.csv",stringsAsFactors=FALSE)
dic$codigos_no_respuesta <- 0L
contar <- c(CH12=sum(ss$ch12==99),CH13=sum(ss$ch13==9),P21=sum(ss$p21<0),
  PP04D_COD=sum(!ss$ocupacion_valida),PP04B_COD=sum(!ss$rama_valida),
  PP3E_TOT=sum(is.na(ss$pp3e_tot)|ss$pp3e_tot<0|ss$pp3e_tot>168),PONDIIO=sum(ss$pondiio<=0))
dic$codigos_no_respuesta[match(names(contar),dic$variable)]<-as.integer(contar)
dic$tipo[dic$variable %in% c("ANO4","TRIMESTRE","CH06","PP3E_TOT")]<-"Num\u00e9rica discreta"
dic$denominador<-nrow(ss)
guardar_tabla(dic,"47_variables_y_faltantes")

# Distribucion compacta para poner junto a descriptivos del cuerpo principal.
g <- ggplot(curva,aes(ingreso,weight=peso,color=grupo))+
  geom_density(linewidth=1.1)+
  scale_x_log10(breaks=c(1e3,1e4,1e5,1e6,1e7),labels=c("1 mil","10 mil","100 mil","1 mill\u00f3n","10 millones"))+
  scale_y_continuous(labels=scales::label_number(decimal.mark=","))+
  scale_color_manual(values=c("#84929C","#23777A"))+
  labs(x="Ingreso mensual (pesos, escala log)",y="Densidad en escala log",color=NULL)+
  theme_minimal(base_size=14,base_family="Arial")+
  theme(panel.grid.minor=element_blank(),legend.position="bottom",legend.text=element_text(size=10.5),
    axis.text.x=element_text(size=10),plot.margin=margin(6,6,6,6))
ggsave("output/graficos/densidad_cuerpo.png",g,width=6.4,height=4.6,dpi=200,device=ragg::agg_png,bg="white")

# Rehacer las imagenes del cuerpo con tildes, decimales y unidades explicitas.
datos_residuos <- diag_largo
datos_residuos$escala<-factor(datos_residuos$escala,levels=c("Ingreso en pesos","Log del ingreso"),
  labels=c("Modelo en millones de pesos","Modelo en logaritmos"))
g <- ggplot(datos_residuos,aes(ajuste,residuo))+geom_point(alpha=.13,size=.6,color="#23777A")+
  geom_hline(yintercept=0,linetype=2,color="#C6533B")+facet_wrap(~escala,scales="free")+
  scale_x_continuous(labels=scales::label_number(decimal.mark=","))+
  scale_y_continuous(labels=scales::label_number(decimal.mark=","))+
  labs(x="Ajuste: millones de pesos (izq.); log de pesos (der.)",
    y="Residuo: millones de pesos (izq.); log-puntos (der.)")+tema
ggsave("output/graficos/diagnosticos_amplios.png",g,width=13,height=5.6,dpi=180,device=ragg::agg_png,bg="white")
secuencia$modelo <- factor(secuencia$modelo,levels=rev(secuencia$modelo))
g <- ggplot(secuencia,aes(diferencia_porcentual,modelo))+
  geom_vline(xintercept=0,linetype=2,color="#77848B")+
  geom_segment(aes(x=ic_pct_inferior,xend=ic_pct_superior,yend=modelo),linewidth=1,color="#23777A")+
  geom_point(size=3.5,color="#C6533B")+
  scale_y_discrete(labels=c("Solo t\u00edtulo"="Solo t\u00edtulo","Titulo y puesto"="Agrega ocupaci\u00f3n y rama",
    "Puesto y controles"="Agrega horas, edad y sexo"))+
  scale_x_continuous(labels=scales::label_number(decimal.mark=","))+
  labs(x="Diferencia multiplicativa condicional (%)",y=NULL)+tema
ggsave("output/graficos/principal_log_diapositiva.png",g,width=13,height=5.4,dpi=180,device=ragg::agg_png,bg="white")

# Faltantes: excluir solo de la barra los cinco sin clasificar, informados en texto.
nr <- subset(read.csv("output/tablas/04_no_respuesta_educacion.csv"),grupo!="Sin clasificar")
nr$grupo<-factor(nr$grupo,levels=c("Sin t\u00edtulo universitario","Con t\u00edtulo universitario"),
  labels=c("Sin t\u00edtulo universitario","Con t\u00edtulo universitario"))
g <- ggplot(nr,aes(tasa_ponderada,grupo))+geom_col(fill="#23777A",width=.5)+
  geom_text(aes(label=paste0(formato_numero(no_utilizables)," / ",formato_numero(n),"; ",
    formato_numero(tasa_ponderada,1)," %")),hjust=-.08,size=4.3)+
  scale_x_continuous(limits=c(0,40),breaks=c(0,10,20,30),labels=scales::label_number(decimal.mark=","))+
  labs(x="Ingresos no utilizables (%, PONDERA)",y=NULL,
    caption="Conteos: casos observados; porcentajes: con PONDERA.")+
  theme_minimal(base_size=14,base_family="Arial")+
  theme(panel.grid.minor=element_blank(),panel.grid.major.y=element_blank(),plot.margin=margin(7,7,7,7))
ggsave("output/graficos/faltantes_cuerpo.png",g,width=6.4,height=4.5,dpi=200,device=ragg::agg_png,bg="white")

# Forest plot: etiquetas abreviadas, coma decimal e IC individuales.
brechas_puesto <- read.csv("output/tablas/15_brechas_ajustadas_por_puesto.csv",stringsAsFactors=FALSE)
etiquetas_puesto <- c("10333_8401"="Adm. p\u00fablica\nAdministrativos", "40312_8600"="Salud\nT\u00e9cnicos",
 "40332_8600"="Salud\nT\u00e9cnicos informatizados", "41312_8501"="Educaci\u00f3n\nT\u00e9cnicos",
 "41332_8501"="Educaci\u00f3n\nT\u00e9cnicos informatizados")
brechas_puesto$nombre <- etiquetas_puesto[brechas_puesto$puesto]
brechas_puesto$grupo <- ifelse(brechas_puesto$p_holm<.05,"Rechaza tras Holm","No rechaza tras Holm")
brechas_puesto$nombre <- factor(brechas_puesto$nombre,levels=brechas_puesto$nombre[order(brechas_puesto$coeficiente)])
forest <- ggplot(brechas_puesto,aes(coeficiente/1e6,nombre,color=grupo))+
 geom_vline(xintercept=0,linetype=2,color="#829199")+
 geom_segment(aes(x=ic_inferior/1e6,xend=ic_superior/1e6,yend=nombre),linewidth=1.2)+
 geom_point(size=3.4)+scale_color_manual(values=c("No rechaza tras Holm"="#84929C","Rechaza tras Holm"="#23777A"))+
 scale_x_continuous(breaks=seq(-.25,1.5,.25),labels=scales::label_number(decimal.mark=",",accuracy=.01))+
 labs(x="Diferencia ajustada (millones de pesos)",y=NULL,color=NULL)+
 theme_minimal(base_family="Arial",base_size=17)+
 theme(panel.grid.minor=element_blank(),panel.grid.major.y=element_blank(),axis.text.y=element_text(size=16),
  axis.text.x=element_text(size=16),axis.title.x=element_text(size=17),legend.position="bottom",legend.text=element_text(size=14))
ggsave("output/graficos/brechas_puestos_diapositiva.png",forest,width=13,height=5.4,dpi=200,device=ragg::agg_png,bg="white")

# Densidad del principal: coma decimal tambien en eje vertical.
datos_principal <- readRDS("input/principal_amplio.rds")$muestra
densidad_datos <- data.frame(ingreso=datos_principal$p21,grupo=factor(datos_principal$titulo,0:1,c("Sin t\u00edtulo universitario","Con t\u00edtulo universitario")),peso=datos_principal$peso)
dens <- ggplot(densidad_datos,aes(ingreso,weight=peso,color=grupo))+geom_density(linewidth=1.1)+
 scale_x_log10(breaks=c(1e5,1e6,1e7),labels=c("100.000","1.000.000","10.000.000"))+
 scale_y_continuous(labels=scales::label_number(decimal.mark=","))+
 scale_color_manual(values=c("#84929C","#23777A"))+
 labs(x="Ingreso mensual (pesos, escala log)",y="Densidad en escala log",color=NULL)+
 theme_minimal(base_size=17,base_family="Arial")+
 theme(panel.grid.minor=element_blank(),legend.position="bottom",legend.text=element_text(size=14))
ggsave("output/graficos/densidad_principal.png",dens,width=13,height=5.6,dpi=200,device=ragg::agg_png,bg="white")

# Graficos complementarios del cierre; no se modifican estimaciones ni muestras.
if (.Platform$OS.type=="windows") invisible(Sys.setlocale("LC_CTYPE",".UTF-8"))
# Histograma ponderado del log natural con ambas cercas IQR.
d23 <- readRDS("input/principal_amplio.rds")$muestra
lim23 <- readRDS("input/diagnosticos_principal.rds")$limites_log
hist23 <- ggplot(d23,aes(log_ingreso,weight=100*peso/sum(peso)))+
 geom_histogram(binwidth=.25,fill="#23777A",color="white",linewidth=.15)+
 geom_vline(xintercept=lim23,color="#C6533B",linetype=2,linewidth=.8)+
 scale_x_continuous(breaks=c(10,12,14,16),labels=scales::label_number(decimal.mark=","))+
 scale_y_continuous(labels=scales::label_number(decimal.mark=","))+
 labs(x="Log natural del ingreso mensual",y="Participaci\u00f3n ponderada (%)",
  subtitle="L\u00edneas: cercas 1,5 \u00d7 IQR en log")+
 theme_minimal(base_family="Arial",base_size=14)+
 theme(panel.grid.minor=element_blank(),plot.subtitle=element_text(size=13))
ggsave("output/graficos/outliers_log_iqr.png",hist23,width=6.5,height=2.9,dpi=200,device=ragg::agg_png,bg="white")

# Composicion del sector estatal en dos soportes distintos.
det23 <- readRDS("input/muestras.rds")$modelo
comp23 <- data.frame(soporte=c("Principal amplio\nn=9.968","Cinco puestos\nn=1.047"),
 porcentaje=c(100*weighted.mean(d23$pp04a==1,d23$peso),100*weighted.mean(det23$pp04a==1,det23$peso)))
comp23$soporte<-factor(comp23$soporte,levels=comp23$soporte)
comp_g <- ggplot(comp23,aes(porcentaje,soporte))+geom_col(fill="#23777A",width=.5)+
 geom_text(aes(label=paste0(format(formato_numero(porcentaje,1),decimal.mark=",",trim=TRUE)," %")),hjust=-.15,size=6)+
 scale_x_continuous(limits=c(0,90),breaks=seq(0,80,20),labels=scales::label_number(decimal.mark=","))+
 labs(x="Participaci\u00f3n del sector estatal (%, PONDIIO)",y=NULL)+
 theme_minimal(base_family="Arial",base_size=17)+theme(panel.grid.major.y=element_blank(),panel.grid.minor=element_blank())
ggsave("output/graficos/composicion_soportes.png",comp_g,width=13,height=5.3,dpi=200,device=ragg::agg_png,bg="white")

# No respuesta por region, mismo universo y datos de la tabla 05.
nr23 <- read.csv("output/tablas/05_no_respuesta_region.csv")
regiones23 <- c("1"="Gran Buenos Aires","40"="NOA","41"="NEA","42"="Cuyo","43"="Pampeana","44"="Patagonia")
nr23$nombre <- regiones23[as.character(nr23$region)]
greg23 <- ggplot(nr23,aes(tasa,reorder(nombre,tasa)))+geom_col(fill="#23777A",width=.58)+
 geom_text(aes(label=paste0(format(round(tasa,1),decimal.mark=",",trim=TRUE)," %")),hjust=-.18,size=5.5)+
 scale_x_continuous(limits=c(0,35),breaks=seq(0,30,5),labels=scales::label_number(decimal.mark=","))+
 labs(x="Ingreso no utilizable (%, PONDERA)",y=NULL)+theme_minimal(base_size=17,base_family="Arial")+
 theme(panel.grid.major.y=element_blank(),panel.grid.minor=element_blank())
ggsave("output/graficos/no_respuesta_region.png",greg23,width=13,height=5.6,dpi=200,device=ragg::agg_png,bg="white")

# Visualizar la estandarizacion sin confundirla con el ajuste del modelo.
es_tab23 <- read.csv("output/tablas/12_estandarizacion.csv")
es23 <- data.frame(distribucion=c("Observada","Com\u00fan"),brecha=es_tab23$brecha_pct)
esg23 <- ggplot(es23,aes(brecha,factor(distribucion,levels=rev(distribucion))))+
 geom_segment(aes(x=0,xend=brecha,yend=factor(distribucion,levels=rev(distribucion))),color="#D2DDDF",linewidth=1)+
 geom_point(size=5,color="#23777A")+
 geom_text(aes(label=paste0(format(round(brecha,1),decimal.mark=",",trim=TRUE)," %")),hjust=-.2,size=6,fontface="bold")+
 scale_x_continuous(limits=c(0,65),breaks=seq(0,60,10),labels=scales::label_number(decimal.mark=","))+
 labs(x="Brecha de medias (%)",y=NULL)+theme_minimal(base_family="Arial",base_size=17)+
 theme(panel.grid.major.y=element_blank(),panel.grid.minor=element_blank())
ggsave("output/graficos/estandarizacion_diapositiva.png",esg23,width=11,height=2.7,dpi=200,device=ragg::agg_png,bg="white")
print(comp23)

