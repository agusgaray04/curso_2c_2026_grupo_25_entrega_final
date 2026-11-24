# Salida de texto UTF-8 en Windows; no modifica los datos.
if (.Platform$OS.type == "windows") invisible(Sys.setlocale("LC_CTYPE", ".UTF-8"))
library(ggplot2)
library(ragg)
source("utils/guardar_tabla.R")
source("utils/cuantil_ponderado.R")
muestras<-readRDS("input/muestras.rds");d<-muestras$modelo
tema<-theme_minimal(base_family="Arial",base_size=16)+theme(
  panel.grid.minor=element_blank(),panel.grid.major.y=element_blank(),
  plot.title=element_text(face="bold",color="#1E2535",size=21),
  plot.caption=element_text(size=10,hjust=0),plot.title.position="plot")
fuente<-"Fuente: elaboraci\u00f3n propia con EPH-INDEC, primer trimestre de 2026."
g<-read.csv("output/tablas/04_no_respuesta_educacion.csv")
faltantes<-ggplot(g,aes(tasa_ponderada,reorder(grupo,-tasa_ponderada)))+
  geom_col(fill="#2D777B",width=.55)+
  geom_text(aes(label=paste0(format(no_utilizables,big.mark=".",trim=TRUE)," / ",
    format(n,big.mark=".",trim=TRUE)," casos; ",
    format(round(tasa_ponderada,1),decimal.mark=",",trim=TRUE)," %")),hjust=-.04,size=4.8)+
  scale_x_continuous(limits=c(0,140),breaks=c(0,25,50,75,100))+
  labs(title="La no respuesta limita la muestra de ingresos",x="Ingresos no utilizables (%, PONDERA)",y=NULL,
    caption=paste(fuente,"Universo: 13.632 asalariados. Los conteos no estan ponderados."))+tema
ggsave("output/graficos/faltantes.png",faltantes,width=13,height=5.3,dpi=180,device=ragg::agg_png,bg="white")
curva<-data.frame(ingreso=d$ingreso,grupo=factor(d$titulo,0:1,c("Sin t\u00edtulo universitario","Con t\u00edtulo universitario")),peso=d$peso)
ingresos<-ggplot(curva,aes(ingreso,weight=peso,fill=grupo))+
  geom_histogram(binwidth=.25,position="identity",alpha=.5,boundary=0)+
  scale_fill_manual(values=c("#84929C","#2D777B"))+
  labs(title="El ingreso tiene una cola derecha extensa",x="Ingreso mensual principal (millones de pesos)",
    y="Personas representadas (PONDIIO)",fill=NULL,caption=paste(fuente,"Muestra del modelo: 1.047 personas; se conservan ingresos cero."))+
  tema+theme(legend.position="top")
ggsave("output/graficos/distribucion_ingreso.png",ingresos,width=12,height=5.2,dpi=180,device=ragg::agg_png,bg="white")
diag<-readRDS("input/diagnosticos.rds")$diagnosticos
diag$indice<-seq_len(nrow(diag))
cook<-ggplot(diag,aes(indice,cook))+geom_segment(aes(xend=indice,yend=0),color="#2D777B")+
  geom_hline(yintercept=4/nrow(d),linetype=2,color="#C6533B")+
  scale_y_continuous(trans="sqrt")+
  labs(title="Influencia: 66 casos superan el umbral 4/n",x="Observaci\u00f3n (orden de la muestra)",
    y="Distancia de Cook (escala ra\u00edz cuadrada)",caption=paste(fuente,"Umbral = 0,00382. Influencia no equivale a error de registro."))+tema
ggsave("output/graficos/cook.png",cook,width=12,height=5.2,dpi=180,device=ragg::agg_png,bg="white")
residuos<-ggplot(diag,aes(ajustado,residuo))+geom_point(alpha=.4,color="#2D777B")+
  geom_hline(yintercept=0,linetype=2)+
  labs(title="La dispersion residual aumenta con el ingreso ajustado",x="Ingreso ajustado (millones de pesos)",
  y="Residuo (millones de pesos)",caption=paste(fuente,"M3 ponderado. Se usa inferencia robusta agrupada por hogar."))+tema
ggsave("output/graficos/residuos.png",residuos,width=12,height=5.2,dpi=180,device=ragg::agg_png,bg="white")
qq<-ggplot(diag,aes(sample=residuo_estandar))+stat_qq(color="#2D777B",alpha=.45)+stat_qq_line(color="#C6533B")+
  labs(title="Los residuos no siguen una distribucion normal",x="Cuantiles teoricos normales",y="Residuos estandarizados WLS",
    caption=paste(fuente,"Diagnostico grafico; no se apoya la inferencia en normalidad exacta."))+tema
ggsave("output/graficos/qq.png",qq,width=10,height=5.8,dpi=180,device=ragg::agg_png,bg="white")
modelos<-read.csv("output/tablas/13_modelos.csv")
coeficientes<-ggplot(modelos,aes(coeficiente/1e6,reorder(modelo,-as.integer(sub("M","",modelo)))))+
  geom_vline(xintercept=0,color="#B9C0C5")+
  geom_segment(aes(x=ic_inferior/1e6,xend=ic_superior/1e6,yend=modelo),linewidth=1,color="#2D777B")+
  geom_point(size=3,color="#C6533B")+
  labs(title="La diferencia persiste al agregar controles",x="Brecha ajustada e IC 95 % (millones de pesos)",y=NULL,
    caption=paste(fuente,"M1-M4: misma muestra, n = 1.047. Errores robustos por hogar; IC individuales."))+tema
ggsave("output/graficos/coeficientes.png",coeficientes,width=11,height=5.3,dpi=180,device=ragg::agg_png,bg="white")
v<-subset(read.csv("output/tablas/25_varianza_y_theil.csv"),escenario=="Original")
v$componente<-factor(v$componente,levels=v$componente)
v_largo<-rbind(data.frame(componente=v$componente,metodo="Varianza",porcentaje=v$porcentaje_varianza),
               data.frame(componente=v$componente,metodo="Theil T",porcentaje=v$porcentaje_theil))
varianza<-ggplot(v_largo,aes(metodo,porcentaje,fill=componente))+geom_col(width=.5)+
  geom_text(aes(label=paste0(round(porcentaje,1)," %")),position=position_stack(vjust=.5),size=5,color="white")+
  scale_fill_manual(values=c("#7A8A96","#C6533B","#2D777B"))+
  labs(title="La mayor dispersion permanece dentro de puesto y t\u00edtulo",x=NULL,y="Participaci\u00f3n en la desigualdad (%)",fill=NULL,
    caption=paste(fuente,"Descomposicion anidada: primero puesto, luego t\u00edtulo. n = 1.047; PONDIIO."))+
  tema+theme(legend.position="bottom",legend.text=element_text(size=11))
ggsave("output/graficos/varianza_theil.png",varianza,width=12,height=6,dpi=180,device=ragg::agg_png,bg="white")
# Diccionario: solo variables efectivamente usadas, nombres originales y reglas de disponibilidad.
variables<-c("ano4","trimestre","codusu","nro_hogar","componente","ch06","estado","cat_ocup","ch04",
 "ch10","ch12","ch13","nivel_ed","pp04d_cod","pp04b_cod","pp04a","region","pp3e_tot","pp07h","p21","pondera","pondiio")
descripciones<-c("A\u00f1o","Trimestre","Identificador vivienda","Numero hogar","Numero persona","Edad (a\u00f1os)","Condicion actividad",
 "Categoria ocupacional","Sexo registrado","Asistencia escolar","Maximo nivel cursado","Finalizacion del nivel","Nivel educativo agrupado",
 "Ocupacion CNO-2001","Rama CAES","Sector del establecimiento","Region estadistica","Horas semanales ocupaci\u00f3n principal",
 "Descuento jubilatorio","Ingreso mensual principal (pesos)","Expansion personas","Expansion ingreso ocupaci\u00f3n principal")
s<-readRDS("input/universos.rds")$asalariados
diccionario<-data.frame(variable=toupper(variables),descripcion=descripciones,
 tipo=c(rep("Categ\u00f3rica/identificador",5),"Num\u00e9rica",rep("Categ\u00f3rica",11),"Num\u00e9rica","Categ\u00f3rica","Num\u00e9rica", "Num\u00e9rica","Num\u00e9rica"),
 na_literal=vapply(s[variables],function(x) sum(is.na(x)),integer(1)),
 regla=c(rep("Disponible en universo",9),"Se usa para nunca asistio","99 desconocido","Clasificar junto con CH12",
 "9 desconocido","Ultimo digito 1 a 4","Excluir codigos no clasificables","Describir y sensibilidad M4","Sensibilidad M4",
 "999 no respuesta; 0 ausencia, separado","1 si; 2 no; solo asalariados","-9 no respuesta; 0 se conserva","Positivo","0 sin ingreso utilizable"))
guardar_tabla(diccionario,"26_diccionario")
