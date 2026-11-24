# Salida de texto UTF-8 en Windows; no modifica los datos.
if (.Platform$OS.type == "windows") invisible(Sys.setlocale("LC_CTYPE", ".UTF-8"))
library(ggplot2)
library(ragg)
d<-readRDS("input/muestras.rds")$modelo
# Adaptacion de nombres a los graficos entregados en Instancia 3.
claves<-read.csv("auxiliar/puestos.csv",stringsAsFactors=FALSE)$puesto
d$puesto<-factor(d$puesto,levels=claves)
salida<-file.path("output","graficos")

# Estadisticas de los graficos. P21 esta expresado en millones de pesos.
etiquetas <- c("Administraci\u00f3n p\u00fablica\nAdministrativos operativos",
               "Salud\nT\u00e9cnicos", "Salud\nT\u00e9cnicos informatizados",
               "Educaci\u00f3n\nT\u00e9cnicos", "Educaci\u00f3n\nT\u00e9cnicos informatizados")
orden <- c(1,2,3,5,4)
d$fila <- 6-match(as.integer(d$puesto),orden)
rotulos <- rev(etiquetas[orden])
navy <- "#202D3C"; naranja <- "#BD5037"; fondo <- "#F7F5F0"
tema <- theme_minimal(base_size=12,base_family="Arial")+
  theme(plot.title.position="plot",plot.caption.position="plot",
        plot.title=element_text(size=19,face="bold",color=navy),
        plot.subtitle=element_text(size=11,lineheight=1.17,margin=margin(b=10)),
        plot.caption=element_text(size=9,hjust=0,lineheight=1.16,margin=margin(t=12)),
        axis.text.y=element_text(face="bold",color=navy,size=10),
        panel.grid.major.y=element_blank(),panel.grid.minor=element_blank(),
        plot.margin=margin(15,18,15,15))
positivos <- subset(d,ingreso>0)
stopifnot(nrow(positivos)==1036,sum(d$ingreso>4)==2)
positivos$edu <- ifelse(positivos$titulo==1,1L,ifelse(positivos$ch12==6 & positivos$ch13==1,2L,3L))
positivos$grupo <- factor(positivos$edu,1:3,c("Universitario completo","Terciario completo","Sin t\u00edtulo terciario ni universitario"))
# Ajustes descriptivos sobre log(P21), con PONDIIO.
positivos$panel <- factor(positivos$fila,levels=5:1,labels=rev(rotulos))
positivos$celda <- interaction(positivos$panel,positivos$grupo,drop=TRUE)
positivos$n_celda <- as.integer(table(positivos$celda)[as.character(positivos$celda)])
lineas <- subset(positivos,n_celda>=20)
conteos <- table(positivos$panel,positivos$grupo)
notas <- data.frame(panel=factor(rownames(conteos),levels=levels(positivos$panel)),
                    n=paste0("n: ",conteos[,1]," univ. / ",conteos[,2]," terc. / ",conteos[,3]," resto"))
paleta <- c("#205D65","#23847B","#93836F")
exploratorio <- ggplot(positivos,aes(horas,ingreso,color=grupo,shape=grupo))+
  geom_point(aes(size=peso),alpha=.46)+
  geom_smooth(data=lineas,aes(weight=peso),method="lm",formula=y~x,se=FALSE,linewidth=.8)+
  geom_text(data=notas,aes(x=103,y=.097,label=n),inherit.aes=FALSE,hjust=1,
            color=navy,size=3.1)+
  facet_wrap(~panel,ncol=3)+
  scale_color_manual(values=paleta)+
  scale_shape_manual(values=c(16,17,15))+
  scale_size_area(max_size=5.2,breaks=c(250,1000,4000),labels=c("250","1.000","4.000"))+
  scale_x_continuous(breaks=seq(0,100,20))+
  scale_y_log10(breaks=c(.125,.25,.5,1,2,4),
                labels=c("125 mil","250 mil","500 mil","1 mill\u00f3n","2 millones","4 millones"))+
  guides(color=guide_legend(order=1,override.aes=list(size=2.5,alpha=1)),
         shape=guide_legend(order=1),size=guide_legend(order=2))+
  labs(title="Ingreso y horas trabajadas en cinco puestos comparables",
       subtitle=paste("Asalariados de 18 a 65 a\u00f1os | EPH, 1.er trimestre de 2026 | 1.036 ingresos positivos",
                      "Rectas: ajuste lineal ponderado del log del ingreso seg\u00fan horas, en subgrupos graficados con n \u2265 20.",sep="\n"),
       x="Horas en la ocupaci\u00f3n principal durante la semana de referencia",
       y="Ingreso mensual principal (pesos, escala logar\u00edtmica)",
       color="Nivel educativo",shape="Nivel educativo",size="Ponderador PONDIIO",
       caption=paste("Fuente: elaboraci\u00f3n propia con EPH-INDEC. Puesto = ocupaci\u00f3n CNO \u00d7 rama. Los n son conteos sin ponderar.",
                     "Selecci\u00f3n de puestos: al menos 20 con y 20 sin t\u00edtulo universitario antes del filtro de horas.",
                     "Se excluyen 11 ingresos cero por la escala. Resto: sin t\u00edtulo terciario ni universitario.",
                     "Las horas son semanales y el ingreso es mensual. Las rectas son descriptivas.",sep="\n"))+
  tema+theme(legend.position="bottom",legend.box="vertical",
             legend.margin=margin(1,0,1,0),strip.text=element_text(face="bold",size=10.5),
             strip.background=element_rect(fill="#E7EEEB",color=NA),
             panel.grid.major.y=element_line(color="#E5E9E8",linewidth=.35),
             panel.background=element_rect(fill="#F7F5F0",color=NA),
             axis.text.y=element_text(face="plain",size=9.5),
             panel.spacing=grid::unit(.75,"lines"))

# Medias descriptivas calculadas antes de limitar la nube de puntos.
medias <- do.call(rbind,lapply(split(d,d$fila),function(z){
  m <- vapply(0:1,function(g) weighted.mean(z$ingreso[z$titulo==g],z$peso[z$titulo==g]),numeric(1))
  data.frame(fila=z$fila[1],sin=m[1],con=m[2],brecha=100*(m[2]/m[1]-1))
}))
stopifnot(identical(as.integer(round(medias$brecha)),c(19L,23L,38L,65L,81L)))
nube <- subset(d,ingreso<=4)
comunicativo <- ggplot()+
  geom_point(data=nube,aes(ingreso,fila,color=factor(titulo)),
             position=position_jitter(width=0,height=.21,seed=25),alpha=.18,size=1.45)+
  geom_segment(data=medias,aes(x=sin,xend=con,y=fila,yend=fila),color=navy,linewidth=1.15)+
  geom_point(data=medias,aes(sin,fila),shape=21,fill="white",color=navy,size=4.8,stroke=1.3)+
  geom_point(data=medias,aes(con,fila),shape=21,fill=naranja,color="white",size=5.2,stroke=.6)+
  geom_text(data=medias,aes(con+.16,fila,label=paste0("+",round(brecha)," %")),
            color=naranja,hjust=0,fontface="bold",size=4.6)+
  scale_color_manual(values=c(navy,naranja),guide="none")+
  scale_y_continuous(breaks=1:5,labels=rotulos,limits=c(.55,5.90))+
   scale_x_continuous(breaks=0:4,labels=c("$0","$1 mill\u00f3n","$2 millones","$3 millones","$4 millones"))+
  labs(title="En los cinco puestos, el ingreso medio es mayor\nentre quienes tienen t\u00edtulo universitario",
       subtitle=paste("Asalariados de 18 a 65 a\u00f1os | EPH, 1.er trimestre de 2026",
                      "C\u00edrculo vac\u00edo: sin t\u00edtulo universitario. C\u00edrculo naranja: con t\u00edtulo universitario.",
                      "Los porcentajes comparan los promedios observados dentro de cada puesto.",sep="\n"),
       x="Ingreso mensual de la ocupaci\u00f3n principal (pesos)",y=NULL,
       caption=paste("Fuente: elaboraci\u00f3n propia con EPH-INDEC.",
                     "Cada punto claro representa una persona. Se ocultan dos puntos superiores a $4 millones; las medias los incluyen.",
                     "Sin t\u00edtulo universitario incluye terciario completo. Porcentajes descriptivos; no son efectos causales.",sep="\n"))+
  tema

# Pieza comunicativa revisada: etiquetas directas, sin n ni tecnicismos de ponderacion.
comunicativo <- comunicativo +
 geom_text(data=subset(medias,fila==5),aes(x=sin,y=5.63,label="Sin t\u00edtulo\nuniversitario"),
  inherit.aes=FALSE,color=navy,fontface="bold",size=4.8,lineheight=.95)+
 geom_text(data=subset(medias,fila==5),aes(x=con,y=5.63,label="Con t\u00edtulo\nuniversitario"),
  inherit.aes=FALSE,color=naranja,fontface="bold",size=4.8,lineheight=.95)+
 labs(title="En cinco puestos comparables, quienes tienen\nt\u00edtulo universitario ganan m\u00e1s",
  subtitle="Ingreso mensual promedio. Asalariados de 18 a 65 a\u00f1os. EPH, primer trimestre de 2026.",
  caption=paste("Fuente: elaboraci\u00f3n propia con EPH-INDEC. Comparaci\u00f3n descriptiva, no causal.",
   "Se ocultan dos ingresos superiores a $4 millones solo en los puntos de fondo; los promedios los incluyen.",sep="\n"))


ggsave(file.path(salida,"grupo_25_exploratorio.jpg"),exploratorio,width=25,height=20,units="cm",dpi=300,device=ragg::agg_jpeg,bg="white")
ggsave(file.path(salida,"grupo_25_comunicativo.jpg"),comunicativo,width=30,height=18,units="cm",dpi=300,device=ragg::agg_jpeg,bg="white")
exploratorio_slide <- exploratorio+labs(title=NULL,subtitle=NULL,caption=NULL)+
 theme(axis.text=element_text(size=14),axis.title=element_text(size=15.5),
  strip.text=element_text(size=15),legend.text=element_text(size=13),
  legend.title=element_text(size=13),plot.margin=margin(3,3,3,3))
exploratorio_slide$layers[[3]]$aes_params$size <- 3.6
ggsave(file.path(salida,"exploratorio_diapositiva.png"),exploratorio_slide,width=14,height=6.3,dpi=200,device=ragg::agg_png,bg="white")
comunicativo_slide <- comunicativo+labs(title=NULL,subtitle=NULL,caption=NULL)+
 theme(axis.text=element_text(size=15),axis.text.y=element_text(size=14,face="bold"),
  axis.title=element_text(size=16),plot.margin=margin(3,3,3,3))
comunicativo_slide$layers[[5]]$aes_params$size <- 6.5
comunicativo_slide$layers[[6]]$aes_params$size <- 5.6
comunicativo_slide$layers[[7]]$aes_params$size <- 5.6
ggsave(file.path(salida,"comunicativo_diapositiva.png"),comunicativo_slide,width=14,height=4.8,dpi=200,device=ragg::agg_png,bg="white")
