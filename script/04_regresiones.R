# Salida de texto UTF-8 en Windows; no modifica los datos.
if (.Platform$OS.type == "windows") invisible(Sys.setlocale("LC_CTYPE", ".UTF-8"))
source("utils/guardar_tabla.R")
source("utils/vcov_hogar.R")
source("utils/informar_coeficiente.R")
d<-readRDS("input/muestras.rds")$modelo
formulas<-list(ingreso~titulo,ingreso~titulo+puesto,
  ingreso~titulo+puesto+horas+edad_centrada+I(edad_centrada^2)+sexo,
  ingreso~titulo+puesto+horas+edad_centrada+I(edad_centrada^2)+sexo+region+sector)
modelos<-lapply(formulas,function(f) lm(f,data=d,weights=peso))
tabla<-do.call(rbind,lapply(seq_along(modelos),function(j)
  informar_coeficiente(modelos[[j]],d$hogar,paste0("M",j))))
guardar_tabla(tabla,"13_modelos")
stopifnot(abs(tabla$coeficiente[3]-470533.3161)<.01)
interaccion<-lm(ingreso~titulo*puesto+horas+edad_centrada+I(edad_centrada^2)+sexo,data=d,weights=peso)
v<-vcov_hogar(interaccion,d$hogar); b<-coef(interaccion)
indices<-grep("titulo:puesto",names(b)); wald<-drop(t(b[indices])%*%solve(v[indices,indices],b[indices]))
guardar_tabla(data.frame(prueba="Igualdad de la brecha ajustada entre puestos",wald=wald,
  gl=length(indices),p=pchisq(wald,length(indices),lower.tail=FALSE)),"14_wald_interaccion")
por_puesto<-do.call(rbind,lapply(levels(d$puesto),function(puesto) {
  contraste<-setNames(rep(0,length(b)),names(b));contraste["titulo"]<-1
  nombre<-paste0("titulo:puesto",puesto);if(nombre %in% names(b)) contraste[nombre]<-1
  beta<-sum(contraste*b);ee<-sqrt(drop(t(contraste)%*%v%*%contraste));gl<-length(unique(d$hogar))-1
  data.frame(puesto=puesto,coeficiente=beta*1e6,error_estandar=ee*1e6,
    ic_inferior=(beta-qt(.975,gl)*ee)*1e6,ic_superior=(beta+qt(.975,gl)*ee)*1e6,
    p=2*pt(abs(beta/ee),gl,lower.tail=FALSE))
}))
por_puesto$p_holm<-p.adjust(por_puesto$p,"holm")
guardar_tabla(por_puesto,"15_brechas_ajustadas_por_puesto")
terciario<-lm(ingreso~educacion+puesto+horas+edad_centrada+I(edad_centrada^2)+sexo,data=d,weights=peso)
guardar_tabla(informar_coeficiente(terciario,d$hogar,"Universitario frente a terciario", "educacionUniversitario completo"),"16_contraste_terciario")
saveRDS(list(modelos=modelos,interaccion=interaccion,terciario=terciario),"input/modelos.rds")
