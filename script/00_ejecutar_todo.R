# Salida de texto UTF-8 en Windows; no modifica los datos.
if (.Platform$OS.type == "windows") invisible(Sys.setlocale("LC_CTYPE", ".UTF-8"))
# Abrir Grupo25.Rproj primero, o ejecutar Rscript script/00_ejecutar_todo.R desde la raiz.
# Cada etapa se ejecuta en un proceso NUEVO: no puede heredar el entorno de la anterior.
necesarios<-c("ggplot2","ragg")
faltan<-necesarios[!vapply(necesarios,requireNamespace,logical(1),quietly=TRUE)]
if(length(faltan)) stop("Instalar antes de ejecutar: ",paste(faltan,collapse=", "))
if(!file.exists("Grupo25.Rproj")) stop("Abrir Grupo25.Rproj y ejecutar desde la ra\u00edz del proyecto.")
scripts<-sort(list.files("script",pattern="^[0-9]{2}_.*[.]R$",full.names=TRUE))
scripts<-scripts[!grepl("/00_",scripts)]
rscript<-file.path(R.home("bin"),"Rscript")
if(.Platform$OS.type=="windows") rscript<-paste0(rscript,".exe")
for(archivo in scripts) {
  cat("\nEjecutando ",archivo,"\n")
  estado<-system2(rscript,c("--vanilla",shQuote(archivo)))
  if(estado!=0) stop("Fallo en ",archivo)
}
capture.output(sessionInfo(),file="output/sesion_R.txt")
cat("\nEntrega final: etapas completadas sin depender de objetos previos.\n")
