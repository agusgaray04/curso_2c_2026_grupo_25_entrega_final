guardar_tabla <- function(tabla, nombre) {
  write.csv(tabla, file.path("output", "tablas", paste0(nombre, ".csv")),
            row.names = FALSE, fileEncoding = "UTF-8")
}
