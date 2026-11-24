# Título universitario e ingresos en ocupaciones comparables

Entrega final de Ciencia de Datos para Economía y Negocios, FCE-UBA. Grupo 25.

## Integrantes
- Agustín Garay, registro 911361.
- Bautista Lezcano, registro 917033.
- Ramiro Leverone, registro 909328.

## Objetivo e hipótesis
¿La brecha de ingresos entre trabajadores con y sin título universitario persiste dentro de ocupación y rama? Esperamos una asociación positiva, con heterogeneidad. 

Instancia 1 comparó niveles educativos amplios, inserción, registro e ingresos. Después distinguimos universitario de terciario y profundizamos ocupación × rama. Por devolución docente, el modelo amplio en log es principal; los cinco puestos de Instancias 2–3 se conservan como detalle. AUH y monotributo se retiraron por falta de conexión con el análisis.

## Conclusiones principales
- Dentro de la misma ocupación y rama, el ingreso mensual asociado al título universitario es alrededor de 28,65 % mayor (IC95 19,08–38,98 %), al considerar horas, edad y sexo. Hay evidencia de asociación positiva bajo el modelo.
- La asociación se mantiene en los chequeos: con región y sector (+28,19 %), acotando extremos al P99 (+28,40 %), sin el caso más influyente sobre el coeficiente del título (+27,18 %) y solo con puestos que tienen ambos grupos (+28,55 %).
- En los cinco puestos de detalle, las brechas observadas van de +19 % (educación) a +81 % (administración pública). En los contrastes de diferencias ajustadas por puesto, solo administración pública conserva significancia al 5 % tras Holm.
- La proporción de varianza entre puestos depende del soporte: 7,7 % con cinco puestos y 37,2 % con 170. Es una descomposición descriptiva, no una atribución causal.
- Es una asociación condicional, no un efecto causal. No se controla experiencia laboral directa ni detalle del empleador; el análisis se limita a un trimestre. La antigüedad disponible en la base no se incluyó en el principal.

## Base y variables
EPH individual, INDEC, primer trimestre de 2026. Publicación trimestral, unidad persona. 43.739 registros y 234 variables originales. Se analizan asalariados de 18–65 años de la cobertura urbana de la encuesta. No se extrapola automáticamente a población rural o independientes.

[Fuente oficial de microdatos](https://www.indec.gob.ar/indec/web/Institucional-Indec-BasesDeDatos) · [Registro EPH 1T 2026](https://www.indec.gob.ar/ftp/cuadros/menusuperior/eph/EPH_registro_1T2026.pdf).

La base original está en raw/. No se combina con otra base empírica; auxiliar/puestos.csv solo aporta etiquetas, sin pérdida de filas por cruce. Las referencias de resultados son para comprobación, no alimentan el análisis.

CH12/CH13/CH10 construyen título universitario; NIVEL_ED sirve al antecedente. Puesto=PP04D_COD × PP04B_COD. P21 es ingreso mensual principal; PP3E_TOT, horas de una semana. CH06/edad² y CH04 son controles; región y PP04A se agregan en sensibilidad. P47T chequea plausibilidad. Edad no es experiencia observada; CH04 es sexo registrado. Diccionario y faltantes: output/tablas/47_variables_y_faltantes.csv (26 conserva el diccionario inicial).

PONDERA expande personas para tasas del universo; PONDIIO es específico del ingreso principal, para modelos, medias y descomposiciones. Las descriptivas de la muestra principal usan PONDIIO también para su composición y lo explicitan. No son intercambiables.

## Muestras y selección
43.739 → 27.789 adultos → 18.800 ocupados → 13.632 asalariados → 11.302 con título, ingreso y ocupación válidos → 11.260 con rama válida → 10.156 con controles y horas válidos → 9.968 con ingreso positivo. Los 188 ceros permanecen en pesos y salen solo del log.

Principal: 7.351 hogares, 1.926 puestos. La variación educativa directa viene de 223 puestos con ambos grupos, 5.540 personas; otros pueden aportar al ajuste de controles comunes. El detalle tiene 1.447 antes de horas, 1.047 después y 1.036 positivos. El mínimo veinte por grupo se aplica antes del filtro de horas; Un tamaño efectivo de Kish menor que diez señala fragilidad, no exclusión automática.

La descomposición ampliada usa 7.823 casos y 170 puestos con mínimo diez casos totales, sin filtro de horas. No es la muestra principal en log. El escenario con horas válidas se presenta separado.

## Métodos y resultados
1. **Regresión WLS**, log(P21), efectos fijos de puesto absorbidos, horas, edad centrada, edad² y sexo. PONDIIO, CR1 por hogar, contraste t con tantos grados de libertad como hogares menos uno. Principal: β=0,251898, EE=0,039416, t=6,3908, gl=7.350, p≈1,75e-10. Comparación multiplicativa condicional: **+28,65 %**, IC95 **19,08–38,98 %**. No es brecha de medias aritméticas ni efecto causal. R² global ponderado≈0,702 incluye muchos efectos fijos. Región/sector: +28,19 %. En pesos, muestra amplia: +$399.533.
2. **Descomposición anidada de varianza**, entre puestos / entre título dentro de puesto / residual. Cinco puestos: 7,66/14,43/77,91 %. En 170: 37,17/6,67/56,15 %. Depende del soporte, no son contribuciones causales.
3. **Theil T**, invariante a escala y descomponible; ceros aportan cero por continuidad. T≈0,157 en cinco puestos y≈0,247 en 170. También es sensible a extremos y no tiene máximo universal uno.
4. **Estandarización descriptiva adicional**, en el detalle: 54,18 % a 51,87 %, reducción de 2,31 puntos sin prueba de significancia del cambio.

El detalle conserva +$470.533, IC95 $231.831–$709.236. Los dos gráficos de Instancia 3 mantienen esa evidencia; Wald/Holm pertenecen al detalle, no a 1.926 contrastes del amplio.

## Limpieza, sensibilidad y límites
No se imputan ingresos. Los 2.322 no utilizables incluyen códigos/ponderadores inválidos, no solo NA. Se describe concentración y selección antes/después. PONDIIO no garantiza ausencia de sesgo. Horas cero puede ser ausencia temporal; referencias mensual y semanal difieren.

En el principal, log-IQR señala 393 bajos y 58 altos, conservados como plausibles sin certificar exactitud. P99 ponderado=$4.800.000 acota 58 casos: +28,40 %. Sin el caso con mayor influencia sobre título: +27,18 %. Solo puestos con ambos grupos: +28,55 %. No son tests de equivalencia.

Cook>4/n marca 456 entre h<1; 1.166 con h=1 no admiten borrado de rango fijo y no se eliminan. No confundir con los 66 del detalle. El borrado del caso más influyente sobre título se verifica por reestimación. Se comprueban absorción y CR1 contra indicadores en cinco puestos.

El log reduce asimetría residual de 3,28 a −0,60 sobre los mismos casos, sin demostrar normalidad u homocedasticidad. CR1 no resuelve selección, omisiones ni diseño EPH completo. No se controla experiencia laboral total ni detalle del empleador. PP07A ofrece antigüedad en el empleo por categorías, pero no se incluyó en el principal. Evaluar ese control, otros trimestres y el diseño de encuesta son próximos pasos.

## Estructura
```text
raw/                 base cruda
auxiliar/            etiquetas, documentos y referencias de comprobacion
input/               universos, muestras, modelos y diagnosticos RDS
output/tablas/       CSV recalculados
output/graficos/     figuras generadas en R con ggsave
script/              etapas 00 a 10
utils/               una funcion propia por archivo
presentacion/        PDF y PPTX finales
README.md
Grupo25.Rproj
MAPA_DIAPOSITIVAS.csv
verificar_reproduccion.R
```

## Reproducción
Probado con R 4.4.3; versiones en output/sesion_R.txt. Instalar ggplot2 y ragg antes de ejecutar:
```r
install.packages(c("ggplot2", "ragg"))
```
Abrir Grupo25.Rproj desde la raíz y ejecutar:
```r
source("script/00_ejecutar_todo.R")
```
O `Rscript --vanilla script/00_ejecutar_todo.R` desde la raíz. Cada etapa arranca en un proceso limpio; lee archivos, no objetos de otra sesión. No instala paquetes, usa rutas personales ni abre ventanas. No requiere Python o Node para análisis y gráficos. La presentación se entrega ya diagramada.

| Etapa | Lee | Produce |
|---|---|---|
| 01 preparar_datos | raw, auxiliar | universos/muestras RDS, flujo |
| 02 descriptivas_y_faltantes | RDS | descriptivos, faltantes y antecedentes |
| 03 estandarizacion | muestras | composición común, medias, Kish |
| 04 regresiones | muestras | modelos del detalle, Wald/Holm, terciario |
| 05 outliers_y_sensibilidades | muestras/modelos | diagnóstico y sensibilidad del detalle |
| 06 varianza_y_theil | muestras | descomposición de cinco puestos |
| 07 graficos_instancia3 | muestras/etiquetas | dos JPG finales |
| 08 graficos_resultados | muestras/tablas | figuras y diccionario |
| 09 ampliar_soporte | muestras | principal amplio y soporte ampliado |
| 10 diagnosticos_modelo_principal | RDS/tablas | descriptivos, influencia, sensibilidad, figuras finales |

`source("verificar_reproduccion.R")` recalcula todo y compara las 47 tablas con tolerancia numérica 1e-7; guarda output/verificacion_reproduccion.csv. Se probó en otra carpeta sin input/output previos. Reproducción no equivale a auditoría estadística externa o prueba causal. Los gráficos pueden variar ligeramente por fuentes del sistema, aunque las tablas coincidan.

## Presentación y entrega
18 diapositivas de contenido, portada, cierre y diecinueve anexos. MAPA_DIAPOSITIVAS.csv conecta cada diapositiva con scripts y resultados. 

Se incorporaron las devoluciones docentes de las instancias 1, 2 y 3; los tres PDF originales se conservan en auxiliar/.

La presentación final está en presentacion/Grupo25_Presentacion_Final_24nov.pdf. La versión editable también se incluye. El análisis y los gráficos se regeneran con R; la diagramación de la presentación se entrega terminada.


## Verificaciones y alcance
Las diez etapas se ejecutaron en procesos limpios, desde otra carpeta sin input/output previos. Las 47 tablas coinciden con las referencias dentro de tolerancia 1e-7. Las dos visualizaciones y sus versiones para diapositivas se generan una vez en la etapa 07; la etapa 10 produce los diagnósticos y figuras complementarias. La reproducción no constituye auditoría estadística externa ni prueba causal.

La heterogeneidad contrastada con Wald/Holm corresponde exclusivamente al detalle de cinco puestos. Los 1.166 casos con palanca igual a uno del principal coinciden con puestos unipersonales y no aportan variación interna al coeficiente de título.

## Contenido de entrega
Se incluyen datos originales y auxiliares, procesados, scripts, funciones, tablas, gráficos, presentación y documentación de ejecución. 

