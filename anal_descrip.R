
## ANALISIS DESCRIPTIVO - Articulo_regresion

if (!requireNamespace("readxl", quietly = TRUE)) {
  install.packages("readxl")
}

library(readxl)

## Excel de GitHub
url <- "https://raw.githubusercontent.com/taticorrea05/Articulo_regresion/main/datos.xlsx"

archivo <- tempfile(fileext = ".xlsx")
download.file(url, archivo, mode = "wb")


datos <- as.data.frame(read_excel(archivo, sheet = 1))


print(datos)
View(datos)


str(datos)
dim(datos)
names(datos)

## Summary
summary(datos)

## Datos faltantes
colSums(is.na(datos))

numericas <- datos[, sapply(datos, is.numeric),
                   drop = FALSE]


numericas <- numericas[, !tolower(names(numericas)) %in%
                         c("sujeto", "participante", "numero_sujeto",
                           "subject_number"), drop = FALSE]

## Estadisticas descriptivas
resumen <- data.frame(
  variable = names(numericas),
  media = sapply(numericas, mean, na.rm = TRUE),
  mediana = sapply(numericas, median, na.rm = TRUE),
  desviacion = sapply(numericas, sd, na.rm = TRUE),
  varianza = sapply(numericas, var, na.rm = TRUE),
  minimo = sapply(numericas, min, na.rm = TRUE),
  maximo = sapply(numericas, max, na.rm = TRUE),
  row.names = NULL
)

print(resumen)

## Frecuencias de variables categoricas
categoricas <- datos[, !sapply(datos, is.numeric),
                     drop = FALSE]

for (nombre in names(categoricas)) {
  cat("\nVariable:", nombre, "\n")
  print(table(categoricas[[nombre]], useNA = "ifany"))
}

## Histogramas
for (nombre in names(numericas)) {
  hist(numericas[[nombre]],
       main = paste("Histograma de", nombre),
       xlab = nombre,
       col = "lightblue")
}

## Diagramas de caja
for (nombre in names(numericas)) {
  boxplot(numericas[[nombre]],
          main = paste("Diagrama de caja de", nombre),
          ylab = nombre,
          col = "lightgreen")
}


## DIAGRAMAS DE DISPERSION

## Edad vs VO2 maximo
plot(datos$Edad_anios, datos$VO2max_ml_kg_min,
     main = "Edad vs VO2 maximo",
     xlab = "Edad (anios)",
     ylab = "VO2 maximo (ml/kg/min)",
     pch = 19,
     col = "purple")

## Peso vs VO2 maximo
plot(datos$Peso_kg, datos$VO2max_ml_kg_min,
     main = "Peso vs VO2 maximo",
     xlab = "Peso (kg)",
     ylab = "VO2 maximo (ml/kg/min)",
     pch = 19,
     col = "pink")

## Estatura vs VO2 maximo
plot(datos$Estatura_cm, datos$VO2max_ml_kg_min,
     main = "Estatura vs VO2 maximo",
     xlab = "Estatura (cm)",
     ylab = "VO2 maximo (ml/kg/min)",
     pch = 19,
     col = "blue")


