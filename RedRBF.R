


## se asume que la última columna de datos es la clase o target

redRBF <- function(datos, nroGausianas, calcularCovarianza=FALSE) {
  
  paramGausianas <- entrenamientoSemiAutomatico(datos, nroGausianas, calcularCovarianza)
  
  salidaGausianas <- evaluarGauciana(datos, paramGausianas)

  
  columnaTarget <- colnames(datos)[ncol(datos)]
  target = datos %>% select(!!as.name(columnaTarget))

  categoriasSalidas <- datos %>%
    distinct(!!as.name(columnaTarget))
  

  for(ncat in seq(1:nrow(categoriasSalidas))) {
    categoria = categoriasSalidas[ncat,1] %>% unlist()
    
    datosEntrenamientoPerceptron <- cbind(salidaGausianas, ifelse(target[,1]==categoria, 1, -1)) 
    perceptron <- entrenarPerceptron(datosEntrenamientoPerceptron, 0.8, maxEpocas = 100, nu = 0.001)
    print(perceptron)
  }
  
  
  
  # para cada clase de los datos
  # entrenar un perceptron simple
  # con entradas correspondientes a las salidas de las gausianas
}



entrenamientoSemiAutomatico <- function(datos, nroGausianas, calcularCovarianza=FALSE) {
  
  datosSinTarget = datos %>% select(-ncol(datos))
  resultado  <- kmeans(datosSinTarget, nroGausianas, nstart = nroGausianas)
  
  resultado$centers
}

evaluarGauciana <- function(datos, centros) {
  
  datosSinTarget = datos %>%
      select(-ncol(datos)) %>% 
      as.matrix()


  salidaGausianas <- apply(centros, 1, function(mu) {
    dmvnorm(datosSinTarget, mu)
    }
  )
  
  salidaGausianas
  
}
