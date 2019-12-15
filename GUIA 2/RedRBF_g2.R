


## se asume que la última columna de datos es la clase o target

redRBF <- function(datos_x, datos_y, nroGausianas, calcularCovarianza=FALSE, funcion = "sigmo", 
                   pnu = 0.05, pepoca = 100, pcritFinalizacion = 0.8, ptolerencia = 0.2) {
  
  paramGausianas <- entrenamientoSemiAutomatico(datos_x, nroGausianas, calcularCovarianza)
  paramGausianas <- paramGausianas$centers  #solo se usan los centros y se toma varianza 1
  
  salidaGausianas <- evaluarGauciana(datos_x, paramGausianas)

  w <- list()
  perceptron <- list()
  for(ncat in seq(1:ncol(datos_y))) {

    datosEntrenamientoPerceptron <- cbind(salidaGausianas, datos_y[,ncat]) 
    if (funcion == "sigmo") {
      perceptron[[ncat]] <- entrenarPerceptronSigmo(datosEntrenamientoPerceptron, 
                                                    critFinalizacion = pcritFinalizacion, 
                                                     maxEpocas = pepoca, nu = pnu)
    } else {
      perceptron[[ncat]] <- entrenarPerceptronLineal(datosEntrenamientoPerceptron, 
                                                     critFinalizacion = pcritFinalizacion, 
                                                     maxEpocas = pepoca, nu = pnu, 
                                                     alfa = 0.6,  tolerancia = ptolerencia)
    }
    #print(perceptron[[ncat]])
    w[[ncat]] = perceptron[[ncat]]$W
  }
  
  return(list(paramGausianas = paramGausianas, w = w, funcion = funcion))
  
  # para cada clase de los datos
  # entrenar un perceptron simple
  # con entradas correspondientes a las salidas de las gausianas
}



entrenamientoSemiAutomatico <- function(datos, nroGausianas, calcularCovarianza=FALSE) {
  resultado  <- kmeans(datos, nroGausianas, nstart = nroGausianas)
  if(calcularCovarianza) {
    covarianza <- list()
    datos <- cbind(datos,resultado$cluster)
    for (i in seq(1,nrow(resultado$centers))) {
      covarianza[[i]] <- cov(datos[which(datos[,ncol(datos)]==i),-ncol(datos)])
    }
    return(list("centers" = resultado$centers, "covarianza" = covarianza))
  } else {
    return(list("centers" = resultado$centers))
  }
}

evaluarGauciana <- function(datos, centros) {
  salidaGausianas <- apply(centros, 1, function(mu) {
    dmvnorm(datos, mu)
    }
  )
  salidaGausianas
}


aplicarRedRBF <- function(modelo, datos_x, datos_y) {
  salidaGausianas <- evaluarGauciana(datos_x, modelo$paramGausianas)
  salida_y <- matrix(nrow = nrow(datos_y), ncol = 1)
  salida <- list()
  for(ncat in seq(1:ncol(datos_y))) {
    datosEntrenamientoPerceptron <- cbind(salidaGausianas, datos_y[,ncat]) 
    if(modelo$funcion == "sigmo") {
      salida[[ncat]] <- aplicarPerceptronSigmo(modelo$w[[ncat]], datosEntrenamientoPerceptron)
    } else {
      salida[[ncat]] <- aplicarPerceptronLineal(modelo$w[[ncat]], datosEntrenamientoPerceptron)
    }
    salida_y <- cbind(salida_y,salida[[ncat]])
    #print(salida[[ncat]])
  }
  salida_y <- salida_y[,-1]
  
  if(modelo$funcion == "sigmo") {
    if(ncol(datos_y)>1) {
      for (i in seq(1:nrow(datos_y))) {
        salida_y[i,which(salida_y[i,]==max(salida_y[i,]))] <- 1
        salida_y[i,which(salida_y[i,]!=1)] <- -1
      }
    } else {
      salida_y[which(salida_y >= 0)] <- 1
      salida_y[which(salida_y <  0)] <- -1
    }
    tasa <- sum(rowSums(salida_y==datos_y)==ncol(datos_y))/nrow(datos_y)
  } else { tasa = 0 }
  
  e <- (datos_y - salida_y) %>% as.matrix()
  if(ncol(datos_y)>1) {
    error <- e %*% t(e)
  } else { error <- e * e }
  error

  return(list(salida = salida_y, error = error, tasa = tasa))
}
