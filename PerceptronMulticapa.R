

# Capas de entrada
#
# Vector de tantas componenete como capas, y la cantidad de neuronas por capa en cada valor.
#
# C = [3, 1]

## Funciones

library(dplyr)
library(ggplot2)
library(plotly)
library(e1071)

sigmoidea <- function(v, b=1){
  return((2/ (1 + exp(-b*v)))-1)
}


sigmoidea_derivada <- function(y,b=1){
  return((b/2) * (1+y) * (1-y))
}

clasificacion <- function(d) {
  resultado <- sign(d)
  resultado[resultado==0] <- 1
  resultado
}

## Perceptron Multicapa

#datos <- concentlite
#datos <- XOR_trn
#critFinalizacion <- 0.8
#maxEpocas <- 30
#nu <- 0.05
#semilla <- 10
#arquitectura <- c(2,1) # [Capas, Neuronas por capa]

entrenarPerceptronM <- function(datos, critFinalizacion, maxEpocas, 
                                nu=0.05, semilla=10, arquitectura) {
  
  nroSalidas <- arquitectura[length(arquitectura)]
  nroEntradas <- dim(datos)[2] - nroSalidas
  cantidadDatos <- dim(datos)[1]
  nroCapas <- length(arquitectura)

  # Inicializando con 1
  #w <- list()
  #w[[1]] <- matrix(rep(1, (nroEntradas+1)*(arquitectura[1])),
  #                 ncol = (nroEntradas+1), nrow = arquitectura[1]) %>% as.matrix()
  #for (j in seq(1:(length(arquitectura)-1))) {
  #  w[[j+1]]<- matrix(rep(1, (arquitectura[j]+1)*arquitectura[j+1]),
  #                    ncol = arquitectura[j]+1, nrow = arquitectura[j+1]) %>% as.matrix()
  #}

  # Inicializando random
  w <- list()
  set.seed(semilla)
  w[[1]] <- matrix(runif((nroEntradas+1)*(arquitectura[1]),-0.5,0.5),
                   ncol = (nroEntradas+1), nrow = arquitectura[1]) %>% as.matrix()
  for (j in seq(1:(length(arquitectura)-1))) {
    w[[j+1]]<- matrix(runif((arquitectura[j]+1)*arquitectura[j+1],-0.5,0.5),
                      ncol = arquitectura[j]+1, nrow = arquitectura[j+1]) %>% as.matrix()
  }
  #print(w)

  # Tenemos una matriz W por cada capa definida en la arquitectura.
  # Cada fila de W se define con el numero de neuronas
  # Cada columna de W se definie con el numero de entradas mas uno por el w0
  
  
  y <- list()
  d <- list()
  dw <- list()
  v_e <- as.data.frame(cbind(rep(0,cantidadDatos*maxEpocas),rep(0,cantidadDatos*maxEpocas)))
  colnames(v_e) <- c("eje_x","v_e")
  for (e in seq(1:maxEpocas)) {
    print("Epoca: ")
    print(glue::glue(e))
    for(i in seq(1:cantidadDatos)) {
      
      # Forward
      #print("Forward")
      x <- datos[i, 1:nroEntradas] %>%  as.matrix()
      x <- cbind(-1,x) %>% as.matrix()
      y[[1]] <- sigmoidea(w[[1]] %*% t(x)) %>% as.matrix()
      
      for (j in seq(1:(length(arquitectura)-1))) {
        x <- y[[j]]
        x <- rbind(-1,x) %>% as.matrix()
        y[[j+1]] <- sigmoidea(w[[j+1]] %*% x) %>% as.matrix()
      }
      
      # Backward
      #print("Backward")
      yd <- datos[i, seq(nroEntradas+1,nroSalidas+nroEntradas)]
      error <- (yd - y[[nroCapas]]) %>% as.numeric()
      
      # Vercor de error para luego graficar
      eje_x <- i * e
      v_e[eje_x,1] <- eje_x
      v_e[eje_x,2] <- error*error
      
      #En capa de salida
      d[[nroCapas]]  <- error * sigmoidea_derivada(y[[nroCapas]]) %>% as.matrix()
      if((dim(d[[nroCapas]])[1] == 1) && (dim(d[[nroCapas]])[2] == 1)){
        # caso en que dw es un numero porque tenemos una sola salida.
        dw[[nroCapas]] <- t(nu * as.numeric(d[[nroCapas]]) * (rbind(-1,y[[nroCapas-1]])
                                                              %>% as.matrix()))
      }
      else{
        dw[[nroCapas]] <- nu * d[[nroCapas]] %*% t(rbind(-1,y[[nroCapas-1]])
                                                   %>% as.matrix())
      }
      
      #En capas intermedias
      if(nroCapas > 2) {
        for (k in seq(nroCapas-1,2)) {
          d[[k]]  <- (t(w[[k+1]]) %*% d[[k+1]])[-1] * sigmoidea_derivada(y[[k]]) %>% as.matrix()
          if((dim(d[[k]])[1] == 1) && (dim(d[[k]])[2] == 1)){
            # caso en que dw es un numero porque tenemos una sola salida.
            dw[[k]] <- t(nu * as.numeric(d[[k]]) * (rbind(-1,y[[k-1]]) %>% as.matrix()))
          }
          else{
            dw[[k]] <- nu * d[[k]] %*% t(rbind(-1,y[[k-1]]) %>% as.matrix())
          }
        }
      }
    
      #En la capa de entrada
      d[[1]]  <- (t(w[[2]]) %*% d[[2]])[-1] * sigmoidea_derivada(y[[1]]) %>% as.matrix()
      x <- datos[i, 1:nroEntradas] %>%  as.matrix()
      x <- cbind(-1,x) %>% as.matrix()
      if((dim(d[[1]])[1] == 1) && (dim(d[[1]])[2] == 1)){
        # caso en que dw es un numero porque tenemos una sola salida.
        dw[[1]] <- t(nu * as.numeric(d[[1]]) * x)
      } else {
        dw[[1]] <- nu * d[[1]] %*% x
      }
    
      # Corrección de pesos
      #print("Correccion de Pesos")
      for (k in seq(1,nroCapas)) {
        w[[k]]  <-  w[[k]] + dw[[k]]
      }
    
    }

    # Calculo la salida para todos los datos
    print("Calculo de salida. Tasa:")
    salida <- rep(0,cantidadDatos)
    for(i in seq(1:cantidadDatos)) {
      # Calculo la salida
      x <- datos[i, 1:nroEntradas] %>%  as.matrix()
      x <- cbind(-1,x) %>% as.matrix()
      y[[1]] <- sigmoidea(w[[1]] %*% t(x)) %>% as.matrix()
      for (j in seq(1:(length(arquitectura)-1))) {
        x <- y[[j]]
        x <- rbind(-1,x) %>% as.matrix()
        y[[j+1]] <- sigmoidea(w[[j+1]] %*% x) %>% as.matrix()
      }
      salida[i] <- y[[j+1]]
    }
    salida = clasificacion(salida)
    yd <- datos[, seq(nroEntradas+1,nroSalidas+nroEntradas)]
    
    tasa <- sum(salida==yd)/nrow(yd)
    print(glue::glue(tasa))
    print("W:")
    print(w)
    if(tasa > critFinalizacion) break
    
  }

  vectorError <- v_e[which(v_e[,1]!=0),]
  resultado <- list("tasa" = tasa, "w" = w, "error" = vectorError)
  return(resultado)
}