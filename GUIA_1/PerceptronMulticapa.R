

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

seleccionar_max <- function(row) {
  r <- rep(-1, length(row))
  r[which.max(row)] <- 1
  r
}

clasificacion <- function(d) {
  if(ncol(d) == 1)  {
    resultado <- sign(d)
    resultado[resultado==0] <- 1
    resultado
  } else {
    apply(d,1,seleccionar_max) %>% t()
  }
}

## Perceptron Multicapa

#datos <- concentlite
#datos <- XOR_trn
#critFinalizacion <- 0.8
#maxEpocas <- 300
#nu <- 0.05
#semilla <- 10
#arquitectura <- c(4,1) # [Capas, Neuronas por capa]

entrenarPerceptronM <- function(datos, critFinalizacion, maxEpocas, nu=0.05, semilla=10, 
                                arquitectura, alfa = 0, imprimir = FALSE) {
  
  # t1 <- timestamp()
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

  # Inicializando random. 
  w <- list()
  set.seed(semilla)
  w[[1]] <- matrix(runif((nroEntradas+1)*(arquitectura[1]),-0.5,0.5),
                   ncol = (nroEntradas+1), nrow = arquitectura[1]) %>% as.matrix()
  for (j in seq(1:(nroCapas-1))) {
    w[[j+1]]<- matrix(runif((arquitectura[j]+1)*arquitectura[j+1],-0.5,0.5),
                      ncol = arquitectura[j]+1, nrow = arquitectura[j+1]) %>% as.matrix()
  }
  #print(w)

  # Tenemos una matriz W por cada capa definida en la arquitectura.
  # Cada fila de W se define con el numero de neuronas
  # Cada columna de W se definie con el numero de entradas mas uno por el w0
  
  
  d <- list()
  dw <- list()
  dw_aux <- list()
  v_e <- rep(0,cantidadDatos)
  v_e2 <- array()
  
  calcularForward <- function(x) {
    y <- list()
    x <- t(x)
    x <- rbind(-1,x) %>% as.matrix()
    y[[1]] <- sigmoidea(w[[1]] %*% x)
    
    for (j in seq(2,nroCapas)) {
      x <- y[[j-1]]
      x <- rbind(-1, x)
      y[[j]] <- sigmoidea(w[[j]] %*% x)
    }
    y
  }
  
  for (e in seq(1:maxEpocas)) {
    #print("Epoca: ")
    #print(glue::glue(e))
    for(i in seq(1:cantidadDatos)) {
      
      # Forward
      #print("Forward")
      x <- datos[i, 1:nroEntradas]
      y <- calcularForward(x)
      
      # Backward
      #print("Backward")
      yd <- datos[i, seq(nroEntradas+1,nroSalidas+nroEntradas)]
      error <- t(yd) - y[[nroCapas]]
      


      #En capas intermedias y salida
      for (k in seq(nroCapas,2)) {
        if (k==nroCapas) {
          l_k = error
        } else {
          l_k = t(w[[k+1]]) %*% d[[k+1]]
        }
        d[[k]]  <- l_k[-1] * sigmoidea_derivada(y[[k]])
        dw[[k]] <- nu * d[[k]] %*% t(rbind(-1,y[[k-1]]))
      }

      #En la capa de entrada
      d[[1]]  <- (t(w[[2]]) %*% d[[2]])[-1] * sigmoidea_derivada(y[[1]]) %>% as.matrix()
      x <- datos[i, 1:nroEntradas]
      x <- cbind(-1,x) %>% as.matrix()
      if((dim(d[[1]])[1] == 1) && (dim(d[[1]])[2] == 1)){
        # caso en que dw es un numero porque tenemos una sola salida.
        dw[[1]] <- t(nu * as.numeric(d[[1]]) * x)
      } else {
        dw[[1]] <- nu * d[[1]] %*% x
      }
    
      # Corrección de pesos
      #print("Correccion de Pesos")
      if (e > 1) {
        for (k in seq(1,nroCapas)) {
          w[[k]]  <-  w[[k]] + dw[[k]] + alfa * dw_aux[[k]]
        }
      } else {
        for (k in seq(1,nroCapas)) {
          w[[k]]  <-  w[[k]] + dw[[k]]
        }
      }
      dw_aux <- dw # Guardo el delta anterior para el termino de momento
      
    
    }

    # Calculo la salida para todos los datos
    # print("Calculo de salida.")
    salida <- matrix(0, nrow = cantidadDatos, ncol = nroSalidas)
    yd <- datos[, seq(nroEntradas+1,nroSalidas+nroEntradas)] %>% as.matrix()
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
      salida[i, ] <- y[[j+1]]
      # Verctor de error para luego promediar
      error = as.numeric(yd[i,] - salida[i])
      v_e[i] <- error %*% error %>%  as.numeric()
    }
    salida = clasificacion(salida)
    v_e2[e] = mean(v_e)
    tasa <- sum(rowSums(salida==yd)==nroSalidas)/nrow(yd)
    # print(glue::glue("Tasa: {tasa}"))
    #print(glue::glue("Error: {v_e2[e]}"))
    #print("W:")
    #print(v_e2[e])
    if(tasa > critFinalizacion) break
    
  }

  vectorError <- v_e2
  resultado <- list("tasa" = tasa, "w" = w, "error" = vectorError, "resultado" = salida, "epocas" = e, "predecir" = function(entrada) {
    y = calcularForward(entrada[,1:nroEntradas])
    y_salida <- y[[length(arquitectura)]]
    resultado <- t(y_salida)
    resultado
  })
  # t2 <- timestamp()
  return(resultado)
}
