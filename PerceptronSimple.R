library(dplyr)

activacion <- function(d) {
  resultado <- sign(d)
  resultado[resultado==0] <- 1
  resultado
}

entrenarPerceptron <- function(datos, critFinalizacion, maxEpocas, nu=0.05, tolerancia=0.1, semilla = 111) {
  
  nroVariables <- dim(datos)[2] 
  cantidadDatos <- dim(datos)[1]
  #w <- rep(1, nroVariables-1) %>% as.matrix()
  #w0 <- 1
  set.seed(semilla)
  w <- runif(nroVariables-1,-1,1) %>% as.matrix()
  w0 <- runif(1,1,1)
  
  v_e <- rep(0,cantidadDatos)
  v_e2 <- array()
  
  for (epoca in seq(1:maxEpocas)) {
    for(i in seq(1:cantidadDatos)) {
      x <- datos[i, 1:nroVariables-1] %>%  as.matrix()
      y <- datos[i, nroVariables]
      
      d <- activacion(x %*% w + w0)
      
      e <- (y - d) %>% as.numeric()
      v_e[i] <- e * e
      if( abs(e) > tolerancia ) {
        w <- w + t(nu/2*(e * x))
        w0 <- w0 + nu/2*(e)*-1
      }
    }
    X <- datos[, 1:nroVariables-1] %>% as.matrix()
    y <- datos[, nroVariables]
    
    d <- activacion(X %*% w + w0)
    
    #tasa <- sum(y==d)/nrow(y)
    tasa <- sum(y==d)/cantidadDatos
    error <- mean(v_e)
    v_e2[epoca] <- error
    print(glue::glue("Epoca: {epoca} - Tasa: {tasa} - Error: {error}"))
    if(tasa > critFinalizacion ) {
      return (list("W"=c(w0, w), "error" = v_e2, "epoca" = epoca, "tasa" = tasa))
    }

  }
  return (list("W"=c(w0, w), "error" = v_e2, "epoca" = epoca, "tasa" = tasa))
  
}

library(ggplot2)
library(plotly)
graficarRectaSeparacion <- function(w, datos) {
  f <- function(x) {
    -w[3]/w[2]*x-w[1]
  }
  d <- datos %>% as.data.frame()
  colnames(d) <- c('x1', 'x2', 'y')
  p<- data.frame(x=c(-2, 2)) %>% 
    ggplot(aes(x)) + stat_function(fun=f) +
    geom_vline(xintercept = 0) +
    geom_hline(yintercept = 0)  +
    geom_point(data=d, aes(x=x1, y=x2, color=y))

  p <- ggplotly(p)
  p
  
}

aplicarPerceptron <- function(pesos, datos) {
  nroVariables <- dim(datos)[2] 
  X <- datos[, 1:nroVariables-1] %>% as.matrix()
  y <- datos[, nroVariables]
  d <- activacion(X %*% pesos[2:nroVariables] + pesos[1])
  tasa <- sum(y==d)/nrow(y)
  return(list(tasa = tasa, resultado = d))
}

sigmoidea <- function(v, b=1){
  return((2/ (1 + exp(-b*v)))-1)
}

sigmoidea_derivada <- function(y,b=1){
  return((b/2) * (1+y) * (1-y))
}

entrenarPerceptronSigmo <- function(datos, critFinalizacion, maxEpocas, nu=0.05, 
                                    tolerancia=0.1, imprimir = TRUE, semilla = 1, W = c(1,1)) {
  nroVariables <- dim(datos)[2] 
  cantidadDatos <- dim(datos)[1]
  
  if(W[1]==1 & W[2]==1){  # si no ingresa n valor lo define al azar.
    set.seed(semilla)
    w <- runif(nroVariables-1,-1,1) %>% as.matrix()
    w0 <- runif(1,-0.5,0.5)
  } else {
    w  <- W[2]
    w0 <- W[1]
  }
  
  v_e <- rep(0,cantidadDatos)
  v_e2 <- array()
  
  for (epoca in seq(1:maxEpocas)) {
    for(i in seq(1:cantidadDatos)) {
      x <- datos[i, 1:nroVariables-1] %>%  as.matrix()
      y <- datos[i, nroVariables]
      
      d <- sigmoidea(x %*% w + w0)
      
      e <- (d - y) %>% as.numeric()
      v_e[i] <- e * e
      if( abs(e) > tolerancia ) {
        w <- w + t(nu/2 * sigmoidea_derivada(d) * (e * x))
        w0 <- w0 + nu/2*(e)*-1
      }
    }
    X <- datos[, 1:nroVariables-1] %>% as.matrix()
    y <- datos[, nroVariables]
    
    d <- activacion(sigmoidea(X %*% w + w0))
    
    #tasa <- sum(y==d)/nrow(y)
    tasa <- sum(y==d)/cantidadDatos
    error <- mean(v_e)
    v_e2[epoca] <- error
    if (imprimir){
      print(glue::glue("Epoca: {epoca} - Tasa: {tasa} - Error: {error}"))
    }
    if(tasa > critFinalizacion ) {
      return (list("W"=c(w0, w), "error" = v_e2, "epoca" = epoca, "tasa" = tasa))
    }
    
  }
  return (list("W"=c(w0, w), "error" = v_e2, "epoca" = epoca, "tasa" = tasa))
  
}




