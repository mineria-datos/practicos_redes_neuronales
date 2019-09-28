library(dplyr)

activacion <- function(d) {
  resultado <- sign(d)
  resultado[resultado==0] <- 1
  resultado
}

entrenarPerceptron <- function(datos, critFinalizacion, maxEpocas, nu=0.05, tolerancia=0.1) {
  
  nroVariables <- dim(datos)[2] 
  cantidadDatos <- dim(datos)[1]
  w <- rep(1, nroVariables-1) %>% as.matrix()
  w0 <- 1
  
  for (e in seq(1:maxEpocas)) {
    for(i in seq(1:cantidadDatos)) {
      x <-datos[i, 1:nroVariables-1] %>%  as.matrix()
      y <- datos[i, nroVariables]
      
      d <- activacion(x %*% w + w0)
      
      e <- (y - d) %>% as.numeric()
      
      if( abs(e) > tolerancia ) {
        w <- w + t(nu/2*(e * x))
        w0 <- w0 + nu/2*(e)*-1
      }
    }
    X <- datos[, 1:nroVariables-1] %>% as.matrix()
    y <- datos[, nroVariables]
    
    d <- activacion(X %*% w + w0)
    
    tasa <- sum(y==d)/nrow(y)
    print(glue::glue(tasa))
    if(tasa > critFinalizacion ) {
      return (c(w, w0))
    }

  }
  return (c(w, w0))
  
}
