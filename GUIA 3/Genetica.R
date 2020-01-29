##################################################################################
# Convierte a binario con un largo de bits pasado por parametro.
# el bit menos significativo queda en el indice 0 del vector
##################################################################################

int2bit <- function(x, cantidadDecimales=2) {
  purrr::map(x, function(.x) {
    intToBits(.x)
  })
  
}

# int2bit(12,cantidadBits)

##################################################################################
# Conviente el dato binario a decimal
##################################################################################

bit2int <- function(.x) {
  purrr::map(.x, function(x) {
    packBits(x, type="integer")
  })
}

##################################################################################
# Crear Pobracion
##################################################################################

crearPoblacion <- function(cantidadIndividuos, cantidadVariables, limiteInf = NULL,
                            limiteSup = NULL, verbose = TRUE) {
  # Matriz donde almacenar los individuos generados.
  # cada renglon es una posible solución
  poblacion <- matrix(data = NA, nrow = cantidadIndividuos, ncol = cantidadVariables)
  
  # Bucle para crear cada individuo.
  for (i in 1:cantidadIndividuos) {
    # Se crea un vector de NA que representa el individuo.
    individuo <- rep(NA, times = cantidadVariables)
    
    for (j in 1:cantidadVariables) {
      # Para cada posición, se genera un valor aleatorio dentro del rango permitido
      # para cada variable.
      individuo[j] <- runif(n = 1, min = limiteInf[j], max = limiteSup[j])
    }
    # Se añade el nuevo individuo a la población.
    poblacion[i, ] <- individuo
  }
  
  return(poblacion)
}

##################################################################################
# Evaluar Funcion
##################################################################################

aptitud <- function(datos, funcion) {
  valorSalida <- funcion(datos)
  return(valorSalida)
}

##################################################################################
# Seleccionar Individuos
##################################################################################

seleccion <- function(aptitud, metodo = "ranking") {
  if (metodo == "ranking") {
    
    probabilidad_seleccion <- 1 / rank(-aptitud)
    
    ind_seleccionado <- sample(
      x = 1:length(aptitud),
      size = 1,
      prob = probabilidad_seleccion
    )
  }
  return(ind_seleccionado)
}

##################################################################################
# Funcion para cruzar dos valores
##################################################################################

cruzar <- function(padre1,padre2,cantidadDecimales=2, metodo = "mitad") {
  #print(glue::glue("cruza  padre1: ", padre1, " padre2: ", padre2 ))
  cantidadBits <- 32
  multiplicador <- 10^cantidadDecimales
  nvar <- length(padre1)
  x <- int2bit(padre1*multiplicador) %>% unlist() %>% matrix(ncol = nvar) %>% t()
  y <- int2bit(padre2*multiplicador) %>% unlist() %>% matrix(ncol = nvar) %>% t()
  #herenciaPadre1 <- trunc(cantidadBits/2)
  
  herenciaPadre1 <- trunc(runif(1, 1, cantidadBits))
  nuevo <- matrix(c(x[,1:herenciaPadre1],y[,(herenciaPadre1+1):cantidadBits]), ncol=cantidadBits)
  nuevo <- split(nuevo, row(nuevo))
  nuevo <- bit2int(nuevo) %>% unlist()
  nuevo <- round(nuevo / multiplicador, cantidadDecimales)
  #print(glue::glue("nuevo : ", nuevo))
  return(nuevo)
}

##################################################################################
# Funcion para mutar
# Cambia el bit menos significativo si un valor random en menor a p
##################################################################################

mutar <- function(individuo, p = 0.5, cantidadDecimales=2, xMin, xMax) {
  nvar <- length(individuo)
  cantidadBits <- 32
  multiplicador <- 10^cantidadDecimales
  x <- int2bit(individuo*multiplicador) %>% unlist() %>% matrix(ncol = nvar) %>% t()
  probMutacion <- runif(n = cantidadBits, min = 0, max = 1)
  posicionesMutadas <- probMutacion < p
  mutar <- FALSE
  for (i in which(posicionesMutadas)) {
    mutar <- TRUE
    x[i] <- ifelse(x[i], as.raw(0), as.raw(1))
  }
  x <- split(x, row(x))
  x <- bit2int(x)
  x <- ifelse(x>xMax, xMax, x)
  x <- ifelse(x<xMin, xMin, x)
  x <- round(x/multiplicador, cantidadDecimales)
  if (mutar) {
    print(glue::glue("individuo original: ", toString(individuo)))
    print(glue::glue("nuevo             : ", unlist(x)))
  }
  return(x)
}


algoritmoGenetico <- function(
  cantidadIndividuos = 100,
  cantidadVariables = 1,
  limiteInf, limiteSup,
  fitnessFn,
  nGeneraciones=100,
  generacionesSinCambio=10,
  toleraciaSinCambio=0.01 # valor minimo de diferencia entre generaciones para considerar cambio en la mejor solucion
  ) {

  # ALMACENAMIENTO DE RESULTADOS
  # ============================================================================
  # Por cada generación se almacena el mejor individuo, su fitness, y la diferencia
  # absoluta respecto a la última generación.
  resultados_fitness   <- vector(mode = "list", length = nGeneraciones)
  resultados_individuo <- vector(mode = "list", length = nGeneraciones)
  diferencia_abs       <- vector(mode = "list", length = nGeneraciones)
  
  
  # 1. Creamos poblacion inicial aleatoria.

  poblacion <- crearPoblacion(cantidadIndividuos, cantidadVariables, limiteInf = limiteInf,
                                limiteSup = limiteSup)
  
  
  for(i in 1:nGeneraciones) {
    
    # 2. Calcular Aptitud
    
    aptitud <- aptitud(poblacion, fitnessFn) %>% as.numeric()
    #print(poblacion)
    
    # mejor  solución en la iteración actual
    fitness_mejor_individuo   <- max(aptitud)
    if(is.na(fitness_mejor_individuo)) {
      print("NA")
    }
    mejor_individuo           <- poblacion[which.max(aptitud), ]
    resultados_fitness[[i]]   <- fitness_mejor_individuo
    resultados_individuo[[i]] <- mejor_individuo
    mejor_individuo_proceso   <- resultados_individuo[[which.max(unlist(resultados_fitness))]]
    print(glue::glue("fitness_mejor_individuo: ",fitness_mejor_individuo, " mejor_individuo: ", toString(mejor_individuo), " mejor del proceso: ", toString(mejor_individuo_proceso)))
    if (i > 1) {
      diferencia_abs[[i]] <- abs(resultados_fitness[[i - 1]] - resultados_fitness[[i]])
    }
    #print(poblacion)
    
    nuevaPoblacion <- matrix(data = NA, nrow = cantidadIndividuos, ncol = cantidadVariables)
    
    for (j in 1:cantidadIndividuos) {
      # 3. Selección de Individuos
      
      individuoA <- seleccion(aptitud, metodo = "ranking")
      individuoB <- seleccion(aptitud, metodo = "ranking")
      
      # 4. Cruza de Individuos
      
      individuoAB <- cruzar(poblacion[individuoA, ], poblacion[individuoB, ],
                            metodo = "mitad")

      # 5. Mutación de Individuo
      
      individuoAB <- mutar(individuoAB, p = 0.5, 
                           xMin = limiteInf, xMax = limiteSup) %>% unlist()
      

      nuevaPoblacion[j, ] <- individuoAB
    }
     
    poblacion <- nuevaPoblacion
    
    if (i > generacionesSinCambio) {
      ultimasDiferencias <- tail(unlist(diferencia_abs), n = generacionesSinCambio)
      if (all(ultimasDiferencias < toleraciaSinCambio)) {
        print(glue::glue("freno por no haber cambios "))
        break()
      }
    }
    
  }
  
  # IDENTIFICACIÓN DEL MEJOR INDIVIDUO DE TODO EL PROCESO
  # ==========================================================================
  print(glue::glue("mejor individuo proceso: ", toString(mejor_individuo_proceso)))
  
  return( 
    list(
      mejor_individuo = mejor_individuo_proceso  
    )
  )
}

errorEj2 <- function(individuos){
  #return(-x*sin(sqrt(abs(x))))
  x <- desconocido1[, 1] %>% as.matrix(ncol=1) %>% t()
  y <- matrix(rep(desconocido1[, 2], nrow(individuos)) %>% unlist(), ncol=length(x))
  a1 <- individuos[,1] %>%  as.matrix(nrow=1)
  a2 <- individuos[,2] %>%  as.matrix(nrow=1)
  a3 <- individuos[,3] %>%  as.matrix(nrow=1)
  a4 <- matrix(rep(individuos[,4], length(x)), ncol=length(x))
  
  salidaCalculada <- a1 %*% x^3 + a2 %*% x^2 +  a3 %*% x + a4
  error <- y-salidaCalculada
  error_cuadratico_medio = rowMeans(error^2)
  
  return(error_cuadratico_medio)
}
