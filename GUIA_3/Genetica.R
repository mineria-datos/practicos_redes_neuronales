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
  args <-  split(datos, rep(1:ncol(datos), each = nrow(datos)))
  names(args) <- NULL
  valorSalida <- do.call(funcion, args=args)
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

cruzar <- function(padre1,padre2) {
  #print(glue::glue("cruza  padre1: ", padre1, " padre2: ", padre2 ))
  menor <- min(padre1, padre2)
  mayor <- max(padre1, padre2)
  nuevo <- runif(1, menor, mayor)
  #print(glue::glue("nuevo : ", nuevo))
  return(nuevo)
}

##################################################################################
# Funcion para mutar
# para cada variable del individuo, se muta con una probabilidad p con valor al azar
# entre xMin y xMax de esa variable
##################################################################################

mutar <- function(individuo, p = 0.5, cantidadDecimales=2, xMin, xMax) {
  runif(n = length(individuo), min = 0, max = 1)
  posicionesMutadasProb <- runif(n = length(individuo), min = 0, max = 1)
  posicionesMutadas <- posicionesMutadasProb < p
  nuevo <- individuo
  for (i in which(posicionesMutadas)) {
    nuevo[i] <- runif(n = 1, min = xMin[i], max = xMax[i])
  }
  return(nuevo)
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
    #print(glue::glue("fitness_mejor_individuo: ",fitness_mejor_individuo, " mejor_individuo: ", toString(mejor_individuo), " mejor del proceso: ", toString(mejor_individuo_proceso)))
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
      
      individuoAB <- cruzar(poblacion[individuoA, ], poblacion[individuoB, ])

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




