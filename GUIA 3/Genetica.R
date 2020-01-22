##################################################################################
# Convierte a binario con un largo de bits pasado por parametro.
# el bit menos significativo queda en el indice 0 del vector
##################################################################################

int2bit <- function(x,i) {
  intToBits(x)[1:i]
}

# int2bit(12,cantidadBits)

##################################################################################
# Conviente el dato binario a decimal
##################################################################################

bit2int <- function(x) {
  sum <- 0 
  for (i in seq(length(x)-1,1)) {
    if (x[i] == 1)
    {
      sum = sum + 2^(i-1)
    }
  }
  sum
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
    
    probabilidad_seleccion <- 1 / rank(aptitud)
    
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

cruzar <- function(x,y,cantidadBits,metodo = "mitad") {
  x <- int2bit(x,cantidadBits)
  y <- int2bit(y,cantidadBits)
  largoMitad <- trunc(cantidadBits/2)
  nuevo <- cbind(t(x[1:largoMitad]),t(y[largoMitad+1:largoMitad]))
  nuevo <- bit2int(nuevo)
  return(nuevo)
}

##################################################################################
# Funcion para mutar
# Cambia el bit menos significativo si un valor random en menor a p
##################################################################################

mutar <- function(x, cantidadBits, p = 0.001, xMin, xMax) {
  x <- int2bit(x,cantidadBits)
  if( runif(1) < p) {
    if(x[1]==1) {
      x[1]=as.raw(0)
    } else {x[1]=as.raw(1)}
  }
  x <- bit2int(x)
  if(x>xMax) x <- xMax
  if(x<xMin) x <- xMin
  return(x)
}


algoritmoGenetico <- function(
  cantidadIndividuos = 100,
  cantidadVariables = 1,
  limiteInf, limiteSup,
  fitnessFn,
  nPoblacion=100,
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
    
    aptitud <- aptitud(poblacion, fitnessFn)
    
    # mejor  solución en la iteración actual
    fitness_mejor_individuo   <- max(aptitud)
    print(glue::glue("fitness_mejor_individuo: {fitness_mejor_individuo}"))
    mejor_individuo           <- poblacion[which.max(aptitud), ]
    resultados_fitness[[i]]   <- fitness_mejor_individuo
    resultados_individuo[[i]] <- mejor_individuo
    if (i > 1) {
      diferencia_abs[[i]] <- abs(resultados_fitness[[i - 1]] - resultados_fitness[[i]])
    }
    
    nuevaPoblacion <- matrix(data = NA, nrow = cantidadIndividuos, ncol = cantidadVariables)
    
    for (j in 1:nPoblacion) {
      # 3. Selección de Individuos
      
      individuoA <- seleccion(aptitud, metodo = "ranking")
      individuoB <- seleccion(aptitud, metodo = "ranking")
      
      # 4. Cruza de Individuos
      
      individuoAB <- cruzar(poblacion[individuoA], poblacion[individuoB],
                            cantidadBits, metodo = "mitad")
      
      # 5. Mutación de Individuo
      
      individuoAB <- mutar(individuoAB, cantidadBits, p = 0.01, 
                           xMin = xMin, xMax = xMax)
      
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
  mejor_individuo <- resultados_individuo[[which.max(unlist(resultados_fitness))]]
  print(glue::glue("mejor individuo: ", mejor_individuo))
  
  return( 
    list(
      mejor_individuo = mejor_individuo  
    )
  )
}