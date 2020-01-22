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
