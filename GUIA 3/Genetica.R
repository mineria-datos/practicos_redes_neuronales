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
# Funcion para mezclar dos valores
##################################################################################

mezclaMitad <- function(x,y) {
  largo <- length(x)
  largoMitad <- trunc(largo/2)
  nuevo <- cbind(t(x[1:largoMitad]),t(y[largoMitad:largo]))
  nuevo
}

#valor1 <- int2bit(1,cantidadBits)
#valor2 <- int2bit(512,cantidadBits)

#mezclaMitad(valor1,valor2)

##################################################################################
# Funcion para mutar
##################################################################################

mutar <- function(x, p = 0.001) {
  if( random(1) < p) {
    if(x[length(x)]==1) {
      x[length(x)]=0
    } else {x[length(x)]=1}
  }
  x
}
