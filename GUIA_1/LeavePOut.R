library(iterators)

## en base a los datos devuelve un iterable que en cada paso
# retorna los datos de train y test del leave-p-out con p como parametro
#
# EJEMPLO: 
# datos <-  matrix(c(1, 2, 3, 4, 5, 6, 7, 8), ncol = 2, byrow = T)
# s <- leavePOutSplit(datos, 2)
# 
# while(hasNext(s)) {
#   e <- nextElem(s)
#   print(e$train)
#   print(e$test)
#   print("-----------")
# }

leavePOutSplit <- function (datos, p=1) {
  ndatos <- nrow(datos)
  combinaciones <- combn(seq(1:ndatos), p)
  nroCombinaciones <- ncol(combinaciones)
  iterador <- function() {
    actual <- 0
    nextEl <- function() {
      actual <<- actual + 1
      r <- list(
        test  = datos[combinaciones[,  actual],],   
        train = datos[combinaciones[, -actual],],
        actual = actual,
        nroCombinaciones = nroCombinaciones
        
      )
      if(actual == nroCombinaciones) {
        stop('StopIteration')
      }
      r
    }
    hasNx <- function(){
      actual < nroCombinaciones
    }
    obj <- list(nextElem=nextEl, hasNext=hasNx)
    class(obj) <- c('leavePOutSplit', 'abstractiter', 'iter')
    obj
  }
  iterador()
}
