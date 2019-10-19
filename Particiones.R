library(dplyr)

###########################################################################
# Realiza particion de entrenemiento y prueba estratificado segun la clase.
###########################################################################

generarParticion <- function(dataset, porcEntrenamiento, semilla, clase){

  dataset  <- as.data.frame(mutate(dataset, idtempo =  row_number()))
  set.seed(semilla )
  dataset_training <- as.data.frame(dataset %>%
                                      group_by(!!as.name(clase)) %>%
                                      sample_frac(porcEntrenamiento) %>%
                                      ungroup)
  dataset_testing  <- as.data.frame(anti_join(dataset, dataset_training, by = "idtempo"))
  dataset_training <- subset(dataset_training, select = -idtempo )
  dataset_testing  <- subset(dataset_testing, select = -idtempo )
  
  return(list(dataset_training, dataset_testing))
  
}

###############################################################################
# Realiza N particiones de entrenemiento y prueba estratificado segun la clase.
###############################################################################

generarNParticiones <- function(dataset, nroParticiones, porcEntrenamiento, semilla, clase){
  
  if(round(1-porcEntrenamiento) == round(1/nroParticiones) ){
    
  dataset    <- as.data.frame(mutate(dataset, idtempo =  row_number()))
  porcPrueba <- 1 - porcEntrenamiento
  dataset_training <- list()
  dataset_testing  <- list()
  set.seed(semilla )
  dataset_testing[[1]] <- as.data.frame(dataset %>%
                                      group_by(!!as.name(clase)) %>%
                                      sample_frac(porcPrueba) %>%
                                      ungroup)
  dataset_training[[1]]  <- as.data.frame(anti_join(dataset, dataset_testing[[1]], by = "idtempo"))
  dataset_aux <- dataset
  for (i in seq(2,nroParticiones)) {
    dataset_aux <- anti_join(dataset_aux, dataset_testing[[i-1]], by = "idtempo")
    dataset_testing[[i]] <- as.data.frame(dataset_aux %>%
                                           group_by(!!as.name(clase)) %>%
                                           sample_frac(porcPrueba*(1/(porcEntrenamiento - porcPrueba * (i-2)))) %>%
                                           ungroup)
    dataset_training[[i]]  <- as.data.frame(anti_join(dataset, dataset_testing[[i]], by = "idtempo"))
  }

  for (i in seq(1,nroParticiones)) {
    dataset_training[[i]] <- subset(dataset_training[[i]], select = -idtempo )
    dataset_testing[[i]]  <- subset(dataset_testing[[i]], select = -idtempo )
  }
  
  return(list(dataset_training, dataset_testing))
  }
  else {
    print("Error en la relación partición y porcentaje de entrenamiento")
    return(-1)
  }
}

###########################################################################
# Realiza particion de entrenemiento y prueba estratificado segun la clase.
# Devuelve los id.
###########################################################################

generarParticionPorID <- function(dataset, porcEntrenamiento, semilla, clase){
  
  dataset  <- as.data.frame(mutate(dataset, idtempo =  row_number()))
  nroReg   <- dim(dataset)[1]
  set.seed(semilla )
  dataset_training <- as.data.frame(dataset %>%
                                      group_by(!!as.name(clase)) %>%
                                      sample_frac(porcEntrenamiento) %>%
                                      ungroup)$idtempo
  dataset_testing  <- seq(1,nroReg)[-dataset_training]
  
  return(list(dataset_training, dataset_testing))
  
}


###############################################################################
# Realiza N particiones de entrenemiento y prueba estratificado segun la clase.
# Devuelve los id.
###############################################################################

generarNParticionesPorID <- function(dataset, nroParticiones, porcEntrenamiento, semilla, clase){
  
  if(round(1-porcEntrenamiento) == round(1/nroParticiones) ){
  
  dataset    <- as.data.frame(mutate(dataset, idtempo =  row_number()))
  nroReg   <- dim(dataset)[1]
  porcPrueba <- 1 - porcEntrenamiento
  dataset_training <- list()
  dataset_testing  <- list()
  set.seed(semilla )
  dataset_testing[[1]] <- as.data.frame(dataset %>%
                                          group_by(!!as.name(clase)) %>%
                                          sample_frac(porcPrueba) %>%
                                          ungroup)$idtempo
  dataset_training[[1]]  <- seq(1,nroReg)[-dataset_testing[[1]]]
  dataset_aux <- dataset
  idusados <- vector()
  for (i in seq(2,nroParticiones)) {
    idusados <- c(idusados,dataset_testing[[i-1]])
    dataset_aux <- dataset[-sort(idusados),]
    dataset_testing[[i]] <- as.data.frame(dataset_aux %>%
                                            group_by(!!as.name(clase)) %>%
                                            sample_frac(porcPrueba*(1/(porcEntrenamiento - porcPrueba * (i-2)))) %>%
                                            ungroup)$idtempo
    dataset_training[[i]]  <- seq(1,nroReg)[-dataset_testing[[i]]]
  }
  
  return(list(dataset_training, dataset_testing))
  }
  else {
    print("Error en la relación partición y porcentaje de entrenamiento")
    return(-1)
  }
}


