library(readr)
library(dplyr)
library(mvtnorm)
source('./RedRBF.R')
source('./PerceptronSimpleRBF.R')


XOR_trn <- read_csv("../PUBLICO/Encuentro 1/Práctica/data/XOR_trn.csv", col_names = FALSE)

resultado <- redRBF(XOR_trn, 4)
names(resultado)