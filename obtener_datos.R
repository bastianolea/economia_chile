library(dplyr)
library(rvest)
library(stringr)
library(tidyr)

source("funciones.R")


pib <- obtener_pib()

imacec <- obtener_imacec()

ipc <- obtener_ipc()

ipsa <- obtener_ipsa()

desempleo <- obtener_desempleo()

uf <- obtener_uf()


pib |> saveRDS("app/datos/pib.rds")
imacec |> saveRDS("app/datos/imacec.rds")
ipc |> saveRDS("app/datos/ipc.rds")
ipsa |> saveRDS("app/datos/ipsa.rds")
desempleo |> saveRDS("app/datos/desempleo.rds")
uf |> saveRDS("app/datos/uf.rds")