library(dplyr)
library(rvest)
library(stringr)
library(tidyr)

source("funciones.R")

# obtener datos ----
pib <- obtener_pib()

pib_regional <- obtener_pib_regional()

imacec <- obtener_imacec()

ipc <- obtener_ipc()

ipsa <- obtener_ipsa()

desempleo <- obtener_desempleo()

uf <- obtener_uf()

remuneraciones <- obtener_remuneraciones()

# hay que automatizarla primero
# canasta <- obtener_canasta()

# desocupados <- obtener_desocupados()



# guardar ----
# # guardar datos obtenidos
# saveRDS(pib, "app/datos/pib.rds")
# saveRDS(imacec, "app/datos/imacec.rds")
# saveRDS(ipc, "app/datos/ipc.rds")
# saveRDS(ipsa, "app/datos/ipsa.rds")
# saveRDS(desempleo, "app/datos/desempleo.rds")
# saveRDS(uf, "app/datos/uf.rds")

# guardar datos nuevos solo si han cambiado
guardar_solo_con_cambios(pib, "app/datos/pib.csv")
guardar_solo_con_cambios(pib_regional, "app/datos/pib_regional.csv")
guardar_solo_con_cambios(imacec, "app/datos/imacec.csv")
guardar_solo_con_cambios(ipc, "app/datos/ipc.csv")
guardar_solo_con_cambios(ipsa, "app/datos/ipsa.csv")
guardar_solo_con_cambios(desempleo, "app/datos/desempleo.csv")
guardar_solo_con_cambios(uf, "app/datos/uf.csv")
guardar_solo_con_cambios(remuneraciones, "app/datos/remuneraciones.csv")
# guardar_solo_con_cambios(desocupados, "app/datos/desocupados.rds")
