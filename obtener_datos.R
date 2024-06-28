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


# guardar ----

# # guardar datos obtenidos
# saveRDS(pib, "app/datos/pib.rds")
# saveRDS(imacec, "app/datos/imacec.rds")
# saveRDS(ipc, "app/datos/ipc.rds")
# saveRDS(ipsa, "app/datos/ipsa.rds")
# saveRDS(desempleo, "app/datos/desempleo.rds")
# saveRDS(uf, "app/datos/uf.rds")


# guardar datos nuevos solo si han cambiado
guardar_solo_con_cambios(pib, "app/datos/pib.rds")
guardar_solo_con_cambios(pib_regional, "app/datos/pib_regional.rds")
guardar_solo_con_cambios(imacec, "app/datos/imacec.rds")
guardar_solo_con_cambios(ipc, "app/datos/ipc.rds")
guardar_solo_con_cambios(ipsa, "app/datos/ipsa.rds")
guardar_solo_con_cambios(desempleo, "app/datos/desempleo.rds")
guardar_solo_con_cambios(uf, "app/datos/uf.rds")

# if (all.equal(pib, readRDS("app/datos/pib.rds")) == FALSE) {
#   saveRDS(pib |> mutate(fecha_scraping = Sys.Date()), 
#           "app/datos/pib.rds")
# }
# 
# if (all.equal(imacec, readRDS("app/datos/imacec.rds")) == FALSE) {
#   saveRDS(imacec |> mutate(fecha_scraping = Sys.Date()), 
#           "app/datos/imacec.rds")
# }
# 
# if (all.equal(ipc, readRDS("app/datos/ipc.rds")) == FALSE) {
#   saveRDS(ipc |> mutate(fecha_scraping = Sys.Date()), 
#           "app/datos/ipc.rds")
# }
# 
# if (all.equal(ipsa, readRDS("app/datos/ipsa.rds")) == FALSE) {
#   saveRDS(ipsa |> mutate(fecha_scraping = Sys.Date()), 
#           "app/datos/ipsa.rds")
# }
# 
# if (all.equal(desempleo, readRDS("app/datos/desempleo.rds")) == FALSE) {
#   saveRDS(desempleo |> mutate(fecha_scraping = Sys.Date()), 
#           "app/datos/desempleo.rds")
# }
# 
# if (all.equal(uf, readRDS("app/datos/uf.rds")) == FALSE) {
#   saveRDS(uf |> mutate(fecha_scraping = Sys.Date()), 
#           "app/datos/uf.rds")
# }