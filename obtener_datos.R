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

saveRDS(pib, "app/datos/pib.rds")
saveRDS(imacec, "app/datos/imacec.rds")
saveRDS(ipc, "app/datos/ipc.rds")
saveRDS(ipsa, "app/datos/ipsa.rds")
saveRDS(desempleo, "app/datos/desempleo.rds")
saveRDS(uf, "app/datos/uf.rds")

# # guardar datos nuevos solo si han cambiado 
# if (all.equal(pib, readRDS("app/datos/pib.rds")) == FALSE) {
#   saveRDS(pib, "app/datos/pib.rds")
# }
# 
# if (all.equal(imacec, readRDS("app/datos/imacec.rds")) == FALSE) {
#   saveRDS(imacec, "app/datos/imacec.rds")
# }
# 
# if (all.equal(ipc, readRDS("app/datos/ipc.rds")) == FALSE) {
#   saveRDS(ipc, "app/datos/ipc.rds")
# }
# 
# if (all.equal(ipsa, readRDS("app/datos/ipsa.rds")) == FALSE) {
#   saveRDS(ipsa, "app/datos/ipsa.rds")
# }
# 
# if (all.equal(desempleo, readRDS("app/datos/desempleo.rds")) == FALSE) {
#   saveRDS(desempleo, "app/datos/desempleo.rds")
# }
# 
# if (all.equal(uf, readRDS("app/datos/uf.rds")) == FALSE) {
#   saveRDS(uf, "app/datos/uf.rds")
# }