library(dplyr)
library(rvest)
library(polite)
library(stringr)
library(tidyr)
library(readxl)
library(janitor)

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

## nuevos ----
inversion_extranjera <- obtener_inversion_extranjera()

precio_cobre <- obtener_precio_cobre()

## nuevos nuevos ----
# actividad en minería e industria
# índice de producción industrial
prod_industrial <- obtener_prod_industrial()



# hay que automatizarla primero
# canasta <- obtener_canasta()
# 
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

# nuevos
guardar_solo_con_cambios(inversion_extranjera, "app/datos/inversion_extranjera.csv")
guardar_solo_con_cambios(precio_cobre, "app/datos/precio_cobre.csv")

# nuevos nuevos
guardar_solo_con_cambios(prod_industrial, "app/datos/prod_industrial.csv")


# unificar ----

message("uniendo datos...")

pib <- cargar_si_no_existe("pib")
imacec <- cargar_si_no_existe("imacec")
ipc  <- cargar_si_no_existe("ipc")
ipsa  <- cargar_si_no_existe("ipsa")
desempleo  <- cargar_si_no_existe("desempleo")
uf  <- cargar_si_no_existe("uf")
remuneraciones  <- cargar_si_no_existe("remuneraciones")
inversion_extranjera  <- cargar_si_no_existe("inversion_extranjera")
precio_cobre  <- cargar_si_no_existe("precio_cobre")
prod_industrial  <- cargar_si_no_existe("prod_industrial")



# unir todos los datos en un solo dataframe
datos_unidos <- bind_rows(pib |> mutate(dato = "pib"),
                          # pib_regional |> mutate(dato = "pib_regional"),
                          imacec |> mutate(dato = "imacec"),
                          ipc |> mutate(dato = "ipc"),
                          ipsa |> mutate(dato = "ipsa"),
                          desempleo |> mutate(dato = "desempleo"),
                          uf |> mutate(dato = "uf"),
                          remuneraciones |> mutate(dato = "remuneraciones"),
                          inversion_extranjera |> mutate(dato = "inversion_extranjera"),
                          precio_cobre |> mutate(dato = "precio_cobre"), 
                          prod_industrial |> mutate(dato = "prod_industrial")
) |> 
  mutate(fecha_union = Sys.Date())

# datos_unidos |>
#   filter(dato == "prod_industrial")

# guardar dato unido
write.csv2(datos_unidos, "app/datos/datos_economia_chile.csv")
