library(dplyr)
library(stringr)

# https://www.ine.gob.cl/docs/default-source/ocupacion-y-desocupacion/bbdd/2024/csv/ene-2024-01-def.csv
# https://www.ine.gob.cl/docs/default-source/ocupacion-y-desocupacion/bbdd/2024/csv/ene-2024-02-efm.csv
# https://www.ine.gob.cl/docs/default-source/ocupacion-y-desocupacion/bbdd/2024/csv/ene-2024-03-fma.csv
# https://www.ine.gob.cl/docs/default-source/ocupacion-y-desocupacion/bbdd/2024/csv/ene-2024-04-mam.csv


# texto con iniciales de los meses; enero empieza siempre por def
meses = "defmamjjasonde" |> str_split("", simplify = T)

# for (paso in 1:12) {
# meses[(1:3) + paso-1] |> paste0(collapse = "") |> print()
# }

# función que para cada mes te da la secuencia de tres iniciales: la anterior, la del mes y la del siguiente
mes_ene <- function(mes) {
  purrr::map(mes, \(paso) {
    meses[(1:3) + (paso-1)] |> paste0(collapse = "")
  }) |> unlist()
}

# mes_ene(mes = 1:12)

# crear las url de las encuestas siguiento la lógica
url_base = "https://www.ine.gob.cl/docs/default-source/ocupacion-y-desocupacion/bbdd/"
año_ene = 2024
meses_n = c(paste0(0, 1:9), 10:12)
meses_txt = mes_ene(mes = 1:12)

enlaces_ene <- paste0(url_base, año_ene, "/csv/ene-", año_ene, "-", meses_n, "-", meses_txt, ".csv")
