# https://www.ine.gob.cl/estadisticas/economia/industria-manufacturera/indice-de-produccion-industrial
# Índice de Producción Industrial (serie original) > Índice

library(readr)
library(tidyverse)

prod <- read_csv("ine_produccion/Índice-datos_exportados_28032024_122113.csv") |> 
  rename(fecha = 1, valor = 2) |>
  # limpiar fecha
  mutate(año = str_extract(fecha, "\\d{2}") |> as.numeric(),
         año = año+2000) |> 
  mutate(mes = str_extract(fecha, "\\w+"),
         mes = recode(mes,
                      "ene" = "1", "jan" = "1", "feb" = "2",
                      "mar" = "3", "abr" = "4", "apr" = "4", "may" = "5",
                      "jun" = "6", "jul" = "7", "ago" = "8",
                      "aug" = "8", "sep" = "9", "oct" = "10",
                      "nov" = "11", "dic" = "12", "dec" = "12")) |> 
  mutate(fecha = ymd(paste(año, mes, 1))) |> 
  # calcular variación mensual
  arrange(fecha) |> 
  mutate(variacion = valor/lag(valor)-1,
         variacion = replace_na(variacion, 0))

prod |> 
  ggplot(aes(fecha, valor)) +
  geom_line()

prod |> 
  ggplot(aes(fecha, variacion)) +
  geom_line()

arrow::write_parquet(prod, "resultados/ine_prod_ind.parquet")