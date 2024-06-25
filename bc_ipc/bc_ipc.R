# https://si3.bcentral.cl/Siete
# https://si3.bcentral.cl/Siete/ES/Siete/Cuadro/CAP_PRECIOS/MN_CAP_PRECIOS/IPC_EMP_2023/638415285164039007
# Precios > IPC General y medidas subyacentes, INE > Índice empalmado - 2023

library(readxl)
library(tidyverse)

ipc <- read_excel("bc_ipc/Cuadro_28032024131518.xlsx") |> 
  rename(fecha = 1, valor = 2) |> 
  mutate(fecha = str_extract(fecha, "\\d{4}-\\d{2}-\\d{2}"),
         fecha = ymd(fecha)) |> 
  filter(!is.na(fecha)) |> 
  mutate(valor = as.numeric(valor)) |> 
  # calcular variación mensual
  arrange(fecha) |> 
  mutate(variacion = valor/lag(valor)-1,
         variacion = replace_na(variacion, 0))

ipc |> 
  ggplot(aes(fecha, valor)) +
  geom_line()

ipc |> 
  ggplot(aes(fecha, variacion)) +
  geom_line()

  
arrow::write_parquet(ipc, "resultados/bc_ipc.parquet")