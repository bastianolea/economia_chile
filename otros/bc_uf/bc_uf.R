# https://si3.bcentral.cl/Siete
# https://si3.bcentral.cl/Siete/ES/Siete/Cuadro/CAP_PRECIOS/MN_CAP_PRECIOS/UF_IVP_UTM/UF_IVP_UTM
# Precios > Unidad de Fomento (UF) - Índice de Valor Promedio (IVP) - Unidad Tributaria Mensual (UTM) > UF-IVP-UTM mensual

library(readxl)
library(tidyverse)

uf <- read_excel("bc_uf/Cuadro_28032024131626.xlsx") |> 
  rename(fecha = 1, valor_uf = 2, valor_ivp = 3, valor_utm = 4) |> 
  # limpiar fecha
  mutate(fecha = str_extract(fecha, "\\d{4}-\\d{2}-\\d{2}"),
         fecha = ymd(fecha)) |> 
  filter(!is.na(fecha)) |> 
  # separar variables
  mutate(across(starts_with("valor"), as.numeric)) |> 
  pivot_longer(cols = starts_with("valor"), values_to = "valor", names_to = "variable") |> 
  mutate(variable = str_remove(variable, "valor_") |> toupper()) |> 
  # calcular variación mensual
  group_by(variable) |> 
  arrange(fecha) |> 
  mutate(variacion = valor/lag(valor)-1,
         variacion = replace_na(variacion, 0))

uf |> 
  ggplot(aes(fecha, valor, color = variable)) +
  geom_line()

uf |> 
  ggplot(aes(fecha, variacion, color = variable)) +
  geom_line()

# guardar ----
arrow::write_parquet(uf, "resultados/bc_uf.parquet")