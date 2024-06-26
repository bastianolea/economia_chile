# https://stat.ine.cl/Index.aspx?DataSetCode=IR_IR2016
# Estadísticas del Mercado de Trabajo > Índices de Remuneraciones y del Costo de la Mano de Obra (IR-ICMO) > Índice nominal de remuneraciones (base 2016=100) > Índice nominal de remuneraciones (base 2016 = 100) > Nacional, mensual

library(readr)
library(tidyverse)

remuneraciones_n <- read_csv("ine_remuneraciones/indice_nominal/IR_IR2016_28032024122420024.csv") |> 
  rename(fecha_t = Mes, valor = Value, unidad = `Unidad de medida`) |> 
  filter(unidad == "Índice") |> 
  # limpiar fecha
  mutate(año = str_extract(fecha_t, "\\d{4}"),
         mes = str_remove(fecha_t, "\\d{4}"),
         mes = str_extract(mes, "\\w+"),
         mes = recode(tolower(mes),
             "enero" = "1", "febrero" = "2", "marzo" = "3",
             "abril" = "4", "mayo" = "5", "junio" = "6",
             "julio" = "7", "agosto" = "8", "septiembre" = "9",
             "octubre" = "10", "noviembre" = "11", "diciembre" = "12")) |> 
  mutate(fecha = ymd(paste(año, mes, 1))) |> 
  # calcular variación mensual
  arrange(fecha) |> 
  mutate(variacion = valor/lag(valor)-1,
         variacion = replace_na(variacion, 0))

remuneraciones_n |> 
  ggplot(aes(fecha, valor)) +
  geom_line()

remuneraciones_n |> 
  ggplot(aes(fecha, variacion)) +
  geom_line()

# guardar ----
arrow::write_parquet(remuneraciones_n, "resultados/ine_remun_nom.parquet")
