# https://si3.bcentral.cl/Siete/ES/Siete/Cuadro/CAP_ESTADIST_MACRO/MN_EST_MACRO_IV/PEM_TC
# Principales Estadísticas Macro > Tipo de cambio

datos_dolar <- fs::dir_info("bc_dolar") |> 
  filter(str_detect(path, "xls")) |> 
  slice_max(birth_time)

dolar <- read_excel(datos_dolar$path) |> 
  rename(fecha = 1, tipo_cambio_1 = 2, tipo_cambio_2 = 3, tipo_cambio_3 = 4) |> 
  # limpiar fechas
  mutate(fecha = str_extract(fecha, "\\d{4}-\\d{2}-\\d{2}"),
         fecha = ymd(fecha)) |> 
  filter(!is.na(fecha)) |> 
  # separar variables
  mutate(across(starts_with("valor"), as.numeric)) |> 
  pivot_longer(cols = starts_with("tipo_cambio"), values_to = "valor", names_to = "variable") |> 
  mutate(valor = as.numeric(valor)) |>
  # 1.Tipo  de  cambio  nominal (dólar observado $CLP/USD) 2.Tipo  de  cambio  nominal multilateral (TCM) 3.Tipo  de  cambio  real (índice 1986=100)
  mutate(variable = recode(variable, 
                           "tipo_cambio_1" = "Dólar observado ($CLP/USD)",
                           "tipo_cambio_2" = "Tipo  de  cambio  nominal multilateral (TCM)",
                           "tipo_cambio_3" = "Tipo  de  cambio  real (índice 1986=100)")) |> 
  # calcular variación mensual
  arrange(variable, fecha) |> 
  mutate(variacion = valor/lag(valor)-1,
         variacion = replace_na(variacion, 0))

dolar |> 
  ggplot(aes(fecha, valor, color = variable)) +
  geom_line() +
  theme(legend.position = "bottom")

dolar |> 
  ggplot(aes(fecha, variacion, color = variable)) +
  geom_line() +
  theme(legend.position = "bottom")

# guardar ----
arrow::write_parquet(dolar, "resultados/bc_dolar.parquet")
