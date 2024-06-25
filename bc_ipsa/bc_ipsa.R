# https://si3.bcentral.cl/Siete/ES/Siete/Cuadro/CAP_ESTADIST_MACRO/MN_EST_MACRO_IV/PEM_INDBUR/PEM_INDBUR
# Principales Estadísticas Macro > Tasa de interés y estadísticas monetarias > Indicadores bursátiles

datos_ipsa <- fs::dir_info("bc_ipsa") |> 
  filter(str_detect(path, "xls")) |> 
  slice_max(birth_time)

ipsa <- read_excel(datos_ipsa$path) |> 
  rename(fecha = 1, valor = 2) |> 
  # limpiar fechas
  mutate(fecha = str_extract(fecha, "\\d{4}-\\d{2}-\\d{2}"),
         fecha = ymd(fecha)) |> 
  filter(!is.na(fecha)) |>
  mutate(valor = as.numeric(valor)) |> 
  # calcular variación mensual
  arrange(fecha) |> 
  mutate(variacion = valor/lag(valor)-1,
         variacion = replace_na(variacion, 0))

ipsa |> 
  ggplot(aes(fecha, valor)) +
  geom_line() +
  theme(legend.position = "bottom")

ipsa |> 
  ggplot(aes(fecha, variacion)) +
  geom_line() +
  theme(legend.position = "bottom")

# guardar ----
arrow::write_parquet(ipsa, "resultados/bc_ipsa.parquet")
