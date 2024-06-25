# https://si3.bcentral.cl/Siete/ES/Siete/Cuadro/CAP_ESTADIST_MACRO/MN_EST_MACRO_IV/PEM_ACTyDDA_IMACEC_2_2018/637807927445790326
# Principales Estadísticas Macro > Actividad y demanda > Indicadores de coyuntura > Imacec

datos_imacec <- fs::dir_info("bc_imacec") |> 
  filter(str_detect(path, "xls")) |> 
  slice_max(birth_time)

imacec <- read_excel(datos_imacec$path) |> 
  rename(fecha = 1, imacec_1 = 2, imacec_2 = 3, imacec_3 = 4) |> 
  # limpiar fechas
  mutate(fecha = str_extract(fecha, "\\d{4}-\\d{2}-\\d{2}"),
         fecha = ymd(fecha)) |> 
  filter(!is.na(fecha)) |> 
  # separar variables
  mutate(across(starts_with("imacec_"), as.numeric)) |> 
  pivot_longer(cols = starts_with("imacec_"), values_to = "valor", names_to = "variable") |> 
  mutate(valor = as.numeric(valor)) |>
  # 1.Tipo  de  cambio  nominal (dólar observado $CLP/USD) 2.Tipo  de  cambio  nominal multilateral (TCM) 3.Tipo  de  cambio  real (índice 1986=100)
  mutate(variable = recode(variable, 
                           "imacec_1" = "Imacec empalmado, serie original (índice 2018=100)",
                           "imacec_2" = "Imacec empalmado, desestacionalizado (índice 2018=100)",
                           "imacec_3" = "Velocidad de expansión (porcentaje), referencia 2018")) |> 
  # calcular variación mensual
  arrange(variable, fecha) |> 
  mutate(variacion = valor/lag(valor)-1,
         variacion = replace_na(variacion, 0))

imacec |> 
  ggplot(aes(fecha, valor, color = variable)) +
  geom_line() +
  theme(legend.position = "bottom")

imacec |> 
  ggplot(aes(fecha, variacion, color = variable)) +
  geom_line() +
  theme(legend.position = "bottom")

# guardar ----
arrow::write_parquet(imacec, "resultados/bc_imacec.parquet")
