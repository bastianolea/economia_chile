# https://si3.bcentral.cl/Siete/ES/Siete/Cuadro/CAP_CCNN/MN_CCNN76/CCNN2018_P0_V2/637801082315858005?cbFechaInicio=2010&cbFechaTermino=2023&cbFrecuencia=QUARTERLY&cbCalculo=NONE&cbFechaBase=
# Cuentas nacionales > Producto Interno Bruto (PIB), gasto e ingreso > Referencia 2018 > Producto interno bruto > PIB total

datos_pib <- fs::dir_info("bc_pib") |> 
  filter(str_detect(path, "xls")) |> 
  slice_max(birth_time)

pib <- read_excel(datos_pib$path) |> 
  rename(fecha = 1, pib_pc = 2, pib_ant = 3, pib_ant_des = 4) |> 
  # limpiar fechas
  mutate(fecha = str_extract(fecha, "\\d{4}-\\d{2}-\\d{2}"),
         fecha = ymd(fecha)) |> 
  filter(!is.na(fecha)) |> 
  # separar variables
  mutate(across(starts_with("valor"), as.numeric)) |> 
  pivot_longer(cols = starts_with("pib"), values_to = "valor", names_to = "variable") |> 
  mutate(variable = str_remove(variable, "valor_") |> toupper()) |> 
  mutate(valor = as.numeric(valor)) |> 
  mutate(variable = recode(variable, 
                           "PIB_PC" = "PIB a precios corrientes",
                           "PIB_ANT" = "PIB volumen a precios del año anterior encadenado",
                           "PIB_ANT_DES" = "PIB volumen a precios del año anterior encadenado (desestacionalizado)")) |> 
  # calcular variación mensual
  arrange(variable, fecha) |> 
  mutate(variacion = valor/lag(valor)-1,
         variacion = replace_na(variacion, 0))

pib |> 
  ggplot(aes(fecha, valor, color = variable)) +
  geom_line() +
  theme(legend.position = "bottom")

pib |> 
  ggplot(aes(fecha, variacion, color = variable)) +
  geom_line() +
  theme(legend.position = "bottom")

# guardar ----
arrow::write_parquet(pib, "resultados/bc_pib.parquet")
