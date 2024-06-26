# https://si3.bcentral.cl/Siete/ES/Siete/Cuadro/CAP_CCNN/MN_CCNN76/CCNN2018_P0_V2/637801082315858005?cbFechaInicio=2010&cbFechaTermino=2023&cbFrecuencia=QUARTERLY&cbCalculo=NONE&cbFechaBase=
# Cuentas nacionales > Producto Interno Bruto (PIB), gasto e ingreso > Referencia 2018 > Producto interno bruto > PIB total

url = "https://si3.bcentral.cl/Siete/ES/Siete/Cuadro/CAP_CCNN/MN_CCNN76/CCNN2018_P0_V2/637801082315858005?cbFechaInicio=2010&cbFechaTermino=2023&cbFrecuencia=QUARTERLY&cbCalculo=NONE&cbFechaBase="

dato_0 <- session(url) |> 
  read_html() |> 
  html_table()

dato_1 <- dato_0[[1]] |> 
  janitor::clean_names() |> 
  select(-1)

dato_2 <- dato_1 |> 
  tidyr::pivot_longer(cols = 2:length(dato_1), names_to = "fecha", values_to = "valor") |> 
  mutate(trimestre = str_extract(fecha, "^\\w+(?=_)"),
         trimestre = trimestre_a_numeric(trimestre),
         año = extraer_año(fecha)) |> 
  mutate(valor = cifra_comas_a_numeric(valor))
