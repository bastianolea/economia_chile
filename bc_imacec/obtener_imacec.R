# https://si3.bcentral.cl/Siete/ES/Siete/Cuadro/CAP_ESTADIST_MACRO/MN_EST_MACRO_IV/PEM_ACTyDDA_IMACEC_2_2018/637807927445790326
# Principales Estadísticas Macro > Actividad y demanda > Indicadores de coyuntura > Imacec

url = "https://si3.bcentral.cl/Siete/ES/Siete/Cuadro/CAP_ESTADIST_MACRO/MN_EST_MACRO_IV/PEM_ACTyDDA_IMACEC_2_2018/637807927445790326"

dato_0 <- session(url) |> 
  read_html() |> 
  html_table()

dato_1 <- dato_0[[1]] |> 
  janitor::clean_names() |> 
  select(-1)

dato_2 <- dato_1 |> 
  tidyr::pivot_longer(cols = 2:length(dato_1), names_to = "fecha", values_to = "valor") |> 
  mutate(mes = str_extract(fecha, "^\\w+(?=_)"),
         mes = mes_a_numeric(mes),
         año = extraer_año(fecha)) |> 
  mutate(valor = cifra_comas_a_numeric(valor)) |> 
  mutate(fecha = paste(año, mes, "1", sep = "-"))
