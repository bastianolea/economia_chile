# https://si3.bcentral.cl/Siete/ES/Siete/Cuadro/CAP_PRECIOS/MN_CAP_PRECIOS/IPC_EMP_2023/638415285164039007
# Precios > IPC General y medidas subyacentes, INE > Índice empalmado - 2023

url = "https://si3.bcentral.cl/Siete/ES/Siete/Cuadro/CAP_PRECIOS/MN_CAP_PRECIOS/IPC_EMP_2023/638415285164039007"

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
