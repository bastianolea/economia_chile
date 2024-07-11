library(rvest)
library(tidyr)

url_precio_cobre = "https://si3.bcentral.cl/Indicadoressiete/secure/Serie.aspx?gcode=LIBRA_COBRE&param=cgBnAE8AOQBlAGcAIwBiAFUALQBsAEcAYgBOAEkASQBCAEcAegBFAFkAeABkADgASAA2AG8AdgB2AFMAUgBYADIAQwBzAEEAMQBJAG8ATwBzAEgATABGAE4AagB1AFcAYgB2AFAAZwBhADIAbABWAHcAXwBXAGgATAAkAFIAVAB1AEIAbAB3AFoAdQBRAFgAZwA5AHgAdgAwACQATwBZADcAMwAuAGIARwBFAFIASwAuAHQA"

sitio_cobre <- session(url_precio_cobre) |> 
  read_html()

tablas <- sitio_cobre |> 
  html_table()

tabla_cobre <- tablas[[2]] |> 
  pivot_longer(cols = 2:length(tablas[[2]]), 
               names_to = "mes", values_to = "valor") |> 
  rename(dia = 1)

tabla_cobre_2 <- tabla_cobre |> 
  mutate(valor = cifra_comas_a_numeric(valor)) |> 
  mutate(con_dato = ifelse(!is.na(valor), TRUE, FALSE))

tabla_cobre_3 <- tabla_cobre_2 |> 
  mutate(mes_t = mes,
         mes = str_extract(mes, "...") |> tolower() |> mes_a_numeric()) |> 
  mutate(fecha = as.Date(paste(2024, mes, dia), 
                         tryFormats = c("%Y %m %d")))

tabla_cobre_4 <- tabla_cobre_3 |> 
  arrange(fecha) |> 
  fill(valor, .direction = "downup") |> 
  mutate(serie = "Precio del cobre")
  

# guardar ----
write.csv2(tabla_cobre_4 |> select(serie, fecha, valor),
           "app/datos/precio_cobre.csv")
