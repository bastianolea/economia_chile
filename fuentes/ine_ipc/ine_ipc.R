# https://www.ine.gob.cl/estadisticas/economia/indices-de-precio-e-inflacion/indice-de-precios-al-consumidor
library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(lubridate)
library(ggplot2)

ipc <- readr::read_delim("ine_ipc/ipc base anual 2018=100/Base anonimizada IPC 2019.csv", delim = '\\')

ipc |> count(Glosa_Producto) |> print(n=Inf)

ipc_filt <- ipc |> 
  select(-starts_with("Reemplaza_"), -starts_with("Estado_"), -starts_with("pm_16_")) |> 
  rename(producto = Glosa_Producto) |> 
  filter(producto %in% c("PAN", "PALTA", "ACEITE VEGETAL"))

ipc_filt |> 
  filter(producto == "PAN") |> 
  glimpse()

ipc_filt_2 <- ipc_filt |> 
  filter(producto == "PAN") |>
pivot_longer(cols = c(starts_with("pm_"), starts_with("Cantidad_"), starts_with("Unidad_")), 
             names_to = "fecha_t", values_to = "precio", values_transform = as.character) |> 
  mutate(prefijo = str_extract(fecha_t, ".*_")) |> 
  mutate(año = str_extract(fecha_t, "\\d{4}"),
         mes_t = str_extract(fecha_t, "(?<=(pm_16_|pm_|Cantidad_|Unidad_))(.*?)(?=\\d{4})"),
         mes = recode(tolower(mes_t),
                      "enero" = "1", "febrero" = "2", "marzo" = "3",
                      "abril" = "4", "mayo" = "5", "junio" = "6",
                      "julio" = "7", "agosto" = "8", "septiembre" = "9",
                      "octubre" = "10", "noviembre" = "11", "diciembre" = "12")) |> 
  mutate(fecha = ymd(paste(año, mes, 15))) |> 
  select(-fecha_t) |> 
  pivot_wider(names_from = prefijo, values_from = precio, id_cols = c(1:7, producto, fecha), 
              values_fn = ~unlist(.x) |> paste(collapse = ", ")) |> 
  filter(Cantidad_ == 1000)


# 
# 
#   pivot_longer(cols = c(starts_with("pm_"), starts_with("Cantidad_"), starts_with("Unidad_")), 
#                names_to = "fecha_t", values_to = "precio", values_transform = as.character) |> 
#   # select(-starts_with("Reemplaza_"), -starts_with("Estado_"), -starts_with("Cantidad_"), -starts_with("Unidad_"))
#   select(producto, fecha_t, precio)
# 
# ipc_filt_2 <- ipc_filt |> 
#   mutate(sufijo = str_extract(fecha_t, ".*_")) |> 
#   filter(sufijo == "pm_") |> 
#   mutate(año = str_extract(fecha_t, "\\d{4}"),
#          mes_t = str_extract(fecha_t, "(?<=(pm_16_|pm_|Cantidad_|Unidad_))(.*?)(?=\\d{4})"),
#          mes = recode(tolower(mes_t),
#                                    "enero" = "1", "febrero" = "2", "marzo" = "3",
#                                    "abril" = "4", "mayo" = "5", "junio" = "6",
#                                    "julio" = "7", "agosto" = "8", "septiembre" = "9",
#                                    "octubre" = "10", "noviembre" = "11", "diciembre" = "12")) |> 
#   mutate(fecha = ymd(paste(año, mes, 15)))

ipc_filt_2 |> 
  group_by(producto, fecha) |> 
  summarize(precio = mean(as.numeric(pm_))) |> 
  ggplot(aes(fecha, precio)) +
  geom_line()
