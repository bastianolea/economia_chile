# https://stat.ine.cl/?lang=es
# Estadísticas del Mercado de Trabajo > Encuesta Nacional de Empleo (ENE) > Tasa de participación > Nacional y regional, según sexo, trimestre móvil

library(tidyverse)
library(readr)

participacion <- read_csv("ine_participacion/ENE_TP_28032024121937836.csv") |> 
  janitor::clean_names() |> 
  rename(valor = value) |> 
  select(indicador, region, sexo, trimestre_movil, valor) |> 
  filter(indicador == "Tasa de participación (proyecciones base 2017)") |> 
  # filter(dti_cl_region == "_T",
  #        dti_cl_sexo == "_T") |> 
  # limpiar fecha
  mutate(año = str_extract(trimestre_movil, "\\d{4}") |> as.numeric()) |> 
  mutate(mes = str_extract(trimestre_movil, "\\w+-\\w+"),
         mes1 = str_extract(mes, "\\w+(?=-)"),
         mes2 = str_extract(mes, "(?<=-)\\w+")) |> 
  mutate(mes1 = recode(mes1,
                       "ene" = "1", "jan" = "1", "feb" = "2",
                       "mar" = "3", "abr" = "4", "apr" = "4", "may" = "5",
                       "jun" = "6", "jul" = "7", "ago" = "8",
                       "aug" = "8", "sep" = "9", "sept" = "9", "oct" = "10",
                       "nov" = "11", "dic" = "12", "dec" = "12") |> as.numeric(),
         mes2 = recode(mes2,
                       "ene" = "1", "jan" = "1", "feb" = "2",
                       "mar" = "3", "abr" = "4", "apr" = "4", "may" = "5",
                       "jun" = "6", "jul" = "7", "ago" = "8",
                       "aug" = "8", "sep" = "9", "sept" = "9", "oct" = "10",
                       "nov" = "11", "dic" = "12", "dec" = "12") |> as.numeric()) |> 
  mutate(mes_intermedio = mes2-1,
         mes_intermedio = case_when(mes1 == 11 & mes2 == 1 ~ 12, 
                                    mes1 == 12 & mes2 == 2 ~ 1, 
                                    .default = mes_intermedio)) |> 
  mutate(fecha = ymd(paste(año, mes_intermedio, 15)))


participacion_sexo <- participacion |> 
  filter(region == "Total país", sexo != "Ambos sexos")

participacion_region <- participacion |> 
  filter(region != "Total país", sexo == "Ambos sexos")
  

participacion <- participacion |> 
  filter(region == "Total país",
         sexo == "Ambos sexos") |> 
  # calcular variación mensual
  arrange(fecha) |> 
  mutate(variacion = valor/lag(valor)-1,
         variacion = replace_na(variacion, 0))
  

participacion_region |> 
  ggplot(aes(fecha, valor, col = region)) +
  geom_line() +
  theme(legend.position = "none")

participacion_sexo |> 
  ggplot(aes(fecha, valor, col = sexo)) +
  geom_line() +
  theme(legend.position = "none")

participacion |> 
  ggplot(aes(fecha, valor)) +
  geom_line()

participacion |> 
ggplot(aes(fecha, variacion)) +
  geom_line()

# guardar ----
arrow::write_parquet(participacion, "resultados/ine_participacion.parquet")