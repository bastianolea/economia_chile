library(tidyverse)

dato <- arrow::read_parquet("resultados/bc_ipc.parquet")

#fecha de hoy, hace un año
hace_1_año <- floor_date((today() %m-% years(1)), unit = "months")

este_año <- year(today())

#dato más reciente
dato |> 
  filter(fecha == max(fecha))

#esta fecha pero del año pasado
dato |> 
  filter(fecha == hace_1_año)


# este año
dato |> 
  filter(year(fecha) == este_año)

# año pasado
dato |> 
  filter(year(fecha) == este_año-1)

# año ante pasado
dato |> 
  filter(year(fecha) == este_año-2)

# desde los últimos 12 meses
dato |> 
  filter(fecha >= max(fecha) %m-% months(12))

# 12 meses atrás, hacia atrás
dato |> 
  filter(fecha <= max(fecha) %m-% months(12)) |> 
  arrange(desc(fecha))




# tendencia ultimos 12 meses
dato |> 
  filter(fecha >= max(fecha) %m-% months(12)) |> 
  summarize(mean(variacion))

dato |> 
  filter(fecha <= max(fecha) %m-% months(12)) |> 
  slice_max(fecha, n = 12) |> 
  summarize(mean(variacion))
