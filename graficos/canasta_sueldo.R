library(dplyr)
library(rvest)
library(stringr)
library(tidyr)
library(ggplot2)
thematic::thematic_on()

source("funciones.R")

pib <- obtener_pib()
ipc <- obtener_ipc()
canasta <- obtener_canasta()
sueldo <- obtener_sueldo_minimo()

sueldo

pib_2 <- pib |> 
  filter(serie == "PIB a precios corrientes") |> 
  select(fecha, valor, serie)

ipc_2 <- ipc |> 
  filter(serie == "Índice IPC General") |> 
  select(fecha, valor, serie)

canasta_2 <- canasta |> 
  select(fecha, valor, serie = variable)

sueldo_2 <- sueldo |> 
  select(fecha, valor = monto_bruto) |> 
  mutate(serie = "Sueldo mínimo")


datos <- bind_rows(#pib_2,
          # ipc_2,
          canasta_2,
          sueldo_2) |> 
  mutate(fecha = as.Date(fecha))


datos |> 
  filter(fecha >= "2018-01-01") |>
  mutate(serie = recode(serie, 
                        "CBA" = "Canasta Básica de Alimentos",
                        "LP" = "Línea de pobreza",
                        "LPE" = "Línea de pobreza extrema")) |> 
  ggplot(aes(fecha, valor, color = serie, fill = serie)) +
  geom_line(linewidth = 1.3) +
  # geom_point() +
  scale_y_continuous(labels = ~scales::comma(.x, big.mark = ".")) +
  scale_x_date(date_breaks = "years", date_labels = "%Y")


datos_prop <- datos |> 
  filter(serie == "CBA" | serie == "Sueldo mínimo") |> 
  group_by(serie) |> 
  arrange(serie, fecha) |> 
  pivot_wider(names_from = serie, values_from = valor) |> 
  janitor::clean_names() |> 
  fill(sueldo_minimo, .direction = "downup") |> 
  mutate(proporcion = cba/sueldo_minimo) |> 
  filter(fecha >= "2018-01-01")
  
fecha_boric = as.Date("2022-03-11")
fecha_piñera = as.Date("2018-03-11")

dato_ultimo <- datos_prop |> 
  filter(!is.na(proporcion)) |> 
  filter(fecha == max(fecha))

dato_max <- datos_prop |> 
  filter(!is.na(proporcion)) |> 
  filter(proporcion == max(proporcion))

dato_min <- datos_prop |> 
  filter(!is.na(proporcion)) |> 
  filter(proporcion == min(proporcion))

datos_prop |> 
  ggplot(aes(fecha, proporcion)) +
  # presidentes
  geom_vline(xintercept = fecha_piñera, color = "gray30") +
  annotate("text", x = fecha_piñera+30, y = 0.16, 
           label = "Piñera", hjust = 0) +
  geom_vline(xintercept = fecha_boric, color = "gray30") +
  annotate("text", x = fecha_boric+30, y = 0.16, 
           label = "Boric", hjust = 0) +
  # datos
  geom_line() +
  geom_point() +
  scale_y_continuous(expand = expansion(.2),
                     labels = ~scales::percent(.x, decimal.mark = ",")) +
  scale_x_date(expand = expansion(c(0.05, .1)), minor_breaks = NULL,
               date_breaks = "years", date_labels = "%Y") +
  #porcentajes
  annotate("text", x = dato_ultimo$fecha+30, y = dato_ultimo$proporcion, 
           label = scales::percent(dato_ultimo$proporcion), hjust = 0) +
  annotate("text", x = dato_max$fecha+30, y = dato_max$proporcion, 
           label = scales::percent(dato_max$proporcion), hjust = 0) +
  annotate("text", x = dato_min$fecha+30, y = dato_min$proporcion, 
           label = scales::percent(dato_min$proporcion), hjust = 0) +
  labs(y = "Valor de la canasta básica de alimentos como porcentaje del sueldo mínimo")


datos_prop_2 <- datos_prop |> 
  pivot_longer(cols = 2:4, names_to = "serie", values_to = "valor") 

datos_prop_2 |> 
  ggplot(aes(fecha, valor)) +
  # presidentes
  geom_vline(xintercept = fecha_piñera, color = "gray30") +
  annotate("text", x = fecha_piñera+30, y = 0.16, 
           label = "Piñera", hjust = 0) +
  geom_vline(xintercept = fecha_boric, color = "gray30") +
  annotate("text", x = fecha_boric+30, y = 0.16, 
           label = "Boric", hjust = 0) +
  # datos
  geom_point() +
  geom_line() +
  facet_grid(rows = vars(serie), scales = "free_y")
