desocupados <- readRDS("datos/desocupados.rds")

datos_desocupados <- desocupados |> 
  calcular_metricas()

datos_desocupados$datos


if (juicio == "bueno") {
  color_subir = "green"
  color_bajar = "red"
  color_neutro = "blue"
} else if (juicio == "malo") {
  color_subir = "red"
  color_bajar = "green"
  color_neutro = "blue"
} else if (juicio == "neutro") {
  color_subir = "blue"
  color_bajar = "blue"
  color_neutro = "blue"
}


datos_desocupados$variacion |> 
  slice(1:12) |> 
  mutate(valor = valor-1) |> 
  mutate(direccion = ifelse(valor > 0, "Aumento", "Disminución"),
         direccion = ifelse(round(valor, 4) == 0.0, "Igual", direccion)) |> 
  # mutate(mes = numeric_a_mes(mes)) |>
  ggplot(aes(fecha, valor, fill = direccion)) +
  geom_col() +
  geom_hline(yintercept = 0) +
  geom_text(data = ~filter(.x, direccion == "Aumento"),
            aes(y = valor+(mean(valor)*0.1), label = scales::percent(valor, big.mark = ".", decimal.mark = ",", accuracy = 0.01)), 
            vjust = 0, size = 3, check_overlap = T) +
  geom_text(data = ~filter(.x, direccion == "Disminución"),
            aes(y = valor+(mean(valor)*0.1), label = scales::percent(valor, big.mark = ".", decimal.mark = ",", accuracy = 0.1)),
            vjust = 1, size = 3, check_overlap = T) +
  scale_x_date(date_breaks = "months",labels = ~numeric_a_mes(month(.x))) +
  scale_y_continuous(expand = expansion(c(0.1, 0.1)), breaks = c(-0.1, 0, 0.1)) +
  scale_fill_manual(values = c("Aumento" = color_subir,
                               "Disminución" = color_bajar,
                               "Igual" = color_neutro), 
                    aesthetics = c("color", "fill")) +
  theme_void() +
  theme(axis.text.x = element_text(),
        plot.margin = margin(l = 15, r = 15, b = 4),
        legend.position = "none")
  
