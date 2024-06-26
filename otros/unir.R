library(tidyverse)
library(slider)

resultados <- fs::dir_info("resultados") |> 
  filter(str_detect(path, "parquet"))


datos <- map(resultados$path, ~{
  arrow::read_parquet(.x) |> 
    select(fecha, valor, any_of("variacion"), any_of("variable")) |> 
    mutate(dato = str_extract(.x, "(?<=resultados/)\\w+(?=\\.parquet)"))
})


economia <- datos |> 
  list_rbind() |> 
  mutate(variable = if_else(is.na(variable), dato, variable)) |> 
  relocate(dato, .before = 1) |> 
  filter(variable != "Velocidad de expansión (porcentaje), referencia 2018")


variables = c("Dólar observado ($CLP/USD)",
              "Imacec empalmado, serie original (índice 2018=100)", "bc_ipc", 
              "bc_ipsa", "PIB a precios corrientes",
              "UF", "ine_prod_ind", "ine_remun_nom")

economia |> 
  filter(variable == "ine_prod_ind") |> 
  arrange(desc(fecha)) |> 
  ggplot(aes(fecha, valor)) +
  geom_line()
  

economia |> 
  filter(variable %in% c(variables)) |> 
  # group_by(variable) |> 
  # mutate(variacion = slide_dbl(variacion, mean, .before = 2)) |>
  filter(year(fecha) >= 2022) |>
  ggplot(aes(fecha, valor, color = variable)) +
  # geom_hline(yintercept = 0, alpha = .2) +
  geom_line(linewidth = 1) +
  geom_point(data = ~filter(.x, fecha == max(fecha)),
             size = 3) +
  geom_text(data = ~filter(.x, fecha == max(fecha)),
            aes(label = paste("  ", variable)), hjust = 0, size = 2.5) +
  # geom_segment(data = ~filter(.x, fecha == max(fecha)),
  #              aes(yend = 0)) +
  # scale_y_continuous(limits = c(-0.2, 0.2)) +
  scale_x_date(date_breaks = "months", date_labels = "%m") +
  theme(legend.position = "bottom") +
  guides(color = guide_legend(ncol = 2)) +
  facet_wrap(~variable, ncol = 2, scales = "free_y")
