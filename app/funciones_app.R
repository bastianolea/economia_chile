
prop_a_porcentaje <- function(x, decimales = 2) {
  x_porcentaje <- round(100*(x-1), decimales)
  
  x_porcentaje_texto <- format(x_porcentaje, decimal.mark = ",", big.mark = ".")
  
  x_porcentaje_texto_2 <- paste0(x_porcentaje_texto, "%")
  
  return(x_porcentaje_texto_2)
}

numeric_a_mes <- function(x) {
  recode_factor(x, 
                "1" = "ene",
                "2" = "feb",
                "3" = "mar",
                "4" = "abr",
                "5" = "may",
                "6" = "jun",
                "7" = "jul",
                "8" = "ago",
                "9" = "sep",
                "10" = "oct",
                "11" = "nov",
                "12" = "dic")
}

miles <- function(x) {
  scales::comma(x, big.mark = ".", decimal.mark = ",", trim = TRUE)
}

porcentaje <- function(x) {
  paste0(format(x, big.mark = ".", decimal.mark = ","), "%")
}




flechita <- function(x, juicio = "bueno") {
  # browser()
  flechita <- case_when(x < 1 ~ "▼",
                        x == 1 ~ "=",
                        x > 1 ~ "▲")
  
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
  
  color <- case_when(x < 1 ~ color_bajar,
                     x == 1 ~ color_neutro,
                     x > 1 ~ color_subir)
  
  div(flechita,
      style = paste("font-size: 100%; color:", color))
}


porcentaje_flechita <- function(dato, juicio = "bueno") {
  div(style = "margin: 0; white-space: nowrap;",
      div(style = "display: inline-block; vertical-align:middle; margin-left: -3px; margin-right: -3px; margin-bottom: 2px;", 
          flechita(dato, juicio = juicio)),
      div(style = "display: inline-block; vertical-align:middle; font-size: 200%; text-align: center;",
          prop_a_porcentaje(dato)
      )
  )
}



formateador_cifra <- function(dato, unidad = "miles de millones", texto = 2018) {
  if (unidad == "miles de millones") {
    p(paste0("$", miles(dato)), "(miles de millones)")
    
  } else if (unidad == "porcentaje") {
    p(porcentaje(dato), paste0("(% respecto a ", texto, ")"))
    
  } else if (unidad == "porciento") {
    p(porcentaje(dato), paste0("(", texto, ")"))
    
  } else if (unidad == "pesos") {
    p(paste0("$", miles(dato)), "(pesos)")
    
  } else if (unidad == "índice 1000") {
    p(paste0(miles(dato), " (índice: ", texto, " = 1.000)"))
    
  } else if (unidad == "índice 100") {
    p(paste0(miles(dato), " (índice: ", texto, " = 100)"))
  } else {
    p(paste0("$", miles(dato)), "(miles de millones)")
  }
}

# calculos ----
calcular_metricas <- function(datos) {
  # browser()
  
  # ordenar
  datos <- datos |> 
    group_by(serie) |> 
    arrange(desc(año), desc(mes)) |> 
    mutate(fecha = as_date(fecha))
  
  # ultimos x años
  datos_recientes <- datos |> 
    filter(fecha >= max(fecha) %m-% months(12*3))
  
  ultimo <- datos |> slice(1)
  penultimo <- datos |> slice(2)
  
  hace_un_año <- datos |> 
    filter(fecha <= max(fecha) %m-% months(12)) |> 
    slice(1)
  
  hace_dos_años <- datos |> 
    filter(fecha <= max(fecha) %m-% months(24)) |> 
    slice(1)
  
  # cambio porcentual entre cada fecha
  variacion <- datos_recientes |> 
    mutate(valor = valor/lead(valor))
  
  # cambio porcentual entre ultimo valor y el anterior
  cambio <- variacion |> 
    slice(1)
  
  # cambio_texto <- prop_a_porcentaje(cambio$cambio)
  
  output <- list("datos" = datos_recientes,
                 "variacion" = variacion,
                 "cambio" = cambio,
                 "ultimo" = ultimo,
                 "penultimo" = penultimo,
                 "hace_un_año" = hace_un_año,
                 "hace_un_año_p" = ultimo$valor/hace_un_año$valor,
                 "hace_dos_años" = hace_dos_años
  )
  
  return(output)
}

# interfaces ----
dato_ui <- function(datos_pib, unidad = "miles de millones", año_base = 2018, subir = "bueno") {
  div(
    div(style = "display: flex;",
        div(style = "flex: 1; border: 2px solid red;",
            
            div(style = "min-height: 70px;",
                strong("Valor actual"),
                p(formateador_cifra(datos_pib$ultimo$valor, unidad, año_base),
                  style = "margin-top: -8px; font-size: 9%;")
            ),
            
            
            porcentaje_flechita(datos_pib$cambio$valor, juicio = subir),
            p(class = "texto-bajo-porcentaje", 
              "versus cifra anterior", 
              paste0("(", format(datos_pib$penultimo$fecha, "%m/%y"), ")"))
        ),
        
        div(style = "flex: 1; margin-left: 6px; border: 2px solid red;",
            
            div(style = "min-height: 70px;",
                strong("Hace un año"),
                p(formateador_cifra(datos_pib$hace_un_año$valor, unidad, año_base),
                  style = "margin-top: -8px; font-size: 90%;")
            ),
            
            porcentaje_flechita(datos_pib$hace_un_año_p, juicio = subir),
            p(class = "texto-bajo-porcentaje", 
              "versus hace 12 meses",
              paste0("(", format(datos_pib$hace_un_año$fecha, "%m/%y"), ")"))
        )
    )
  )
}

# paneles ----
fila_indicador <- function(...) {
  fluidRow(
    column(12, style = "border: 2px solid red; padding: 12px;",
           ...
    )
  )
}

panel <- function(width, ...) {
  column(width, class = "outer-panel",
         div(class = "panel",
             ...
         )
  )
}

panel_vacio <- function(width, ...) {
  column(width, class = "outer-panel",
         div(class = "panel-vacio",
             ...
         )
  )
}

panel_titular <- function(titulo, subtitulo) {
  div(style = "margin-bottom: 12px; line-height: 1.1;",
      h4(titulo),
      em(subtitulo, style = "font-size: 95%;")
  )
}


panel_cuadro_resumen <- function(output) {
  panel(4, 
        div(
          htmlOutput(output)
        ))
}




panel_grafico_variacion <- function(titulo, output) {
  panel(8, 
        div(
          h4(titulo),
          plotOutput(output, height = 240)
        )
  )
}


# gráficos ----
grafico_variacion <- function(dato, escala = "mensual", subir = "bueno", 
                              #color_fondo = "#808080"
                              color_fondo = color_paneles
                              ) {
  
  if (subir == "bueno") {
    color_subir = "green"
    color_bajar = "red"
    color_neutro = "blue"
  } else if (subir == "malo") {
    color_subir = "red"
    color_bajar = "green"
    color_neutro = "blue"
  } else if (subir == "neutro") {
    color_subir = "blue"
    color_bajar = "blue"
    color_neutro = "blue"
  }
  
  dato_2 <- dato$variacion |> 
    ungroup() |> 
    slice(1:12) |> 
    mutate(valor = valor-1) |> 
    mutate(direccion = ifelse(valor > 0, "Aumento", "Disminución"),
           direccion = ifelse(round(valor, 4) == 0.0, "Igual", direccion))
  
  if (escala == "trimestre") {
    # browser()
    etiquetas_trimestre <- paste(dato_2$año, dato_2$trimestre, sep = "/")
  }
  
  # para que el espaciado sea equivalente a la barra más alta * x, y así la línea esté al medio
  alto_max <- max(abs(dato_2$valor))*1.2
  lineas_punteadas = 0.05
  espaciado_vertical_texto = 0.1
  tamaño_texto = 3
  decimales_texto = 0.1
  
  n_aumentos = length(dato_2$direccion[dato_2$direccion == "Aumento"])
  n_disminuciones = length(dato_2$direccion[dato_2$direccion == "Disminución"])
  gradiente_posicion = ifelse(n_aumentos > n_disminuciones, "arriba", "abajo")
  color_gradiente = if_else(n_aumentos > n_disminuciones, color_subir, color_bajar)
  colores_gradiente = c(color_gradiente, color_fondo)
  colores_gradiente_pos = if_else(gradiente_posicion == "arriba", list(colores_gradiente), list(rev(colores_gradiente)))
  posicion_y_gradiente = if_else(gradiente_posicion == "arriba", alto_max*0.8, -alto_max*0.8)
  
  plot <- dato_2 |>
    # dato_2 |> 
    ggplot(aes(fecha, valor, fill = direccion)) +
    geom_ribbon(aes(x = c(fecha[1]+60, fecha[2:11], fecha[12]-60),
                    ymin = 0, ymax = posicion_y_gradiente), #max(valor)), 
                fill = grid::linearGradient(
                  colours = colores_gradiente_pos[[1]],
                  x1 = unit(0, "npc"), y1 = unit(0, "npc"),
                  x2 = unit(0, "npc"), y2 = unit(1, "npc")
                ), alpha = 0.4) +
    geom_hline(yintercept = lineas_punteadas, linetype = "dashed", alpha = 0.2) +
    geom_hline(yintercept = -lineas_punteadas, linetype = "dashed", alpha = 0.2) +
    geom_col(color = "grey40") +
    geom_hline(yintercept = 0) +
    geom_text(data = ~filter(.x, direccion == "Aumento"),
              aes(y = valor+(mean(valor)*espaciado_vertical_texto), 
                  label = scales::percent(valor, big.mark = ".", decimal.mark = ",", accuracy = decimales_texto)), 
              vjust = 0, size = tamaño_texto, check_overlap = T) +
    geom_text(data = ~filter(.x, direccion == "Disminución"),
              aes(y = valor+(mean(valor)*espaciado_vertical_texto), 
                  label = scales::percent(valor, big.mark = ".", decimal.mark = ",", accuracy = decimales_texto)),
              vjust = 1, size = tamaño_texto, check_overlap = T) +
    scale_x_date(date_breaks = "months", labels = ~numeric_a_mes(month(.x))) +
    scale_y_continuous(limits = c(-alto_max,
                                  alto_max)) +
    scale_fill_manual(values = c("Aumento" = color_subir,
                                 "Disminución" = color_bajar,
                                 "Igual" = color_neutro), 
                      aesthetics = c("color", "fill")) +
    theme_void() +
    theme(axis.text.x = element_text(),
          plot.margin = margin(l = 15, r = 15, b = 4),
          legend.position = "none") +
    theme(plot.background = element_rect(fill = color_fondo, color = color_fondo),
          panel.background = element_rect(fill = color_fondo, color = color_fondo))
  
  if (escala == "trimestre") {
    plot <- plot +
      scale_x_continuous(breaks = dato_2$mes, labels = etiquetas_trimestre)
    # scale_x_date(date_breaks = "months", labels = etiquetas_trimestre)
  }
  
  return(plot)
}