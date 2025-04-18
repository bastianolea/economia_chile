css <- function(...) {
  tags$style(HTML(...))
}

notificacion <- function(titulo, subtitulo) {
  showNotification(ui = div(p(titulo), 
                            p(subtitulo, 
                              style = "margin-top: -10px; margin-bottom: -4px; opacity: 0.5;")), 
                   duration = 3)
}

cargar_datos_web <- function(archivo = "pib", descargar = FALSE, local = FALSE) {
  
  #carga el dato desde github, y si no se puede por algún motivo, desde local 
  if (descargar == TRUE) {
    url = paste0("https://github.com/bastianolea/economia_chile/raw/main/app/datos/", archivo, ".csv")
    
    message("cargando desde url: ", url) 
    notificacion("Cargando datos remotos del Banco Central:", archivo)
    
    data <- try(read_csv2(url))
    
    # si falla, intentar cargar archivo local
    if ("try-error" %in% class(data)) {
      path = paste0("datos/", archivo, ".csv")
      message("cargando desde archivo local: ", path)
      data <- try(read_csv2(path))
      
    } else if (length(data) == 0) {
      path = paste0("datos/", archivo, ".csv")
      message("cargando desde archivo local: ", path)
      data <- try(read_csv2(path))
    }
    
    if ("try-error" %in% class(data)) {
      data <- tibble()
    }
    
    # si se especifica, cargar dato local
  } else {
    path = paste0(ifelse(local == TRUE, "app/", ""), "datos/", archivo, ".csv")
    message("cargando desde archivo local: ", path)
    if (local == FALSE) notificacion("Cargando datos pre-guardados:", archivo)
    
    data <- try(read_csv2(path))
  }
  
  return(data)
}


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

fecha_redactada <- function(fecha, diario = FALSE) {
  if (diario == FALSE) {
    paste(numeric_a_mes(month(fecha)), format(fecha, "%Y"))
  } else if (diario == TRUE) {
    paste0(format(fecha, "%d"), "/", numeric_a_mes(month(fecha)), "/", format(fecha, "%Y"))
  }
}

mes_a_trimestre <- function(x) {
  recode(as.character(x),
         "3" = "I",
         "6" = "II",
         "9" = "III" ,
         "12" = "IV")
}

miles <- function(x) {
  scales::comma(x, big.mark = ".", decimal.mark = ",", trim = TRUE)
}

porcentaje <- function(x) {
  paste0(format(x, big.mark = ".", decimal.mark = ","), "%")
}




flechita <- function(x, juicio = "bueno") {
  # browser()
  flechita <- case_when(round(x, 3) < 1 ~ "▼",
                        round(x, 3) == 1 ~ "=",
                        round(x, 3) > 1 ~ "▲")
  
  if (juicio == "bueno") {
    color_subir = color_positivo
    color_bajar = color_negativo
    color_neutro = color_neutro
  } else if (juicio == "malo") {
    color_subir = color_negativo
    color_bajar = color_positivo
    color_neutro = color_neutro
  } else if (juicio == "neutro") {
    color_subir = color_neutro
    color_bajar = color_neutro
    color_neutro = color_neutro
  }
  
  color <- case_when(x < 1 ~ color_bajar,
                     x == 1 ~ color_neutro,
                     x > 1 ~ color_subir)
  
  # ajuste_flecha_abajo <- ifelse(x < 1, "margin-top: 10px;", "")
  ajuste_flecha_abajo <- ifelse(x < 1, "margin-bottom: -10px;", "")
  # ajuste_flecha_abajo <- ""
  
  div(flechita,
      style = paste("font-size: 110%;", ajuste_flecha_abajo, "color:", color, ";"))
}


porcentaje_flechita <- function(dato, juicio = "bueno") {
  div(style = "margin: 0; white-space: nowrap;",
      div(style = "display: inline-block; vertical-align:middle; margin-left: -3px; margin-right: 0px; margin-bottom: 2px;", 
          flechita(dato, juicio = juicio)),
      div(style = "display: inline-block; vertical-align:middle; font-size: 200%; text-align: center;",
          prop_a_porcentaje(dato)
      )
  )
}



formateador_cifra <- function(dato, unidad = "miles de millones", texto = 2018) {
  if (unidad == "miles de millones") {
    paste(paste0("$", miles(dato)), "(miles de millones)")
    
  } else if (unidad == "porcentaje") {
    paste(porcentaje(dato), paste0("(% respecto a ", texto, ")"))
    
  } else if (unidad == "porciento") {
    paste(porcentaje(dato), paste0("(", texto, ")"))
    
  } else if (unidad == "pesos") {
    paste(paste0("$", miles(dato)), "(pesos)")
    
  } else if (unidad == "índice 1000") {
    paste(paste0(miles(dato), " (índice: ", texto, " = 1.000)"))
    
  } else if (unidad == "índice 100") {
    paste(paste0(miles(dato), " (índice: ", texto, " = 100)"))
    
    # } else if (unidad == "millones de dólares") {
    #   paste(paste0("$", miles(dato)), "(millones de dólares)")
    #   
  } else if (unidad == "dólares por libra") {
    paste(paste0("$", format(round(dato, 2), big.mark = ".", decimal.mark = ",")), 
          "(dólares por libra)")
    
  } else {
    # paste(paste0("$", miles(dato)), "(miles de millones)")
    paste(paste0("$", miles(dato)), paste0("(", unidad, ")"))
  }
}

# calculos ----
calcular_metricas <- function(datos, media_movil = NULL) {
  # browser()
  # datos <- invext
  # ordenar
  datos <- datos |> 
    group_by(serie) |> 
    # arrange(desc(año), desc(mes)) |> 
    mutate(fecha = as_date(fecha)) |> 
    arrange(desc(fecha))
  
  if (!is.null(media_movil)) {
   datos <- datos |> 
     mutate(valor = slider::slide_dbl(valor, .after = media_movil, mean))
  }
  
  # ultimos x años
  datos_recientes <- datos |> 
    filter(fecha >= max(fecha) %m-% months(12*3))
  
  ultimo <- datos |> slice(1)
  penultimo <- datos |> slice(2)
  
  hace_un_año <- datos |> 
    filter(fecha <= max(fecha) %m-% months(12)) |> 
    slice(1)
  
  # hace_dos_años <- datos |> 
  #   filter(fecha <= max(fecha) %m-% months(24)) |> 
  #   slice(1)
  
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
                 "hace_un_año_p" = ultimo$valor/hace_un_año$valor
                 # "hace_dos_años" = hace_dos_años
  )
  
  return(output)
}


# interfaces ----
dato_ui <- function(datos_pib, 
                    unidad = "miles de millones", 
                    año_base = 2018, 
                    subir = "bueno",
                    diario = FALSE
) {
  
  estilo_texto_izquierdo = "flex: 1; font-size: 110%; text-align: right; 
                            padding-top: 8px; margin-bottom: 0; margin-right: 18px;"
  
  estilo_cifra_izquierda = "font-size: 120%; margin-top: 0px; line-height: 1.1;"
  
  estilo_texto_derecha = "margin-left: 10px; margin-top: -6px; margin-right: 0; padding-right: 0;"
  
  output <- div(
    # fila 1
    div(style = "display: flex; margin-bottom: -4px;",
        # cuadro izq
        div(style = estilo_texto_izquierdo,
            strong("Valor actual"),
            p(style = estilo_cifra_izquierda,
              formateador_cifra(datos_pib$ultimo$valor, unidad, año_base))
        ),
        # cuadro der
        div(style = "flex: 1;",
            porcentaje_flechita(datos_pib$cambio$valor, juicio = subir),
            p(style = estilo_texto_derecha,
              "versus cifra anterior",
              paste0("(", fecha_redactada(datos_pib$penultimo$fecha, diario), ")")
            )
        )
    ),
    # fila 2
    div(style = "display: flex;",
        # cuadro izq
        div(style = estilo_texto_izquierdo,
            strong("Hace un año"),
            p(style = estilo_cifra_izquierda,
              formateador_cifra(datos_pib$hace_un_año$valor, unidad, año_base))
        ),
        # cuadro der
        div(style = "flex: 1;",
            porcentaje_flechita(datos_pib$hace_un_año_p, juicio = subir),
            p(style = estilo_texto_derecha,
              "versus hace un año",
              paste0("(", fecha_redactada(datos_pib$hace_un_año$fecha, diario), ")")
            )
        )
    )
  )
  return(output)
}

variacion_corte_fecha <- function(datos, fecha_corte, input) {
  req(datos)
  req(fecha_corte)
  
  datos_variacion <- datos$variacion |> 
    filter(fecha >= fecha_corte) 
  
  # correción para indicadores que se cortan antes de tener alguna medición
  if (nrow(datos_variacion) == 0 & input$fecha_corte == "3 meses") {
    # browser()
    # if (unique(datos$variacion$serie) == "PIB a precios corrientes") {
    datos_variacion <- datos$variacion |> 
      slice(1)
    # }
  }
  
  # se aplica redondeo para simplificar comparaciones, para que un 0.01 no sea reducción, sino que se vea como 0 (para cambiarlo hay que ajustar la accuracy del scales::percent mas abajo en esta función)
  variacion <- mean(datos_variacion$valor, na.rm = T) |> round(3)
  return(variacion)
}

tendencia_ui <- function(datos, fecha_corte, input, subir = "bueno") {
  # req(datos)
  # req(fecha_corte)
  # 
  # fecha_corte <- fecha_corte()
  # datos <- datos_prod_industrial
  # datos_variacion <- datos$variacion |> 
  #   filter(fecha >= fecha_corte) 
  # 
  # # correción para indicadores que se cortan antes de tener alguna medición
  # if (nrow(datos_variacion) == 0 & input$fecha_corte == "3 meses") {
  #   # browser()
  #   # if (unique(datos$variacion$serie) == "PIB a precios corrientes") {
  #   datos_variacion <- datos$variacion |> 
  #     slice(1)
  #   # }
  # }
  # 
  # # se aplica redondeo para simplificar comparaciones, para que un 0.01 no sea reducción, sino que se vea como 0 (para cambiarlo hay que ajustar la accuracy del scales::percent mas abajo en esta función)
  # variacion <- mean(datos_variacion$valor, na.rm = T) |> round(3)
  # 
  # browser()
  variacion <- variacion_corte_fecha(datos, fecha_corte, input)
  
  tendencia <- calcular_tendencia(variacion) #en texto
  
  # determinar si subir es bueno o malo
  if (subir == "bueno") {
    color_subir = color_positivo
    color_bajar = color_negativo
    color_neutro = color_neutro
  } else if (subir == "malo") {
    color_subir = color_negativo
    color_bajar = color_positivo
    color_neutro = color_neutro
  } else if (subir == "neutro") {
    color_subir = color_neutro
    color_bajar = color_neutro
    color_neutro = color_neutro
  }
  
  # dependiendo de si subir es bueno o malo, poner el color si sube o baja
  if (tendencia == "aumentó") {
    color_texto = color_subir
  } else if (tendencia == "disminuyó") {
    color_texto = color_bajar
  } else if (tendencia == "se mantuvo") {
    color_texto = color_neutro
  }
  
  # aplicar color a la palabra
  tendencia_2 <- p(tendencia, style = paste("color:", color_texto, ";"))
  
  # flechita
  flecha <- flechita(variacion, juicio = subir)
  
  # variación en porcentaje
  tendencia_cifra <- scales::percent(variacion-1, big.mark = ".", decimal.mark = ",", accuracy = 0.1)
  
  if (variacion-1 == 0.00) {
    tendencia_cifra = "0%"
  }
  
  # artículo conector
  if (tendencia == "aumentó") {
    articulo = "un"
  } else if (tendencia == "disminuyó") {
    articulo = "un"
  } else if (tendencia == "se mantuvo") {
    articulo = "en"
  }
  
  # crear html
  out <- div(
    div(
      div(flecha, style = "display: inline-block;"),
      div(tendencia_2, style = "display: inline-block;"),
      div(articulo, style = "display: inline-block;"),
      div(tendencia_cifra, style = "display: inline-block;")
    )
    # 
    # div(em("Esto significa que..."))
  )
  
  return(out)
}

calcular_tendencia <- function(variacion) {
  if (variacion > 1) {
    tendencia = "aumentó"
  } else if (variacion == 1) {
    tendencia = "se mantuvo"
  } else if (variacion < 1) {
    tendencia = "disminuyó"
  }
  return(tendencia)
}

tendencia_texto <- function(datos, fecha_corte, input) {
  # browser()
  
  # sacar los datos
  dato <- datos$datos$dato[1]
  variacion <- variacion_corte_fecha(datos, fecha_corte, input)
  tendencia <- calcular_tendencia(variacion)
  
  
  if (dato == "pib") {
    texto <- case_when(tendencia == "aumentó" ~ "Significa que la economía del país está creciendo en la producción, inversión, y prestación de servicios",
                       tendencia == "disminuyó" ~ "Significa que la economía está decreciendo, debido a una baja en al producción, la inversión, o la prestación de servicios",
                       tendencia == "se mantuvo" ~ "Esto significa que la producción, inversión y prestación de servicios se ha mantenido"
    )
  } else if (dato == "imacec") {
    texto <- case_when(tendencia == "aumentó" ~ "Significa que la actividad de los distintos sectores de la economía del país está en aumento",
                       tendencia == "disminuyó" ~ "Significa que la actividad económica del país está bajando",
                       tendencia == "se mantuvo" ~ "Significa que la actividad económica del país se ha mantenido en los mismos niveles"
    )
  } else if (dato == "ipc") {
    texto <- case_when(tendencia == "aumentó" ~ "Indica que los precios de los bienes y servicios más comunes han subido",
                       tendencia == "disminuyó" ~ "Indica que los precios de los bienes y servicios más comunes están bajando",
                       tendencia == "se mantuvo" ~ "Esto indica que los precios de los bienes y servicios más comunes se han mantenido"
    )
  } else if (dato == "inversion_extranjera") {
    texto <- case_when(tendencia == "aumentó" ~ "Significa que las empresas extranjeras están creando nuevas empresas en el país, y aumentando su inversión en empresas chilenas",
                       tendencia == "disminuyó" ~ "Significa que empresas extranjeras están invirtiendo menos en empresas o creación de empresas en el país",
                       tendencia == "se mantuvo" ~ "Significa que el nivel de inversión de las empresas extranjeras en crear nuevas empresas o controlar empresas chilenas se ha mantenido"
    )
  } else if (dato == "uf") {
    texto <- case_when(tendencia == "aumentó" ~ "Refleja que el precio de bienes y servicios ha subido, por lo que la UF se alinea a estos aumentos",
                       tendencia == "disminuyó" ~ "Refleja que el precio de bienes y servicios ha bajado, por lo que la UF se alinea a esta disminución",
                       tendencia == "se mantuvo" ~ "Refleja que el precio de bienes y servicios se ha mantenido estable"
    )
  } else if (dato == "ipsa") {
    texto <- case_when(tendencia == "aumentó" ~ "Significa que mejoró el desempeño de las acciones de las mayores compañías del país, reflejando crecimiento económico",
                       tendencia == "disminuyó" ~ "Significa que el desempeño de las acciones de las mayores compañías del país está empeorando, indicando recesión económica",
                       tendencia == "se mantuvo" ~ "Significa que el desempeño de las acciones de las mayores compañías del país se ha mantenido estable"
    )
  } else if (dato == "desempleo") {
    texto <- case_when(tendencia == "aumentó" ~ "Significa que ha aumentado la cantidad de personas sin trabajo",
                       tendencia == "disminuyó" ~ "Significa que hay más puestos de trabajo en Chile, y por lo tanto está disminuyendo la cantidad de personas sin trabajo",
                       tendencia == "se mantuvo" ~ "Significa que la cantidad de chilenos/as sin trabajo se ha mantenido"
    )
  } else if (dato == "remuneraciones") {
    texto <- case_when(tendencia == "aumentó" ~ "Significa que los ingresos de los chilenos están aumentando, a pesar de la inflación",
                       tendencia == "disminuyó" ~ "Significa que los ingresos de los chilenos están bajando, producto de la inflación",
                       tendencia == "se mantuvo" ~ "Significa que los ingresos de los chilenos han aumentado lo suficiente como para anular el efecto negativo de la inflación"
    )
  } else if (dato == "precio_cobre") {
    texto <- case_when(tendencia == "aumentó" ~ "Indica que el cobre está subiendo de precio a nivel mundial, beneficiando el presupuesto del Estado y la planificación económica a futuro",
                       tendencia == "disminuyó" ~ "Indica que el precio del cobre está bajando en el mercado mundial, lo que afectará el presupuesto del Estado a futuro",
                       tendencia == "se mantuvo" ~ "Indica que el precio del cobre a nivel mundial se ha mantenido, lo que significa estabilidad para los presupuestos nacionales"
    )
  } else if (dato == "prod_industrial") {
    texto <- case_when(tendencia == "aumentó" ~ "Indica que el volumen de la producción nacional en actividades como minería, manufactura y energías está aumentando",
                       tendencia == "disminuyó" ~ "Indica que el volumen de la producción nacional en actividades como minería, manufactura y energías se ha mantenido constante",
                       tendencia == "se mantuvo" ~ "Indica que el volumen de la producción nacional en actividades como minería, manufactura y energías está disminuyendo"
    )
  }
  
  return(texto)
}


# paneles ----
fila_indicador <- function(...) {
  fluidRow(
    column(12, style = "padding: 12px; margin-bottom: 14px;",
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

panel_texto <- function(width, ...) {
  column(width, class = "outer-panel",
         div(style = "font-size: 130%;",
             #class = "panel",
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
      h3(titulo),
      em(subtitulo, style = "font-size: 130%;")
  )
}


panel_tendencia <- function(texto, ui,
                            texto_interpretacion = "Significa que...") {
  panel_texto(8,
              div(style = paste("display: inline-block; margin-right: 4px; color:", color_detalle, ";"),
                  p("⏺︎︎")
              ),
              div(style = "display: inline-block;",
                  p(texto)
              ),
              div(style = "display: inline-block;",
                  uiOutput(ui) 
              ),
              div(style = "margin-left: 20px; margin-top: -10px; 
                  font-size: 55%; opacity: 70%; line-height: 1.05;",
                  # em("Esto significa que...")
                  em(textOutput(texto_interpretacion))
              )
  )
}

panel_cuadro_resumen <- function(titulo = "Resumen", output) {
  panel(4, 
        h4(titulo, style = "margin-bottom: 4px;"),
        div(
          htmlOutput(output) |> withSpinner(proxy.height = 180)
        ))
}




panel_grafico_variacion <- function(titulo, output) {
  panel(8, 
        div(#style = "height: 220px;",
          h4(titulo),
          plotOutput(output, height = 190) |> withSpinner()
        )
  )
}


panel_grafico_historico <- function(titulo, output) {
  panel(12, 
        div(
          h4(titulo),
          plotOutput(output, height = 180) |> withSpinner()
        )
  )
}


# gráficos ----
grafico_variacion <- function(dato, escala = "mensual", subir = "bueno",
                              mensualizar = FALSE, media_movil = FALSE
                              # color_fondo = color_secundario
) {
  # dato <- datos_invext
  if (subir == "bueno") {
    color_subir = color_positivo
    color_bajar = color_negativo
    color_neutro = color_neutro
  } else if (subir == "malo") {
    color_subir = color_negativo
    color_bajar = color_positivo
    color_neutro = color_neutro
  } else if (subir == "neutro") {
    color_subir = color_neutro
    color_bajar = color_neutro
    color_neutro = color_neutro
  }
  
  # por si el dato no viene por meses, se calcula el promedio mensual
  if (mensualizar == TRUE) {
    # browser()
    dato$variacion <- dato$variacion |> 
      group_by(año, mes) |> 
      summarize(valor = mean(valor, na.rm = T),
                fecha = min(fecha), .groups = "drop")
  }
  
  dato_2 <- dato$variacion |> 
    ungroup() |> 
    arrange(desc(fecha)) |> 
    slice(1:12) |> 
    mutate(valor = valor-1) |> 
    mutate(direccion = ifelse(valor > 0, "Aumento", "Disminución"),
           direccion = ifelse(round(valor, 4) == 0.0, "Igual", direccion))
  
  # library(slider)
  # dato_2 |> 
  #   mutate(valor = slide_dbl(valor, .after = 3, mean)) |>
  #   print() |> 
  #   ggplot() +
  #   geom_col(aes(fecha, valor))
  
  if (media_movil) {
    dato_2 <- dato_2 |> 
        mutate(valor = slider::slide_dbl(valor, .after = 3, mean))
  }
  
  if (escala == "trimestre") {
    # browser()
    etiquetas_trimestre <- paste(dato_2$año, dato_2$trimestre, sep = "/")
  }
  
  # para que el espaciado sea equivalente a la barra más alta * x, y así la línea esté al medio
  valor_max <- max(abs(dato_2$valor))
  alto_max <- valor_max*1.2
  # lineas_punteadas = ifelse(valor_max > 0.02, 0.05, 0.01)
  espaciado_vertical_texto = 0.2
  tamaño_texto = 3.5
  decimales_texto = 0.1
  espaciado_meses = ifelse(escala == "trimestre", 90, 30)
  
  n_aumentos = length(dato_2$direccion[dato_2$direccion == "Aumento"])
  n_disminuciones = length(dato_2$direccion[dato_2$direccion == "Disminución"])
  gradiente_posicion = ifelse(n_aumentos > n_disminuciones, "arriba", "abajo")
  color_gradiente = if_else(n_aumentos > n_disminuciones, color_subir, color_bajar)
  colores_gradiente = c(color_gradiente, color_secundario)
  colores_gradiente_pos = if_else(gradiente_posicion == "arriba", list(colores_gradiente), list(rev(colores_gradiente)))
  posicion_y_gradiente = if_else(gradiente_posicion == "arriba", alto_max*0.8, -alto_max*0.8)
  
  plot <- dato_2 |>
    # dato_2 |> 
    ggplot(aes(fecha, valor, fill = direccion)) +
    # degradado de color transparente que cambia de posición y color según tendencia anual
    geom_ribbon(aes(x = c(fecha[1]+espaciado_meses, fecha[2:11], fecha[12]-espaciado_meses),
                    ymin = 0, ymax = posicion_y_gradiente), #max(valor)), 
                fill = grid::linearGradient(
                  colours = colores_gradiente_pos[[1]],
                  x1 = unit(0, "npc"), y1 = unit(0, "npc"),
                  x2 = unit(0, "npc"), y2 = unit(1, "npc")
                ), alpha = 0.25) +
    # geom_hline(yintercept = lineas_punteadas, color = color_secundario_detalle, linetype = "dashed") +
    # geom_hline(yintercept = -lineas_punteadas, color = color_secundario_detalle, linetype = "dashed") +
    geom_col() +
    geom_hline(yintercept = 0, linewidth = 1, color = color_fondo) +
    geom_text(data = ~filter(.x, direccion == "Aumento"),
              aes(y = valor+(mean(valor)*espaciado_vertical_texto), 
                  label = scales::percent(valor, big.mark = ".", decimal.mark = ",", accuracy = decimales_texto)), 
              color = color_fondo, vjust = 0, size = tamaño_texto, check_overlap = T) +
    geom_text(data = ~filter(.x, direccion == "Disminución"),
              aes(y = valor+(mean(valor)*espaciado_vertical_texto), 
                  label = scales::percent(valor, big.mark = ".", decimal.mark = ",", accuracy = decimales_texto)),
              color = color_fondo, vjust = 1, size = tamaño_texto, check_overlap = T) +
    scale_x_date(date_breaks = "months", labels = ~numeric_a_mes(month(.x)),
                 expand = expansion(0)) +
    scale_y_continuous(limits = c(-alto_max,
                                  alto_max),
                       # labels = ~percent_format(.x, big.mark = ".", decimal.mark = ",")
                       # labels = ~scales::percent(.x-1, big.mark = ".", decimal.mark = ",", accuracy = 0.01)
                       labels = ~scales::percent(.x, big.mark = ".", decimal.mark = ",", accuracy = decimales_texto)
    ) +
    scale_fill_manual(values = c("Aumento" = color_subir,
                                 "Disminución" = color_bajar,
                                 "Igual" = color_neutro), 
                      aesthetics = c("color", "fill")) +
    theme_void() +
    theme(text = element_text(color = color_fondo),
          axis.text.x = element_text(),
          axis.text.y = element_text(margin = margin(r = 4)),
          panel.grid.major.y = element_line(linetype = "dashed", color = color_secundario_detalle),
          plot.margin = margin(l = 15, r = 15, b = 4),
          legend.position = "none") +
    theme(plot.background = element_rect(fill = color_secundario, color = color_secundario),
          panel.background = element_rect(fill = color_secundario, color = color_secundario))
  
  if (escala == "trimestre") {
    plot <- plot +
      scale_x_date(breaks = dato_2$fecha, labels = ~paste(year(.x), mes_a_trimestre(month(.x))),
                   expand = expansion(0)) +
      theme(axis.text.x = element_text(angle = -90, vjust = .5, hjust = 0))
  }
  
  # dev.new()
  # plot
  return(plot)
}


grafico_historico <- function(dato, escala = "mensual"
) {
  # browser()
  
  datos <- dato$datos |> 
    filter(year(fecha) >= 2019)
  
  datos_2 <- datos |> 
    # mayor variación con respecto al valor mas reciente
    mutate(variacion = valor/lead(valor),
           variacion_ultimo = valor/first(valor))
  # # mayor aumento usando media movil
  # mutate(valor_media = slider::slide_dbl(valor, mean, .after = 3),
  #        variacion_media = valor_media/lead(valor_media))
  
  disminucion = filter(datos_2, variacion_ultimo == min(variacion_ultimo)) |> slice(1)
  
  mayor_disminucion = filter(datos_2, variacion == min(variacion, na.rm = T)) |> slice(1)
  mayor_aumento = filter(datos_2, variacion == max(variacion, na.rm = T)) |> slice(1)
  
  ultimo <- datos_2 |> filter(fecha == max(fecha))
  
  datos_2 |> 
    ggplot(aes(fecha, valor)) +
    # línea del punto actual al inicio
    annotate("segment", x = min(datos_2$fecha), xend = ultimo$fecha,
             y = ultimo$valor, yend = ultimo$valor, 
             color = color_detalle, linetype = "dashed") +
    # puntos de mayor y menor disminucion
    annotate("point", x = mayor_aumento$fecha, y = mayor_aumento$valor,
             size = 9, alpha = .3, color = color_positivo) +
    annotate("point", x = mayor_disminucion$fecha, y = mayor_disminucion$valor,
             size = 9, alpha = .3, color = color_negativo) +
    # linea
    geom_line(color = color_fondo, linewidth = 1.1) +
    # puntos encima
    annotate("point", x = mayor_aumento$fecha, y = mayor_aumento$valor,
             size = 3, alpha = .9, color = color_positivo) +
    annotate("point", x = mayor_disminucion$fecha, y = mayor_disminucion$valor,
             size = 3, alpha = .9, color = color_negativo) +
    # punto al final
    geom_point(data = ultimo, size = 5, color = color_secundario_detalle, alpha = .7) +
    geom_point(data = ultimo, size = 3, color = color_fondo) +
    scale_y_continuous(labels = ~miles(.x), expand = expansion(c(.25, .25))) +
    scale_x_date(expand = expansion(c(0, .02))) +
    coord_cartesian(clip = "off") +
    theme_void() +
    theme(text = element_text(color = color_fondo),
          axis.text.x = element_text(),
          axis.text.y = element_text(margin = margin(r = 4))) +
    theme(panel.grid.major.y = element_line(linetype = "dashed", color = color_secundario_detalle),
          panel.grid.major.x = element_line(color = color_secundario_detalle)
    ) +
    theme(plot.margin = margin(l = 15, r = 15, t = 4, b = 4),
          legend.position = "none") +
    theme(plot.background = element_rect(fill = color_secundario, color = color_secundario),
          panel.background = element_rect(fill = color_secundario, color = color_secundario))
}