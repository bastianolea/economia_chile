
prop_a_porcentaje <- function(x, decimales = 2) {
  x_porcentaje <- round(100*(x-1), decimales)
  
  x_porcentaje_texto <- format(x_porcentaje, decimal.mark = ",", big.mark = ".")
  
  x_porcentaje_texto_2 <- paste0(x_porcentaje_texto, "%")
  
  return(x_porcentaje_texto_2)
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


miles <- function(x) {
  scales::comma(x, big.mark = ".", decimal.mark = ",", trim = TRUE)
}

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



formateador_cifra <- function(dato, unidad = "miles de millones", año_base = 2018) {
  if (unidad == "miles de millones") {
    p(paste0("$", miles(dato)), "(miles de millones)")
    
  } else if (unidad == "porcentaje") {
    p(paste0(dato, "%"), paste0("(porcentaje respecto a ", año_base, ")"))
    
  } else {
    p(paste0("$", miles(dato)), "(miles de millones)")
  }
}


dato_ui <- function(datos_pib, unidad = "miles de millones", año_base = 2018, subir = "bueno") {
  div(
    div(style = "display: flex;",
        div(style = "flex: 1.5;",
            
            strong("Valor actual"),
            # if (unidad == "miles de millones") {
            #   p(paste0("$", miles(datos_pib$ultimo$valor)), "(miles de millones)")
            #   
            # } else if (unidad == "porcentaje") {
            #   p(paste0(datos_pib$ultimo$valor, "%"), paste0("(porcentaje respecto a ", año_base, ")"))
            # },
            p(formateador_cifra(datos_pib$ultimo$valor, unidad, año_base)),
            
            
            porcentaje_flechita(datos_pib$cambio$valor, juicio = subir),
            p(class = "texto-bajo-porcentaje", 
              "versus cifra anterior", 
              paste0("(", format(datos_pib$penultimo$fecha, "%m/%y"), ")"))
            
        ),
        
        div(style = "flex: 1;",
            
            strong("Hace un año"),
            # p(paste0("$", miles(datos_pib$hace_un_año$valor)), "(miles de millones)"),
            p(formateador_cifra(datos_pib$hace_un_año$valor, unidad, año_base)),
            
            porcentaje_flechita(datos_pib$hace_un_año_p, juicio = subir),
            p(class = "texto-bajo-porcentaje", 
              "versus hace 12 meses",
              paste0("(", format(datos_pib$hace_un_año$fecha, "%m/%y"), ")"))
            
        )
    )
    
  )
}



panel_cuadro_resumen <- function(titulo, subtitulo, output) {
  panel(4, 
        div(
          h4(titulo),
          em(subtitulo, style = "font-size: 95%;"),
          style = "margin-bottom: 12px; line-height: 1.1;"),
        
        
        htmlOutput(output)
  )
}