
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


flechita <- function(x) {
  flechita <- case_when(x < 0 ~ "▼",
            x == 0 ~ "=",
            x > 0 ~ "▲")
  
  color <- case_when(x < 0 ~ "red",
                        x == 0 ~ "blue",
                        x > 0 ~ "green")
  
  div(flechita,
      style = paste("font-size: 100%; color:", color))
}


porcentaje_flechita <- function(dato) {
div(style = "margin: 0; white-space: nowrap;",
    div(style = "display: inline-block; vertical-align:middle; margin-left: -3px; margin-right: -3px; margin-bottom: 2px;", flechita(dato)),
    div(style = "display: inline-block; vertical-align:middle; font-size: 200%; text-align: center;",
        prop_a_porcentaje(dato)
    )
)
}