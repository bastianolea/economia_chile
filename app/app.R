library(shiny)
library(shinyWidgets)
library(shinycssloaders)
library(dplyr)
library(scales)
library(lubridate)
library(ggplot2)
library(fresh)
library(shades)

# setup ----
# setwd("app")

# color_fondo = "#202020"
# color_texto = "#FFFFFF"
# color_paneles = "#707070"
# color_panel_detalle = "#505050"
# color_positivo = "green"
# color_negativo = "red"
# color_neutro = "blue"

# color_fondo = "#2A3950"
# color_texto = "#FFFFFF"
# color_paneles = "#974063"
# color_panel_detalle = "#974063"
# color_positivo = "#96E472"
# color_negativo = "#F44868"
# color_neutro = "blue"
# color_titulo_paneles = "#FFFFFF"

# color_fondo = "#1D2D45"
# color_texto = "#BAA1ED"
# color_secundario = "#594484"

color_principal = "#594484"
color_principal = "#4D4484"
# color_principal = "#FF007E"

color_fondo = color_principal |> lightness(10) |> chroma(20)
color_detalle = color_principal |> lightness(15) |> chroma(40)
color_destacado = color_principal |> lightness(40) |> chroma(65)

# color_secundario = color_principal |> lightness(45)
# color_secundario_detalle = color_principal |> lightness(55) |> chroma(40)
color_secundario = color_principal |> lightness(35)
color_secundario_detalle = color_principal |> lightness(44) |> chroma(40)

color_texto = color_principal |> lightness(80)


# shades::swatch(
#   c(
#     color_fondo,
#     color_detalle,
#     color_destacado,
#     color_secundario,
#     color_secundario_detalle,
#     color_texto
#   ),
#   bg = "#151515"
# )


# color_paneles = color_secundario
# color_panel_detalle = color_secundario |> lightness(40)
# color_titulo_paneles = "#BFC8FF"
# 
# color_titulos = color_secundario
# color_detalle = color_panel_detalle
# color_destacado = color_paneles

color_positivo = "#96E472"
color_negativo = "#F44868"
color_neutro = "#5059F4"

source("funciones_app.R", local = TRUE)

options(spinner.type = 8, spinner.color = color_detalle)

ui <- fluidPage(title = "Economía chilena", lang = "es", 
                includeCSS("style.css"),
                
                # tipografías
                tags$style(HTML("@import url('https://fonts.googleapis.com/css2?family=Archivo+Narrow:ital,wght@0,400..700;1,400..700&family=Archivo:ital,wght@0,100..900;1,100..900&display=swap');")),
                tags$style(HTML("@import url('https://fonts.googleapis.com/css2?family=Archivo:ital,wght@0,100..900;1,100..900&display=swap');")),
                
                # css ----
                # texto cuerpo
                tags$style(HTML("body {
                  background-color: ", color_fondo, ";
                  color: ", color_texto, ";
                  font-family: 'Archivo Narrow';
                  font-size: 140%}")),
                
                # títulos
                tags$style(HTML("h1, h2, h3, h4, h5 {
                  font-family: 'Archivo', sans-serif;
                  font-weight: 900;
                  font-style: italic;}")),
                
                tags$style(HTML("h1, h2, h3 {
                  color:", color_destacado, ";")),
                
                tags$style(HTML("h4 {
                color:", color_texto, ";")),
                
                tags$style(HTML("hr {
                  border-top: 2px solid ", color_detalle, ";}")),
                
                # estilo paneles por defecto
                tags$style(HTML("
                  .panel {
                      /*min-height: 180px;*/
                      background-color:", color_secundario, ";
                      color:", color_fondo, ";
                      border: 2px", color_secundario_detalle, "solid;
                      border-radius: 9px;
                      padding: 12px;
                      margin: 0;}")),
                
                
                #colores pickers
                tags$style(paste(".btn.dropdown-toggle { /* color del picker mismo */
                   color:", color_texto, ";
                   background-color:", color_fondo, ";
                   font-weight: 800;
                   font-size: 135%;
                   border: 0;
                   border-radius: 0;
                   border-bottom: 3px", color_destacado, "solid;
                   }
                   
         .dropdown-menu, .divider {
          color: white !important;
         background: ", color_detalle, " !important;
         }
  
         .dropdown-header {
         color: white !important;
         font-weight: bold;
         font-size: 110%;
         }
         
         .text {
         color: white;
         font-size: 80%;
         }
         
         .form-control {
         color: ", color_texto, " !important;
         box-shadow: none;
         }
         
         .no-results {
         color: black !important;
         background: ", color_detalle, " !important;
         }
         
         .selected {
         background-color: ", color_secundario, " !important;
         color: ", color_detalle, " !important;
         }
         
         .bs-placeholder, .bs-placeholder:active, bs-placeholder:focus, .bs-placeholder:hover {
         color: ", color_fondo, " !important;
         }
  
         /*color de fondo de opción elegida*/
         .dropdown-item.selected {
         background-color: ", color_destacado, " !important;
         color: black !important;
         }
         
         /*color del fondo de la opción en hover*/
         .dropdown-item:hover {
         color: red;
         background-color: ", color_secundario, " !important;
         }
  ")),
                
                
                
                # responsividad del alto de paneles: si la app es en dos columnas, largo fijo; si es en una columna, largo ajustado al contenido
                tags$style(HTML("
                    @media (min-width: 765px) {
                      .panel {
                        min-height: 240px;
                        height: auto !important;}")),
                
                # —----
                # header ----
                
                fluidRow(
                  column(12,
                         h1("Indicadores económicos de Chile")
                  )
                ),
                
                
                # tendencias ----
                fluidRow(
                  column(12,
                         hr(),
                         h2("Resumen de tendencias")
                  ),
                  
                  ## fecha corte ----
                  column(12, style = "margin-bottom: -25px; font-size: 130%;",
                         div(style = "display: inline-block;",
                             p("Durante los últimos")
                         ),
                         div(style = "display: inline-block;",
                             pickerInput("fecha_corte", label = NULL,
                                         choices = c("3 meses",
                                                     "6 meses",
                                                     "12 meses",
                                                     "24 meses"),
                                         selected = "6 meses",
                                         width = 110)
                         ),
                         div(style = "display: inline-block;",
                             strong(":")
                         ),
                  ),
                  
                  
                  ## tendencias ----
                  column(12, style = "padding: 24px;",
                         panel_tendencia("El PIB",
                                         "pib_tendencia"),
                         panel_tendencia("El Imacec",
                                         "imacec_tendencia"),
                         panel_tendencia("El IPC",
                                         "ipc_tendencia"),
                         panel_tendencia("El valor de la UF",
                                         "uf_tendencia"),
                         panel_tendencia("El IPSA",
                                         "ipsa_tendencia"),
                         panel_tendencia("El desempleo",
                                         "desempleo_tendencia"),
                         panel_tendencia("El valor de las remuneraciones",
                                         "remuneraciones_tendencia")
                  )
                ),
                
                #paneles indicadores ----
                fluidRow(
                  column(12, style = "margin-bottom: -30px;",
                         hr(),
                         h2("Variación de indicadores económicos")
                  )
                ),
                fluidRow(
                  div(style = "padding: 30px; max-width: 900px; margin: auto;", # ancho fijo
                      
                      ## pib ----
                      fila_indicador(
                        panel_titular(titulo = "Producto Interno bruto (PIB)",
                                      subtitulo = "Valor monetario de todos los bienes y servicios producidos en el país."
                        ),
                        panel_cuadro_resumen("Resumen PIB", "pib_ui"),
                        
                        panel_grafico_variacion("Variación trimestral del PIB",
                                                "pib_g_var"),
                        
                        panel_grafico_historico("Evolución del PIB",
                                                "pib_g_hist")
                      ),
                      
                      ## imacec ----
                      fila_indicador(
                        panel_titular(titulo = "Indicador Mensual de Actividad Económica (Imacec)",
                                      subtitulo = "Resume la actividad de los sectores de la economía en un mes, a precios del año anterior."),
                        
                        panel_cuadro_resumen("Resumen Imacec", "imacec_ui"),
                        
                        panel_grafico_variacion("Variación mensual del Imacec",
                                                "imacec_g_var"),
                        
                        panel_grafico_historico("Evolución del Imacec",
                                                "imacec_g_hist")
                      ),
                      
                      ## ipc ----
                      fila_indicador(
                        panel_titular(titulo = "Índice de Precios al Consumidor (IPC)",
                                      subtitulo = "Mide la variación de los precios de una canasta de bienes y servicios representativa del consumo de los hogares urbanos en Chile."),
                        panel_cuadro_resumen("Resumen IPC", "ipc_ui"),
                        
                        panel_grafico_variacion("Variación mensual del IPC",
                                                "ipc_g_var"),
                        
                        panel_grafico_historico("Evolución del IPC",
                                                "ipc_g_hist")
                      ),
                      
                      ## uf ----
                      fila_indicador(
                        panel_titular(titulo = "Unidad de Fomento (UF)",
                                      subtitulo = "Cifra que expresa el valor del peso chileno según la inflación."),
                        panel_cuadro_resumen("Resumen UF", "uf_ui"),
                        
                        panel_grafico_variacion("Variación mensual de la UF",
                                                "uf_g_var"),
                        
                        panel_grafico_historico("Evolución de la UF",
                                                "uf_g_hist")
                        
                      ),
                      
                      ## ipsa ----
                      fila_indicador(
                        panel_titular(titulo = "Índice de Precios Selectivo de Acciones (IPSA)",
                                      subtitulo = "Indicador de desempeño de las acciones con mayor capitalización en la Bolsa de Comercio de Santiago."),
                        panel_cuadro_resumen("Resumen IPSA", "ipsa_ui"),
                        
                        panel_grafico_variacion("Variación mensual del IPSA",
                                                "ipsa_g_var"),
                        
                        panel_grafico_historico("Evolución del IPSA",
                                                "ipsa_g_hist")
                      ),
                      
                      ## desempleo ----
                      fila_indicador(
                        panel_titular(titulo = "Tasa de desempleo",
                                      subtitulo = "Mide el porcentaje de la fuerza de trabajo nacional que se encuentra sin empleo."),
                        panel_cuadro_resumen("Resumen desempleo", "desempleo_ui"),
                        
                        panel_grafico_variacion("Variación mensual de la tasa de desempleo",
                                                "desempleo_g_var"),
                        
                        panel_grafico_historico("Evolución de la tasa de desempleo",
                                                "desempleo_g_hist")
                      ),
                      
                      ## remuneraciones ----
                      fila_indicador(
                        panel_titular(titulo = "Índice real de remuneraciones",
                                      subtitulo = "Remuneración por hora ordinaria, considerando la variación del Índice de Precios al Consumidor."),
                        panel_cuadro_resumen("Resumen remuneraciones", "remuneraciones_ui"),
                        
                        panel_grafico_variacion("Variación mensual de las remuneraciones reales",
                                                "remuneraciones_g_var"),
                        
                        panel_grafico_historico("Evolución de las remuneraciones reales",
                                                "remuneraciones_g_hist")
                      )
                  )
                ),
                
                # firma ----
                fluidRow(
                  column(12, style = "opacity: 1; font-size: 90%; margin-top: 24px;",
                         
                         markdown("Desarrollado y programado por [Bastián Olea Herrera,](https://bastian.olea.biz) usando el lenguaje de programación estadístico R."),
                         
                         markdown("Puedes explorar mis otras [aplicaciones interactivas sobre datos sociales en mi portafolio.](https://bastianolea.github.io/shiny_apps/)"),
                         
                         markdown("Datos, código de fuente de esta app, y código del procesamiento de los datos [disponible en el repositorio de GitHub.](https://github.com/bastianolea/economia_chile)"),
                         
                         div(style = "height: 20px")
                  )
                )
)

#—----

server <- function(input, output) {
  
  ## cargar datos ----
  # setwd("app")
  
  pib <- readRDS("datos/pib.rds")
  imacec <- readRDS("datos/imacec.rds")
  ipc <- readRDS("datos/ipc.rds")
  ipsa <- readRDS("datos/ipsa.rds")
  desempleo <- readRDS("datos/desempleo.rds")
  uf <- readRDS("datos/uf.rds")
  desocupados <- readRDS("datos/desocupados.rds")
  remuneraciones <- readRDS("datos/remuneraciones.rds")
  
  
  ## calcular indicadores ----
  datos_pib <- pib |> 
    filter(serie == "PIB a precios corrientes") |> 
    calcular_metricas()
  
  datos_imacec <- imacec |> 
    filter(serie == "Imacec empalmado, serie original (índice 2018=100)") |> 
    calcular_metricas()
  
  datos_ipc <- ipc |> 
    filter(serie == "Índice IPC General") |> 
    calcular_metricas()
  
  datos_uf <- uf |> 
    filter(serie == "Unidad de fomento (UF)") |> 
    calcular_metricas()
  
  datos_ipsa <- ipsa |> 
    filter(serie == "IPSA  (índice enero 2003=1000)") |> 
    calcular_metricas()
  
  datos_desempleo <- desempleo |> 
    filter(serie == "Tasa  de  desempleo  (porcentaje)") |> 
    calcular_metricas()
  
  datos_desocupados <- desocupados |> 
    calcular_metricas()
  
  datos_remuneraciones <- remuneraciones |> 
    filter(serie == "Índice real de remuneraciones") |> 
    calcular_metricas()
  
  
  # fecha ----
  fecha_corte <- reactive({
    if (input$fecha_corte == "3 meses") {
      corte = 3
    } else if (input$fecha_corte == "6 meses") {
      corte = 6
    } else if (input$fecha_corte == "12 meses") {
      corte = 12
    } else if (input$fecha_corte == "24 meses") {
      corte = 24
    }
    fecha_corte <- Sys.Date() %m-% months(corte)
    return(fecha_corte)
  })
  
  
  ## tendencias texto ----
  output$pib_tendencia <- renderUI(tendencia_ui(datos_pib, fecha_corte(), input, subir = "bueno"))
  output$imacec_tendencia <- renderUI(tendencia_ui(datos_imacec, fecha_corte(), input, subir = "bueno"))
  output$ipc_tendencia <- renderUI(tendencia_ui(datos_ipc, fecha_corte(), input, subir = "malo"))
  output$uf_tendencia <- renderUI(tendencia_ui(datos_uf, fecha_corte(), input, subir = "malo"))
  output$ipsa_tendencia <- renderUI(tendencia_ui(datos_ipsa, fecha_corte(), input, subir = "bueno"))
  output$desempleo_tendencia <- renderUI(tendencia_ui(datos_desempleo, fecha_corte(), input, subir = "malo"))
  output$remuneraciones_tendencia <- renderUI(tendencia_ui(datos_remuneraciones, fecha_corte(), input, subir = "bueno"))
  
  ## cuadros interfaces ----
  # para panel_cuadro_resumen() en ui
  output$pib_ui <- renderUI(dato_ui(datos_pib))
  output$imacec_ui <- renderUI(dato_ui(datos_imacec, unidad = "índice 100", año_base = 2018))
  output$ipc_ui <- renderUI(dato_ui(datos_ipc, unidad = "índice 100", año_base = 2023, subir = "neutro"))
  output$uf_ui <- renderUI(dato_ui(datos_uf, unidad = "pesos", subir = "neutro"))
  output$ipsa_ui <- renderUI(dato_ui(datos_ipsa, unidad = "índice 1000", año_base = 2003))
  output$desempleo_ui <- renderUI(dato_ui(datos_desempleo, unidad = "porciento", año_base = "% fuerza de trabajo", subir = "malo"))
  output$remuneraciones_ui <- renderUI(dato_ui(datos_remuneraciones, unidad = "índice 100", año_base = 2023))
  
  
  ## gráficos variación ----
  output$pib_g_var <- renderPlot(grafico_variacion(datos_pib, escala = "trimestre"))
  output$imacec_g_var <- renderPlot(grafico_variacion(datos_imacec))
  output$ipc_g_var <- renderPlot(grafico_variacion(datos_ipc, subir = "malo")) #ex neutro
  output$uf_g_var <- renderPlot(grafico_variacion(datos_uf, subir = "malo")) #ex neutro
  output$ipsa_g_var <- renderPlot(grafico_variacion(datos_ipsa))
  output$desempleo_g_var <- renderPlot(grafico_variacion(datos_desempleo, subir = "malo"))
  output$remuneraciones_g_var <- renderPlot(grafico_variacion(datos_remuneraciones))
  
  ## gráficos históricos ----
  output$pib_g_hist <- renderPlot(grafico_historico(datos_pib, escala = "trimestre"))
  output$imacec_g_hist <- renderPlot(grafico_historico(datos_imacec))
  output$ipc_g_hist <- renderPlot(grafico_historico(datos_ipc))
  output$uf_g_hist <- renderPlot(grafico_historico(datos_uf))
  output$ipsa_g_hist <- renderPlot(grafico_historico(datos_ipsa))
  output$desempleo_g_hist <- renderPlot(grafico_historico(datos_desempleo))
  output$remuneraciones_g_hist <- renderPlot(grafico_historico(datos_remuneraciones))
  
  
  
}

shinyApp(ui = ui, server = server)
