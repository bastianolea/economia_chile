library(shiny)
library(shinyWidgets)
library(shinycssloaders)
library(dplyr)
library(scales)
library(lubridate)
library(ggplot2)
library(shades)

descargar = TRUE #descargar datos desde GitHub, o cargar datos locales

# setup ----

# color_principal = "#594484"
color_principal = "#4D4484"

color_fondo = color_principal |> lightness(10) |> chroma(20)
color_detalle = color_principal |> lightness(15) |> chroma(40)
color_destacado = color_principal |> lightness(50) |> chroma(65)

# color_secundario = color_principal |> lightness(45)
# color_secundario_detalle = color_principal |> lightness(55) |> chroma(40)
color_secundario = color_principal |> lightness(35)
color_secundario_detalle = color_principal |> lightness(44) |> chroma(40)
color_texto = color_principal |> lightness(80)

# # previsualizar colores
# shades::swatch(
#   c(
#     color_fondo,
#     color_detalle,
#     color_destacado,
#     color_secundario, color_secundario_detalle,
#     color_texto
#   ),
#   bg = "#151515")

color_positivo = "#96E472"
color_negativo = "#F44868"
color_neutro = "#5059F4"

source("funciones_app.R", local = TRUE)

options(spinner.type = 8, spinner.color = color_detalle)

# ui ----
ui <- fluidPage(title = "Economía chilena", lang = "es", 
                includeCSS("style.css"),
                # useWaiter(),
                
                ## tipografías ----
                tags$style(HTML("@import url('https://fonts.googleapis.com/css2?family=Archivo+Narrow:ital,wght@0,400..700;1,400..700&family=Archivo:ital,wght@0,100..900;1,100..900&display=swap');")),
                tags$style(HTML("@import url('https://fonts.googleapis.com/css2?family=Archivo:ital,wght@0,100..900;1,100..900&display=swap');")),
                
                ## css ----
                # texto cuerpo
                tags$style(HTML("body {
                  background-color: ", color_fondo, ";
                  color: ", color_texto, ";
                  font-family: 'Archivo Narrow';
                  font-size: 140%}")),
                
                tags$style(HTML("em {
                font-size: 140% }")),
                
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
                
                tags$style(HTML("a, a:hover {
                  color:", color_destacado, ";}")),
                
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
         }")),
                
                
                tags$style(HTML(".shiny-notification {
                                background-color:", color_secundario, ";
                                border: 2px ", color_secundario_detalle, " solid;
                                color:", color_fondo, ";}")),
                
                
                # responsividad del alto de paneles: si la app es en dos columnas, largo fijo; si es en una columna, largo ajustado al contenido
                tags$style(HTML("
                    @media (min-width: 765px) {
                      .panel {
                        min-height: 250px;
                        height: auto !important;}")),
                
                # —----
                # header ----
                
                fluidRow(
                  column(12,
                         h1("Indicadores económicos de Chile"),
                         
                         div(markdown("[Bastián Olea Herrera](https://bastianolea.github.io/shiny_apps/)"), 
                             style = "margin-top: 16px; margin-bottom: 16px; font-family: 'Archivo', sans-serif; font-weight: 900;"),
                         
                         div(style = "font-size: 130%;",
                         p("Tablero que reune los principales indicadores para comprender la situación económica del país."),
                         
                         p(markdown("Todos los datos son obtenidos directamente desde la base de datos estadísticos del [Banco Central](https://si3.bcentral.cl/siete). Los datos se actualizan automáticamente dos veces al día."))
                         )
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
                  column(12, style = "padding: 24px; margin-bottom: -20px;",
                         uiOutput("tendencias_ui") |> withSpinner(color = color_secundario, proxy.height = 400)
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
                        panel_cuadro_resumen("Remuneraciones", "remuneraciones_ui"),
                        
                        panel_grafico_variacion("Variación mensual de las remuneraciones reales",
                                                "remuneraciones_g_var"),
                        
                        panel_grafico_historico("Evolución de las remuneraciones reales",
                                                "remuneraciones_g_hist")
                      )
                  )
                ),
                
                # firma ----
                fluidRow(
                  column(12, style = "opacity: 1; font-size: 90%; margin-top: 0;",
                         
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
  
  # opción 1: cargar datos locales
  # pib <- read.csv2("datos/pib.csv")
  # imacec <- read.csv2("datos/imacec.csv")
  # ipc <- read.csv2("datos/ipc.csv")
  # ipsa <- read.csv2("datos/ipsa.csv")
  # desempleo <- read.csv2("datos/desempleo.csv")
  # uf <- read.csv2("datos/uf.csv")
  # remuneraciones <- read.csv2("datos/remuneraciones.csv")
  
  local = FALSE #cargar datos locales desde el inicio del proyecto, o desde la carpeta app
  
  # opción 2: cargar datos individuales desde GitHub
  # pib <- cargar_datos_web("pib", descargar, local)
  # imacec <- cargar_datos_web("imacec", descargar, local)
  # ipc <- cargar_datos_web("ipc", descargar, local)
  # ipsa <- cargar_datos_web("ipsa", descargar, local)
  # desempleo <- cargar_datos_web("desempleo", descargar, local)
  # uf <- cargar_datos_web("uf", descargar, local)
  # remuneraciones <- cargar_datos_web("remuneraciones", descargar, local)
  
  # opción 3: cargar un solo archivo desde GitHub, que son los datos unidos
  descargar = FALSE; local = FALSE
  datos <- cargar_datos_web("datos_economia_chile", descargar, local)
  # browser()
  
  pib <- datos |> filter(dato == "pib")
  imacec <- datos |> filter(dato == "imacec")
  ipc <- datos |> filter(dato == "ipc")
  ipsa <- datos |> filter(dato == "ipsa")
  desempleo <- datos |> filter(dato == "desempleo")
  uf <- datos |> filter(dato == "uf")
  remuneraciones <- datos |> filter(dato == "remuneraciones")
  
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
  
  # datos_desocupados <- desocupados |> 
  #   calcular_metricas()
  
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
  
  output$tendencias_ui <- renderUI({
    div(
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
  })
  
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
