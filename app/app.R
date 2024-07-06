library(shiny)
library(dplyr)
library(scales)
library(lubridate)
library(ggplot2)
# library(bslib)

# setwd("app")

# tema <- bs_theme(bg = "#202020", fg = "#FFFFFF", primary = "#909090")
color_fondo = "#202020"
color_texto = "#FFFFFF"
color_paneles = "#707070"

source("funciones_app.R")

ui <- fluidPage(title = "Economía chilena", lang = "es", 
                # theme = tema,
                includeCSS("style.css"),
                
                tags$style(HTML("body {
                  background-color: ", color_fondo, ";
                  color: ", color_texto, ";
                }")),
                
                tags$style(HTML(
                    ".panel {
                      min-height: 180px;
                      background-color: ", color_paneles, ";
                      color: black;
                      border: 2px #404040 solid;
                      border-radius: 9px;
                      padding: 12px;
                      margin: 0;}")),
                
                fluidRow(
                  div(style = "padding: 30px; max-width: 900px; margin: auto;", # ancho fijo
                      
                      ## pib ----
                      fila_indicador(
                        panel_titular(titulo = "Producto Interno bruto (PIB)",
                                      subtitulo = "Valor monetario de todos los bienes y servicios producidos en el país."
                        ),
                        panel_cuadro_resumen("pib_ui"),
                        
                        panel_grafico_variacion("prueba",
                                                "pib_g_var")
                      ),
                      
                      ## imacec ----
                      fila_indicador(
                        panel_titular(titulo = "Indicador Mensual de Actividad Económica (Imacec)",
                                      subtitulo = "Resume la actividad de los sectores de la economía en un mes, a precios del año anterior."),
                        
                        panel_cuadro_resumen("imacec_ui"),
                        
                        panel_grafico_variacion("prueba",
                                                "imacec_g_var")
                      ),
                      
                      ## ipc ----
                      fila_indicador(
                        panel_titular(titulo = "Índice de Precios al Consumidor (IPC)",
                                      subtitulo = "Mide la variación de los precios de una canasta de bienes y servicios representativa del consumo de los hogares urbanos en Chile."),
                        panel_cuadro_resumen("ipc_ui"),
                        
                        panel_grafico_variacion("prueba",
                                                "ipc_g_var")
                      ),
                      
                      ## uf ----
                      fila_indicador(
                        panel_titular(titulo = "Unidad de Fomento (UF)",
                                      subtitulo = "Cifra que expresa el valor del peso chileno según la inflación."),
                        panel_cuadro_resumen("uf_ui"),
                        
                        panel_grafico_variacion("prueba",
                                                "uf_g_var")
                        
                      ),
                      
                      ## ipsa ----
                      fila_indicador(
                        panel_titular(titulo = "Índice de Precios Selectivo de Acciones (IPSA)",
                                      subtitulo = "Indicador de desempeño de las acciones con mayor capitalización en la Bolsa de Comercio de Santiago."),
                        panel_cuadro_resumen("ipsa_ui"),
                        
                        panel_grafico_variacion("prueba",
                                                "ipsa_g_var")
                      ),
                      
                      ## desempleo ----
                      fila_indicador(
                        panel_titular(titulo = "Tasa de desempleo",
                                      subtitulo = "Mide el porcentaje de la fuerza de trabajo nacional que se encuentra sin empleo."),
                        panel_cuadro_resumen("desempleo_ui"),
                        
                        panel_grafico_variacion("prueba",
                                                "desempleo_g_var")
                      ),
                      
                      ## remuneraciones ----
                      fila_indicador(
                        panel_titular(titulo = "Índice real de remuneraciones",
                                      subtitulo = "Remuneración por hora ordinaria, considerando la variación del Índice de Precios al Consumidor."),
                        panel_cuadro_resumen("remuneraciones_ui"),
                        
                        panel_grafico_variacion("prueba",
                                                "remuneraciones_g_var")
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
}

shinyApp(ui = ui, server = server)
