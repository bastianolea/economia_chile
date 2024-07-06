library(shiny)
library(dplyr)
library(scales)
library(lubridate)
library(ggplot2)

# setwd("app")
source("funciones_app.R")


ui <- fluidPage(title = "Economía chilena", lang = "es", 
                includeCSS("style.css"),
                
                # sitio de ancho fijo
                fluidRow(
                  div(style = "padding: 30px; max-width: 840px; margin: auto;",
                      
                      # fila 1 ----
                      fluidRow(
                        
                        panel_vacio(4,
                                    h1("prueba")
                        ),
                        
                        panel(4,
                              h2("Prueba")
                              
                        ),
                        
                        panel(4, 
                              h3("prueba")
                        )
                      ),
                      
                      ## pib ----
                      fluidRow(
                        panel_cuadro_resumen(titulo = "Producto Interno bruto (PIB)",
                                             subtitulo = "Valor monetario de todos los bienes y servicios producidos en el país.",
                                             "pib_ui"),
                        
                        panel(8, 
                              panel_grafico_variacion("prueba",
                              "pib_g_var")
                        )
                      ),
                      
                      ## imacec ----
                      fluidRow(
                        panel_cuadro_resumen(titulo = "Indicador Mensual de Actividad Económica (Imacec)",
                                             subtitulo = "Resume la actividad de los sectores de la economía en un mes, a precios del año anterior.",
                                             "imacec_ui"),
                        
                        panel(8, 
                              panel_grafico_variacion("prueba",
                                                      "imacec_g_var")
                        )
                      ),
                      
                      ## ipc ----
                      fluidRow(
                        panel_cuadro_resumen(titulo = "Índice de Precios al Consumidos (IPC)",
                                             subtitulo = "Mide la variación de los precios de una canasta de bienes y servicios representativa del consumo de los hogares urbanos en Chile.",
                                             "ipc_ui"),
                        
                        panel(8, 
                              panel_grafico_variacion("prueba",
                                                      "ipc_g_var")
                        )
                      ),
                      
                      
                      ## uf ----
                      fluidRow(
                        panel_cuadro_resumen(titulo = "Unidad de Fomento (UF)",
                                             subtitulo = "Cifra que expresa el valor del peso chileno según la inflación.",
                                             "uf_ui"),
                        
                        panel(8, 
                              panel_grafico_variacion("prueba",
                                                      "uf_g_var")
                        )
                      ),
                      
                      ## ipsa ----
                      fluidRow(
                        panel_cuadro_resumen(titulo = "Índice de Precios Selectivo de Acciones (IPSA)",
                          subtitulo = "Indicador de desempeño de las acciones con mayor capitalización en la Bolsa de Comercio de Santiago.",
                          "ipsa_ui"),
                        
                        panel(8, 
                              panel_grafico_variacion("prueba",
                              "ipsa_g_var")
                        )
                      ),
                      
                      ## desempleo ----
                      fluidRow(
                        panel_cuadro_resumen(titulo = "Tasa de desempleo",
                          subtitulo = "Mide el porcentaje de la fuerza de trabajo nacional que se encuentra sin empleo.",
                          "desempleo_ui"),
                        
                        panel(8, 
                              panel_grafico_variacion("prueba",
                              "desempleo_g_var")
                        )
                      ),
                      
                      # ## desocupados ----
                      # fluidRow(
                      #   panel_cuadro_resumen(titulo = "desocupados_ui",
                      #                        subtitulo = "Blablablabla.",
                      #                        "desocupados_ui"),
                      #   
                      #   panel(8, 
                      #         h1("prueba")
                      #   )
                      # ),
                      
                      
                      ## remuneraciones ----
                      fluidRow(
                        panel_cuadro_resumen(titulo = "Índice real de remuneraciones",
                                             subtitulo = "Remuneración por hora ordinaria, considerando la variación del Índice de Precios al Consumidor.",
                                             "remuneraciones_ui"),
                        
                        panel(8, 
                              panel_grafico_variacion("prueba",
                              "remuneraciones_g_var")
                        )
                      ),
                      
                      
                      
                      
                      
                      # fila 3 ----
                      fluidRow(
                        panel(4,
                              h1("prueba")
                        ),
                        
                        panel(4,
                              h1("prueba")
                        ),
                        
                        panel(4,
                              h1("prueba")
                        )
                      ),
                      
                      
                  )
                ),
                
                # firma ----
                fluidRow(
                  column(12, style = "opacity: 1; font-size: 90%; margin-top: 24px;",
                         
                         markdown("Desarrollado y programado por [Bastián Olea Herrera,](https://bastian.olea.biz) usando el lenguaje de programación estadístico R."),
                         
                         markdown("Puedes explorar mis otras [aplicaciones interactivas sobre datos sociales en mi portafolio.](https://bastianolea.github.io/shiny_apps/)"),
                         
                         # markdown("Si deseas que incluya nuevas variables o fuentes de datos, no dudes en contactarme [por correo](mailto:bastianolea@gmail.com) o por [Twitter.](https://x.com/bastimapache)"),
                         
                         markdown("Datos, código de fuente de esta app, y código del procesamiento de los datos [disponible en el repositorio de GitHub.](https://github.com/bastianolea/economia_chile)"),
                         
                         div(style = "height: 20px")
                         
                  )
                )
)

server <- function(input, output) {
  
  ## carga de datos ----
  
  # setwd("app")
  pib <- readRDS("datos/pib.rds")
  imacec <- readRDS("datos/imacec.rds")
  ipc <- readRDS("datos/ipc.rds")
  ipsa <- readRDS("datos/ipsa.rds")
  desempleo <- readRDS("datos/desempleo.rds")
  uf <- readRDS("datos/uf.rds")
  desocupados <- readRDS("datos/desocupados.rds")
  remuneraciones <- readRDS("datos/remuneraciones.rds")
  
  
  ## indicadores ----
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
  
  
  ## interfaces ----
  output$pib_ui <- renderUI(dato_ui(datos_pib))
  
  output$imacec_ui <- renderUI(dato_ui(datos_imacec, unidad = "índice 100", año_base = 2018))
  
  # hay que ir poniendo las unidades correctas en cada cosito
  output$ipc_ui <- renderUI(dato_ui(datos_ipc, unidad = "índice 100", año_base = 2023, subir = "neutro"))
  
  output$uf_ui <- renderUI(dato_ui(datos_uf, unidad = "pesos", subir = "neutro"))
  
  output$ipsa_ui <- renderUI(dato_ui(datos_ipsa, unidad = "índice 1000", año_base = 2003))
  
  output$desempleo_ui <- renderUI(dato_ui(datos_desempleo, unidad = "porciento", año_base = "% fuerza de trabajo", subir = "malo"))
  
  # output$desocupados_ui <- renderUI(dato_ui(datos_desocupados, subir = "malo"))
  
  output$remuneraciones_ui <- renderUI(dato_ui(datos_remuneraciones, unidad = "índice 100", año_base = 2023))
  
  ## gráficos variación ----
  
  output$pib_g_var <- renderPlot(grafico_variacion(datos_pib, escala = "trimestre"))
  output$imacec_g_var <- renderPlot(grafico_variacion(datos_imacec))
  output$ipc_g_var <- renderPlot(grafico_variacion(datos_ipc, subir = "neutro"))
  output$uf_g_var <- renderPlot(grafico_variacion(datos_uf, subir = "neutro"))
  output$ipsa_g_var <- renderPlot(grafico_variacion(datos_ipsa))
  output$desempleo_g_var <- renderPlot(grafico_variacion(datos_desempleo, subir = "malo"))
  output$remuneraciones_g_var <- renderPlot(grafico_variacion(datos_remuneraciones))
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)
