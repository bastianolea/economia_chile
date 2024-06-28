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
                              div(
                                h4("Producto Interno bruto (PIB)"),
                                em("Valor monetario de todos los bienes y servicios producidos en el país.", style = "font-size: 95%;"),
                                style = "margin-bottom: 12px; line-height: 1.1;"),
                              
                              
                              htmlOutput("pib_ui")
                              
                        ),
                        
                        panel(4, style = "background-color: grey;",
                              h4("Evolución del PIB"),
                              plotOutput("g_pib", height = 80),
                              
                              plotOutput("g_pib_variacion", height = 80)
                        )
                      ),
                      
                      # fila 2 ----
                      fluidRow(
                        panel(4, 
                              # htmlOutput("pib_ui")
                        ),
                        
                        panel(8, 
                              h1("prueba")
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
  
  # carga de datos ----
  
  # setwd("app")
  pib <- readRDS("datos/pib.rds")
  imacec <- readRDS("datos/imacec.rds")
  ipc <- readRDS("datos/ipc.rds")
  ipsa <- readRDS("datos/ipsa.rds")
  desempleo <- readRDS("datos/desempleo.rds")
  uf <- readRDS("datos/uf.rds")
  
  # pib ----
  datos_pib <- pib |> 
    filter(serie == "PIB a precios corrientes") |> 
    calcular_metricas()
  
  output$t_pib_cifra <- renderText(miles(datos_pib$ultimo$valor))
  output$t_pib_cambio <- renderText(prop_a_porcentaje(datos_pib$cambio$valor))
  output$t_pib_hace_un_año <- renderText(miles(datos_pib$hace_un_año$valor))
  output$t_pib_hace_un_año_p <- renderText(prop_a_porcentaje(datos_pib$hace_un_año_p))
  
  output$g_pib <- renderPlot({
    datos_pib$datos |> 
      ggplot(aes(fecha, valor)) +
      geom_line() +
      geom_point() +
      theme_void() +
      theme(plot.background = element_rect(fill = "grey30", color = "grey30"))
  })
  
  output$g_pib_variacion <- renderPlot({
    datos_pib$variacion |> 
      ggplot(aes(fecha, valor)) +
      geom_line() +
      geom_point() +
      theme_void() +
      theme(plot.background = element_rect(fill = "grey30", color = "grey30"))
  })
  
  
  
  
  output$pib_ui <- renderUI({
    
    div(
      div(style = "display: flex;",
          div(style = "flex: 1.5;",
              
              strong("Valor actual"),
              p(paste0("$", miles(datos_pib$ultimo$valor)), "(miles de millones)"),
              
              porcentaje_flechita(datos_pib$cambio$valor),
              p(class = "texto-bajo-porcentaje", 
                "versus cifra anterior", 
                paste0("(", format(datos_pib$penultimo$fecha, "%m/%y"), ")"))
              
          ),
          
          div(style = "flex: 1;",
              
              strong("Hace un año"),
              p(paste0("$", miles(datos_pib$hace_un_año$valor)), "(miles de millones)"),
              
              porcentaje_flechita(datos_pib$hace_un_año_p),
              p(class = "texto-bajo-porcentaje", 
                "versus hace 12 meses",
                paste0("(", format(datos_pib$hace_un_año$fecha, "%m/%y"), ")"))
              
          )
      )
      
    )
    
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
