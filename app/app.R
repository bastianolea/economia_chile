library(shiny)

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


ui <- fluidPage(title = "Economía chilena", lang = "es",
  
  tags$style(".outer-panel {
  padding: 8px;
  }"),
  
  tags$style(".panel {
  min-height: 180px;
  background-color: black;
  border: 1px red solid;
  border-radius: 9px;
  padding: 12px;
  margin: 0;
  }"),
  
  tags$style(".panel-vacio {
  min-height: 180px;
  padding: 12px;
  margin: 0;
  }"),
  
  # sitio de ancho fijo
  fluidRow(
    div(style = "padding: 30px; max-width: 840px; margin: auto;",
           
           # fila 1 ----
           fluidRow(
             
             panel_vacio(4,
                          h1("prueba")
             ),
             
             
             panel(4,
                   h1("prueba")
             ),
             
             panel(4, style = "background-color: grey;",
                   h1("prueba")
             )
           ),
           
           # fila 2 ----
           fluidRow(
             panel(4, 
                   h1("prueba")
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
}

# Run the application 
shinyApp(ui = ui, server = server)
