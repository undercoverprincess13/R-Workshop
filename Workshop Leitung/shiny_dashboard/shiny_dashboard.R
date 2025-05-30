library(shiny)
library(bslib)

ui <- page_sidebar(
  title = "R Workshop - Coffee @ HHN",
  sidebar = sidebar("Filter",
    selectInput(
      "select widget",
      "Geschlecht",
      choices = list("Alle" = 1, "Frauen" = 2, "Männer" = 3),
      selected = 1
    ),
    sliderInput(
      "slider widget",
      "Alter der Befragten",
      min = 18,
      max = 35,
      value = c(22, 28)
    )
  ),
  
  
  # Cards für Visualisierungen
  card(
    card_header("Durchschnittliche Anzahl an Kaffee"),
    # Code
    
    
  ),
  card(
    card_header("Korrelation Kaffeekonsum während und außerhalb der Prüfungsphase"),
    # Code
    
    
  ),
  card(
    card_header("Trinken Frauen mehr Kaffee als Männer bezogen auf Lernzeit (Hypothesentest / T-Test)"),
    # Code
    
    
  )
)

server <- function(input, output) {}

shinyApp(ui, server)
