library(shiny)
library(bslib)

ui <- page_sidebar(
  title = "R Workshop - Coffee @ HHN",
  sidebar = sidebar(
    sliderInput(
      "slider",
      "Alter der Befragten",
      min = 0,
      max = 100,
      value = c(18, 40)
    )
  ),
  
  
  # Cards für Visualisierungen
  card(
    card_header("Durchschnittliche Anzahl an Kaffee")
  ),
  card(
    card_header("Korrelation Kaffeekonsum während und außerhalb der Prüfungsphase")
  ),
  card(
    card_header("Trinken Frauen mehr Kaffee als Männer bezogen auf Lernzeit (Hypothesentest / T-Test)")
  )
)

server <- function(input, output) {}

shinyApp(ui, server)

