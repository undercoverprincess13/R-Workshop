# ----------------
# Bibliotheken
# ----------------

library(shiny)
library(bslib)
library(readxl)
library(ggplot2)
library(dplyr) 

# ----------------
# SERVER
# ----------------
server <- function(input, output) {
  
  # Excel-Datei einlesen 
  daten <- reactive({
    read_excel("kaffee_lernzeit_pruefungsphase_alter.xlsx")
  })
  
  # Gefilterte Daten basierend auf UI-Eingaben
  gefilterte_daten <- reactive({
    df <- daten()
    
    # Filter Alter
    df <- df %>%
      filter(Alter >= input$`slider widget`[1],
             Alter <= input$`slider widget`[2])
    
    # Filter Geschlecht, falls nicht "Alle"
    if (input$`select widget` == 2) {
      df <- df %>% filter(Geschlecht == "Frau")
    } else if (input$`select widget` == 3) {
      df <- df %>% filter(Geschlecht == "Mann")
    }
    
    return(df)
  })
  
  # -------------------------
  # Histogramm für den Kaffee
  # ------------------------- 
  output$hist_kaffee <- renderPlot({
    ggplot(gefilterte_daten(), aes(x = Kaffeetassen_pro_Tag)) +
      geom_histogram(binwidth = 1, fill = "#1f77b4", color = "white") +
      labs(
        title = "Verteilung: Tassen Kaffee pro Tag",
        x = "Tassen Kaffee pro Tag",
        y = "Anzahl Personen"
      ) +
      theme_minimal()
  })

}

# ----------------
# UI
# ----------------

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
    card_header("Deskiptive Statistik"),
    p("Dieses Histogramm zeigt, wie viele Tassen Kaffee pro Tag konsumiert werden."),
    plotOutput("hist_kaffee")
    
    
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

# ----------------
# APP STARTEN
# ----------------

shinyApp(ui, server)
