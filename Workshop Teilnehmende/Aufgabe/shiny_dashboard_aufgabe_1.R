# ----------------
# Bibliotheken
# ----------------

library(shiny)
library(bslib)
library(readxl)
library(ggplot2)
library(dplyr)
library(ggcorrplot) # Für Korrelationsmatrix

# ----------------------
# SERVER nicht verändern 
# ----------------------
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
  
# -------------------------------------------------------------------
  
# Histogramm für den Kaffee Aufgabe 1 
  
  output$hist_kaffee <- renderPlot({
    # TODO 
    # Erstelle ein Histogramm (geom_histogram), welches das ganze Dataframe (df) nutzt 
    # und auf der X Achse die Kaffeetassen pro Tag (Kaffeetassen_pro_Tag) anzeigt und beschrifte es. 
    # TIPP: CheatSheet 📊 Visualisierung mit ggplot2 
    df <- gefilterte_daten()
    ggplot( # TODO , # TODO )) +
      geom_histogram(binwidth = 1, fill = "#1f77b4", color = "white") +
      labs(
        title = " # TODO ",
        x = " # TODO ",
        y = " #TODO "
      ) +
      theme_minimal()
    })
  
}

# -------------------------------------------------------------------
  

# -------------------
# UI nicht verändern 
# -------------------

# Sidebar mit Filteroptionen
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
div(
  style = "padding: 1rem; box-sizing: border-box;",
  layout_columns(
    col_widths = c(12),
    card(
      card_header("Histogramm Kaffee pro Tag"),
      p("Dieses Histogramm zeigt, wie viele Tassen Kaffee pro Tag konsumiert werden. (Gefiltert auf Alter und Geschlecht per Sidebar)"),
      plotOutput("hist_kaffee")
    
  ))))



# ----------------
# APP STARTEN
# ----------------

shinyApp(ui, server)
