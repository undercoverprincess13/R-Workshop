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
  
# Histogramm für den Kaffee Lösung Aufgabe 1 
  
  output$hist_kaffee <- renderPlot({
    df <- gefilterte_daten()
    ggplot(df, aes(x = Kaffeetassen_pro_Tag)) +
      geom_histogram(binwidth = 1, fill = "#1f77b4", color = "white") +
      labs(
        title = "Verteilung: Tassen Kaffee pro Tag",
        x = "Tassen Kaffee pro Tag",
        y = "Anzahl Personen"
      ) +
      theme_minimal()
  })
  

 
# Mittelwert, Median, Standardabweichung Aufgabe 2
    # TODO berechne den Mittelwertm den Median und die Standardabweichung 
    # TIPP: 🧮 Mittelwert, Median und Standardabweichung berechnen 

  output$stat_summary <- renderPrint({
    df <- gefilterte_daten()
    kaffee <- df$Kaffeetassen_pro_Tag
    
    summary <- list(
      "Mittelwert" = # TODO,
      "Median" = # TODO,
      "Standardabweichung" = # TODO
      
    )
    print(summary)
  })
# Mittelwert und Median Boxplot Aufgabe 2 
  # TODO Interpretation des Boxplots (führe den Code aus und schau dir den Boxplot an):
  # Was sagt der Boxplot aus? 
  # In welchen Fällen kann er gut eingesetzt werden?
  
  output$boxplot_mittelwerte <- renderPlot({
    df <- gefilterte_daten()
    ggplot(df, aes(x = "", y = Kaffeetassen_pro_Tag)) +
      geom_boxplot(fill = "#FFDDC1") +
      stat_summary(fun = mean, geom = "point", shape = 20, size = 4, color = "red") +
      labs(
        title = "Boxplot mit Mittelwert",
        y = "Tassen Kaffee pro Tag",
        x = ""
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
    
  )),
  layout_columns(
    col_widths = c(6,6),  
    card(
      card_header("Statistische Kennwerte"),
      p("Mittelwert, Median und Standardabweichung des Kaffeekonsums in der gewählten Stichprobe.(Gefiltert auf Alter und Geschlecht per Sidebar)"),
      verbatimTextOutput("stat_summary")
    ),
    card(
      card_header("Boxplot: Median + Mittelwert"),
      p("Der Boxplot zeigt Median, Streuung (Quartile) und Ausreißer. Der rote Punkt ist der Mittelwert.(Gefiltert auf Alter und Geschlecht per Sidebar)"),
      plotOutput("boxplot_mittelwerte", height = "300px")
    )
  )
    )
  )


# ----------------
# APP STARTEN
# ----------------

shinyApp(ui, server)
