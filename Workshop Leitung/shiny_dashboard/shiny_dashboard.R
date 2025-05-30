# ----------------
# Bibliotheken
# ----------------

library(shiny)
library(bslib)
library(readxl)
library(ggplot2)
library(dplyr)
library(ggcorrplot) # Für Korrelationsmatrix

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
  
  # -------------------------
  # Korrelationsmatrix
  # ------------------------- 
  
  output$cor_matrix <- renderPlot({
    df <- gefilterte_daten()
    
    # Umkodierung Geschlecht und Prüfungsphase in numerische Werte
    df$Geschlecht <- ifelse(df$Geschlecht == "Frau", 1, 0)
    df$Pruefungsphase <- ifelse(df$Pruefungsphase == "Ja", 1, 0)
    
    # Nur numerische Spalten
    numeric_df <- df %>% select(where(is.numeric))
    
    # Korrelationsmatrix berechnen
    cor_matrix <- cor(numeric_df, use = "complete.obs")
    
    # Darstellung mit ggcorrplot
    ggcorrplot(cor_matrix,
               method = "square",
               type = "upper",
               lab = TRUE,
               lab_size = 3,
               colors = c("red", "white", "blue"),
               title = "Korrelationsmatrix Kaffekonsum @ HHN",
               ggtheme = theme_minimal())
  })
  
  # -------------------------
  # Regression
  # ------------------------- 
  
  output$regression_plot <- renderPlot({
    df <- gefilterte_daten()
    
    ggplot(df, aes(x = Lernzeit_pro_Tag_in_Stunden, y = Kaffeetassen_pro_Tag)) +
      geom_point(color = "#1f77b4") +
      geom_smooth(method = "lm", se = TRUE, color = "darkred") +
      labs(
        title = "Regression: Lernzeit vs. Kaffeekonsum",
        x = "Lernzeit (Stunden pro Tag)",
        y = "Tassen Kaffee pro Tag"
      ) +
      theme_minimal()
  })

}

# ----------------
# UI
# ----------------

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
  style = "height: 100vh; overflow-y: auto; padding: 1rem; box-sizing: border-box;",
  card(
    card_header("Deskiptive Statistik"),
    p("Dieses Histogramm zeigt, wie viele Tassen Kaffee pro Tag konsumiert werden."),
    plotOutput("hist_kaffee")
    
    
  ),
layout_columns(
  col_widths = c(6, 6),
  
  card(
    card_header("Korrelation Kaffeekonsum während und außerhalb der Prüfungsphase"),
    p("Diese Korrelationsmatrix zeigt Zusammenhänge zwischen Alter, Kaffee- und Lernverhalten."),
    plotOutput("cor_matrix", height = "400px")
  ),
  
  card(
    card_header("Regression"),
    p("Zusammenhang zwischen Lernzeit und Kaffeekonsum"),
    plotOutput("regression_plot", height = "400px")
  )
),
  card(
    card_header("Trinken Frauen mehr Kaffee als Männer bezogen auf Lernzeit (Hypothesentest / T-Test)"),
    # Code
    
    
  )
)
)

# ----------------
# APP STARTEN
# ----------------

shinyApp(ui, server)
