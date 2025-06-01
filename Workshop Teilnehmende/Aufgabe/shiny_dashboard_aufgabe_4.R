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
  

 
# Mittelwert, Median, Standardabweichung Lösung Aufgabe 2 
   

  output$stat_summary <- renderPrint({
    df <- gefilterte_daten()
    kaffee <- df$Kaffeetassen_pro_Tag
    
    summary <- list(
      "Mittelwert" = mean(kaffee, na.rm = TRUE),
      "Median" = median(kaffee, na.rm = TRUE),
      "Standardabweichung" = sd(kaffee, na.rm = TRUE)
    )
    print(summary)
  })
  
# Mittelwert und Median Boxplot Lösung Aufgabe 2:
  # Lösung Aufgabe 2: Interpretation des Boxplots:
  # Was sagt der Boxplot aus? 
  # Antwort: Ein Boxplot zeigt die Verteilung einer numerischen Variable. Dabei gibt er wichtige Lage- und Streuungsmaße wieder. 
  # - Median (Zentralwert): Die mittlere Linie in der Box.
  # - Box: Spannt sich vom 1. Quartil (Q1) bis zum 3. Quartil (Q3) und enthält die mittleren 50 % der Werte.
  # - "Whiskers" (Antennen): Reichen üblicherweise bis zu 1,5-fache der interquartilen Spannweite (IQR). Werte außerhalb gelten als Ausreißer und werden als Punkte dargestellt
  
  # In welchen Fällen kann er gut eingesetzt werden?
  # Antwort: 
  # Ein Boxplot eignet sich besonders gut, wenn man:
  # - Verteilungen visuell vergleichen will
  # - Ausreißer identifizieren möchte
  # - Symmetrie oder Schiefe der Verteilung beurteilen will
  # - Vergleiche mehrerer Gruppen durchführen will
  
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
  
# Korrelationsmatrix Lösung Aufgabe 3 
  # Interpretation (führe den Code aus und schau dir die KOrrelationsmatrix an): 
  # Wie hängen die Variablen Alter, Lernzeit, Kaffetassen, Geschlecht, Prüfungsphase zusammen?  
  # Antwort: 
  # Kaffeekonsum und Lernzeit: r = 0.60 → starker positiver Zusammenhang -> Wer mehr lernt, trinkt auch tendenziell mehr Kaffee.
  # Kaffeekonsum und Prüfungsphase: r = 0.44 → moderater positiver Zusammenhang -> In der Prüfungsphase wird mehr Kaffee konsumiert.
  # Lernzeit und Prüfungsphase: r = 0.52 → moderater positiver Zusammenhang -> In der Prüfungsphase steigt auch die Lernzeit deutlich.
  # Kaffeekonsum und Geschlecht: r = -0.43 → moderater negativer Zusammenhang ->  Ein Geschlecht (je nach Kodierung, z. B. weiblich = 0, männlich = 1) trinkt deutlich weniger Kaffee. (Hinweis: Die genaue Interpretation hängt von der Kodierung ab.)
  # Lernzeit und Geschlecht: r = -0.25 → leichter negativer Zusammenhang ->  Ein Geschlecht lernt tendenziell etwas weniger.
  # Alter und andere Variablen:
  #  •	Mit Kaffeetassen: r = 0.29 → leichte positive Korrelation.
  #  •	Mit Lernzeit: r = 0.15 → kaum Zusammenhang.
  #  •	Mit Prüfungsphase: r = 0.21 → geringer Zusammenhang.
  
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
  
  # Regression Aufgabe 4
  # TODO Erstelle ein Diagramm mit ggplot auf dem die X Achse die Lernzeit pro Tag in Stunden (Lernzeit_pro_Tag_in_Stunden) und die Y Achse die Kaffeetassen pro Tag (Kaffeetassen_pro_Tag) sind
  # Beschrifte das Diagramm mit Titel, X Achse und Y Achse 
  output$regression_plot <- renderPlot({
    df <- gefilterte_daten()
    
    ggplot(df, aes(x = #TODO, #TODO)) +
      geom_point(color = "#1f77b4") +
      geom_smooth(method = "lm", se = TRUE, color = "darkred") +
      labs(
        title = "# TODO",
        x = "# TODO",
        y = "# TODO"
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
  )
    )
  )


# ----------------
# APP STARTEN
# ----------------

shinyApp(ui, server)
