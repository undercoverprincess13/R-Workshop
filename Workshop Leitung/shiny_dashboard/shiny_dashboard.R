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
  
  # -------------------------
  # Mittelwert, Median, Standardabweichung
  # ------------------------- 
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
  
  # -------------------------
  # Mittelwert und Median Boxplot
  # ------------------------- 
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
  
  
  
  
  # ------------------------------------------------------------------------------------------
  # Korrelationsmatrix
  # Wie hängen die Variablen Alter, Lernzeit, Kaffetassen, Geschlecht, Prüfungsphase zusammen 
  # ------------------------------------------------------------------------------------------ 
  
  output$cor_matrix <- renderPlot({
    df <- gefilterte_daten()
    
    # Umkodierung Geschlecht und Prüfungsphase in numerische Werte
    # Wir machen aus kategorialen Variablen (Text) numerische Werte,
    # damit sie in der Korrelationsberechnung berücksichtigt werden können.
    # Geschlecht: Frau = 1, Mann = 0 / Prüfungsphase: Ja = 1, Nein = 0
    df$Geschlecht <- ifelse(df$Geschlecht == "Frau", 1, 0)
    df$Pruefungsphase <- ifelse(df$Pruefungsphase == "Ja", 1, 0)
    
    # Wir behalten nur die numerischen Spalten, da die Funktion cor()
    # nur mit numerischen Daten arbeiten kann.
    numeric_df <- df %>% select(where(is.numeric))
    
    # Korrelationsmatrix berechnen
    # Wie stark hängen die numerischen Variablen miteinander zusammen?
    # "complete.obs" bedeutet: Zeilen mit fehlenden Werten (NA) werden ignoriert.
    cor_matrix <- cor(numeric_df, use = "complete.obs")
    
    # Darstellung / Die Matrix wird mit ggcorrplot als farbige Kachelgrafik dargestellt.
    ggcorrplot(cor_matrix,
               method = "square",
               type = "upper", #Nur die obere Hälfte der Matrix anzeigen (symmetrisch / "full" für komplett)
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
    
    # Erstelle ein Streudiagramm mit ggplot:
    ggplot(df, aes(x = Lernzeit_pro_Tag_in_Stunden, y = Kaffeetassen_pro_Tag)) +
      geom_point(color = "#1f77b4") +                            # Zeichne die einzelnen Datenpunkte
      geom_smooth(method = "lm", se = TRUE, color = "darkred") + # Füge eine Regressionslinie hinzu (lm = lineares Modell),
                                                                 # mit Konfidenzintervall (se = TRUE)
      labs(
        title = "Regression: Lernzeit vs. Kaffeekonsum",
        x = "Lernzeit (Stunden pro Tag)",
        y = "Tassen Kaffee pro Tag"
      ) +
      theme_minimal()
  })
  
  # --------------------------------------------------------------------------
  # Hypothesentest
  # Trinken Männer wirklich mehr Kaffee als Frauen?
  # --------------------------------------------------------------------------
  output$t_test_ergebnis <- renderPrint({
    df <- gefilterte_daten()
    t_test <- t.test(Kaffeetassen_pro_Tag ~ Geschlecht, data = df, alternative = "greater")
    # R vergleicht Mittelwerte alphabetisch (Frau, Mann) danach geht es von rechts nach linkts in Bezus auf Y~X-> X = Mann > Y = Frau 
    # alternative = "greater" bedeutet: „Zweite Gruppe > Erste Gruppe“
    t_test
    
    
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
  style = "padding: 1rem; box-sizing: border-box;",
  layout_columns(
    col_widths = c(12),
    card(
      card_header("Deskiptive Statistik"),
      p("Dieses Histogramm zeigt, wie viele Tassen Kaffee pro Tag konsumiert werden."),
      plotOutput("hist_kaffee")
    
  )),
  layout_columns(
    col_widths = c(6,6),  
    card(
      card_header("Statistische Kennwerte"),
      p("Mittelwert, Median und Standardabweichung des Kaffeekonsums in der gewählten Stichprobe."),
      verbatimTextOutput("stat_summary")
    ),
    card(
      card_header("Boxplot: Median + Mittelwert"),
      p("Der Boxplot zeigt Median, Streuung (Quartile) und Ausreißer. Der rote Punkt ist der Mittelwert."),
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
  ),
  layout_columns(
    col_widths = c(12),

    card(
      card_header("Trinken Frauen mehr Kaffee als Männer bezogen auf Lernzeit (Hypothesentest / T-Test)"),
      p("Der T-Test prüft, ob es einen signifikanten Unterschied im Kaffeekonsum zwischen Männern und Frauen gibt."),
      verbatimTextOutput("t_test_ergebnis")
      )
    )
  )
)


# ----------------
# APP STARTEN
# ----------------

shinyApp(ui, server)
