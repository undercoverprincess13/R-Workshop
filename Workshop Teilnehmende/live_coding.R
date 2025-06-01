# Alle Bibliotheken installieren 
install.packages(c(
  "shiny",
  "bslib",
  "readxl",
  "ggplot2",
  "dplyr",
  "ggcorrplot",
  "tidyverse"
))

# Bibliotheken laden
library(shiny)
library(bslib)
library(readxl)
library(ggplot2)
library(dplyr)
library(ggcorrplot)
library(tidyverse)

# Code ausführen mit Str+ Enter / Cmd Enter -> Führt die aktuelle Zeile oder den markierten Code in der Console aus

# Datensätze anzeigen und auswählen (Datensätze die in den Bibliotheken enthalten sind)


# Ausgewählten Datensatz anschauen


# Beschreibung des Datensatzes im Help Tab öffnen/sich anzeigen lassen


# Überblick über Funktionen bzw. Methoden


# Überblick über den Datensatz (Spalten & Zeilen) anzeigen lassen


# Datensatz filtern Bsp. cty(city miles per gallon) >= 20
# Das wird dann anschließend in einen neuen Datensatz/df gespeichert


# Neuen Datensatz/df ausgeben


# Filterung von values ("==" statt "=")


# "Mutate" fügt Variablen zu einem Datensatz hinzu oder ändert bestehende. 
# Bsp. gallons/mile in km/l + anschließende Speicherung als neue Spalte


# "Pipe-Operator" um Ausdrücke lesbarer und übersichtlicher zu machen, 
# indem man die Ausgabe einer Funktion direkt als Eingabe an die nächste 
# Funktion weitergibt. %>% (cmd/ctrl + shift + m)


# Datenvisualisierung mit ggplot2 (grammar of graphics)
  # Der Aufbau eines Plots in `ggplot2`
  # - Daten (data) der zugrundeliegende Datensatz (mpg)
  # - `aes()` = aesthetics: Definiert, welche Variablen wie dargestellt werden sollen
  # - `geom_` = Geometrie des Plots, also Welche Art von Diagramm (z.B. Punkte, Balken, Linien)
  # - Plots können gelayert werden, d.h. mehrere Plots übereinandergelegt

# Datensatz wählen und ggplot starten 

# Histogramm
# Wie viele Autos kommen in der Stadt mit einer Galleone wie weit? 



# Anderer Diagramm Typ: Frequenzpolynom statt Histogramm 



# Plots layern


# Scatterplot



# Scatterplot nach Fahrzeugklasse einfärben


# Farbe aus Farbpalette wählen  



# Gerichteter Zweistichproben-t-Test
  # Fragestellung:  
    # Verbrauchen SUVs mehr Sprit in der Stadt als Kompaktwagen?
    # - Wir vergleichen den Stadtverbrauch (`cty`, miles per gallon) zwischen Fahrzeugklassen (`class`). 
    # - Je höher der Wert, desto sparsam das Fahrzeug.

  # Variablen:
    # - `cty` = Stadtverbrauch (je mehr Meilen pro Gallone, desto sparsamer).
    # - `class` = Fahrzeugklasse (`"compact"`, `"suv"`)
  
  # Hypothesen:
    # - H₀ (Nullhypothese): Kompaktwagen sind nicht sparsamer als SUVs → `compact ≤ suv`
    # - H₁ (Alternativhypothese): Kompaktwagen sind sparsamer → `compact > suv`
    

# Teilen mit anderen Personen (Kollegen, Stakeholder...) -> Markdown-Dokument in files erstellen

