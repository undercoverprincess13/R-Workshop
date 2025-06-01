# Library hinzufügen und ausführen
library(shiny)
library(bslib)
library(readxl)
library(ggplot2)
library(dplyr)
library(ggcorrplot)
library(tidyverse)

# Code ausführen mit Str+ Enter / Cmd Enter -> Führt die aktuelle Zeile oder den markierten Code in der Console aus

# Console Scooby # TODO 



# Datensätze anzeigen und auswählen (Datensätze die in den Bibliotheken enthalten sind)
data()

# Dataset anschauen
View(mpg)

# Öffnet beschreibung im Help Tab über das Dataset
?mpg

# Überblick über Funktionen/methoden
?mean

# Überblick über Dataset (Cols & Rows)
glimpse(mpg)

# Filterung des Datasets - cty(city miles per gallon) >= 20. Wird in neues ds
# gespeichert und ausgegeben mit view()
filter(mpg, cty >= 20)
mpg_efficient <- filter(mpg, cty >= 20)
View(mpg_efficient)


# Um values zu filtern muss == statt = angegeben werden.
mpg_ford <- filter(mpg, manufacturer == "ford")
View(mpg_ford)

# Mutate adds or changes variable in dataset. Hier wurden gallons/mile in km/l
# umgerechnet und als neue spalte hinzugefügt
mpg_metric <-  mutate(mpg, cty_metric = 0.425144 *cty)
glimpse(mpg_metric)


# Tool um Argument zur nächsten funktion hinzuzufügen %>% 
mpg_metric <- mpg %>% #cmd+shift+m /ctrl+shift+m
  mutate(cty_metric = 0.425144 *cty)

mpg %>%
  group_by(class) %>% 
  summarise(mean(cty),
            median(cty))

# Data viz with ggplot2 (grammar of graphics)
# aes (aesthentics) the way of saying welche var sollen wie kommunizieren
# geom_ + Type of plot -> ist die syntax zum plotten
ggplot(mpg, aes(x=cty)) + 
  geom_histogram() +
  labs( x= "City milage")

# Plots können gelayert werden
ggplot(mpg, aes(x=cty)) + 
  geom_histogram() +
  geom_freqpoly() +
  labs( x= "City milage")

# Scatter plot
ggplot(mpg, aes(x = cty,
                y = hwy,
                color = class)) + 
  geom_point() +
  scale_color_brewer(palette = "Dark2")
# geom_smooth(method = "lm")  linear relationship durch einfaches hinzufügen einer Regression line


# Gerichteter Zweistichproben-t-Test 
# "Verbrauchen SUVs mehr Sprit in der Stadt als kompaktautos?"
#  Variablen:
#   cty = Stadtverbrauch (je mehr man pro Gallone fahren kann, desto sparsamer!)
#   class = Fahrzeugklasse (z.B. "suv", "compact")

t.test(
  cty ~ class,
  data = mpg %>% filter(class %in% c("suv", "compact")),
  alternative = "greater"
)
# t.test() vergleicht Durchschnittswerte zwischen zwei Gruppen.
# cty ~ class bedeutet: "Vergleiche Stadtverbrauch (cty) nach Fahrzeugklasse (class)"
# data = ... übergibt das gefilterte mpg-Dataframe (nur "suv" und "compact").
# alternative = "greater" prüft: Ist cty (Wie weit kommt man mit einer Galleone) bei Compact größer als bei SUV ? -> Dann wäre der Verbrauch von SUVs höher!
# R vergleicht alphabetisch: "compact" (links), "suv" (rechts) → prüft „Linke (erste) Gruppe < rechte (zweite) Gruppe“

# Teilen mit anderen Leute (Kollegen, Stakeholder...) -> Markdown dokument in files erstellen