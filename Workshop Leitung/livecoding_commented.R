# Library hinzufügen und ausführen
library(tidyverse)
# Code ausführen mit Str+ Enter / Cmd Enter -> Führt die aktuelle Zeile oder den markierten Code in der Console aus

# Datensätze anzeigen und auswählen (Datensätze die in den Bibliotheken enthalten sind)
data()

# Dataset anschauen
view(mpg)

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
view(mpg_efficient)


# Um values zu filtern muss == statt = angegeben werden.
mpg_ford <- filter(mpg, manufacturer == "ford")
view(mpg_ford)


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
  alternative = "less"
)
# cty(abhängige Variable, was gemessen wird -> Stadtverbrauch) ~ class(Gruppierungsvariable: 2 Gruppen "suv" und "compact")
# -> R vergleicht den Durchschnitt von cty zwischen beiden Klassen 

# data ist das dataframe was wir R mitgeben für den t-test, in dem Fall geben wir das gesamte mpg Dataframe mit aber gefiltert auf alle Zeilen die in Classe SUV oder COMPACT sind 

# alternative = "less" Compact, SUV (Alphabet) Das rechte wie als Vergleich genommen von R. Man fragt sich ob SUVs mehr verbrauchen als Compact also SUV fährt kürzere Strecken mit einer Gallone daher less 

# Teilen mit anderen Leute (Kollegen, Stakeholder...) -> Markdown dokument in files erstellen