library(tidyverse)
# Dataset auswählen welche in den Bibs included sind 
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

# Teilen mit anderen Leute (Kollegen, Stakeholder...) -> Markdown dokument in files erstellen