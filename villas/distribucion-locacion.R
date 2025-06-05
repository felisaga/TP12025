# Un aspecto a tener en cuenta en las respuestas
# de la encuesta que quienes respondieron eran de diferentes
# provincias. Se logra ver que en su mayoria son de la provincia 
# y ciudad de Buenos Aires.

library(ggplot2)
library(tidyverse)
library(janitor)

attach(datos_limpios)

ggplot(datos_limpios, aes(x = reorder(provincia, provincia, function(x) -length(x)))) +
  geom_bar(fill = "darkorange") +
  labs(title = "Cantidad de respuestas por provincia",
       x = "Provincia",
       y = "Cantidad de respuestas") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


tabla_provincia <- datos_limpios %>%
  tabyl(provincia) %>%
  adorn_totals("row") %>%
  adorn_pct_formatting(digits = 1)

tabla_provincia

# Esto la hace mas linda en el print nomas
knitr::kable(tabla_provincia, caption = "Respuestas por provincia")


# Agrupar las 2 provincias más frecuentes y el resto como "Otros"
provincia_pie <- datos_limpios %>%
  count(provincia) %>%
  mutate(provincia = fct_lump_n(provincia, n = 2, w = n, other_level = "Otros")) %>%
  group_by(provincia) %>%
  summarise(cantidad = sum(n)) %>%
  mutate(porcentaje = round(100 * cantidad / sum(cantidad), 1),
         etiqueta = paste0(provincia, ": ", porcentaje, "%"))

ggplot(provincia_pie, aes(x = "", y = cantidad, fill = provincia)) +
  geom_bar(stat = "identity", width = 1, color = "white") +
  coord_polar("y") +
  labs(title = "Ubicación de la vivienda") +
  theme_void() +
  theme(
    legend.title = element_blank(),
    plot.title = element_text(hjust = 0.5)) 
