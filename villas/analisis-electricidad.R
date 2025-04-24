library(ggplot2)
library(tidyverse)
library(janitor)

attach(datos_limpios)

####################
# conexion electrica #
####################
# tabla de frecuencias
tabla_conexion_electrica <- tabyl(conexion_electrica) %>%
  arrange(desc(n)) %>%
  adorn_pct_formatting(digits = 2) %>%
  adorn_totals()
tabla_conexion_electrica

# datos ordenados
datos_grafico_barras <- datos_limpios %>%
  count(conexion_electrica) %>%
  mutate(
    pct = round(n / sum(n) * 100, 2),
    conexion_electrica = reorder(conexion_electrica, n)
)
# grafico de barras horizontal
ggplot(datos_grafico_barras, aes(x = conexion_electrica, y = n)) +
  geom_col(show.legend = FALSE) +
  geom_text(aes(label = paste0(pct, "%")), hjust = -0.1) +
  labs(
    title = "Distribución de la conexion electrica",
    x = "Tipo de conexion",
    y = "Cantidad"
  ) +
  coord_flip() + 
  theme_minimal()


####################
# incendios x electricidad #
####################
resumen_incendios <- datos_limpios %>%
  count(incendios_por_electricidad) %>%
  mutate(porcentaje = round(n / sum(n) * 100, 1),
         etiqueta = paste0(incendios_por_electricidad," ", porcentaje, "%"))

# grafico de torta
ggplot(resumen_incendios, aes(x = "", y = n, fill = incendios_por_electricidad)) +
  geom_col(width = 1, color = "white") +
  coord_polar(theta = "y") +
  theme_void() +
  geom_text(aes(label = etiqueta), position = position_stack(vjust = 0.5)) +
  labs(title = "Incendios por condiciones eléctricas en el último año") +
  scale_fill_brewer(palette = "Set2") +
  theme(legend.position = "none")



####################
# cortes en el año #
####################
unique(cortes_en_invierno)
unique(cortes_en_verano)
# orden de los niveles
niveles_ordenados <- c(
  "No son frecuentes",
  "Por lo menos 1 corte en el mes",
  "Por lo menos 2 cortes en el mes",
  "Por lo menos 3 cortes en el mes",
  "Más de 4 cortes mensuales"
)

# columna nueva con los cortes anuales
datos_limpios <- datos_limpios %>%
  mutate(
    cortes_en_verano = factor(cortes_en_verano, levels = niveles_ordenados, ordered = TRUE),
    cortes_en_invierno = factor(cortes_en_invierno, levels = niveles_ordenados, ordered = TRUE),
    cortes_anuales = pmax(cortes_en_verano, cortes_en_invierno)  # toma el valor más grave
  )

tabla_cortes <- datos_limpios %>%
  tabyl(cortes_anuales) %>%
  arrange(desc(n)) %>%
  adorn_totals("row") %>%
  adorn_pct_formatting(digits = 1)
tabla_cortes

# tabla ordenada por las categorias y no las cantidades
# tabla_cortes <- datos_limpios %>%
#   tabyl(cortes_anuales) %>%
#   adorn_totals("row") %>%
#   adorn_pct_formatting(digits = 1)
# tabla_cortes

# Gráfico de barras ordenado por cantidad
datos_limpios %>%
  count(cortes_anuales) %>%
  mutate(cortes_anuales = fct_reorder(cortes_anuales, n)) %>%
  ggplot(aes(x = cortes_anuales, y = n)) +
  geom_col(fill = "#ffcc70") +
  coord_flip() +
  labs(
    title = "Frecuencia de cortes eléctricos anuales",
    x = "Respuesta",
    y = "Cantidad de hogares"
  ) +
  theme_minimal()
