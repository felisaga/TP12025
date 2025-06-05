library(ggplot2)
library(tidyverse)
library(janitor)

attach(datos_limpios)

####################
# tipo de tenencia #
####################
summary(propiedad)

# datos con %
datos_grafico_torta <- datos_limpios %>%
  count(propiedad) %>%
  mutate(
    pct = round(n / sum(n) * 100, 2),
    label = paste0(pct, "%")
)
# grafico de torta
ggplot(datos_grafico_torta, aes(x = "", y = pct, fill = propiedad)) +
  geom_col(color = "black") +
  coord_polar(theta = "y") +
  labs(
    title = "Distribución de la tenencia de la propiedad",
    fill = "Tipo de tenencia"
  ) +
  theme_void() +
  theme(
    legend.position = "right",
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 9)
)
# tiene muchos valores por lo que no es la mejor opcion


# datos ordenados
datos_grafico_barras <- datos_limpios %>%
  count(propiedad) %>%
  mutate(
    pct = round(n / sum(n) * 100, 2),
    propiedad = reorder(propiedad, n)
)
# grafico de barras horizontal
ggplot(datos_grafico_barras, aes(x=propiedad, y = n)) +
  geom_col(show.legend = FALSE, fill = "#1deef5") +
  geom_text(aes(label = paste0(pct, "%")), hjust = -0.1) +
  labs(
    title = "Tenencia de la propiedad",
    x = "Tipo de tenencia",
    y = "Cantidad de viviendas") +
  coord_flip() + 
  theme_minimal()

# tabla de frecuencias
tabla_propiedad <- tabyl(propiedad) %>%
  arrange(desc(n)) %>%
  adorn_pct_formatting(digits = 2) %>%
  adorn_totals()
tabla_propiedad


detach(datos_limpios)
# Ahora vamos a analizar solo datos de quienes alquilan
attach(datos_alquiler)

####################
# costo de alquiler #
####################
summary(costo_alquiler)
sd(costo_alquiler)
##############
# Histograma #
##############
ggplot(datos_alquiler) +
	aes(x = costo_alquiler) +
	geom_histogram(fill = "lightgray", col = "black", 
	               bins = ceiling(sqrt(dim(datos_alquiler)[1])) # numero de intervalos es raiz de N
								 ) +
	labs(x = "Costo alquiler ($ARS)", y = "Cant. de alquileres") +
  theme_minimal()
