library(ggplot2)
library(tidyverse)
library(janitor)

attach(datos_limpios)

##################
# techo aislante #
##################
resumen_techo <- datos %>%
  count(techo_aislante)

# Gráfico de barras simples
ggplot(resumen_techo, aes(x = fct_reorder(techo_aislante, -n), y = n)) +
  geom_bar(stat = "identity", fill = "#ffcc70") +
  labs(title = "Aislamiento en el Techo",
       x = "Respuesta",
       y = "Cantidad de viviendas") +
  theme_minimal()

##################
# problemas de humedad #
##################
tabla_humedad <- datos_limpios %>%
  select(humedad_dormitorio, humedad_cocina, humedad_baño, humedad_living, no_humedad, humedad_otro) %>%
  summarise(across(everything(), ~sum(. == 1, na.rm = TRUE))) %>%
  pivot_longer(cols = everything(), names_to = "lugar", values_to = "cantidad")

# nombres para que el grafico quede bien
tabla_humedad$lugar <- recode(tabla_humedad$lugar,
  "humedad_dormitorio" = "Dormitorio",
  "humedad_cocina" = "Cocina",
  "humedad_baño" = "Baño",
  "humedad_living" = "Living",
  "no_humedad" = "No presenta humedad",
  "humedad_otro" = "Otro"
)
ggplot(tabla_humedad, aes(x = reorder(lugar, -cantidad), y = cantidad)) +
  geom_bar(stat = "identity", fill = "#ffcc70")+
  labs(
    title = "Lugares de humedad reportados",
    x = "Ambiente",
    y = "Respuestas"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



#########################################
# problemas de estructurales / derrumes #
#########################################
tabla_derrumbe <- datos_limpios %>%
  select(derrumbe_dormitorio, derrumbe_cocina, derrumbe_baño, derrumbe_living, no_derrumbe, derrumbe_otro) %>%
  summarise(across(everything(), ~sum(. == 1, na.rm = TRUE))) %>%
  pivot_longer(cols = everything(), names_to = "lugar", values_to = "cantidad")

# nombres para que el grafico quede bien
tabla_derrumbe$lugar <- recode(tabla_derrumbe$lugar,
  "derrumbe_dormitorio" = "Dormitorio",
  "derrumbe_cocina" = "Cocina",
  "derrumbe_baño" = "Baño",
  "derrumbe_living" = "Living",
  "no_derrumbe" = "No presenta derrumbe",
  "derrumbe_otro" = "Otro"
)
ggplot(tabla_derrumbe, aes(x = reorder(lugar, -cantidad), y = cantidad)) +
  geom_bar(stat = "identity", fill = "#ffcc70")+
  labs(
    title = "Riesgo de derrumbe reportado",
    x = "Ambiente",
    y = "Cantidad"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
