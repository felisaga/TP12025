library(ggplot2)
library(tidyverse)
library(janitor)

attach(datos_limpios)

##################
# presencia de plagas #
##################
resumen_presencia_plagas <- datos_limpios %>%
  count(plagas) %>%
  mutate(porcentaje = round(n / sum(n) * 100, 1),
         etiqueta = paste0(plagas," ", porcentaje, "%"))

# grafico de torta
ggplot(resumen_presencia_plagas, aes(x = "", y = n, fill = plagas)) +
  geom_col(width = 1, color = "white") +
  coord_polar(theta = "y") +
  theme_void() +
  geom_text(aes(label = etiqueta), position = position_stack(vjust = 0.5)) +
  labs(title = "Presencia de plagas en el hogar y el barrio") +
  scale_fill_brewer(palette = "Set2") +
  theme(legend.position = "none")+scale_fill_manual(values = c("Sí" = "tomato", "No" = "#1deef5")) +
  theme(
    legend.position = "none",
    plot.title = element_text(hjust = 0.5, size = 14)
  )

##################
# tipo de plaga #
##################
tabla_plagas <- datos_limpios %>%
  select(cucarachas, mosquitos, ratas) %>%
  summarise(across(everything(), sum, na.rm = TRUE)) %>%
  pivot_longer(cols = everything(), names_to = "plaga", values_to = "frecuencia") %>%
  arrange(desc(frecuencia))
tabla_plagas

# calcular la moda, la columna con mas valores
moda <- tabla_plagas$plaga[which.max(tabla_plagas$frecuencia)]
# la moda es mosquitos

ggplot(tabla_plagas, aes(x = reorder(plaga, -frecuencia), y = frecuencia)) +
  geom_col(fill = "#1deef5") +
  labs(title = "Cantidad de plagas reportadas",
       x = "Plaga",
       y = "Cantidad de viviendas con plaga") +
  theme_minimal() +
  theme(
    legend.position = "none",
    plot.title = element_text(hjust = 0.5, size = 14)
  )
