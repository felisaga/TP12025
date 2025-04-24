library(ggplot2)
library(tidyverse)
library(janitor)

attach(datos_limpios)

# ver de hace que funcione esta variable
# color <- "#ffcc70"

########################
# Tiempo de residencia #
########################
summary(tiempo_de_residencia) # el maximo es 111 llama bastante la atencion
sd(tiempo_de_residencia)      # desvio estandar
var(tiempo_de_residencia)     # varianza
IQR(tiempo_de_residencia)     # rango intercuartil
# boxplot
ggplot(datos_limpios, aes(x = tiempo_de_residencia,y="")) +
  geom_boxplot(fill = "#ffcc70") +
  labs( x = "Años", y="")
# en el boxplot se puede ver que hay outliers

# pero se ven entre el 30 y el 75 varias respuestas
datos_limpios %>%
  filter(tiempo_de_residencia >= 30 & tiempo_de_residencia <= 75) %>%
  count() # son 128 (un 10.5% de la muestra)



########################
# Cant. de integrantes #
########################
summary(total_integrantes)
mean(total_integrantes)
# Tabla de frecuencias
tabla_total_integrantes <- tabyl(total_integrantes) %>%
arrange(desc(n)) %>%
adorn_pct_formatting(digits = 2) %>%
adorn_totals()
tabla_total_integrantes



########################
# Menores en la vivienda #
########################

summary(total_integrantes_menores)
tabla_total_integrantes_menores <- tabyl(total_integrantes_menores) %>%
arrange(desc(n)) %>%
adorn_pct_formatting(digits = 2) %>%
adorn_totals()
tabla_total_integrantes_menores

ggplot(data = data.frame(total_integrantes_menores), aes(x = factor(total_integrantes_menores))) +
  geom_bar(width = 0.5) +
  labs(title = "Distribución de Total de Integrantes Menores",
       x = "Total de Integrantes Menores",
       y = "Frecuencia") +
  theme_minimal()



########################
# total vs menores en la vivienda #
########################
# esto se analiza en el bivariado pero fue una primera arpoximacion
ggplot(data = data.frame(total_integrantes, total_integrantes_menores),
       aes(x = total_integrantes, y = total_integrantes_menores)) +
  geom_point(aes(color = total_integrantes_menores), size = 3, alpha = 0.8) +
  scale_color_gradient(low = "#00BFC4", high = "#F8766D") + 
  labs(
    title = "Relación entre Total de Integrantes y Menores",
    x = "Total de Integrantes",
    y = "Total de Integrantes Menores",
    color = "Menores"
  ) +
  theme_minimal() +
  theme(legend.position = "right")
