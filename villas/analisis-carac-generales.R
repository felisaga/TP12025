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
ggplot(datos_limpios, aes(x = "",y=tiempo_de_residencia)) +
  geom_boxplot(fill = "#ffcc70") +
  labs( x = "Años", y="")
# en el boxplot se puede ver que hay outliers
# pero se ven entre el 30 y el 75 varias respuestas
datos_limpios %>%
  filter(tiempo_de_residencia >= 30 & tiempo_de_residencia <= 75) %>%
  count() # son 128 (un 10.5% de la muestra)


ggplot(datos_limpios) +
  aes(x = tiempo_de_residencia) +
  geom_histogram(fill = "lightgray", col = "black", 
                 bins = 20) + # numero de intervalos es raiz de N
  labs(x = "Años1", y = "Años") +
  theme_minimal()


#quiero sacar los outliers para que se vea mejor el grafico
valores_a_eliminar <- sort(unique(tiempo_de_residencia), decreasing = TRUE)[1:2]
primeras_ocurrencias <- match(valores_a_eliminar, tiempo_de_residencia) #indices
tiempo_filtrado <- tiempo_de_residencia[-primeras_ocurrencias]
datos_filtrados <- data.frame(tiempo_de_residencia = tiempo_filtrado)
# boxplot para mostrar la 
ggplot(datos_filtrados, aes(x = "", y = tiempo_de_residencia)) +
  geom_boxplot(fill = "#1deef5") +
  labs(x = "", y = "Años", title= "Tiempo de residencia") + 
  theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5))





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

# si bien el promedio es 4 es interesante la cantidad que tiene mas de 4
sum(total_integrantes == 10, total_integrantes == 9, total_integrantes == 8,
 total_integrantes == 7, total_integrantes == 6, total_integrantes == 5) # esto da 467



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
       x = "Cant. de Integrantes Menores",
       y = "Cantidad") +
  theme_minimal()

# ordeno por frecuencia
df <- data.frame(total_integrantes_menores) |>
  count(total_integrantes_menores) |>
  mutate(total_integrantes_menores = factor(total_integrantes_menores, 
                                            levels = total_integrantes_menores[order(n, decreasing = TRUE)]))
ggplot(df, aes(x = total_integrantes_menores, y = n)) +
  geom_segment(aes(x = total_integrantes_menores, xend = total_integrantes_menores, y = 0, yend = n),
               size = 1) +
  labs(title = "Total de Integrantes Menores",
       x = "Cantidad de Menores",
       y = "Cantidad de familias") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))




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
