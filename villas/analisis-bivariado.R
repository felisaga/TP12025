library(ggplot2)
library(tidyverse)
library(janitor)

attach(datos_limpios)

###############################################
# Tipo de conexion electrica vs tuvo incendios#
###############################################
# Tabla de contingencia
tabyl(datos_limpios, conexion_electrica, incendios_por_electricidad) %>%
	adorn_totals(where = c("row", "col")) %>%
	adorn_title(placement = "top", "Tipo de conexión eléctrica", "Incendios por electricidad")

# grafico de barras apiladas
datos_limpios %>%
  filter(!is.na(conexion_electrica), !is.na(incendios_por_electricidad)) %>%
  ggplot(aes(x = conexion_electrica, fill = incendios_por_electricidad)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(
    title = "Tipo de conexión eléctrica vs Incendios por electricidad",
    x = "Tipo de conexión eléctrica",
    y = "Porcentaje",
    fill = "¿Hubo incendios?"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(hjust=0.2, size = 13),
        axis.text.x = element_text(angle = 45, hjust = 1))

# proporciones condicionales
proporciones_incendios <- datos_limpios %>%
  tabyl(conexion_electrica, incendios_por_electricidad) %>%
  adorn_percentages("row") %>%
  adorn_pct_formatting(digits = 1) %>%
  adorn_title("combined")
proporciones_incendios

# conexion_electrica/incendios_por_electricidad    No   Sí
#                                    Comunitario 96.3% 3.7%
#                                    Con medidor 95.5% 4.5%
#                                       Informal 91.7% 8.3%
#                                   Sin conexión 96.2% 3.8%


###############################################
# Tiempo de tenencia vs Tipo de residencia #
###############################################
# boxplot comparativo
ggplot(datos_limpios, aes(x = reorder(propiedad, tiempo_de_residencia, median, na.rm = TRUE), 
                          y = tiempo_de_residencia)) +
  geom_boxplot(fill = "#1deef5") +
  labs(
    x = "Tipo de tenencia de la propiedad",
    y = "Tiempo de residencia (en años)",
    title = "Tiempo de residencia vs tipo de tenencia"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# valores resumen
resumen_tiempo_vs_propiedad <- datos_limpios %>%
  group_by(propiedad) %>%
  summarise(
    cantidad = n(),
    media = mean(tiempo_de_residencia, na.rm = TRUE),
    mediana = median(tiempo_de_residencia, na.rm = TRUE),
    min = min(tiempo_de_residencia, na.rm = TRUE),
    max = max(tiempo_de_residencia, na.rm = TRUE),
    desvio_estandar = sd(tiempo_de_residencia, na.rm = TRUE),
    iqr = IQR(tiempo_de_residencia, na.rm = TRUE)
  ) %>%
  arrange(desc(media))

resumen_tiempo_vs_propiedad


##################################
# Cant habitantes vs cant menores#
##################################
# se nota que hay datos que dicen que hay mas menores que habitantes, no tiene sentido
# agrego el + 1 porque seguian sin tener sentido algunas respuestas
# y aun asi siguen habiendo algunas sin sentido pero no quiero manipular de manera
# excesiva los datos
datos_limpios222 <- datos_limpios %>%
  filter(total_integrantes_menores < total_integrantes) 

# grafico de dispersión
ggplot(datos_limpios222) +
  aes(x = datos_limpios222$total_integrantes, y = datos_limpios222$total_integrantes_menores) +
  geom_point(color = "#1deef5") + # Color de los puntos originales
  labs(x = "Habitantes totales", y = "Habitantes menores de edad")+
  geom_jitter(width = 0, # Jitter horizontal
              height = 0.5, # Jitter vertical
              alpha = 0.4, # Transparencia
              color = "#1deef5") + # Color de los puntos jitter
  ggtitle("Relación entre la cantidad de habitantes 
          y cantidad de menores en la vivienda") +
  theme_minimal()+
  scale_x_continuous(breaks = seq(0, max(datos_limpios222$total_integrantes), by = 1)) +         # modifico los ejes para que quede bien y no ocn numeros sin sentido como 2.5
  scale_y_continuous(breaks = seq(0, max(datos_limpios222$total_integrantes_menores), by = 1))
