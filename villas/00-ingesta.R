# Instalo los paquetes necesarios (si aún no los tengo instalados)
# install.packages("googledrive")
# install.packages("readxl")

# Cargo el archivo como .xlsx
datos <- readxl::read_excel("./Datos_LP.xlsx", 
														col_names = FALSE, 
														skip = 3)

# Veo la estructura del dataset
str(datos)
