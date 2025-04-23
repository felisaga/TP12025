# Instalo los paquetes necesarios (si aún no los tengo instalados)
# install.packages("tidyverse")

# Cargo los paquetes que voy a usar
library(tidyverse)

# Fijo el dataset
attach(datos)

######################
# Renombrar columnas #
######################
colnames(datos) <- c("id","provincia","barrio",
					"edad_jefe","tiempo_de_residencia","total_integrantes","grupos_familiares","total_integrantes_varones","total_integrantes_mujeres","total_integrantes_gen_disidente","total_integrantes_menores", "total_integrantes_discapacitados",
					"dormitorios","max_capacidad_dormitorio",
					"renabap","intento_desalojo","cant_desaolojos","tiempo_desaolojo","propiedad","contrato_alquiler","costo_alquiler","aumento_alquiler","porcentaje_aumento",
					"origen_agua","compra_agua_embotellada","presion_agua","tanque_agua","capacidad_tanque_agua","tiene_baño","baño_fuera_casa","baño_compartido","baño_descarga","tipo_desague","agua_cocina","agua_caliente_cocina","agua_baño","agua_caliente_baño",
					"cocina_gas_natural","cocina_gas_envasado","cocina_electricidad","cocina_leña","cocina_sin_energia",
					"calefaccion_gas_natural","calefaccion_gas_envasado","calefaccion_electricidad","calefaccion_leña","sin_calefaccion","no_necesita_calefaccion","ventilacion",
					"conexion_electrica","tipo_tendido_electrico_interior","perdida_por_electricidad","incendios_por_electricidad","cortes_en_verano","cortes_en_invierno",
					"internet","internet_celular","abonos_celulares","computadoras","cant_telefonos",
					"contrapiso","material_piso","material_techo","techo_aislante","puerta_cemento","puerta_madera","puerta_ceramico","puerta_tierra","material_paredes","terminacion_ext","tipo_terminacion_ext","pintura_ext","humedad_dormitorio","humedad_cocina","humedad_baño","humedad_living","no_humedad","humedad_otro","derrumbe_dormitorio","derrumbe_cocina","derrumbe_baño","derrumbe_living","no_derrumbe","derrumbe_otro","home_office","tipo_home_office",
					"calle_asfaltada","casa_de_pasillo","veredas_calle","alumbrado_publico","arbolado","plagas","cucarachas","mosquitos","ratas",
					"polideportivo","natatorio","playon","cancha","ejercicio","skatepark","balneario","sin_esparcimiento","esparcimiento_otro","uso_esparcimiento","placita","plaza","parque","sin_verde","uso_verde","frecuencia_colectivo","frec_colectivo_dispar","bicicleta_publica","basurales_cerca","container_basura","eliminacion_basura","frecuencia_recoleccion_residuos","riesgo_inundacion")

###################
# Modificar datos #
###################
datos_limpios <- datos %>% # Los pipelines permiten encadenar acciones
	
	mutate(   # Para crear nuevas variables y editar las ya existentes
		# Modifico las columnas de la variable de respuesta múltiple "¿Qué fuentes de energía utilizan para cocinar en su vivienda?"
		cocina_gas_natural = ifelse(cocina_gas_natural == "Gas natural (red de gas)", 1, 0),
		cocina_gas_envasado = ifelse(cocina_gas_envasado == "Gas natural (red de gas)", 1, 0), # es gas natural por un error en el .xls
		cocina_electricidad = ifelse(cocina_electricidad == "Electricidad", 1, 0),
		cocina_leña = ifelse(cocina_leña == "Leña/Carbón", 1, 0),
		cocina_sin_energia = ifelse(cocina_sin_energia == "No tengo", 1, 0),
		# Saco los NA
		cocina_gas_natural = ifelse(is.na(cocina_gas_natural), 0, 1),
		cocina_gas_envasado = ifelse(is.na(cocina_gas_envasado), 0, 1),
		cocina_electricidad = ifelse(is.na(cocina_electricidad), 0, 1),
		cocina_leña = ifelse(is.na(cocina_leña), 0, 1),
		cocina_sin_energia = ifelse(is.na(cocina_sin_energia), 0, 1),

		# "¿Cuál es la principal fuente de energía que utiliza para calefaccionar la vivienda?"
		calefaccion_gas_natural = ifelse(calefaccion_gas_natural == "Gas natural (red de gas)", 1, 0),
		calefaccion_gas_envasado = ifelse(calefaccion_gas_envasado == "Gas envasado (garrafa)", 1, 0),
		calefaccion_electricidad = ifelse(calefaccion_electricidad == "Electricidad", 1, 0),
		calefaccion_leña = ifelse(calefaccion_leña == "Leña/Carbón", 1, 0),
		sin_calefaccion = ifelse(sin_calefaccion == "No tengo para calefaccionar mi vivienda", 1, 0),
		no_necesita_calefaccion = ifelse(no_necesita_calefaccion == "No necesito calefaccionar mi vivienda en ninguna época del año", 1, 0),
		# Saco los NA
		calefaccion_gas_natural = ifelse(is.na(calefaccion_gas_natural), 0, 1),
		calefaccion_gas_envasado = ifelse(is.na(calefaccion_gas_envasado), 0, 1),
		calefaccion_electricidad = ifelse(is.na(calefaccion_electricidad), 0, 1),
		calefaccion_leña = ifelse(is.na(calefaccion_leña), 0, 1),
		sin_calefaccion = ifelse(is.na(sin_calefaccion), 0, 1),
		no_necesita_calefaccion = ifelse(is.na(no_necesita_calefaccion), 0, 1),

		ventilacion = ifelse(is.na(ventilacion), "No", ventilacion),

		# No tiene mucho sentido puertas de cemento o ceramico, en todo caso seria material de piso
		# por eso no va a ser tenido en cuenta para su analisis
		# "¿De qué material son las puertas que dan al exterior de la vivienda? Marque TODAS las opciones que corresponda"
		puerta_cemento = ifelse(puerta_cemento == "Carpeta de cemento", 1, 0),
		puerta_madera = ifelse(puerta_madera == "Madera", 1, 0),
		puerta_ceramico = ifelse(puerta_ceramico == "Cerámico", 1, 0),
		puerta_tierra = ifelse(puerta_tierra == "Sin piso/tierra", 1, 0),
		# Saco los NA
		puerta_cemento = ifelse(is.na(puerta_cemento), 0, 1),
		puerta_madera = ifelse(is.na(puerta_madera), 0, 1),
		puerta_ceramico = ifelse(is.na(puerta_ceramico), 0, 1),
		puerta_tierra = ifelse(is.na(puerta_tierra), 0, 1),

		# "Su vivienda, ¿posee problemas de humedad graves y/o filtraciones?"
		humedad_dormitorio = ifelse(humedad_dormitorio == "Dormitorios", 1, 0),
		humedad_cocina = ifelse(humedad_cocina == "Cocina", 1, 0),
		humedad_baño = ifelse(humedad_baño == "Baño", 1, 0),
		humedad_living = ifelse(humedad_living == "Living", 1, 0),
		no_humedad = ifelse(no_humedad == "No hay ningún problema de filtraciones/humedad", 1, 0),
		humedad_otro = ifelse(humedad_otro == "Otro", 1, 0),
		# Saco los NA
		humedad_dormitorio = ifelse(is.na(humedad_dormitorio), 0, 1),
		humedad_cocina = ifelse(is.na(humedad_cocina), 0, 1),
		humedad_baño = ifelse(is.na(humedad_baño), 0, 1),
		humedad_living = ifelse(is.na(humedad_living), 0, 1),
		no_humedad = ifelse(is.na(no_humedad), 0, 1),
		humedad_otro = ifelse(is.na(humedad_otro), 0, 1),

		# "Su vivienda, ¿posee problemas estructurales graves, con riesgo de derrumbe?"
		derrumbe_dormitorio = ifelse(derrumbe_dormitorio == "Dormitorios", 1, 0),
		derrumbe_cocina = ifelse(derrumbe_cocina == "Cocina", 1, 0),
		derrumbe_baño = ifelse(derrumbe_baño == "Baño", 1, 0),
		derrumbe_living = ifelse(derrumbe_living == "Living", 1, 0),
		no_derrumbe = ifelse(no_derrumbe == "No hay ningún problema de filtraciones/humedad", 1, 0), # dice humedad por un error en el .xls
		derrumbe_otro = ifelse(derrumbe_otro == "Otro", 1, 0),
		# Saco los NA
		derrumbe_dormitorio = ifelse(is.na(derrumbe_dormitorio), 0, 1),
		derrumbe_cocina = ifelse(is.na(derrumbe_cocina), 0, 1),
		derrumbe_baño = ifelse(is.na(derrumbe_baño), 0, 1),
		derrumbe_living = ifelse(is.na(derrumbe_living), 0, 1),
		no_derrumbe = ifelse(is.na(no_derrumbe), 0, 1),
		derrumbe_otro = ifelse(is.na(derrumbe_otro), 0, 1)

	)

###########################################
# Seleccionar un subconjunto de registros #
###########################################

# Opción 1: por criterio
datos_alquiler <-datos_orden %>%
	filter(propiedad == "Alquilado")
