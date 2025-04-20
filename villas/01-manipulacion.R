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
										"edad-jefe","tiempo_de_residencia","total_integrantes","grupos_familiares","total_integrantes_varones","total_integrantes_mujeres","total_integrantes_gen_disidente","total_integrantes_mayores", "total_integrantes_discapacitados",
										"dormitorios", "max_capacidad_dormitorio",
										"renabap", "intento_desalojo", "cant_desaolojos", "tiempo_desaolojo","propiedad","contrato_alquiler","costo_alquiler","aumento_alquiler","porcentaje_aumento",
										"origen_agua", "compra_agua_embotellada", "presion_agua","tanque_agua","capacidad_tanque_agua","tiene_baño","baño_fuera_casa","baño_compartido","baño_descarga","tipo_desague","agua_cocina","agua_caliente_cocina","agua_baño","agua_caliente_baño",
										"cocina_gas_natural","cocina_gas_envasado","cocina_electricidad","cocina_leña","cocina_sin_energia",
										"calefaccion_gas_natural","calefaccion_gas_envasado","calefaccion_electricidad","calefaccion_leña","sin_calefaccion","no_necesita_calefaccion","ventilacion",
										"conexion_electrica","tipo_tendido_electrico_interior","perdida_por_electricidad", "incendios_por_electricidad","cortes_en_verano","cortes_en_invierno",
										"internet","internet_celular","abonos_celulares","computadoras","cant_telefonos",
										"contrapiso","material_piso","material_techo","techo_aislante","puerta_cemento","puerta_madera","puerta_ceramico","puerta_tierra","material_paredes","terminacion_ext","tipo_terminacion_ext","pintura_ext","humedad_dormitorio","humedad_cocina","humedad_baño","humedad_living","no_humedad","humedad_otro","derrumbe_dormitorio","derrumbe_cocina","derrumbe_baño","derrumbe_living","no_derrumbe","derrumbe_otro","home_office","tipo_home_office",
										"calle_asfaltada","casa_de_pasillo","veredas_calle","alumbrado_publico","arbolado","plagas","cucarachas","mosquitos","ratas",
										"polideportivo","natatorio","playon","cancha","ejercicio","skatepark","balneario","sin_esparcimiento","esparcimiento_otro","uso_esparcimiento","placita","plaza","parque","sin_verde","uso_verde","frecuencia_colectivo","frec_colectivo_dispar","bicicleta_publica","basurales_cerca","container_basura","eliminacion_basura","frecuencia_recoleccion_residuos","riesgo_inundacion")

###################
# Modificar datos #
###################
datos_limpios <- datos %>% # Los pipelines permiten encadenar acciones
	
	mutate(   # Para crear nuevas variables y editar las ya existentes
		
		# Veo valores min y max de la variable para elegir una
		# particion en intervalos apropiada
		# min(altura)
		# max(altura)
		# sqrt(nrow(datos))
		
		# Creo una variable nueva, con la partición en intervalos de altura
		altura_int = cut(altura,
										 breaks = seq(from=0, to=50, by = 5),
										 right = F),
		
		# Modifico las columnas de la variable de respuesta múltiple
		# para dejarlas como indicadoras con valores 1 (en caso de presentar
		# el atributo) y 0 (en caso de no presentarlo)
		atracnosis = ifelse( atracnosis == "atracnosis", 1, 0 ),
		roya = ifelse( roya == "roya", 1, 0 ),
		manchas = ifelse( manchas == "manchas", 1, 0 ),
		ampollas = ifelse( ampollas == "ampollas", 1, 0),
		# Notar que los NA no entran dentro de la categoría "no presentar 
		# el atributo", por lo que requieren un tratamiento particular:
		
		atracnosis = ifelse(is.na(atracnosis), 0, 1),
		roya = ifelse(is.na(roya), 0, 1),
		manchas = ifelse(is.na(manchas), 0, 1),
		ampollas = ifelse(is.na(ampollas), 0, 1),
		# Esto solo es correcto porque teníamos dos valores posibles en estas
		# columnas: presencia de atributo (nombre de la plaga) y ausencia (NA).
		# En los casos en los que se presenten ambas categorías además del NA
		# correspondería trabajarlos como tres valores distintos (presencia,
		# ausencia y faltante) y su tratamiento dependerá de lo que se desee hacer
		
		# Para condiciones ifelse múltiples puedo usar la función case_when
		inclinacion_cate = case_when(inclinacion == 0 ~ "Sin inclinación",
																 inclinacion < 15 ~ "Inclinación leve",
																 inclinacion < 30 ~ "Inclinación moderada",
																 TRUE ~ "Inclinación alta"),
		
		# Recodifico las etiquetas de una variable categórica
		especie = recode(especie, "ala" = "Álamo",
										 "casu" = "Casuarina",
										 "euca" = "Eucalipto",
										 "jaca" = "Jacarandá",
										 "palo"  = "Palo borracho"),
		
		# Especifico ordinalidad a las categorías de una variable
		tiempo = factor(tiempo,
										levels = 1:5,
										labels = c("Menos de 2 años", "Entre 2 y 5 años",
																				 "Entre 5 y 10 años", "Entre 10 y 20 años",
																				 "20 años o más"))

	)

##########################################
# Seleccionar un subconjunto de columnas #
##########################################

# Opcion 1
datos_chico1 <- datos_limpios %>%
	select(   # Seleccionar las columnas que quiero conservar
		id, altura, edad, follaje, inclinacion_cate
	)

# Opcion 2
datos_chico2 <- datos_limpios %>%
	select(   # Eliminar las columnas que no quiero conservar
		-altura, -edad, -follaje, -inclinacion_cate
	)

# Opcion 3
datos_orden <- datos_limpios %>%
	select(   # Reordeno columnas
		id, especie, tiempo, everything()
	)


###########################################
# Seleccionar un subconjunto de registros #
###########################################

# Opción 1: por criterio
datos_reducido1 <-datos_orden %>%
	filter((brotes > 4 & origen == "Nativo/Autóctono") | tiempo == "20 años o más")

# Opción 2: por indexación
datos_reducido2 <-datos_orden %>%
	slice(1:500)
