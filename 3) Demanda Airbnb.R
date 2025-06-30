#Proporiones entre Airbnb y mercadolibre por barrio


#Airbnb
setwd("C:\\Mis Archivos\\Facundo\\Tesis ley de alquileres\\Datos\\Airbnb")
#Los datos estan muy sucios, por lo que desde excel elimine:
#141 observaciones con letras donde va el ID,
#luego 27 observaciones con numeros donde va nombre

Listings <- read.csv2("listings.csv") 

#Veremos si hay duplicados analizando a mismo host_id, si hay coincidencias en latitud y longitud
#si no aparece nada, implica que no hay duplicados, pero si hay valores, puede ser porque son del mismo departamento

duplicated <- Listings %>% 
  group_by(host_id) %>%
  filter(latitude==longitude) 

#aparecen 147 observaciones que solo tienen valor en "host_id" y "name", en lat y longitud no aparece nada
#elimino esas observaciones ya que son pocas pero complican el analisis

Listings <- Listings %>% 
  filter(!latitude=="")

#Ahora, para ver la evolucion de la cantidad de transacciones en Airbnb, usamos como proxy los reviews

Reviews <- read.csv2("reviews.csv")
Reviews <- Reviews %>% 
  mutate(date=substr(Reviews$date, 4,10)) %>% 
  mutate(date=case_when(
    grepl("01/|02/|03/",Reviews$date) ~ paste0(substr(date, 4, 7),"Q1"),
    grepl("04/|05/|06/",Reviews$date) ~ paste0(substr(date, 4, 7),"Q2"),
    grepl("07/|08/|09/",Reviews$date) ~ paste0(substr(date, 4, 7),"Q3"),
    grepl("10/|11/|12/",Reviews$date) ~ paste0(substr(date, 4, 7),"Q4")
  )) 
Reviews <- Reviews %>% 
  left_join(Listings, by=c("listing_id"="id")) %>% 
  select(date, listing_id, neighbourhood) %>% 
  group_by(date, neighbourhood) %>% 
  mutate(Listings=n()) %>% 
  select(-listing_id) %>% 
  ungroup() %>% 
  filter(!duplicated(.)) %>% 
  mutate(Periodo = date)



#Lo que tendria que averiguar es ccuales son los barrios con mayor porcentage de publicaciones de Airbnb 
#pero dividido por la densidad urbana que tengan
#Entonces, primero divido la cantidad de listings por la cantidad de publicaciones en MELI (podria hacer Robustez con ARGENPROP)

#Primero divido la cantidad de publicaciones por M2 publicado en Argenprop (en base a eso, sacar porcentajes de participacion de cada barrio)
setwd("C:\\Mis Archivos\\Facundo\\Tesis ley de alquileres\\Datos\\Airbnb")

Argenprop_M2_caba_dpto <- read_excel("Argenprop_M2publicado_caba.xlsx")
Argenprop_M2_caba_dpto_eliminar <- Argenprop_M2_caba_dpto %>%
  pivot_longer(-date, names_to = "neighbourhood",
               values_to = "M2") %>% 
  filter(!neighbourhood == "Total") %>% 
  mutate(Periodo = date)

#Dado que existieron variaciones en la cantidad de M2 por barrio (aunque no muchas), podria sacar un promedio de cada dos años

Airbnb_por_M2 <- Reviews %>% 
  left_join(Argenprop_M2_caba_dpto_eliminar, 
            by=c("Periodo","neighbourhood")) %>% 
  filter(!is.na(M2)) %>% 
  mutate("L_M2"=Listings/M2) %>% 
  group_by(Periodo) %>% 
  mutate(pre_proporcion=sum(L_M2)) %>% 
  mutate(proporcion=L_M2/pre_proporcion) #esto nos dice cuantos L/M2 tenia cada barrio en relacion a los demas barrios (para cada trimestre)

#En base al grafico, podemos ver que los barrios mas prominentes en cantidad de Airbnb por M2 se mantienen bastante constantes
#Eliminaremos los barrios que lograron tener mas del 5% de la participacion de todo el mercado

valores_a_eliminar <- Airbnb_por_M2 %>%
  filter(proporcion >= 0.1) %>%     # Filtrar las filas donde B es igual a Y
  pull(neighbourhood)                # Extraer los valores correspondientes de la columna A

Airbnb_por_M2 <- Airbnb_por_M2 %>% 
  filter(!neighbourhood %in% valores_a_eliminar) 


valores_a_eliminardf <- toupper(valores_a_eliminar)
valores_a_eliminardf <- as.data.frame(valores_a_eliminardf) 
unique(valores_a_eliminardf$valores_a_eliminardf)

#LISTO, estos son los barrios que fueron afectados por Airbnb

#Analisis para la estrategia de identificacion
#Sumar los Metros cuadrados por barrio con datos de Argenprop

Argenprop_M2_caba_dpto <- Argenprop_M2_caba_dpto %>% 
  select(-Palermo, -Retiro, -`San Telmo`, -Recoleta, -Chacarita, -`Puerto Madero`, -`Parque Chas`,
         #-Belgrano, -`Villa Crespo`, -`Villa Crespo`, `Parque Patricios`, `Boca`, `Barracas`, `Colegiales`, `Floresta`,`Nueva Pompeya`, `Boedo`, `Agronomia`
         ) %>% 
  mutate(`Caba Robustez Oferta` = rowSums(select(., -date, -Total))) %>% 
  mutate(pais = "Argentina") %>% 
  mutate( Periodo = date) %>% 
  select(Periodo, Total, `Caba Robustez Oferta`, pais)

#Armamos una tabla que sume toda la cantidad de Reviews en Airbnb
Reviews <- Reviews %>% 
  select(Periodo, Listings) %>% 
  group_by(Periodo) %>% 
  summarise(Listings=sum(Listings)) %>% 
  mutate(pais = "Argentina")
  

#Joineamos

Variables_dependientes2016 <- Argenprop_M2_caba_dpto %>% 
  full_join(Reviews, by=c("Periodo","pais")) %>% 
  left_join(VAB_inmobiliario_deflactado) %>% 
  select(Periodo,Listings, Total,`Caba Robustez Oferta`, VAB_inmo_deflactado, IPC_real_Alquileres, VAB_real_inmo_deflactado) %>% 
  na.omit(.)













