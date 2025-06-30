library(CausalImpact)


Timeseries_independientes <- Variables_independientes %>%
  mutate(Fecha=case_when(
    grepl("Q1", Periodo) ~ paste0(substr(Periodo,1,4), "-1-1"),
    grepl("Q2", Periodo) ~ paste0(substr(Periodo,1,4), "-4-1"),
    grepl("Q3", Periodo) ~ paste0(substr(Periodo,1,4), "-7-1"),
    grepl("Q4", Periodo) ~ paste0(substr(Periodo,1,4), "-10-1")
  )) %>% 
  filter(!Periodo %in% c("2024Q2", "2024Q1", "2024Q3",
                         "2016Q4", "2017Q1", "2017Q3",
                         "2017Q2", "2017Q4")) 



#plot(Join_timeseries_arg$`CABA Oferta alquiler Casa`,
# type = "line")

Timeseries_dependientes <- Variables_dependientes %>%
  mutate(Fecha=case_when(
    grepl("Q1", Periodo) ~ paste0(substr(Periodo,1,4), "-1-1"),
    grepl("Q2", Periodo) ~ paste0(substr(Periodo,1,4), "-4-1"),
    grepl("Q3", Periodo) ~ paste0(substr(Periodo,1,4), "-7-1"),
    grepl("Q4", Periodo) ~ paste0(substr(Periodo,1,4), "-10-1")
  )) %>% 
  filter(!Periodo %in% c("2024Q2", "2024Q1", "2024Q3", "2024Q4")) 



#Declare time series 

#Variables dependientes

oferta_casa_amba <- ts(Timeseries_dependientes$`AMBA Oferta alquiler casa`)
oferta_dpto_amba <- ts(Timeseries_dependientes$`AMBA Oferta alquiler dpto`)
oferta_casa_CABA <- ts(Timeseries_dependientes$`CABA Oferta alquiler Casa`)
oferta_dpto_CABA <- ts(Timeseries_dependientes$`CABA Oferta alquiler Departamento`)
Precio_casa_amba <- ts(Timeseries_dependientes$`AMBA Precios alquiler casA`)
Precio_dpto_amba <- ts(Timeseries_dependientes$`AMBA Precios alquiler dpto`)
Precio_casa_CABA <- ts(Timeseries_dependientes$`CABA Precios alquiler Casa`)
Precio_dpto_CABA <- ts(Timeseries_dependientes$`CABA Precios alquiler Departamento`)

#variables dependientes cordoba
oferta_casa_CBAnorte <- ts(Timeseries_dependientes$`CBA Norte Oferta alquiler Casa`)
oferta_casa_CBAcentro <- ts(Timeseries_dependientes$`CBA Centro Oferta alquiler Casa`)
#oferta_casa_CBAoeste <- ts(Timeseries_dependientes$`CBA Oeste Oferta alquiler Casa`)
oferta_casa_CBAsur <- ts(Timeseries_dependientes$`CBA Sur Oferta alquiler Casa`)
oferta_dpto_CBAnorte <- ts(Timeseries_dependientes$`CBA Norte Oferta alquiler dpto`)
oferta_dpto_CBAcentro <- ts(Timeseries_dependientes$`CBA Centro Oferta alquiler dpto`)
oferta_dpto_CBAeste <- ts(Timeseries_dependientes$`CBA Este Oferta alquiler dpto`)
oferta_dpto_CBAoeste <- ts(Timeseries_dependientes$`CBA Oeste Oferta alquiler dpto`)
oferta_dpto_CBAsur <- ts(Timeseries_dependientes$`CBA Sur Oferta alquiler dpto`)

precio_casa_CBAnorte <- ts(Timeseries_dependientes$`CBA Norte Precios alquiler Casa`)
precio_casa_CBAcentro <- ts(Timeseries_dependientes$`CBA Centro Precios alquiler Casa`)
#precio_casa_CBAoeste <- ts(Timeseries_dependientes$`CBA Oeste Precios alquiler Casa`)
precio_casa_CBAsur <- ts(Timeseries_dependientes$`CBA Sur Precios alquiler Casa`)
precio_dpto_CBAnorte <- ts(Timeseries_dependientes$`CBA Norte Precios alquiler dpto`)
precio_dpto_CBAcentro <- ts(Timeseries_dependientes$`CBA Centro Precios alquiler dpto`)
precio_dpto_CBAeste <- ts(Timeseries_dependientes$`CBA Este Precios alquiler dpto`)
precio_dpto_CBAoeste <- ts(Timeseries_dependientes$`CBA Oeste Precios alquiler dpto`)
precio_dpto_CBAsur <- ts(Timeseries_dependientes$`CBA Sur Precios alquiler dpto`)



#Variables dependientes CIUDADES
oferta_casa_Rosario <- ts(Timeseries_dependientes$`Rosario Oferta alquiler Casa`)
precio_casa_Rosario <- ts(Timeseries_dependientes$`Rosario Precios alquiler Casa`)
oferta_dpto_Rosario <- ts(Timeseries_dependientes$`Rosario Oferta alquiler Departamento`)
precio_dpto_Rosario <- ts(Timeseries_dependientes$`Rosario Precios alquiler Departamento`)
oferta_casa_Cordoba <- ts(Timeseries_dependientes$`Cordoba Oferta alquiler Casa`)
Precio_casa_Cordoba <- ts(Timeseries_dependientes$`Cordoba Precios alquiler Casa`)
oferta_dpto_Cordoba <- ts(Timeseries_dependientes$`Cordoba Oferta alquiler Departamento`)
Precio_dpto_Cordoba <- ts(Timeseries_dependientes$`Cordoba Precios alquiler Departamento`)





#Variables independientes
ripte <- ts(Timeseries_independientes$ripte_deflactado) 
TOR <- ts(Timeseries_independientes$TOR) 
TOI <- ts(Timeseries_independientes$TOI) 
TOV <- ts(Timeseries_independientes$TOV) 
gdplcupc <- ts(Timeseries_independientes$gdplcupc)
gdpusdpc <- ts(Timeseries_independientes$gdpusdpc)
DesempleoBSAS <- ts(Timeseries_independientes$DesempleoBSAS)
DesempleoCBA <- ts(Timeseries_independientes$DesempleoCBA)
DesempleoRO <- ts(Timeseries_independientes$DesempleoRO)
consumo_priv <- ts(Timeseries_independientes$`consumo privado real`)
consumo_pub <- ts(Timeseries_independientes$`consumo publico real`)
inflacion <- ts(Timeseries_independientes$Inflacion)
Unemployment <- ts(Timeseries_independientes$Unemployment)
gdpusd <- ts(Timeseries_independientes$`GDP (USD)`)
gdplcu <- ts(Timeseries_independientes$`GDP (LCU)`)
TCR <- ts(Timeseries_independientes$TCR)
IPC_real_Alquileres_uru <- ts(Timeseries_independientes$IPC_real_Alquileres_uru)
VAB_uru <- ts(Timeseries_independientes$VAB_inmo_deflactado_uru)
gdplcu_uru <- ts(Timeseries_independientes$gdplcu_uru)


#Declaramos los time points
time.points <- seq.Date(as.Date("2018-1-1"), by="quarter", length.out = 24)
print(time.points)
class(time.points)




 






#OFERTA CIUDADES DEL INTERIOR


#Cordoba Casa
oferta_casa_Cordoba <- zoo(cbind(oferta_casa_Cordoba, gdplcu, gdplcupc, consumo_priv, DesempleoCBA), time.points)
oferta_casa_Cordoba <- as.data.frame(oferta_casa_Cordoba)
pre.period <- c(1, 10)  # Pre-intervention period
post.period <- c(11, 24)  # Post-intervention period
oferta_casa_Cordoba <- CausalImpact(oferta_casa_Cordoba, pre.period, post.period)
plot(oferta_casa_Cordoba)



#Cordoba departamento
oferta_dpto_Cordoba <- zoo(cbind(oferta_dpto_Cordoba, gdpusd, gdpusdpc, DesempleoCBA), time.points)
oferta_dpto_Cordoba <- as.data.frame(oferta_dpto_Cordoba)
pre.period <- c(1, 10)  # Pre-intervention period
post.period <- c(11, 24)  # Post-intervention period
oferta_dpto_Cordoba <- CausalImpact(oferta_dpto_Cordoba, pre.period, post.period)
#plot(oferta_dpto_Cordoba)


#Rosario Casa
oferta_casa_Rosario <- zoo(cbind(oferta_casa_Rosario, gdplcu, gdplcupc, DesempleoRO), time.points)
oferta_casa_Rosario <- as.data.frame(oferta_casa_Rosario)
pre.period <- c(1, 10)  # Pre-intervention period
post.period <- c(11, 24)  # Post-intervention period
oferta_casa_Rosario <- CausalImpact(oferta_casa_Rosario, pre.period, post.period)
#plot(oferta_casa_Rosario)


#Rosario departamento
oferta_dpto_Rosario <- zoo(cbind(oferta_dpto_Rosario, gdplcu, gdplcupc, consumo_priv, ripte), time.points)
oferta_dpto_Rosario <- as.data.frame(oferta_dpto_Rosario)
pre.period <- c(1, 10)  # Pre-intervention period
post.period <- c(11, 24)  # Post-intervention period
oferta_dpto_Rosario <- CausalImpact(oferta_dpto_Rosario, pre.period, post.period)
#plot(oferta_dpto_Rosario)



#PRECIOS CIUDADES DEL INTERIOR

#Cordoba Casa
Precio_casa_Cordoba <- zoo(cbind(Precio_casa_Cordoba, gdpusd, gdplcupc, 
                                   ripte), time.points)
Precio_casa_Cordoba <- as.data.frame(Precio_casa_Cordoba)
pre.period <- c(1, 10)  # Pre-intervention period
post.period <- c(11, 24)  # Post-intervention period
Precio_casa_Cordoba <- CausalImpact(Precio_casa_Cordoba, pre.period, post.period)
#plot(Precio_casa_Cordoba)



#Cordoba departamento
Precio_dpto_Cordoba <- zoo(cbind(Precio_dpto_Cordoba, gdpusd, gdpusdpc, DesempleoCBA), time.points)
Precio_dpto_Cordoba <- as.data.frame(Precio_dpto_Cordoba)
pre.period <- c(1, 10)  # Pre-intervention period
post.period <- c(11, 24)  # Post-intervention period
Precio_dpto_Cordoba <- CausalImpact(Precio_dpto_Cordoba, pre.period, post.period)
#plot(Precio_dpto_Cordoba)


#Rosario Casa
precio_casa_Rosario <- zoo(cbind(precio_casa_Rosario, gdplcu, gdplcupc, DesempleoRO), time.points)
precio_casa_Rosario <- as.data.frame(precio_casa_Rosario)
pre.period <- c(1, 10)  # Pre-intervention period
post.period <- c(11, 24)  # Post-intervention period
precio_casa_Rosario <- CausalImpact(precio_casa_Rosario, pre.period, post.period)
#plot(precio_casa_Rosario)


#Rosario departamento
precio_dpto_Rosario <- zoo(cbind(precio_dpto_Rosario, gdplcu, gdplcupc, consumo_priv), time.points)
precio_dpto_Rosario <- as.data.frame(precio_dpto_Rosario)
pre.period <- c(1, 10)  # Pre-intervention period
post.period <- c(11, 24)  # Post-intervention period
precio_dpto_Rosario <- CausalImpact(precio_dpto_Rosario, pre.period, post.period)
#plot(precio_dpto_Rosario)




# CORDOBA ROBUSTEZ
oferta_casa_CBAnorte <- zoo(cbind(oferta_casa_CBAnorte, gdplcu, gdplcupc), time.points)
oferta_casa_CBAsur <- zoo(cbind(oferta_casa_CBAsur,gdpusdpc, gdpusd, DesempleoCBA), time.points)
oferta_casa_CBAoeste <- zoo(cbind(oferta_casa_CBAoeste,gdplcupc, gdplcu, DesempleoCBA), time.points)
oferta_casa_CBAcentro <- zoo(cbind(oferta_casa_CBAcentro,gdpusdpc, gdpusd, DesempleoCBA), time.points)

#pasamos a dataframe
oferta_casa_CBAnorte <- as.data.frame(oferta_casa_CBAnorte)
oferta_casa_CBAsur <- as.data.frame(oferta_casa_CBAsur)
oferta_casa_CBAoeste <- as.data.frame(oferta_casa_CBAoeste)
oferta_casa_CBAcentro <- as.data.frame(oferta_casa_CBAcentro)

#no debo controlar por inflacion, porque sino me saca la variable mediadora por la cual la ley de alquileres tuvo efecto
#Es mas, controlar por la inflacion me dejaria argumentar que si no hubiera sido por su aumento, no hubiera habido efectos negativos

#set pre and post intervention periods
pre.period <- c(1, 10)  # Pre-intervention period
post.period <- c(11, 24)  # Post-intervention period


#determining the causal impact en el amba
#oferta_casa_CBAnorte <- CausalImpact(oferta_casa_CBAnorte, pre.period, post.period)
oferta_casa_CBAsur <- CausalImpact(oferta_casa_CBAsur, pre.period, post.period)
oferta_casa_CBAoeste <- CausalImpact(oferta_casa_CBAoeste, pre.period, post.period)
oferta_casa_CBAcentro <- CausalImpact(oferta_casa_CBAcentro, pre.period, post.period)

#Ploteamos
plot(oferta_casa_CBAnorte)
plot(oferta_casa_CBAsur)
plot(oferta_casa_CBAoeste)
plot(oferta_casa_CBAcentro)


summary(oferta_casa_CBAnorte)
summary(oferta_casa_CBAsur)
summary(oferta_casa_CBAoeste)
summary(oferta_casa_CBAcentro)


#Oferta departamentos cordoba

oferta_dpto_CBAnorte <- zoo(cbind(oferta_dpto_CBAnorte,gdplcupc,gdplcu), time.points)
oferta_dpto_CBAsur <- zoo(cbind(oferta_dpto_CBAsur,gdplcupc, gdplcu, consumo_priv, TCR), time.points)
oferta_dpto_CBAoeste <- zoo(cbind(oferta_dpto_CBAoeste, gdplcupc, gdplcu), time.points)
oferta_dpto_CBAcentro <- zoo(cbind(oferta_dpto_CBAcentro,gdplcupc, gdplcu,DesempleoCBA), time.points)

#pasamos a dataframe
oferta_dpto_CBAnorte <- as.data.frame(oferta_dpto_CBAnorte)
oferta_dpto_CBAsur <- as.data.frame(oferta_dpto_CBAsur)
oferta_dpto_CBAoeste <- as.data.frame(oferta_dpto_CBAoeste)
oferta_dpto_CBAcentro <- as.data.frame(oferta_dpto_CBAcentro)

#no debo controlar por inflacion, porque sino me saca la variable mediadora por la cual la ley de alquileres tuvo efecto
#Es mas, controlar por la inflacion me dejaria argumentar que si no hubiera sido por su aumento, no hubiera habido efectos negativos

#set pre and post intervention periods
pre.period <- c(1, 10)  # Pre-intervention period
post.period <- c(11, 24)  # Post-intervention period


#determining the causal impact en el amba
oferta_dpto_CBAnorte <- CausalImpact(oferta_dpto_CBAnorte, pre.period, post.period)
oferta_dpto_CBAsur <- CausalImpact(oferta_dpto_CBAsur, pre.period, post.period)
oferta_dpto_CBAoeste <- CausalImpact(oferta_dpto_CBAoeste, pre.period, post.period)
oferta_dpto_CBAcentro <- CausalImpact(oferta_dpto_CBAcentro, pre.period, post.period)

#Ploteamos
plot(oferta_dpto_CBAnorte)
plot(oferta_dpto_CBAsur)
plot(oferta_dpto_CBAoeste)
plot(oferta_dpto_CBAcentro)

summary(oferta_dpto_CBAnorte)
summary(oferta_dpto_CBAsur)
summary(oferta_dpto_CBAoeste)
summary(oferta_dpto_CBAcentro)





#YA HICE TODO OFERTA EN CBA, SEGUIR CON PRECIOS

precio_casa_CBAnorte <- zoo(cbind(precio_casa_CBAnorte, gdplcu, gdplcupc), time.points)
precio_casa_CBAsur <- zoo(cbind(precio_casa_CBAsur,gdplcu, gdplcupc), time.points)
precio_casa_CBAoeste <- zoo(cbind(precio_casa_CBAoeste,gdplcu, gdplcupc), time.points)
precio_casa_CBAcentro <- zoo(cbind(precio_casa_CBAcentro,DesempleoCBA, consumo_pub), time.points)

#pasamos a dataframe
precio_casa_CBAnorte <- as.data.frame(precio_casa_CBAnorte)
precio_casa_CBAsur <- as.data.frame(precio_casa_CBAsur)
precio_casa_CBAoeste <- as.data.frame(precio_casa_CBAoeste)
precio_casa_CBAcentro <- as.data.frame(precio_casa_CBAcentro)

#no debo controlar por inflacion, porque sino me saca la variable mediadora por la cual la ley de alquileres tuvo efecto
#Es mas, controlar por la inflacion me dejaria argumentar que si no hubiera sido por su aumento, no hubiera habido efectos negativos

#set pre and post intervention periods
pre.period <- c(1, 10)  # Pre-intervention period
post.period <- c(11, 24)  # Post-intervention period


#determining the causal impact en el amba
precio_casa_CBAnorte <- CausalImpact(precio_casa_CBAnorte, pre.period, post.period)
precio_casa_CBAsur <- CausalImpact(precio_casa_CBAsur, pre.period, post.period)
precio_casa_CBAoeste <- CausalImpact(precio_casa_CBAoeste, pre.period, post.period)
precio_casa_CBAcentro <- CausalImpact(precio_casa_CBAcentro, pre.period, post.period)

#Ploteamos

plot(precio_casa_CBAnorte)
plot(precio_casa_CBAsur)
plot(precio_casa_CBAoeste)
plot(precio_casa_CBAcentro)


summary(precio_casa_CBAnorte)
summary(oferta_casa_CBAsur)
summary(precio_casa_CBAoeste)
summary(precio_casa_CBAcentro)



#Precio departamentos cordoba (ESTO E SLO ULTIMO QUE FALTA, ADEMAS DE ROBUSTEZ)
precio_dpto_CBAnorte <- zoo(cbind(precio_dpto_CBAnorte,gdplcupc,gdplcu, 
                                  consumo_priv, Unemployment), time.points)
precio_dpto_CBAsur <- zoo(cbind(precio_dpto_CBAsur,gdplcupc,
                                 consumo_pub, TCR, VAB_uru), time.points)
precio_dpto_CBAoeste <- zoo(cbind(precio_dpto_CBAoeste, gdplcupc,gdplcu,
                                  consumo_priv, consumo_pub, TCR, VAB_uru), time.points)
precio_dpto_CBAeste <- zoo(cbind(precio_dpto_CBAeste, gdplcupc,gdplcu,
                                  VAB_uru), time.points)
precio_dpto_CBAcentro <- zoo(cbind(precio_dpto_CBAcentro, gdplcupc, gdplcu,
                                   consumo_priv), time.points)

#pasamos a dataframe
precio_dpto_CBAnorte <- as.data.frame(precio_dpto_CBAnorte)
precio_dpto_CBAsur <- as.data.frame(precio_dpto_CBAsur)
precio_dpto_CBAoeste <- as.data.frame(precio_dpto_CBAoeste)
precio_dpto_CBAeste <- as.data.frame(precio_dpto_CBAeste)
precio_dpto_CBAcentro <- as.data.frame(precio_dpto_CBAcentro)

#no debo controlar por inflacion, porque sino me saca la variable mediadora por la cual la ley de alquileres tuvo efecto
#Es mas, controlar por la inflacion me dejaria argumentar que si no hubiera sido por su aumento, no hubiera habido efectos negativos

#set pre and post intervention periods
pre.period <- c(1, 10)  # Pre-intervention period
post.period <- c(11, 24)  # Post-intervention period


#determining the causal impact en el amba
precio_dpto_CBAnorte <- CausalImpact(precio_dpto_CBAnorte, pre.period, post.period)
precio_dpto_CBAsur <- CausalImpact(precio_dpto_CBAsur, pre.period, post.period)
precio_dpto_CBAoeste <- CausalImpact(precio_dpto_CBAoeste, pre.period, post.period)
precio_dpto_CBAeste <- CausalImpact(precio_dpto_CBAeste, pre.period, post.period)
precio_dpto_CBAcentro <- CausalImpact(precio_dpto_CBAcentro, pre.period, post.period)

#Ploteamos
plot(precio_dpto_CBAnorte)
plot(precio_dpto_CBAsur)
plot(precio_dpto_CBAoeste)
plot(precio_dpto_CBAeste)
plot(precio_dpto_CBAcentro) 












#ANALISIS DE AMBA

#Variables independientes


oferta_casa_amba <- zoo(cbind(oferta_casa_amba, DesempleoBSAS, gdplcu, gdplcupc,
                               consumo_pub), time.points) #ver que onda si despues inlcuimos inflacion
oferta_dpto_amba <- zoo(cbind(oferta_dpto_amba, gdplcu, gdpusdpc), time.points)      #vver que onda si despues incluimos inflacion
Precio_casa_amba <- zoo(cbind(Precio_casa_amba, 
                               gdplcupc, VAB_uru), time.points) 

Precio_dpto_amba <- zoo(cbind(Precio_dpto_amba, gdplcupc,
                                consumo_priv, VAB_uru), time.points) 

#pasamos a dataframe
oferta_casa_amba <- as.data.frame(oferta_casa_amba)
oferta_dpto_amba <- as.data.frame(oferta_dpto_amba)
Precio_casa_amba <- as.data.frame(Precio_casa_amba)
Precio_dpto_amba <- as.data.frame(Precio_dpto_amba)

#no debo controlar por inflacion, porque sino me saca la variable mediadora por la cual la ley de alquileres tuvo efecto
#Es mas, controlar por la inflacion me dejaria argumentar que si no hubiera sido por su aumento, no hubiera habido efectos negativos

#set pre and post intervention periods
pre.period <- c(1, 10)  # Pre-intervention period
post.period <- c(11, 24)  # Post-intervention period


#determining the causal impact en el amba
oferta_casa_amba <- CausalImpact(oferta_casa_amba, pre.period, post.period)
oferta_dpto_amba <- CausalImpact(oferta_dpto_amba, pre.period, post.period)
Precio_casa_amba <- CausalImpact(Precio_casa_amba, pre.period, post.period)
Precio_dpto_amba <- CausalImpact(Precio_dpto_amba, pre.period, post.period)

#Ploteamos
plot(oferta_casa_amba)
plot(oferta_dpto_amba)
plot(Precio_casa_amba)
plot(Precio_dpto_amba)

summary(oferta_casa_amba)
summary(oferta_dpto_amba, "report") 
summary(Precio_casa_amba)
summary(Precio_dpto_amba)



#ANALISIS DE CABA#
oferta_casa_CABA <- zoo(cbind(oferta_casa_CABA, gdplcu, gdplcupc), time.points) 
oferta_dpto_CABA <- zoo(cbind(oferta_dpto_CABA, gdpusd, gdplcupc, DesempleoBSAS, consumo_priv), time.points)
Precio_casa_CABA <- zoo(cbind(Precio_casa_CABA,  gdplcu, gdplcupc, Unemployment ), time.points) 
Precio_dpto_CABA <- zoo(cbind(Precio_dpto_CABA,  gdplcupc, consumo_priv, VAB_uru), time.points) 


#pasamos a dataframe
oferta_casa_CABA <- as.data.frame(oferta_casa_CABA)
oferta_dpto_CABA <- as.data.frame(oferta_dpto_CABA)
Precio_casa_CABA <- as.data.frame(Precio_casa_CABA)
Precio_dpto_CABA <- as.data.frame(Precio_dpto_CABA)

#no debo controlar por inflacion, porque sino me saca la variable mediadora por la cual la ley de alquileres tuvo efecto
#Es mas, controlar por la inflacion me dejaria argumentar que si no hubiera sido por su aumento, no hubiera habido efectos negativos

#set pre and post intervention periods
pre.period <- c(1, 10)  # Pre-intervention period
post.period <- c(11, 24)  # Post-intervention period

#Determining causal impact en CABA
oferta_casa_CABA <- CausalImpact(oferta_casa_CABA, pre.period, post.period)
oferta_dpto_CABA <- CausalImpact(oferta_dpto_CABA, pre.period, post.period)
Precio_casa_CABA <- CausalImpact(Precio_casa_CABA, pre.period, post.period)
Precio_dpto_CABA <- CausalImpact(Precio_dpto_CABA, pre.period, post.period)

plot(oferta_casa_CABA)
plot(oferta_dpto_CABA)
plot(Precio_casa_CABA)
plot(Precio_dpto_CABA)


summary(oferta_casa_CABA)
summary(oferta_dpto_CABA)
summary(Precio_casa_CABA)
summary(Precio_dpto_CABA)














#Ahora trabajamos con las vartiables desde 2016

Timeseries_independientes2016 <- Variables_independientes %>%
  mutate(Fecha=case_when(
    grepl("Q1", Periodo) ~ paste0(substr(Periodo,1,4), "-1-1"),
    grepl("Q2", Periodo) ~ paste0(substr(Periodo,1,4), "-4-1"),
    grepl("Q3", Periodo) ~ paste0(substr(Periodo,1,4), "-7-1"),
    grepl("Q4", Periodo) ~ paste0(substr(Periodo,1,4), "-10-1")
  )) %>% 
  filter(!Periodo %in% c("2024Q2", "2024Q1", "2024Q3")) 

#Para el analisis de robustez, de VAB y de precios de alquileres segun el IPC
#tomamos datos desde  2016Q4

Timeseries_dependientes2016 <- Variables_dependientes2016 %>%
  mutate(Fecha=case_when(
    grepl("Q1", Periodo) ~ paste0(substr(Periodo,1,4), "-1-1"),
    grepl("Q2", Periodo) ~ paste0(substr(Periodo,1,4), "-4-1"),
    grepl("Q3", Periodo) ~ paste0(substr(Periodo,1,4), "-7-1"),
    grepl("Q4", Periodo) ~ paste0(substr(Periodo,1,4), "-10-1")
  )) %>% 
  filter(!Periodo %in% c("2024Q2", "2024Q1", "2024Q3")) 

#VARIABLES 2016
#Dependientes
Robustez_Oferta <- ts(Timeseries_dependientes2016$`Caba Robustez Oferta`) 
VAB_inmobiliario_deflactado <- ts(Timeseries_dependientes2016$VAB_inmo_deflactado)
VAB_real_inmo_deflactado <- ts(Timeseries_dependientes2016$VAB_real_inmo_deflactado)
total_M2 <- ts(Timeseries_dependientes2016$Total) 
Reviews <- ts(Timeseries_dependientes2016$Listings) 
Ipc_real <- ts(Timeseries_dependientes2016$IPC_real_Alquileres)
#Independientes
TOR2 <- ts(Timeseries_independientes2016$TOR) 
TOI2 <- ts(Timeseries_independientes2016$TOI) 
TOV2 <- ts(Timeseries_independientes2016$TOV) 
gdplcupc2 <- ts(Timeseries_independientes2016$gdplcupc)
gdpusdpc2 <- ts(Timeseries_independientes2016$gdpusdpc)
DesempleoBSAS2 <- ts(Timeseries_independientes2016$DesempleoBSAS)
DesempleoCBA2 <- ts(Timeseries_independientes2016$DesempleoCBA)
consumo_priv2 <- ts(Timeseries_independientes2016$`consumo privado real`)
consumo_pub2 <- ts(Timeseries_independientes2016$`consumo publico real`)
inflacion2 <- ts(Timeseries_independientes2016$Inflacion)
Unemployment2 <- ts(Timeseries_independientes2016$Unemployment)
gdpusd2 <- ts(Timeseries_independientes2016$`GDP (USD)`)
gdplcu2 <- ts(Timeseries_independientes2016$`GDP (LCU)`)
TCR2 <- ts(Timeseries_independientes2016$TCR)
IPC_real_Alquileres_uru2 <- ts(Timeseries_independientes2016$IPC_real_Alquileres_uru)
VAB_uru2 <- ts(Timeseries_independientes2016$VAB_inmo_deflactado_uru)
gdplcu_uru2 <- ts(Timeseries_independientes2016$gdplcu_uru)
ripte2 <- ts(Timeseries_independientes2016$ripte_deflactado)


#Declaramos los time points
time.points <- seq.Date(as.Date("2016-10-1"), by="quarter", length.out = 29)
print(time.points)
class(time.points)


#ANALISIS DEL IPC
#IPC <- zoo(cbind(Ipc_real, consumo_priv2, gdplcu2, gdplcupc2, ripte2), time.points)
IPC <- zoo(cbind(Ipc_real, gdplcupc2,consumo_priv2,
                  TOR2), time.points) 
IPC <- as.data.frame(IPC)

#set pre and post intervention periods
pre.period <- c(1, 15)  # Pre-intervention period
post.period <- c(16, 29)  # Post-intervention period
#determining the causal impact

IPC_CI <- CausalImpact(IPC, pre.period, post.period)

plot(IPC_CI)
summary(IPC_CI, "report")

print(ripte2)


#VAB inmobiliario

VAB_inmobiliario_deflactado <- zoo(cbind(VAB_inmobiliario_deflactado, consumo_priv2), time.points) 
VAB_inmobiliario_deflactado <- as.data.frame(VAB_inmobiliario_deflactado)
pre.period <- c(1, 15)  # Pre-intervention period
post.period <- c(16, 29)  # Post-intervention period
VAB_inmobiliario_deflactado <- CausalImpact(VAB_inmobiliario_deflactado, pre.period, post.period)
#plot(VAB_inmobiliario_deflactado)
summary(VAB_inmobiliario_deflactado)

#VAB_REAL
VAB_real_inmo_deflactado <- zoo(cbind(VAB_real_inmo_deflactado, gdplcu2, gdplcupc2, Unemployment2,
                                      consumo_priv2, consumo_pub2), time.points)
VAB_real_inmo_deflactado <- as.data.frame(VAB_real_inmo_deflactado)
pre.period <- c(1, 15)  # Pre-intervention period
post.period <- c(16, 29)  # Post-intervention period
VAB_real_inmo_deflactado <- CausalImpact(VAB_real_inmo_deflactado, pre.period, post.period)
plot(VAB_real_inmo_deflactado)
summary(VAB_real_inmo_deflactado)




#ANALISIS DE ROBUSTEZ ARGENPROP, TOTAL Y POR BARRIO#
#Bindemos todo

total_M2 <- zoo(cbind(total_M2, TCR2, Unemployment2), time.points) 
Robustez_Oferta <- zoo(cbind(Robustez_Oferta, TCR2, Unemployment2), time.points) 

#no debo controlar por inflacion, porque sino me saca la variable mediadora por la cual la ley de alquileres tuvo efecto
#Es mas, controlar por la inflacion me dejaria argumentar que si no hubiera sido por su aumento, no hubiera habido efectos negativos

#pasamos a Data frame
total_M2 <- as.data.frame(total_M2)
Robustez_Oferta <- as.data.frame(Robustez_Oferta)

#definimos periodos
pre.period <- c(1, 15)  # Pre-intervention period
post.period <- c(16, 29)  # Post-intervention period

#determining the causal impact en el amba
total_M2_CI <- CausalImpact(total_M2, pre.period, post.period)
Robustez_Oferta_CI <- CausalImpact(Robustez_Oferta, pre.period, post.period)

plot(total_M2_CI)
plot(Robustez_Oferta_CI)

summary(Robustez_Oferta_CI)
summary(total_M2_CI)




#ANALISIS DEL TOTAL EN AIRBNB 
Aribnb_total <- zoo(cbind(Reviews, TCR2, Unemployment2, gdplcupc2), time.points) 
Aribnb_total <- zoo(cbind(Reviews, TCR2, Unemployment2), time.points) 

#set pre and post intervention periods

pre.period <- as.Date(c("2016-10-01", "2020-04-01"))
post.period <- as.Date(c("2020-07-01","2023-10-01"))

#determining the causal impact

impact <- CausalImpact(Aribnb_total, pre.period, post.period)
plot(impact)


#IGUAL, me da duda de que el analisis de robustez este bien hecho por mas que haya inelasticidad espacial de la demanda
#lo que tendria que ver es simplemente si es significativo el cambio de en la cantidad de propiedades en AIRBNB antes 
#comparado a despues del tratamiento
#Deberia hacer eso para los proximos pasos de la tesis

