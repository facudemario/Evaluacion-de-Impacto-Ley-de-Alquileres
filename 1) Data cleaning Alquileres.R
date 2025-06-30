library(tidyverse)
library(readr)
library(dplyr)
library(igraph)
library(readxl)
library(ggplot2)
library(Synth)
library(readxl)
library(openxlsx)


#Calleamos los datos que usamos

#Datos de Encuestas
setwd("C:\\Mis Archivos\\Facundo\\Tesis ley de alquileres\\Datos\\Datos (variable dependiente)\\Encuestas\\Precios Alquileres")
ipc_arg_nacional <- read_csv2("IPC Argentina (base 2004)\\Por provincia.csv")
ipc_ecu_nacional <- read_excel("IPC ecuador (Base 2014)\\ipc ecuador.xlsx") 
ipc_col_nacional <- read_excel("IPC Colombia (Base 2018)\\IPC_colombia.xlsx") 
ipc_mex_alq_real <- read_excel("IPC Mexico (Base 2da quincena Agosto 2015)\\IPC Mexico.xlsx") 
ipc_uru_alq_real <- read_excel("IPC uruguay (Base 2021)\\IPC Uruguay.xlsx") %>% 
  mutate(pais="Uruguay") %>% 
  mutate(IPC_real_Alquileres=IPC_real_Alquileres/1.2383511)
ipc_peru_nacional <- read_excel("IPC Peru (Base 2021)\\IPC Peru.xlsx") 
ipc_Alq_g20 <- read.csv2("IPC G20etc.csv")
setwd("C:\\Mis Archivos\\Facundo\\Tesis ley de alquileres\\Datos\\Correlating data\\G20 Data")
ipc_g20 <- read.csv2("IPC.csv")


#Datos de Precios de oferta y Oferta de portales inmobiliarios
setwd("C:\\Mis Archivos\\Facundo\\Tesis ley de alquileres\\Datos\\Datos (variable dependiente)\\Portales inmobiliarios\\Argentina (MELI) precio mediano y oferta")
Alquileres_BSAS <- read_excel("alquileresagrupacion_202402.xlsx")
setwd("C:\\Mis Archivos\\Facundo\\Tesis ley de alquileres\\Datos\\Datos (variable dependiente)\\Portales inmobiliarios\\Argentina (MELI) precio mediano y oferta")
Alquileres_Cordoba_aglomerados <- read_excel("alquileresagrupacion_202409_interior_cordoba.xlsx")
setwd("C:\\Mis Archivos\\Facundo\\Tesis ley de alquileres\\Datos\\Datos (variable dependiente)\\Portales inmobiliarios\\Peru (Urbania.com) precio mediano y oferta")
Alquileres_peru <- read_excel("Oferta y precio Peru.xlsx")
setwd("C:\\Mis Archivos\\Facundo\\Tesis ley de alquileres\\Datos\\Datos (variable dependiente)\\Portales inmobiliarios\\Chile (portal inmobiliario) precio mediano y oferta")
Alquileres_chile <- read_excel("Datos chile.xlsx") %>% 
  mutate(pais="Chile")
setwd("C:\\Mis Archivos\\Facundo\\Tesis ley de alquileres\\Datos\\Datos (variable dependiente)\\Portales inmobiliarios\\Uruguay (MELI) Precios medianos constantes a 2021")
Alquileres_uruguay <- read_excel("Precios medianos constantes a 2021.xlsx") %>% 
  mutate(pais="Uruguay")
setwd("C:\\Mis Archivos\\Facundo\\Tesis ley de alquileres\\Datos\\Datos (variable dependiente)\\Portales inmobiliarios\\Argentina (MELI) precio mediano y oferta")
Alquileres_interior <- read_excel("alquileresmunicipios_202412_interior.xlsx")

#Datos de precios de oferta de portales inmobiliarios
setwd("C:\\Mis Archivos\\Facundo\\Tesis ley de alquileres\\Datos\\Datos (variable dependiente)\\Portales inmobiliarios\\Brazil (zap.com) precio mediano")
Precios_Brasil <- read_excel("Precio Brasil (Precios medios).xlsx")  

#Datos de variables independientes
setwd("C:\\Mis Archivos\\Facundo\\Tesis ley de alquileres\\Datos\\Correlating data\\World Monitor que me sirve")
Inflation <- read_excel("CPI Price, % y-o-y, nominal, seas. adj. (Inflacion, ajustes estacionales).xlsx")
Exp_Merch <- read_excel("Exports Merchandise, Customs, constant 2010 US$, millions, seas. adj. (goods).xlsx")
GDP_LCU <- read_excel("GDP at market prices, constant 2010 LCU, millions, seas. adj..xlsx")
GDP_USD <- read_excel("GDP at market prices, constant 2010 US$, millions, seas. adj..xlsx")
Imp_Merch <- read_excel("Imports Merchandise, Customs, constant 2010 US$, millions, seas. adj. (goods).xlsx")
Stock_Market_USD <- read_excel("Stock Markets, US$.xlsx")
Total_Reserves <- read_excel("Total Reserves.xlsx")
Retail <- read_excel("Retail Sales Volume Index, seas. adj. (Local equity market index valued in local currency unit (LCU) terms).xlsx")
Effective_TCR <- read_excel("Real Effective Exchange Rate.xlsx")
Unemployment <- read_excel("Unemployment Rate, seas. adj..xlsx")
setwd("C:\\Mis Archivos\\Facundo\\Tesis ley de alquileres\\Datos\\Correlating data\\VAB del sector inmobiliario")
VAB_inmobiliario <- read_excel("VAB del sector inmobiliario, precios corrientes.xlsx")
setwd("C:\\Mis Archivos\\Facundo\\Tesis ley de alquileres\\Datos\\Correlating data")
Consumo <- read_excel("Consumo.xlsx")
Percapita <- read_csv("percapita.csv")
RIPTE <- read_csv2("RIPTE.csv")



#Data Cleaning

#Encuestas#

#IPC argentina (el ipc junta todos los centros urbanos y solo desgrana por localidad en partidos del gran buenos aires, no en otras provincias)
ipc_arg_nacional_alquileres <- ipc_arg_nacional %>% 
  filter(Codigo=="04.1.1") %>%
  group_by(Periodo) %>% 
  summarise(IPC_Alq=mean(Indice_IPC)) %>% 
  ungroup() %>% 
  mutate(Trimestre = case_when(
    grepl("01$|02$|03$", Periodo) ~ paste0(substr(Periodo, 1, 4), "Q1"),
    grepl("04$|05$|06$$", Periodo) ~ paste0(substr(Periodo, 1, 4), "Q2"),
    grepl("07$|08$|09$", Periodo) ~ paste0(substr(Periodo, 1, 4), "Q3"),
    grepl("10$|11$|12$", Periodo) ~ paste0(substr(Periodo, 1, 4), "Q4")
  )) %>% 
  group_by(Trimestre) %>% 
  select(Periodo = Trimestre, IPC_Alq) %>% 
  summarise(IPC_Alq=mean(IPC_Alq))

ipc_arg_nacional_general <- ipc_arg_nacional %>% 
  filter(Codigo=="0") %>% 
  group_by(Periodo) %>% 
  summarise(Inflacion=mean(Indice_IPC)) %>% 
  ungroup() %>% 
  mutate(Trimestre = case_when(
    grepl("01$|02$|03$", Periodo) ~ paste0(substr(Periodo, 1, 4), "Q1"),
    grepl("04$|05$|06$$", Periodo) ~ paste0(substr(Periodo, 1, 4), "Q2"),
    grepl("07$|08$|09$", Periodo) ~ paste0(substr(Periodo, 1, 4), "Q3"),
    grepl("10$|11$|12$", Periodo) ~ paste0(substr(Periodo, 1, 4), "Q4")
  )) %>% 
  group_by(Trimestre) %>% 
  select(Periodo = Trimestre, Inflacion) %>% 
  summarise(Inflacion=mean(Inflacion))

ipc_arg_nacional_alquileres_real <- ipc_arg_nacional_alquileres %>% 
  full_join(ipc_arg_nacional_general, by="Periodo") %>% 
  mutate(IPC_real_Alquileres=IPC_Alq/Inflacion) %>% 
  mutate(pais="Argentina")

#ipc ecuador
ipc_ecu_nacional_real <- ipc_ecu_nacional %>%
  mutate(General=as.double(GENERAL))%>%
  mutate(Ipc_alq_ecu=as.double(`Alquileres efectivos pagados por los inquilinos (S)`)) %>% 
  group_by(Periodo) %>% 
  summarise(Ipc_alq_ecu=mean(Ipc_alq_ecu), General=mean(General)) %>% 
  mutate(Periodo = case_when(
    grepl("-01-01$|-02-01$|-03-01$", Periodo) ~ paste0(substr(Periodo, 1, 4), "Q1"),
    grepl("-04-01$|-05-01$|-06-01$", Periodo) ~ paste0(substr(Periodo, 1, 4), "Q2"),
    grepl("-07-01$|-08-01$|-09-01$", Periodo) ~ paste0(substr(Periodo, 1, 4), "Q3"),
    grepl("-10-01$|-11-01$|-12-01$", Periodo) ~ paste0(substr(Periodo, 1, 4), "Q4")
  )) %>% 
  select(Periodo, Inflacion=General, IPC_Alq=Ipc_alq_ecu) %>% 
  mutate(IPC_real_Alquileres=(IPC_Alq/Inflacion)) %>% 
  ungroup() %>% 
  group_by(Periodo) %>% 
  select(Periodo, Inflacion, IPC_real_Alquileres) %>% 
  summarise(IPC_real_Alquileres=mean(IPC_real_Alquileres), Inflacion=mean(Inflacion)) %>% 
  mutate(pais="Ecuador")

#Ahora, hacemos que 2016Q4 sea el punto de expansion para IPC_real

ipc_ecu_nacional_real <- ipc_ecu_nacional_real %>% 
  mutate(IPC_real_Alquileres=1.066223)

#Colombia
ipc_col_nacional_alquileres <- ipc_col_nacional %>% 
  select(Periodo=Trimestre , IPC_real_Alquileres=IPC_Alq_real_col, Inflacion=general) %>% 
  mutate(pais="Colombia")


#Pasamos a punto de expansion 2016Q4
ipc_col_nacional_alquileres <- ipc_col_nacional_alquileres %>% 
  mutate(IPC_real_Alquileres=as.double(IPC_real_Alquileres)) %>% 
  mutate(IPC_real_Alquileres=IPC_real_Alquileres/0.9983121)


#Mexico
ipc_mex_alq_real <- ipc_mex_alq_real %>% 
  group_by(Periodo) %>% 
  summarise(IPC_Alq=mean(IPC_Alq), Inflacion=mean(Inflacion),IPC_real_Alquileres=mean(IPC_real_Alquileres)) %>% 
  mutate(Periodo = case_when(
    grepl("-01-01$|-02-01$|-03-01$", Periodo) ~ paste0(substr(Periodo, 1, 4), "Q1"),
    grepl("-04-01$|-05-01$|-06-01$", Periodo) ~ paste0(substr(Periodo, 1, 4), "Q2"),
    grepl("-07-01$|-08-01$|-09-01$", Periodo) ~ paste0(substr(Periodo, 1, 4), "Q3"),
    grepl("-10-01$|-11-01$|-12-01$", Periodo) ~ paste0(substr(Periodo, 1, 4), "Q4")
  )) %>% 
  select(Periodo, Inflacion, IPC_real_Alquileres)%>% 
  ungroup() %>% 
  group_by(Periodo) %>% 
  select(Periodo, Inflacion, IPC_real_Alquileres) %>% 
  summarise(IPC_real_Alquileres=mean(IPC_real_Alquileres), Inflacion=mean(Inflacion)) %>% 
  mutate(pais="Mexico") 


#Pasamos a punto de expansion 2016Q4
ipc_mex_alq_real <- ipc_mex_alq_real %>% 
  mutate(IPC_real_Alquileres=IPC_real_Alquileres/0.9553252) 


#Peru
ipc_peru_nacional <- ipc_peru_nacional %>% 
  mutate(Periodo = case_when(
    grepl("Enero$|Febrero$|Marzo$", Periodo) ~ paste0(substr(Periodo, 1, 4), "Q1"),
    grepl("Abril$|Mayo$|Junio$", Periodo) ~ paste0(substr(Periodo, 1, 4), "Q2"),
    grepl("Julio$|Agosto$|Setiembre$", Periodo) ~ paste0(substr(Periodo, 1, 4), "Q3"),
    grepl("Octubre$|Noviembre$|Diciembre$", Periodo) ~ paste0(substr(Periodo, 1, 4), "Q4")
  )) %>% 
  group_by(Periodo) %>% 
  summarise(Inflacion=mean(Inflacion)) %>% 
  mutate(pais="Peru")
  
  
#G20/OECD

ipc_G20_OECD <- ipc_Alq_g20 %>% 
  full_join(ipc_g20, by=c("pais","Periodo","Base")) %>% 
  mutate(Inflacion=as.double(Inflacion)) %>% 
  mutate(IPC_Alq=as.double(IPC_Alq)) %>% 
  mutate(IPC_real_Alquileres=IPC_Alq/Inflacion) %>% 
  mutate(Periodo=case_when(
    grepl("-Q1$", Periodo) ~ paste0(substr(Periodo, 1, 4), "Q1"),
    grepl("-Q2$", Periodo) ~ paste0(substr(Periodo, 1, 4), "Q2"),
    grepl("-Q3$", Periodo) ~ paste0(substr(Periodo, 1, 4), "Q3"),
    grepl("-Q4$", Periodo) ~ paste0(substr(Periodo, 1, 4), "Q4")
  )) 


IPC_paises <- ipc_arg_nacional_alquileres_real %>% 
  full_join(ipc_col_nacional_alquileres, by=c("Periodo", "Inflacion", "IPC_real_Alquileres", "pais")) %>%
  full_join(ipc_ecu_nacional_real, by=c("Periodo", "Inflacion", "IPC_real_Alquileres", "pais")) %>% 
  full_join(ipc_mex_alq_real, by=c("Periodo", "Inflacion", "IPC_real_Alquileres", "pais")) %>% 
  full_join(ipc_uru_alq_real, by=c("Periodo", "Inflacion", "IPC_real_Alquileres", "pais", "IPC_Alq")) %>% 
  full_join(ipc_peru_nacional) %>% 
  full_join(ipc_G20_OECD)
  
  
#Portales
  
#(PERU)
Alquileres_peru <- Alquileres_peru %>% 
  mutate(ID=row_number())%>% 
  group_by(Trimestre) %>%
  mutate(oferta=n()) %>% 
  ungroup() %>% 
  group_by(Distrito, Trimestre) %>% 
  mutate(precios_medianos=median(`Alquiler mensual en soles corrientes`)) %>% 
  ungroup() %>% 
  group_by(Trimestre) %>% 
  mutate(Precios_medianos_promedio=ave(precios_medianos)) %>% 
  ungroup() %>% 
  select(Trimestre, oferta, Precios_medianos_promedio) %>% 
  filter(!duplicated(.))

Trimestrar <- function(Alquileres_peru) {
  Alquileres_peru <- Alquileres_peru    %>% 
    mutate(Trimestre = case_when(
      grepl("-1$", Trimestre) ~ paste0(substr(Trimestre, 1, 4), "Q1"),
      grepl("-2$", Trimestre) ~ paste0(substr(Trimestre, 1, 4), "Q2"),
      grepl("-3$", Trimestre) ~ paste0(substr(Trimestre, 1, 4), "Q3"),
      grepl("-4$", Trimestre) ~ paste0(substr(Trimestre, 1, 4), "Q4")
    )) %>% 
    group_by(Trimestre) %>% 
    select(Periodo = Trimestre, Precios_medianos_promedio, oferta) %>% 
    ungroup()
  
  return(Alquileres_peru)
}

Alquileres_peru <- Trimestrar(Alquileres_peru) %>% 
  full_join(ipc_peru_nacional) %>%  
  mutate(precios_medianos_reales=Precios_medianos_promedio/Inflacion) %>% 
  mutate("Oferta alquiler"=oferta/813) %>% 
  mutate("Precios alquiler"=precios_medianos_reales) %>% 
  select(-Precios_medianos_promedio, -precios_medianos_reales, -oferta, -Inflacion) 


#(BRASIL)
Mesificar <- function(Precios_Brasil) {
  Precios_Brasil <- Precios_Brasil    %>% 
    mutate(Mes = case_when(
      grepl("-01", Data) ~ paste0(substr(Data, 1, 7)),
    )) %>%
    group_by(Mes) %>%
    ungroup() %>%
    select(Periodo = Mes, `Precio Mediano`)
  
  return(Precios_Brasil)
}
Precios_Brasil <- Mesificar(Precios_Brasil)

Trimestrar <- function(Precios_Brasil) {
  Precios_Brasil <- Precios_Brasil    %>% 
    mutate(Trimestre = case_when(
      grepl("-01$|-02$|-03$", Periodo) ~ paste0(substr(Periodo, 1, 4), "Q1"),
      grepl("-04$|-05$|-06$", Periodo) ~ paste0(substr(Periodo, 1, 4), "Q2"),
      grepl("-07$|-08$|-09$", Periodo) ~ paste0(substr(Periodo, 1, 4), "Q3"),
      grepl("-10$|-11$|-12$", Periodo) ~ paste0(substr(Periodo, 1, 4), "Q4")
    )) %>%
    group_by(Trimestre) %>%
    summarise(`Precio Mediano` = mean(`Precio Mediano`)) %>%
    ungroup() %>%
    select(Periodo = Trimestre, `Precios alquiler`=`Precio Mediano`)
  
  return(Precios_Brasil)
}
Precios_Brasil <- Trimestrar(Precios_Brasil) 
Precios_Brasil <- Precios_Brasil %>% 
  mutate(pais="Brazil")


# VARIABLES INDEPENDIENTES #

#Ahora, creamos la tabla con las variables que correlacionan

#PBI, poblacion
Percapita <- Percapita %>% 
  mutate(Periodo = case_when(
    grepl("-01$|", indice_tiempo) ~ paste0(substr(indice_tiempo, 1, 7))
  ))  %>% 
  mutate(Periodo = case_when(
    grepl("-01$", Periodo) ~ paste0(substr(Periodo, 1, 4), "Q1"),
    grepl("-04$", Periodo) ~ paste0(substr(Periodo, 1, 4), "Q2"),
    grepl("-07$", Periodo) ~ paste0(substr(Periodo, 1, 4), "Q3"),
    grepl("-10$", Periodo) ~ paste0(substr(Periodo, 1, 4), "Q4")
  )) %>%
  mutate(poblacion =poblacion/1000000) %>% 
  select(Periodo, poblacion) %>%
  mutate(pais="Argentina") %>% 
  filter(!Periodo %in% c("2024Q1","2024Q2","2024Q3"))


#RIPTE
RIPTE <- RIPTE %>% 
  group_by(Periodo) %>% 
  summarize(Periodo, ripte=mean(ripte)) %>% 
  filter(!duplicated(Periodo)) %>% 
  right_join(ipc_arg_nacional_general, by="Periodo") %>% 
  mutate(ripte_deflactado=ripte/Inflacion) %>% 
  select(Periodo, ripte_deflactado)
  

#Inflacion
Inflation <- pivot_longer(Inflation, cols = -Periodo, names_to = "pais", values_to = "Inflation") %>% 
  filter(!Periodo=="1994M06") %>% 
  filter(!Periodo=="1994M05")  

#Consumo (pesos corrientes, deflactamos)
Consumo <- ipc_arg_nacional_general  %>% 
  filter(!Periodo=="2024Q1") %>% 
  filter(!Periodo=="2024Q2") %>% 
  mutate(pais = "Argentina") %>%
  left_join(Consumo, by="Periodo") %>% 
  mutate(`consumo privado real` = Consumo$`Consumo  privado nacional`/Inflacion) %>% 
  mutate(`consumo publico real` = Consumo$`Consumo público nacional`/Inflacion)


#Export Merchandise constant 2010 (goods)
Exp_Merch <- pivot_longer(Exp_Merch, cols = -Periodo, names_to = "pais", values_to = "Export Goods") %>% 
  filter(!is.na(Periodo))
Exp_Merch <- Exp_Merch %>% 
  mutate(`Export Goods` = na_if(`Export Goods`, "NA"))

#GDP at market prices, constant 2010 LCU, millions, seas. adj.
GDP_LCU <- pivot_longer(GDP_LCU, cols = -Periodo, names_to = "pais", values_to = "GDP (LCU)") %>% 
  filter(!is.na(Periodo))
GDP_LCU <- GDP_LCU %>% 
  mutate(`GDP (LCU)` = na_if(`GDP (LCU)`, "NA")) %>% 
  mutate(`GDP (LCU)`=as.double(`GDP (LCU)`)) %>% 
  full_join(Percapita) %>% 
  mutate(gdplcupc= `GDP (LCU)`/poblacion) %>% 
  select(everything(), -poblacion)




#GDP at market prices, constant 2010 US$, millions, seas. adj.
GDP_USD <- pivot_longer(GDP_USD, cols = -Periodo, names_to = "pais", values_to = "GDP (USD)") %>% 
  filter(!is.na(Periodo))
GDP_USD <- GDP_USD %>% 
  mutate(`GDP (USD)` = na_if(`GDP (USD)`, "NA")) %>% 
  mutate(`GDP (USD)`=as.double(`GDP (USD)`)) %>% 
  full_join(Percapita) %>% 
  mutate(gdpusdpc= `GDP (USD)`/poblacion)

#Import Merchandise constant 2010 (goods)
Imp_Merch <- pivot_longer(Imp_Merch, cols = -Periodo, names_to = "pais", values_to = "Import goods") %>% 
  filter(!is.na(Periodo))
Imp_Merch <- Imp_Merch %>% 
  mutate(`Import goods` = na_if(`Import goods`, "NA"))

#Effective TCR
Effective_TCR <- Effective_TCR %>%
  mutate(`Advanced Economies` = as.character(`Advanced Economies`)) %>% 
  mutate(`Argentina` = as.character(`Argentina`)) %>% 
  mutate(`Australia` = as.character(`Australia`)) %>% 
  mutate(`Brazil` = as.character(`Brazil`)) %>% 
  mutate(`Canada` = as.character(`Canada`)) %>% 
  mutate(`Switzerland` = as.character(`Switzerland`)) %>% 
  mutate(`Chile` = as.character(`Chile`)) %>% 
  mutate(`China` = as.character(`China`)) %>% 
  mutate(`Colombia` = as.character(`Colombia`)) %>% 
  mutate(`Czech Republic` = as.character(`Czech Republic`)) %>% 
  mutate(`Denmark` = as.character(`Denmark`)) %>% 
  mutate(`EMDE East Asia & Pacific` = as.character(`EMDE East Asia & Pacific`)) %>% 
  mutate(`Ecuador` = as.character(`Ecuador`)) %>% 
  mutate(`Egypt, Arab Rep.` = as.character(`Egypt, Arab Rep.`)) %>% 
  mutate(`Emerging Market and Developing Economies (EMDEs)` = as.character(`Emerging Market and Developing Economies (EMDEs)`)) %>% 
  mutate(`United Kingdom` = as.character(`United Kingdom`)) %>% 
  mutate(`High Income Countries` = as.character(`High Income Countries`)) %>% 
  mutate(`Hong Kong SAR, China` = as.character(`Hong Kong SAR, China`)) %>% 
  mutate(`Croatia` = as.character(`Croatia`)) %>% 
  mutate(`Hungary` = as.character(`Hungary`)) %>% 
  mutate(`Indonesia` = as.character(`Indonesia`)) %>% 
  mutate(`India` = as.character(`India`)) %>% 
  mutate(`Israel` = as.character(`Israel`)) %>% 
  mutate(`Japan` = as.character(`Japan`)) %>% 
  mutate(`Korea, Rep.` = as.character(`Korea, Rep.`)) %>% 
  mutate(`Kuwait` = as.character(`Kuwait`)) %>% 
  mutate(`EMDE Latin America & Caribbean` = as.character(`EMDE Latin America & Caribbean`)) %>% 
  mutate(`Morocco` = as.character(`Morocco`)) %>% 
  mutate(`Mexico` = as.character(`Mexico`)) %>% 
  mutate(`Middle-Income Countries (MIC)` = as.character(`Middle-Income Countries (MIC)`)) %>% 
  mutate(`EMDE Middle East & N. Africa` = as.character(`EMDE Middle East & N. Africa`)) %>% 
  mutate(`Malaysia` = as.character(`Malaysia`)) %>% 
  mutate(`Norway` = as.character(`Norway`)) %>% 
  mutate(`Pakistan` = as.character(`Pakistan`))  %>% 
  mutate(`Norway` = as.character(`Norway`))  %>% 
  mutate(`Peru` = as.character(`Peru`))  %>% 
  mutate(`Philippines` = as.character(`Philippines`))  %>% 
  mutate(`Poland` = as.character(`Poland`))  %>% 
  mutate(`Romania` = as.character(`Romania`))  %>% 
  mutate(`EMDE South Asia` = as.character(`EMDE South Asia`))  %>% 
  mutate(`Saudi Arabia` = as.character(`Saudi Arabia`))  %>% 
  mutate(`Singapore` = as.character(`Singapore`))  %>% 
  mutate(`Sweden` = as.character(`Sweden`)) %>% 
  mutate(`Thailand` = as.character(`Thailand`)) %>% 
  mutate(`Turkey` = as.character(`Turkey`)) %>% 
  mutate(`Taiwan, China` = as.character(`Taiwan, China`)) %>% 
  mutate(`United States` = as.character(`United States`)) %>% 
  mutate(`World (WBG members)` = as.character(`World (WBG members)`)) %>% 
  mutate(`South Africa` = as.character(`South Africa`)) %>% 
  filter(!Periodo=="1994M06") %>% 
  filter(!Periodo=="1994M05") 
  
Effective_TCR <- pivot_longer(Effective_TCR, cols = -Periodo, names_to = "pais", values_to = "TCR")
Effective_TCR <- Effective_TCR %>% 
  filter(!is.na(Periodo)) %>% 
  mutate(TCR = na_if(TCR, "NA"))

#Retail Sales Volume, LCU (index)
Retail <- pivot_longer(Retail, cols = -Periodo, names_to = "pais", values_to = "Retail") %>% 
  filter(!is.na(Periodo)) %>% 
  filter(!Periodo=="1994M06") %>% 
  filter(!Periodo=="1994M05") 
Retail <- Retail %>% 
  mutate(Retail = na_if(Retail, "NA"))

#Stock Markets, USD
Stock_Market_USD <- Stock_Market_USD %>% 
  mutate(`Argentina` = as.character(`Argentina`)) %>% 
  mutate(`Australia` = as.character(`Australia`)) %>% 
  mutate(`Austria` = as.character(`Austria`)) %>% 
  mutate(`Brazil` = as.character(`Brazil`)) %>% 
  mutate(`Belgium` = as.character(`Belgium`)) %>% 
  mutate(`Canada` = as.character(`Canada`)) %>% 
  mutate(`Switzerland` = as.character(`Switzerland`)) %>% 
  mutate(`Chile` = as.character(`Chile`)) %>% 
  mutate(`China` = as.character(`China`)) %>% 
  mutate(`Colombia` = as.character(`Colombia`)) %>% 
  mutate(`Czech Republic` = as.character(`Czech Republic`)) %>% 
  mutate(`Denmark` = as.character(`Denmark`)) %>% 
  mutate(`Egypt, Arab Rep.` = as.character(`Egypt, Arab Rep.`)) %>% 
  mutate(`United Kingdom` = as.character(`United Kingdom`)) %>% 
  mutate(`France` = as.character(`France`)) %>% 
  mutate(`High Income Countries` = as.character(`High Income Countries`)) %>% 
  mutate(`Hong Kong SAR, China` = as.character(`Hong Kong SAR, China`)) %>% 
  mutate(`Croatia` = as.character(`Croatia`)) %>% 
  mutate(`Germany` = as.character(`Germany`)) %>% 
  mutate(`Hungary` = as.character(`Hungary`)) %>% 
  mutate(`Indonesia` = as.character(`Indonesia`)) %>% 
  mutate(`India` = as.character(`India`)) %>% 
  mutate(`Israel` = as.character(`Israel`)) %>% 
  mutate(`Japan` = as.character(`Japan`)) %>% 
  mutate(`Korea, Rep.` = as.character(`Korea, Rep.`)) %>% 
  mutate(`Morocco` = as.character(`Morocco`)) %>% 
  mutate(`Mexico` = as.character(`Mexico`)) %>% 
  mutate(`Malaysia` = as.character(`Malaysia`)) %>% 
  mutate(`Norway` = as.character(`Norway`)) %>% 
  mutate(`Netherlands` = as.character(`Netherlands`)) %>% 
  mutate(`Pakistan` = as.character(`Pakistan`))  %>% 
  mutate(`Norway` = as.character(`Norway`))  %>% 
  mutate(`Peru` = as.character(`Peru`))  %>% 
  mutate(`Philippines` = as.character(`Philippines`))  %>% 
  mutate(`Poland` = as.character(`Poland`))  %>% 
  mutate(`Portugal` = as.character(`Portugal`))  %>% 
  mutate(`Romania` = as.character(`Romania`))  %>% 
  mutate(`Saudi Arabia` = as.character(`Saudi Arabia`))  %>% 
  mutate(`Singapore` = as.character(`Singapore`))  %>% 
  mutate(`Sweden` = as.character(`Sweden`)) %>% 
  mutate(`Spain` = as.character(`Spain`)) %>% 
  mutate(`Thailand` = as.character(`Thailand`)) %>% 
  mutate(`Turkey` = as.character(`Turkey`)) %>% 
  mutate(`Taiwan, China` = as.character(`Taiwan, China`)) %>% 
  mutate(`United States` = as.character(`United States`)) %>% 
  mutate(`South Africa` = as.character(`South Africa`)) %>% 
  filter(!Periodo=="1994M06") %>% 
  filter(!Periodo=="1994M05") 

Stock_Market_USD <- pivot_longer(Stock_Market_USD, cols = -Periodo, names_to = "pais", values_to = "SMUSD") %>% 
  filter(!is.na(Periodo))
Stock_Market_USD <- Stock_Market_USD %>% 
  mutate(SMUSD = na_if(SMUSD, "NA"))

#Total Reserves
Total_Reserves <- pivot_longer(Total_Reserves, cols = -Periodo, names_to = "pais", values_to = "Reserves") %>% 
  filter(!is.na(Periodo)) %>% 
  filter(!Periodo=="1994M06") %>% 
  filter(!Periodo=="1994M05") 
Total_Reserves <- Total_Reserves %>% 
  mutate(Reserves = na_if(Reserves, "NA"))

#Unemployment
Unemployment <- Unemployment %>% 
  mutate(`Argentina` = as.character(`Argentina`)) %>% 
  mutate(`Australia` = as.character(`Australia`)) %>% 
  mutate(`Austria` = as.character(`Austria`)) %>% 
  mutate(`Brazil` = as.character(`Brazil`)) %>% 
  mutate(`Belgium` = as.character(`Belgium`)) %>% 
  mutate(`Bulgaria` = as.character(`Bulgaria`)) %>% 
  mutate(`Canada` = as.character(`Canada`)) %>% 
  mutate(`Switzerland` = as.character(`Switzerland`)) %>% 
  mutate(`Chile` = as.character(`Chile`)) %>% 
  mutate(`Colombia` = as.character(`Colombia`)) %>% 
  mutate(`Czech Republic` = as.character(`Czech Republic`)) %>% 
  mutate(`Denmark` = as.character(`Denmark`)) %>% 
  mutate(`Egypt, Arab Rep.` = as.character(`Egypt, Arab Rep.`)) %>% 
  mutate(`United Kingdom` = as.character(`United Kingdom`)) %>% 
  mutate(`France` = as.character(`France`)) %>% 
  mutate(`High Income Countries` = as.character(`High Income Countries`)) %>% 
  mutate(`Hong Kong SAR, China` = as.character(`Hong Kong SAR, China`)) %>% 
  mutate(`Croatia` = as.character(`Croatia`)) %>% 
  mutate(`Germany` = as.character(`Germany`)) %>% 
  mutate(`Hungary` = as.character(`Hungary`)) %>% 
  mutate(`Israel` = as.character(`Israel`)) %>%
  mutate(`Ireland` = as.character(`Ireland`)) %>% 
  mutate(`Japan` = as.character(`Japan`)) %>% 
  mutate(`Korea, Rep.` = as.character(`Korea, Rep.`)) %>% 
  mutate(`Morocco` = as.character(`Morocco`)) %>% 
  mutate(`Lithuania` = as.character(`Lithuania`)) %>% 
  mutate(`Mexico` = as.character(`Mexico`)) %>% 
  mutate(`Norway` = as.character(`Norway`)) %>% 
  mutate(`Netherlands` = as.character(`Netherlands`)) %>% 
  mutate(`Pakistan` = as.character(`Pakistan`))  %>% 
  mutate(`Norway` = as.character(`Norway`))  %>% 
  mutate(`Peru` = as.character(`Peru`))  %>% 
  mutate(`Philippines` = as.character(`Philippines`))  %>% 
  mutate(`Poland` = as.character(`Poland`))  %>% 
  mutate(`Portugal` = as.character(`Portugal`))  %>% 
  mutate(`Romania` = as.character(`Romania`))  %>% 
  mutate(`Saudi Arabia` = as.character(`Saudi Arabia`))  %>% 
  mutate(`Singapore` = as.character(`Singapore`))  %>% 
  mutate(`Sweden` = as.character(`Sweden`)) %>% 
  mutate(`Spain` = as.character(`Spain`)) %>% 
  mutate(`Thailand` = as.character(`Thailand`)) %>% 
  mutate(`Turkey` = as.character(`Turkey`)) %>% 
  mutate(`Taiwan, China` = as.character(`Taiwan, China`)) %>% 
  mutate(`United States` = as.character(`United States`)) %>% 
  mutate(`South Africa` = as.character(`South Africa`)) %>% 
  filter(!Periodo=="1994M06") %>% 
  filter(!Periodo=="1994M05") 

Unemployment <- pivot_longer(Unemployment, cols = -Periodo, names_to = "pais", values_to = "Unemployment") %>% 
  filter(!is.na(Periodo)) %>% 
  mutate(Unemployment = na_if(Unemployment, "NA"))

#Antes de Joinear y analizar los datos, debemos homogeneizar los periodos.
#Esto es, porque hay datos cuatrimestrales y mensuales. 
#Expresaremos todo en terminos cuatrimestrales porque el gdp no esta en temrinos mensuales

#Para eso, ¿estan siempre todos los meses de todos los años?

count <- Inflation %>% 
  group_by(Periodo) %>% 
  count(Periodo) #efectivamente, todos los meses aparecen 109 veces

count <- Exp_Merch %>% 
  group_by(Periodo) %>% 
  count(Periodo) #efectivamente, todos los meses aparecen 52 veces

count <- Stock_Market_USD %>% 
  group_by(Periodo) %>% 
  count(Periodo) #efectivamente, todos los meses aparecen 78 veces

count <- Total_Reserves %>% 
  group_by(Periodo) %>% 
  count(Periodo) #efectivamente, todos los meses aparecen 184 veces

count <- Unemployment %>% 
  group_by(Periodo) %>% 
  count(Periodo) #efectivamente, todos los meses aparecen 78 veces

#Solo contamos las variables que tenemos datos mensuales para verificar que todos los meses siempre se reporten todas las veces

#Ahora, para poder joinear todo expresamos todos los periodos en terminos trimestrales
#El comando synth solo corre el codigo si el time variable es un entero, entonces expresamos los periodos en enteros


#Exp_Merch <- Exp_Merch %>% 
 # mutate(Periodo = case_when(
  #  grepl("Q1$", Periodo) ~ paste0(substr(Periodo, 1, 4), "1"),
   # grepl("Q2$", Periodo) ~ paste0(substr(Periodo, 1, 4), "2"),
    #grepl("Q3$", Periodo) ~ paste0(substr(Periodo, 1, 4), "3"),
    #grepl("Q4$", Periodo) ~ paste0(substr(Periodo, 1, 4), "4")
#  ))

#GDP_LCU <- GDP_LCU %>% 
 # mutate(Periodo = case_when(
  #  grepl("Q1$", Periodo) ~ paste0(substr(Periodo, 1, 4), "1"),
   # grepl("Q2$", Periodo) ~ paste0(substr(Periodo, 1, 4), "2"),
    #grepl("Q3$", Periodo) ~ paste0(substr(Periodo, 1, 4), "3"),
    #grepl("Q4$", Periodo) ~ paste0(substr(Periodo, 1, 4), "4")
  #))

#GDP_USD <- GDP_USD %>% 
  #mutate(Periodo = case_when(
   # grepl("Q1$", Periodo) ~ paste0(substr(Periodo, 1, 4), "1"),
    #grepl("Q2$", Periodo) ~ paste0(substr(Periodo, 1, 4), "2"),
    #grepl("Q3$", Periodo) ~ paste0(substr(Periodo, 1, 4), "3"),
    #grepl("Q4$", Periodo) ~ paste0(substr(Periodo, 1, 4), "4")
  #))

#Imp_Merch <- Imp_Merch %>% 
  #mutate(Periodo = case_when(
   # grepl("Q1$", Periodo) ~ paste0(substr(Periodo, 1, 4), "1"),
    #grepl("Q2$", Periodo) ~ paste0(substr(Periodo, 1, 4), "2"),
    #grepl("Q3$", Periodo) ~ paste0(substr(Periodo, 1, 4), "3"),
    #grepl("Q4$", Periodo) ~ paste0(substr(Periodo, 1, 4), "4")
#  ))

Unemployment <- Unemployment %>% 
  mutate(Unemployment = as.double(Unemployment))
trimestrar <- function(Unemployment) {
  Unemployment <- Unemployment %>%
    mutate(Trimestre = case_when(
      grepl("01$|02$|03$", Periodo) ~ paste0(substr(Periodo, 1, 4), "Q1"),
      grepl("04$|05$|06$", Periodo) ~ paste0(substr(Periodo, 1, 4), "Q2"),
      grepl("07$|08$|09$", Periodo) ~ paste0(substr(Periodo, 1, 4), "Q3"),
      grepl("10$|11$|12$", Periodo) ~ paste0(substr(Periodo, 1, 4), "Q4")
    )) %>%
    group_by(pais, Trimestre) %>%
    summarise(Unemployment = mean(Unemployment)) %>%
    ungroup() %>%
    select(pais, Periodo = Trimestre, Unemployment)
  
  return(Unemployment)
}
Unemployment <- trimestrar(Unemployment)


Stock_Market_USD <- Stock_Market_USD %>% 
  mutate(SMUSD = as.double(SMUSD))
trimestrar <- function(Stock_Market_USD) {
  Stock_Market_USD <- Stock_Market_USD %>%
    mutate(Trimestre = case_when(
      grepl("01$|02$|03$", Periodo) ~ paste0(substr(Periodo, 1, 4), "Q1"),
      grepl("04$|05$|06$", Periodo) ~ paste0(substr(Periodo, 1, 4), "Q2"),
      grepl("07$|08$|09$", Periodo) ~ paste0(substr(Periodo, 1, 4), "Q3"),
      grepl("10$|11$|12$", Periodo) ~ paste0(substr(Periodo, 1, 4), "Q4")
    )) %>%
    group_by(pais, Trimestre) %>%
    summarise(SMUSD = mean(SMUSD)) %>%
    ungroup() %>%
    select(pais, Periodo = Trimestre, SMUSD)
  
  return(Stock_Market_USD)
}
Stock_Market_USD <- trimestrar(Stock_Market_USD)

Retail <- Retail %>% 
  mutate(Retail = as.double(Retail))
trimestrar <- function(Retail) {
  Retail <- Retail %>%
    mutate(Trimestre = case_when(
      grepl("01$|02$|03$", Periodo) ~ paste0(substr(Periodo, 1, 4), "Q1"),
      grepl("04$|05$|06$", Periodo) ~ paste0(substr(Periodo, 1, 4), "Q2"),
      grepl("07$|08$|09$", Periodo) ~ paste0(substr(Periodo, 1, 4), "Q3"),
      grepl("10$|11$|12$", Periodo) ~ paste0(substr(Periodo, 1, 4), "Q4")
    )) %>%
    group_by(pais, Trimestre) %>%
    summarise(Retail = mean(Retail)) %>%
    ungroup() %>%
    select(pais, Periodo = Trimestre, Retail)
  
  return(Retail)
}
Retail <- trimestrar(Retail)


Inflation <- Inflation %>% 
  mutate(Inflation = as.double(Inflation))
trimestrar <- function(Inflation) {
  Inflation <- Inflation %>%
    mutate(Trimestre = case_when(
      grepl("01$|02$|03$", Periodo) ~ paste0(substr(Periodo, 1, 4), "Q1"),
      grepl("04$|05$|06$", Periodo) ~ paste0(substr(Periodo, 1, 4), "Q2"),
      grepl("07$|08$|09$", Periodo) ~ paste0(substr(Periodo, 1, 4), "Q3"),
      grepl("10$|11$|12$", Periodo) ~ paste0(substr(Periodo, 1, 4), "Q4")
    )) %>%
    group_by(pais, Trimestre) %>%
    summarise(Inflation = mean(Inflation)) %>%
    ungroup() %>%
    select(pais, Periodo = Trimestre, Inflation)
  
  return(Inflation)
}
Inflation <- trimestrar(Inflation)


Effective_TCR <- Effective_TCR %>% 
  mutate(TCR = as.double(TCR))
trimestrar <- function(Effective_TCR) {
  Effective_TCR <- Effective_TCR %>%
    mutate(Trimestre = case_when(
      grepl("01$|02$|03$", Periodo) ~ paste0(substr(Periodo, 1, 4), "Q1"),
      grepl("04$|05$|06$", Periodo) ~ paste0(substr(Periodo, 1, 4), "Q2"),
      grepl("07$|08$|09$", Periodo) ~ paste0(substr(Periodo, 1, 4), "Q3"),
      grepl("10$|11$|12$", Periodo) ~ paste0(substr(Periodo, 1, 4), "Q4")
    )) %>%
    group_by(pais, Trimestre) %>%
    summarise(TCR = mean(TCR)) %>%
    ungroup() %>%
    select(pais, Periodo = Trimestre, TCR)
  
  return(Effective_TCR)
}
Effective_TCR <- trimestrar(Effective_TCR)


Total_Reserves <- Total_Reserves %>% 
  mutate(Reserves = as.double(Reserves))
trimestrar <- function(Total_Reserves) {
  Total_Reserves <- Total_Reserves %>%
    mutate(Trimestre = case_when(
      grepl("01$|02$|03$", Periodo) ~ paste0(substr(Periodo, 1, 4), "Q1"),
      grepl("04$|05$|06$", Periodo) ~ paste0(substr(Periodo, 1, 4), "Q2"),
      grepl("07$|08$|09$", Periodo) ~ paste0(substr(Periodo, 1, 4), "Q3"),
      grepl("10$|11$|12$", Periodo) ~ paste0(substr(Periodo, 1, 4), "Q4")
    )) %>%
    group_by(pais, Trimestre) %>%
    summarise(Reserves = mean(Reserves)) %>%
    ungroup() %>%
    select(pais, Periodo = Trimestre, Reserves)
  
  return(Total_Reserves)
}
Total_Reserves <- trimestrar(Total_Reserves)


#VAB Inmobiliario
VAB_inmobiliario <- VAB_inmobiliario %>%
  mutate(Periodo = case_when(
    grepl("-1º trimestre", Periodo) ~ paste0(substr(Periodo, 1, 4), "Q1"),
    grepl("-2º trimestre", Periodo) ~ paste0(substr(Periodo, 1, 4), "Q2"),
    grepl("-3º trimestre", Periodo) ~ paste0(substr(Periodo, 1, 4), "Q3"),
    grepl("-4º trimestre", Periodo) ~ paste0(substr(Periodo, 1, 4), "Q4")
  )) 

VAB_inmobiliario_deflactado <- VAB_inmobiliario %>% 
  left_join(IPC_paises, by=c("Periodo","pais")) %>% 
  filter(!is.na(Inflacion)) %>% 
  mutate(VAB_inmo_deflactado=case_when(
    grepl("Uruguay", pais) ~ paste0(`VAB inmo`/Inflacion),
    grepl("Argentina", pais) ~ paste0(`VAB inmo`/Inflacion),
    grepl("Estonia", pais) ~ paste0(`VAB inmo`/Inflacion),
    grepl("Brazil", pais) ~ paste0(`VAB inmo`/Inflacion),
    grepl("Colombia", pais) ~ paste0(`VAB inmo`/Inflacion),
    grepl("Peru", pais) ~ paste0(`VAB inmo`/Inflacion),
    grepl("Chile", pais) ~ paste0(`VAB inmo`/Inflacion),
    grepl("Australia", pais) ~ paste0(`VAB inmo`/Inflacion),
    grepl("Belgium", pais) ~ paste0(`VAB inmo`/Inflacion),
    grepl("Bulgaria", pais) ~ paste0(`VAB inmo`/Inflacion),
    grepl("Denmark", pais) ~ paste0(`VAB inmo`/Inflacion),
    grepl("Estonia", pais) ~ paste0(`VAB inmo`/Inflacion),
    grepl("Ireland", pais) ~ paste0(`VAB inmo`/Inflacion),
    grepl("Greece", pais) ~ paste0(`VAB inmo`/Inflacion),
    grepl("Croatia", pais) ~ paste0(`VAB inmo`/Inflacion),
    grepl("Cyprus", pais) ~ paste0(`VAB inmo`/Inflacion),
    grepl("Lithuania", pais) ~ paste0(`VAB inmo`/Inflacion),
    grepl("Luxembourg", pais) ~ paste0(`VAB inmo`/Inflacion),
    grepl("Hungary", pais) ~ paste0(`VAB inmo`/Inflacion),
    grepl("Malta", pais) ~ paste0(`VAB inmo`/Inflacion),
    grepl("Netherlands", pais) ~ paste0(`VAB inmo`/Inflacion),
    grepl("Austria", pais) ~ paste0(`VAB inmo`/Inflacion),
    grepl("Poland", pais) ~ paste0(`VAB inmo`/Inflacion),
    grepl("Romania", pais) ~ paste0(`VAB inmo`/Inflacion),
    grepl("Slovenia", pais) ~ paste0(`VAB inmo`/Inflacion),
    grepl("Slovakia", pais) ~ paste0(`VAB inmo`/Inflacion),
    grepl("Finland", pais) ~ paste0(`VAB inmo`/Inflacion),
    grepl("Sweden", pais) ~ paste0(`VAB inmo`/Inflacion),
    grepl("Switzerland", pais) ~ paste0(`VAB inmo`/Inflacion),
    grepl("Serbia", pais) ~ paste0(`VAB inmo`/Inflacion),
    grepl("North Macedonia", pais) ~ paste0(`VAB inmo`/Inflacion),
    grepl("Lithuania", pais) ~ paste0(`VAB inmo`/Inflacion),
    .default = as.character(`VAB inmo`)
  )) %>% 
  mutate(VAB_inmo_deflactado=as.double(VAB_inmo_deflactado)) %>% 
  mutate(VAB_real_inmo_deflactado=as.double(VAB_inmo_deflactado/IPC_real_Alquileres)) %>% 
  select(Periodo, VAB_real_inmo_deflactado,VAB_inmo_deflactado, Inflacion, pais, IPC_real_Alquileres, IPC_Alq)
  
#Pasamos a desestacionalizar los paises con los que estamos operando con VAB inmobiliario (solo urugay por ahora)



#Amenazas a la estrategia de identificacion (tenemos datos para CABA)
#Vemos como evolucionaron las variables de CABA
#Portales#
#(ARGENTINA)
Alquileres_BSAS$Mes <- as.Date(paste(Alquileres_BSAS$Mes, "-01", sep = ""), format = "%Y-%m-%d") 

#para ver como evolucionaron los precios y la oferta necesitamos promediar la evolucion de la oferta
#y los precios. Sin embargo, debemos ponderar la variacion en oferta segun cuanta proporcion representan del total 
#Entonces,hacemos lo siguiente para encontrar ponderadores y multiplicarlos por tales

#Amba as whole
Alquileres_AMBA_Departamento <- Alquileres_BSAS %>% 
  filter(Inmueble=="Departamento") %>%
  filter(Aglomerado=="AMBA") %>% 
  select(Aglomerado, Inmueble, Mes, Oferta, Mediana.por.m2.a.precios.constantes, Mediana.por.m2.a.precios.corrientes, Contactos) %>% 
  mutate(Mes = case_when(
    grepl("-01$|", Mes) ~ paste0(substr(Mes, 1, 7))
  )) %>% 
  mutate(Mes = case_when(
    grepl("-01$|-02$|-03$", Mes) ~ paste0(substr(Mes, 1, 4), "Q1"),
    grepl("-04$|-05$|-06$", Mes) ~ paste0(substr(Mes, 1, 4), "Q2"),
    grepl("-07$|-08$|-09$", Mes) ~ paste0(substr(Mes, 1, 4), "Q3"),
    grepl("-10$|-11$|-12$", Mes) ~ paste0(substr(Mes, 1, 4), "Q4")
  )) %>% 
  select(Aglomerado, Mes, Oferta, Alq_reales=Mediana.por.m2.a.precios.constantes, Contactos) %>% 
  group_by(Mes) %>%  
  summarise("AMBA Oferta alquiler dpto"=mean(Oferta),"AMBA Precios alquiler dpto"=mean(Alq_reales), "AMBA Contactos dpto"=mean(Contactos)) %>% #2018Q1 da mayor a 1 porque 1 es la oferta del primer mes de 2018, pero yo estoy promediando todo el trimestre. Tendria que despues arreglarlo y  hacer que todo el cuatrimestre sea el mes numero 1. Pero para eso necesito los datos de alquileres puros
  mutate(pais="Argentina") %>% 
  rename(Periodo=Mes)

Alquileres_AMBA_Casa <- Alquileres_BSAS %>% 
  filter(Inmueble=="Casa") %>%
  filter(Aglomerado=="AMBA") %>% 
  select(Aglomerado, Inmueble, Mes, Oferta, Mediana.por.m2.a.precios.constantes, Mediana.por.m2.a.precios.corrientes, Contactos) %>% 
  mutate(Mes = case_when(
    grepl("-01$|", Mes) ~ paste0(substr(Mes, 1, 7))
  )) %>% 
  mutate(Mes = case_when(
    grepl("-01$|-02$|-03$", Mes) ~ paste0(substr(Mes, 1, 4), "Q1"),
    grepl("-04$|-05$|-06$", Mes) ~ paste0(substr(Mes, 1, 4), "Q2"),
    grepl("-07$|-08$|-09$", Mes) ~ paste0(substr(Mes, 1, 4), "Q3"),
    grepl("-10$|-11$|-12$", Mes) ~ paste0(substr(Mes, 1, 4), "Q4")
  )) %>% 
  select(Aglomerado, Mes, Oferta, Alq_reales=Mediana.por.m2.a.precios.constantes, Contactos) %>% 
  group_by(Mes) %>% 
  summarise("AMBA Oferta alquiler casa"=mean(Oferta),"AMBA Precios alquiler casA"=mean(Alq_reales),"AMBA Contactos casA"=mean(Contactos)) %>% #2018Q1 da mayor a 1 porque 1 es la oferta del primer mes de 2018, pero yo estoy promediando todo el trimestre. Tendria que despues arreglarlo y  hacer que todo el cuatrimestre sea el mes numero 1. Pero para eso necesito los datos de alquileres puros
  mutate(pais="Argentina") %>% 
  rename(Periodo=Mes)

#CABA as a whole
setwd("C:\\Mis Archivos\\Facundo\\Tesis ley de alquileres\\Datos\\Datos (variable dependiente)\\Portales inmobiliarios\\Argentina (MELI) precio mediano y oferta")
Alquileres_BSAS <- read_excel("alquileresagrupacion_202402.xlsx")
Alquileres_BSAS$Mes <- as.Date(paste(Alquileres_BSAS$Mes, "-01", sep = ""), format = "%Y-%m-%d") 

Alquileres_CABA_Departamento <- Alquileres_BSAS %>%
  filter(Aglomerado=="CABA") %>% 
  filter(Inmueble=="Departamento") %>% 
  select(Aglomerado, Inmueble, Mes, Oferta, Mediana.por.m2.a.precios.constantes, Mediana.por.m2.a.precios.corrientes, Contactos) %>% 
  mutate(Mes = case_when(
    grepl("-01$|", Mes) ~ paste0(substr(Mes, 1, 7))
  )) %>% 
  mutate(Mes = case_when(
    grepl("-01$|-02$|-03$", Mes) ~ paste0(substr(Mes, 1, 4), "Q1"),
    grepl("-04$|-05$|-06$", Mes) ~ paste0(substr(Mes, 1, 4), "Q2"),
    grepl("-07$|-08$|-09$", Mes) ~ paste0(substr(Mes, 1, 4), "Q3"),
    grepl("-10$|-11$|-12$", Mes) ~ paste0(substr(Mes, 1, 4), "Q4")
  )) %>%
  select(Aglomerado, Mes, Oferta, Alq_reales=Mediana.por.m2.a.precios.constantes, Contactos) %>% 
  group_by(Mes) %>% 
  summarise("CABA Oferta alquiler Departamento"=mean(Oferta),"CABA Precios alquiler Departamento"=mean(Alq_reales),"CABA Contactos Departamento"=mean(Contactos)) %>% #2018Q1 da mayor a 1 porque 1 es la oferta del primer mes de 2018, pero yo estoy promediando todo el trimestre. Tendria que despues arreglarlo y  hacer que todo el cuatrimestre sea el mes numero 1. Pero para eso necesito los datos de alquileres puros
  mutate(pais="Argentina") %>% 
  rename(Periodo=Mes)

Alquileres_CABA_Casa <- Alquileres_BSAS %>% 
  filter(Aglomerado=="CABA") %>% 
  filter(Inmueble=="Casa") %>% 
  select(Aglomerado, Inmueble, Mes, Oferta, Mediana.por.m2.a.precios.constantes, Mediana.por.m2.a.precios.corrientes, Contactos) %>% 
  mutate(Mes = case_when(
    grepl("-01$|", Mes) ~ paste0(substr(Mes, 1, 7))
  )) %>% 
  mutate(Mes = case_when(
    grepl("-01$|-02$|-03$", Mes) ~ paste0(substr(Mes, 1, 4), "Q1"),
    grepl("-04$|-05$|-06$", Mes) ~ paste0(substr(Mes, 1, 4), "Q2"),
    grepl("-07$|-08$|-09$", Mes) ~ paste0(substr(Mes, 1, 4), "Q3"),
    grepl("-10$|-11$|-12$", Mes) ~ paste0(substr(Mes, 1, 4), "Q4")
  )) %>%
  select(Aglomerado, Mes, Oferta, Alq_reales=Mediana.por.m2.a.precios.constantes, Contactos) %>% 
  group_by(Mes) %>% 
  summarise("CABA Oferta alquiler Casa"=mean(Oferta),"CABA Precios alquiler Casa"=mean(Alq_reales),"CABA Contactos Casa"=mean(Contactos)) %>% #2018Q1 da mayor a 1 porque 1 es la oferta del primer mes de 2018, pero yo estoy promediando todo el trimestre. Tendria que despues arreglarlo y  hacer que todo el cuatrimestre sea el mes numero 1. Pero para eso necesito los datos de alquileres puros
  mutate(pais="Argentina") %>% 
  rename(Periodo=Mes)



#Departamento
#Centro
Alquileres_CBACentro_Departamento <- Alquileres_Cordoba_aglomerados %>% 
  filter(Inmueble=="Departamento") %>%
  filter(Aglomerado=="Cordoba Centro") %>% 
  select(Aglomerado, Inmueble, Mes, Oferta, Mediana.por.m2.a.precios.constantes, Mediana.por.m2.a.precios.corrientes, Contactos) %>% 
  mutate(Mes = case_when(
    grepl("-01$|", Mes) ~ paste0(substr(Mes, 1, 7))
  )) %>% 
  mutate(Mes = case_when(
    grepl("-01$|-02$|-03$", Mes) ~ paste0(substr(Mes, 1, 4), "Q1"),
    grepl("-04$|-05$|-06$", Mes) ~ paste0(substr(Mes, 1, 4), "Q2"),
    grepl("-07$|-08$|-09$", Mes) ~ paste0(substr(Mes, 1, 4), "Q3"),
    grepl("-10$|-11$|-12$", Mes) ~ paste0(substr(Mes, 1, 4), "Q4")
  )) %>% 
  select(Aglomerado, Mes, Oferta, Alq_reales=Mediana.por.m2.a.precios.constantes, Contactos) %>% 
  group_by(Mes) %>%  
  summarise("CBA Centro Oferta alquiler dpto"=mean(Oferta),"CBA Centro Precios alquiler dpto"=mean(Alq_reales), "CBA Centro Contactos dpto"=mean(Contactos)) %>% #2018Q1 da mayor a 1 porque 1 es la oferta del primer mes de 2018, pero yo estoy promediando todo el trimestre. Tendria que despues arreglarlo y  hacer que todo el cuatrimestre sea el mes numero 1. Pero para eso necesito los datos de alquileres puros
  mutate(pais="Argentina") %>% 
  rename(Periodo=Mes)


#Norte
Alquileres_CBANorte_Departamento <- Alquileres_Cordoba_aglomerados %>% 
  filter(Inmueble=="Departamento") %>%
  filter(Aglomerado=="Cordoba Norte") %>% 
  select(Aglomerado, Inmueble, Mes, Oferta, Mediana.por.m2.a.precios.constantes, Mediana.por.m2.a.precios.corrientes, Contactos) %>% 
  mutate(Mes = case_when(
    grepl("-01$|", Mes) ~ paste0(substr(Mes, 1, 7))
  )) %>% 
  mutate(Mes = case_when(
    grepl("-01$|-02$|-03$", Mes) ~ paste0(substr(Mes, 1, 4), "Q1"),
    grepl("-04$|-05$|-06$", Mes) ~ paste0(substr(Mes, 1, 4), "Q2"),
    grepl("-07$|-08$|-09$", Mes) ~ paste0(substr(Mes, 1, 4), "Q3"),
    grepl("-10$|-11$|-12$", Mes) ~ paste0(substr(Mes, 1, 4), "Q4")
  )) %>% 
  select(Aglomerado, Mes, Oferta, Alq_reales=Mediana.por.m2.a.precios.constantes, Contactos) %>% 
  group_by(Mes) %>%  
  summarise("CBA Norte Oferta alquiler dpto"=mean(Oferta),"CBA Norte Precios alquiler dpto"=mean(Alq_reales), "CBA Norte Contactos dpto"=mean(Contactos)) %>% #2018Q1 da mayor a 1 porque 1 es la oferta del primer mes de 2018, pero yo estoy promediando todo el trimestre. Tendria que despues arreglarlo y  hacer que todo el cuatrimestre sea el mes numero 1. Pero para eso necesito los datos de alquileres puros
  mutate(pais="Argentina") %>% 
  rename(Periodo=Mes)


#Sur
Alquileres_CBASur_Departamento <- Alquileres_Cordoba_aglomerados %>% 
  filter(Inmueble=="Departamento") %>%
  filter(Aglomerado=="Cordoba Sur") %>% 
  select(Aglomerado, Inmueble, Mes, Oferta, Mediana.por.m2.a.precios.constantes, Mediana.por.m2.a.precios.corrientes, Contactos) %>% 
  mutate(Mes = case_when(
    grepl("-01$|", Mes) ~ paste0(substr(Mes, 1, 7))
  )) %>% 
  mutate(Mes = case_when(
    grepl("-01$|-02$|-03$", Mes) ~ paste0(substr(Mes, 1, 4), "Q1"),
    grepl("-04$|-05$|-06$", Mes) ~ paste0(substr(Mes, 1, 4), "Q2"),
    grepl("-07$|-08$|-09$", Mes) ~ paste0(substr(Mes, 1, 4), "Q3"),
    grepl("-10$|-11$|-12$", Mes) ~ paste0(substr(Mes, 1, 4), "Q4")
  )) %>% 
  select(Aglomerado, Mes, Oferta, Alq_reales=Mediana.por.m2.a.precios.constantes, Contactos) %>% 
  group_by(Mes) %>%  
  summarise("CBA Sur Oferta alquiler dpto"=mean(Oferta),"CBA Sur Precios alquiler dpto"=mean(Alq_reales), "CBA Sur Contactos dpto"=mean(Contactos)) %>% #2018Q1 da mayor a 1 porque 1 es la oferta del primer mes de 2018, pero yo estoy promediando todo el trimestre. Tendria que despues arreglarlo y  hacer que todo el cuatrimestre sea el mes numero 1. Pero para eso necesito los datos de alquileres puros
  mutate(pais="Argentina") %>% 
  rename(Periodo=Mes)

#Este
Alquileres_CBAEste_Departamento <- Alquileres_Cordoba_aglomerados %>% 
  filter(Inmueble=="Departamento") %>%
  filter(Aglomerado=="Cordoba Este") %>% 
  select(Aglomerado, Inmueble, Mes, Oferta, Mediana.por.m2.a.precios.constantes, Mediana.por.m2.a.precios.corrientes, Contactos) %>% 
  mutate(Mes = case_when(
    grepl("-01$|", Mes) ~ paste0(substr(Mes, 1, 7))
  )) %>% 
  mutate(Mes = case_when(
    grepl("-01$|-02$|-03$", Mes) ~ paste0(substr(Mes, 1, 4), "Q1"),
    grepl("-04$|-05$|-06$", Mes) ~ paste0(substr(Mes, 1, 4), "Q2"),
    grepl("-07$|-08$|-09$", Mes) ~ paste0(substr(Mes, 1, 4), "Q3"),
    grepl("-10$|-11$|-12$", Mes) ~ paste0(substr(Mes, 1, 4), "Q4")
  )) %>% 
  select(Aglomerado, Mes, Oferta, Alq_reales=Mediana.por.m2.a.precios.constantes, Contactos) %>% 
  group_by(Mes) %>%  
  summarise("CBA Este Oferta alquiler dpto"=mean(Oferta),"CBA Este Precios alquiler dpto"=mean(Alq_reales), "CBA Este Contactos dpto"=mean(Contactos)) %>% #2018Q1 da mayor a 1 porque 1 es la oferta del primer mes de 2018, pero yo estoy promediando todo el trimestre. Tendria que despues arreglarlo y  hacer que todo el cuatrimestre sea el mes numero 1. Pero para eso necesito los datos de alquileres puros
  mutate(pais="Argentina") %>% 
  rename(Periodo=Mes)

#Oeste
Alquileres_CBAOeste_Departamento <- Alquileres_Cordoba_aglomerados %>% 
  filter(Inmueble=="Departamento") %>%
  filter(Aglomerado=="Cordoba Oeste") %>% 
  select(Aglomerado, Inmueble, Mes, Oferta, Mediana.por.m2.a.precios.constantes, Mediana.por.m2.a.precios.corrientes, Contactos) %>% 
  mutate(Mes = case_when(
    grepl("-01$|", Mes) ~ paste0(substr(Mes, 1, 7))
  )) %>% 
  mutate(Mes = case_when(
    grepl("-01$|-02$|-03$", Mes) ~ paste0(substr(Mes, 1, 4), "Q1"),
    grepl("-04$|-05$|-06$", Mes) ~ paste0(substr(Mes, 1, 4), "Q2"),
    grepl("-07$|-08$|-09$", Mes) ~ paste0(substr(Mes, 1, 4), "Q3"),
    grepl("-10$|-11$|-12$", Mes) ~ paste0(substr(Mes, 1, 4), "Q4")
  )) %>% 
  select(Aglomerado, Mes, Oferta, Alq_reales=Mediana.por.m2.a.precios.constantes, Contactos) %>% 
  group_by(Mes) %>%  
  summarise("CBA Oeste Oferta alquiler dpto"=mean(Oferta),"CBA Oeste Precios alquiler dpto"=mean(Alq_reales), "CBA Oeste Contactos dpto"=mean(Contactos)) %>% #2018Q1 da mayor a 1 porque 1 es la oferta del primer mes de 2018, pero yo estoy promediando todo el trimestre. Tendria que despues arreglarlo y  hacer que todo el cuatrimestre sea el mes numero 1. Pero para eso necesito los datos de alquileres puros
  mutate(pais="Argentina") %>% 
  rename(Periodo=Mes)

# Casa
#Centro
Alquileres_CBACentro_Casa <- Alquileres_Cordoba_aglomerados %>% 
  filter(Inmueble=="Casa") %>%
  filter(Aglomerado=="Cordoba Centro") %>% 
  select(Aglomerado, Inmueble, Mes, Oferta, Mediana.por.m2.a.precios.constantes, Mediana.por.m2.a.precios.corrientes, Contactos) %>% 
  mutate(Mes = case_when(
    grepl("-01$|", Mes) ~ paste0(substr(Mes, 1, 7))
  )) %>% 
  mutate(Mes = case_when(
    grepl("-01$|-02$|-03$", Mes) ~ paste0(substr(Mes, 1, 4), "Q1"),
    grepl("-04$|-05$|-06$", Mes) ~ paste0(substr(Mes, 1, 4), "Q2"),
    grepl("-07$|-08$|-09$", Mes) ~ paste0(substr(Mes, 1, 4), "Q3"),
    grepl("-10$|-11$|-12$", Mes) ~ paste0(substr(Mes, 1, 4), "Q4")
  )) %>% 
  select(Aglomerado, Mes, Oferta, Alq_reales=Mediana.por.m2.a.precios.constantes, Contactos) %>% 
  group_by(Mes) %>%  
  summarise("CBA Centro Oferta alquiler Casa"=mean(Oferta),"CBA Centro Precios alquiler Casa"=mean(Alq_reales), "CBA Centro Contactos Casa"=mean(Contactos)) %>% #2018Q1 da mayor a 1 porque 1 es la oferta del primer mes de 2018, pero yo estoy promediando todo el trimestre. Tendria que despues arreglarlo y  hacer que todo el cuatrimestre sea el mes numero 1. Pero para eso necesito los datos de alquileres puros
  mutate(pais="Argentina") %>% 
  rename(Periodo=Mes)


#Norte
Alquileres_CBANorte_Casa <- Alquileres_Cordoba_aglomerados %>% 
  filter(Inmueble=="Casa") %>%
  filter(Aglomerado=="Cordoba Norte") %>% 
  select(Aglomerado, Inmueble, Mes, Oferta, Mediana.por.m2.a.precios.constantes, Mediana.por.m2.a.precios.corrientes, Contactos) %>% 
  mutate(Mes = case_when(
    grepl("-01$|", Mes) ~ paste0(substr(Mes, 1, 7))
  )) %>% 
  mutate(Mes = case_when(
    grepl("-01$|-02$|-03$", Mes) ~ paste0(substr(Mes, 1, 4), "Q1"),
    grepl("-04$|-05$|-06$", Mes) ~ paste0(substr(Mes, 1, 4), "Q2"),
    grepl("-07$|-08$|-09$", Mes) ~ paste0(substr(Mes, 1, 4), "Q3"),
    grepl("-10$|-11$|-12$", Mes) ~ paste0(substr(Mes, 1, 4), "Q4")
  )) %>% 
  select(Aglomerado, Mes, Oferta, Alq_reales=Mediana.por.m2.a.precios.constantes, Contactos) %>% 
  group_by(Mes) %>%  
  summarise("CBA Norte Oferta alquiler Casa"=mean(Oferta),"CBA Norte Precios alquiler Casa"=mean(Alq_reales), "CBA Norte Contactos Casa"=mean(Contactos)) %>% #2018Q1 da mayor a 1 porque 1 es la oferta del primer mes de 2018, pero yo estoy promediando todo el trimestre. Tendria que despues arreglarlo y  hacer que todo el cuatrimestre sea el mes numero 1. Pero para eso necesito los datos de alquileres puros
  mutate(pais="Argentina") %>% 
  rename(Periodo=Mes)


#Sur
Alquileres_CBASur_Casa <- Alquileres_Cordoba_aglomerados %>% 
  filter(Inmueble=="Casa") %>%
  filter(Aglomerado=="Cordoba Sur") %>% 
  select(Aglomerado, Inmueble, Mes, Oferta, Mediana.por.m2.a.precios.constantes, Mediana.por.m2.a.precios.corrientes, Contactos) %>% 
  mutate(Mes = case_when(
    grepl("-01$|", Mes) ~ paste0(substr(Mes, 1, 7))
  )) %>% 
  mutate(Mes = case_when(
    grepl("-01$|-02$|-03$", Mes) ~ paste0(substr(Mes, 1, 4), "Q1"),
    grepl("-04$|-05$|-06$", Mes) ~ paste0(substr(Mes, 1, 4), "Q2"),
    grepl("-07$|-08$|-09$", Mes) ~ paste0(substr(Mes, 1, 4), "Q3"),
    grepl("-10$|-11$|-12$", Mes) ~ paste0(substr(Mes, 1, 4), "Q4")
  )) %>% 
  select(Aglomerado, Mes, Oferta, Alq_reales=Mediana.por.m2.a.precios.constantes, Contactos) %>% 
  group_by(Mes) %>%  
  summarise("CBA Sur Oferta alquiler Casa"=mean(Oferta),"CBA Sur Precios alquiler Casa"=mean(Alq_reales), "CBA Sur Contactos Casa"=mean(Contactos)) %>% #2018Q1 da mayor a 1 porque 1 es la oferta del primer mes de 2018, pero yo estoy promediando todo el trimestre. Tendria que despues arreglarlo y  hacer que todo el cuatrimestre sea el mes numero 1. Pero para eso necesito los datos de alquileres puros
  mutate(pais="Argentina") %>% 
  rename(Periodo=Mes)

#Oeste
Alquileres_CBAOeste_Casa <- Alquileres_Cordoba_aglomerados %>% 
  filter(Inmueble=="Casa") %>%
  filter(Aglomerado=="Cordoba Oeste") %>% 
  select(Aglomerado, Inmueble, Mes, Oferta, Mediana.por.m2.a.precios.constantes, Mediana.por.m2.a.precios.corrientes, Contactos) %>% 
  mutate(Mes = case_when(
    grepl("-01$|", Mes) ~ paste0(substr(Mes, 1, 7))
  )) %>% 
  mutate(Mes = case_when(
    grepl("-01$|-02$|-03$", Mes) ~ paste0(substr(Mes, 1, 4), "Q1"),
    grepl("-04$|-05$|-06$", Mes) ~ paste0(substr(Mes, 1, 4), "Q2"),
    grepl("-07$|-08$|-09$", Mes) ~ paste0(substr(Mes, 1, 4), "Q3"),
    grepl("-10$|-11$|-12$", Mes) ~ paste0(substr(Mes, 1, 4), "Q4")
  )) %>% 
  select(Aglomerado, Mes, Oferta, Alq_reales=Mediana.por.m2.a.precios.constantes, Contactos) %>% 
  group_by(Mes) %>%  
  summarise("CBA Oeste Oferta alquiler Casa"=mean(Oferta),"CBA Oeste Precios alquiler Casa"=mean(Alq_reales), "CBA Oeste Contactos Casa"=mean(Contactos)) %>% #2018Q1 da mayor a 1 porque 1 es la oferta del primer mes de 2018, pero yo estoy promediando todo el trimestre. Tendria que despues arreglarlo y  hacer que todo el cuatrimestre sea el mes numero 1. Pero para eso necesito los datos de alquileres puros
  mutate(pais="Argentina") %>% 
  rename(Periodo=Mes)




#CIUDADES DEL INTERIOR (MELI)

#Cordoba
Alquileres_cordoba_casa <- Alquileres_interior %>% 
  filter(Inmueble=="Casa") %>%
  filter(Municipio=="Cordoba") %>% 
  select(Municipio, Inmueble, Mes, Oferta, Mediana.por.m2.a.precios.constantes, Mediana.por.m2.a.precios.corrientes) %>% 
  mutate(Mes = case_when(
    grepl("-01$|", Mes) ~ paste0(substr(Mes, 1, 7))
  )) %>% 
  mutate(Mes = case_when(
    grepl("-01$|-02$|-03$", Mes) ~ paste0(substr(Mes, 1, 4), "Q1"),
    grepl("-04$|-05$|-06$", Mes) ~ paste0(substr(Mes, 1, 4), "Q2"),
    grepl("-07$|-08$|-09$", Mes) ~ paste0(substr(Mes, 1, 4), "Q3"),
    grepl("-10$|-11$|-12$", Mes) ~ paste0(substr(Mes, 1, 4), "Q4")
  )) %>% 
  select(Municipio, Mes, Oferta, Alq_reales=Mediana.por.m2.a.precios.constantes) %>% 
  group_by(Mes) %>%  
  summarise("Cordoba Oferta alquiler Casa"=mean(Oferta),"Cordoba Precios alquiler Casa"=mean(Alq_reales)) %>% #2018Q1 da mayor a 1 porque 1 es la oferta del primer mes de 2018, pero yo estoy promediando todo el trimestre. Tendria que despues arreglarlo y  hacer que todo el cuatrimestre sea el mes numero 1. Pero para eso necesito los datos de alquileres puros
  mutate(pais="Argentina") %>% 
  rename(Periodo=Mes)

Alquileres_cordoba_departamento <- Alquileres_interior %>% 
  filter(Inmueble=="Departamento") %>%
  filter(Municipio=="Cordoba") %>% 
  select(Municipio, Inmueble, Mes, Oferta, Mediana.por.m2.a.precios.constantes, Mediana.por.m2.a.precios.corrientes) %>% 
  mutate(Mes = case_when(
    grepl("-01$|", Mes) ~ paste0(substr(Mes, 1, 7))
  )) %>% 
  mutate(Mes = case_when(
    grepl("-01$|-02$|-03$", Mes) ~ paste0(substr(Mes, 1, 4), "Q1"),
    grepl("-04$|-05$|-06$", Mes) ~ paste0(substr(Mes, 1, 4), "Q2"),
    grepl("-07$|-08$|-09$", Mes) ~ paste0(substr(Mes, 1, 4), "Q3"),
    grepl("-10$|-11$|-12$", Mes) ~ paste0(substr(Mes, 1, 4), "Q4")
  )) %>% 
  select(Municipio, Mes, Oferta, Alq_reales=Mediana.por.m2.a.precios.constantes) %>% 
  group_by(Mes) %>%  
  summarise("Cordoba Oferta alquiler Departamento"=mean(Oferta),"Cordoba Precios alquiler Departamento"=mean(Alq_reales)) %>% #2018Q1 da mayor a 1 porque 1 es la oferta del primer mes de 2018, pero yo estoy promediando todo el trimestre. Tendria que despues arreglarlo y  hacer que todo el cuatrimestre sea el mes numero 1. Pero para eso necesito los datos de alquileres puros
  mutate(pais="Argentina") %>% 
  rename(Periodo=Mes)

#Rosario
Alquileres_rosario_casa <- Alquileres_interior %>% 
  filter(Inmueble=="Casa") %>%
  filter(Municipio=="Rosario") %>% 
  select(Municipio, Inmueble, Mes, Oferta, Mediana.por.m2.a.precios.constantes, Mediana.por.m2.a.precios.corrientes) %>% 
  mutate(Mes = case_when(
    grepl("-01$|", Mes) ~ paste0(substr(Mes, 1, 7))
  )) %>% 
  mutate(Mes = case_when(
    grepl("-01$|-02$|-03$", Mes) ~ paste0(substr(Mes, 1, 4), "Q1"),
    grepl("-04$|-05$|-06$", Mes) ~ paste0(substr(Mes, 1, 4), "Q2"),
    grepl("-07$|-08$|-09$", Mes) ~ paste0(substr(Mes, 1, 4), "Q3"),
    grepl("-10$|-11$|-12$", Mes) ~ paste0(substr(Mes, 1, 4), "Q4")
  )) %>% 
  select(Municipio, Mes, Oferta, Alq_reales=Mediana.por.m2.a.precios.constantes) %>% 
  group_by(Mes) %>%  
  summarise("Rosario Oferta alquiler Casa"=mean(Oferta),"Rosario Precios alquiler Casa"=mean(Alq_reales)) %>% #2018Q1 da mayor a 1 porque 1 es la oferta del primer mes de 2018, pero yo estoy promediando todo el trimestre. Tendria que despues arreglarlo y  hacer que todo el cuatrimestre sea el mes numero 1. Pero para eso necesito los datos de alquileres puros
  mutate(pais="Argentina") %>% 
  rename(Periodo=Mes)

Alquileres_rosario_departamento<- Alquileres_interior %>% 
  filter(Inmueble=="Departamento") %>%
  filter(Municipio=="Rosario") %>% 
  select(Municipio, Inmueble, Mes, Oferta, Mediana.por.m2.a.precios.constantes, Mediana.por.m2.a.precios.corrientes) %>% 
  mutate(Mes = case_when(
    grepl("-01$|", Mes) ~ paste0(substr(Mes, 1, 7))
  )) %>% 
  mutate(Mes = case_when(
    grepl("-01$|-02$|-03$", Mes) ~ paste0(substr(Mes, 1, 4), "Q1"),
    grepl("-04$|-05$|-06$", Mes) ~ paste0(substr(Mes, 1, 4), "Q2"),
    grepl("-07$|-08$|-09$", Mes) ~ paste0(substr(Mes, 1, 4), "Q3"),
    grepl("-10$|-11$|-12$", Mes) ~ paste0(substr(Mes, 1, 4), "Q4")
  )) %>% 
  select(Municipio, Mes, Oferta, Alq_reales=Mediana.por.m2.a.precios.constantes) %>% 
  group_by(Mes) %>%  
  summarise("Rosario Oferta alquiler Departamento"=mean(Oferta),"Rosario Precios alquiler Departamento"=mean(Alq_reales)) %>% #2018Q1 da mayor a 1 porque 1 es la oferta del primer mes de 2018, pero yo estoy promediando todo el trimestre. Tendria que despues arreglarlo y  hacer que todo el cuatrimestre sea el mes numero 1. Pero para eso necesito los datos de alquileres puros
  mutate(pais="Argentina") %>% 
  rename(Periodo=Mes)


#Precios de Zonaprop

#Cordoba 
Alquileres_CBAOeste_Casa <- Alquileres_Cordoba_aglomerados %>% 
  filter(Inmueble=="Casa") %>%
  filter(Aglomerado=="Cordoba Oeste") %>% 
  select(Aglomerado, Inmueble, Mes, Oferta, Mediana.por.m2.a.precios.constantes, Mediana.por.m2.a.precios.corrientes, Contactos) %>% 
  mutate(Mes = case_when(
    grepl("-01$|", Mes) ~ paste0(substr(Mes, 1, 7))
  )) %>% 
  mutate(Mes = case_when(
    grepl("-01$|-02$|-03$", Mes) ~ paste0(substr(Mes, 1, 4), "Q1"),
    grepl("-04$|-05$|-06$", Mes) ~ paste0(substr(Mes, 1, 4), "Q2"),
    grepl("-07$|-08$|-09$", Mes) ~ paste0(substr(Mes, 1, 4), "Q3"),
    grepl("-10$|-11$|-12$", Mes) ~ paste0(substr(Mes, 1, 4), "Q4")
  )) %>% 
  select(Aglomerado, Mes, Oferta, Alq_reales=Mediana.por.m2.a.precios.constantes, Contactos) %>% 
  group_by(Mes) %>%  
  summarise("ZPCordoba Oeste Oferta alquiler Casa"=mean(Oferta),"ZPCordoba Oeste Precios alquiler Casa"=mean(Alq_reales), "CBA Oeste Contactos Casa"=mean(Contactos)) %>% #2018Q1 da mayor a 1 porque 1 es la oferta del primer mes de 2018, pero yo estoy promediando todo el trimestre. Tendria que despues arreglarlo y  hacer que todo el cuatrimestre sea el mes numero 1. Pero para eso necesito los datos de alquileres puros
  mutate(pais="Argentina") %>% 
  rename(Periodo=Mes)


#Rosario




#TOD O ESTE CODIGO LO DEJO EN PAUSA HASTA HACER EL SINTETICO, AHORA DEBERIA ARMAR UN SINTETICO DISTINTO PARA CASAS Y DPTOS
#Oferta_portales <- Portales_Alquileres_Arg %>% 
  #full_join(Alquileres_chile, by=c("Oferta alquiler","Periodo","pais","Precios alquiler")) %>% 
  #full_join(Alquileres_peru, by=c("Oferta alquiler","Periodo","pais","Precios alquiler")) %>% 
  #full_join(Precios_Brasil, by=c("Periodo", "pais", "Precios alquiler"))


#Ahora, luego de expresar todo en el mismo formato, joineamos todo

Variables_independientes <- GDP_USD %>% 
  full_join(Effective_TCR, by=c("pais","Periodo")) %>% 
  full_join(GDP_LCU, by=c("pais","Periodo")) %>%
  full_join(Unemployment, by=c("pais","Periodo")) %>%
  full_join(Imp_Merch, by=c("pais","Periodo")) %>%
  full_join(Stock_Market_USD, by=c("pais","Periodo")) %>%
  full_join(Exp_Merch, by=c("pais","Periodo")) %>% 
  full_join(VAB_inmobiliario_deflactado) %>% 
  full_join(Consumo) %>% 
  full_join(RIPTE)

Id <- Variables_independientes %>% 
  select(pais) %>% 
  filter(!duplicated(pais)) %>% 
  mutate(ID=row_number())

#Por ultimo, para que luego podamos correr "synth" debemos hacer que no haya años no incluidos entre los periodos
#Para ello, enumeramos del 1 al 120 todos los trimestres y nos queda una serie temporal continua y en numeros enteros

Trimestres <- Variables_independientes %>% 
  select(Periodo) %>% 
  filter(!duplicated(.)) %>% 
  mutate(Trimestre=row_number()) 

  #Esto ultimo lo hicimos porque tenemos que usar los periodos para los cuales podemos correlacionar con la oferta de alquileres

#Ahora tenemos la tabla completa

Independientes_uru <- Variables_independientes %>% 
  filter(pais=="Uruguay") %>% 
  mutate(
    IPC_real_Alquileres_uru =IPC_real_Alquileres,
    VAB_inmo_deflactado_uru =VAB_inmo_deflactado,
    gdplcu_uru =`GDP (USD)`
  ) %>% 
  select(Periodo, IPC_real_Alquileres_uru, VAB_inmo_deflactado_uru, gdplcu_uru)


Variables_independientes <- Variables_independientes %>% 
  full_join(Id, by="pais") %>%
  full_join(Trimestres, by="Periodo")  %>% 
  #full_join(IPC_paises, by="Periodo") %>%  
  #filter(!Trimestre <= 94) %>%# 
  group_by(pais) %>% 
  #mutate(Periodo=as.numeric(Periodo)) %>%# 
  mutate(`GDP (USD)`=as.numeric(`GDP (USD)`)) %>% 
  mutate(`GDP (LCU)`=as.numeric(`GDP (LCU)`)) %>% 
  #mutate(`Import goods`=as.numeric(`Import goods`)) %>% # Estas dos las anulo porque colombia y uru no tienen datos de eso y no creo que correlacionen de todas formas
  #mutate(`Export Goods`=as.numeric(`Export Goods`)) %>% # 
  mutate(column1=sum(is.na(`GDP (USD)`))) %>% #El comando synth de stata no corre si todos los periodos de una variable para un pais son todos NA, entonces buscamos los paises que tienen todo NA en GDPUSD para borrarlos, hacemos despues lo mismo para las demas variables
  #mutate(column2=sum(is.na(`Import goods`))) %>%
  mutate(column3=sum(is.na(`GDP (LCU)`))) %>% 
  #mutate(column4=sum(is.na(`Export Goods`))) %>% Expo e impo los saco porque uru y colombia no tienen datos ahi
  mutate(column6=sum(is.na(Unemployment))) %>% 
  #mutate(column8=sum(is.na(SMUSD))) %>% Colombia no tiene valores para este dato
  filter(!column1==120) %>%  
  #filter(!column2==120) %>%
  filter(!column3==120) %>% 
  #filter(!column4==120) %>% 
  filter(!column6==120) %>%
  #filter(!column8==120) %>% 
  filter(pais=="Argentina") %>% 
  ungroup() %>% 
  full_join(Independientes_uru,
            by="Periodo") %>% 
  filter(!Trimestre<=89) %>% 
  select(Trimestre, Periodo, everything(),-"Import goods",-"Export Goods", -column1, -column3, -column6,
         -"ID", -"SMUSD", -"Consumo público nacional", -"Consumo  privado nacional", -"pais")
  
Variables_independientes <- as.data.frame(Variables_independientes)  #para que al correr el synth no aparezca un error


#write.csv(Join, "Correlating_data.csv")


Variables_dependientes <- Alquileres_AMBA_Departamento %>%
  full_join(Alquileres_AMBA_Casa, by=c("pais","Periodo")) %>% 
  full_join(Alquileres_CABA_Departamento, by=c("pais","Periodo")) %>% 
  full_join(Alquileres_CABA_Casa, by=c("pais","Periodo")) %>% 
  full_join(Alquileres_CBANorte_Departamento, by=c("pais","Periodo")) %>% 
  full_join(Alquileres_CBASur_Departamento, by=c("pais","Periodo")) %>% 
  full_join(Alquileres_CBACentro_Departamento, by=c("pais","Periodo")) %>% 
  full_join(Alquileres_CBAEste_Departamento, by=c("pais","Periodo")) %>% 
  full_join(Alquileres_CBAOeste_Departamento, by=c("pais","Periodo")) %>% 
  full_join(Alquileres_CBAOeste_Casa, by=c("pais","Periodo")) %>% 
  full_join(Alquileres_CBANorte_Casa, by=c("pais","Periodo")) %>% 
  full_join(Alquileres_CBASur_Casa, by=c("pais","Periodo")) %>% 
  full_join(Alquileres_CBACentro_Casa, by=c("pais","Periodo")) %>% 
  full_join(Alquileres_cordoba_casa, by=c("pais","Periodo")) %>% 
  full_join(Alquileres_cordoba_departamento, by=c("pais","Periodo")) %>% 
  full_join(Alquileres_rosario_casa, by=c("pais","Periodo")) %>% 
  full_join(Alquileres_rosario_departamento, by=c("pais","Periodo"))
  


Variables_dependientes <- as.data.frame(Variables_dependientes)  #para que al correr el synth no aparezca un error
