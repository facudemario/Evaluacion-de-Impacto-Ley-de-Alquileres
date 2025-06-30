library(tidyverse)
library(readr)
library(dplyr)
library(igraph)
library(readxl)
library(ggplot2)
library(Synth)
library(readxl)
library(openxlsx)


setwd("C:\\Mis Archivos\\Facundo\\Tesis ley de alquileres\\Datos\\EPH\\EPH 2016Q4")
EPH2016Q4hog<-read_excel("usu_hogar_T416.xlsx")
EPH2016Q4ind<-read_excel("usu_individual_T416.xlsx")
setwd("C:\\Mis Archivos\\Facundo\\Tesis ley de alquileres\\Datos\\EPH\\EPH 2017Q1")
EPH2017Q1hog<-read_excel("usu_hogar_T117.xlsx")
EPH2017Q1ind<-read_excel("usu_individual_T117_ult.xlsx")
setwd("C:\\Mis Archivos\\Facundo\\Tesis ley de alquileres\\Datos\\EPH\\EPH 2017Q2")
EPH2017Q2hog<-read_excel("usu_hogar_t217.xls")
EPH2017Q2ind<-read_excel("usu_individual_t217.xls")
setwd("C:\\Mis Archivos\\Facundo\\Tesis ley de alquileres\\Datos\\EPH\\EPH 2017Q3")
EPH2017Q3hog<-read_excel("usu_hogar_T317.xls")
EPH2017Q3ind<-read_excel("usu_individual_T317.xls")
setwd("C:\\Mis Archivos\\Facundo\\Tesis ley de alquileres\\Datos\\EPH\\EPH 2017Q4")
EPH2017Q4hog<-read_excel("usu_hogar_T417.xls")
EPH2017Q4ind<-read_excel("usu_individual_T417.xls")
setwd("C:\\Mis Archivos\\Facundo\\Tesis ley de alquileres\\Datos\\EPH\\EPH 2018Q1")
EPH2018Q1hog<-read_excel("usu_hogar_T118.xls")
EPH2018Q1ind<-read_excel("usu_individual_T118.xls")
setwd("C:\\Mis Archivos\\Facundo\\Tesis ley de alquileres\\Datos\\EPH\\EPH 2018Q2")
EPH2018Q2hog<-read_excel("usu_hogar_T218.xls")
EPH2018Q2ind<-read_excel("usu_individual_T218.xls")
setwd("C:\\Mis Archivos\\Facundo\\Tesis ley de alquileres\\Datos\\EPH\\EPH 2018Q3")
EPH2018Q3hog<-read_excel("usu_hogar_T318.xls")
EPH2018Q3ind<-read_excel("usu_individual_T318..xls")
setwd("C:\\Mis Archivos\\Facundo\\Tesis ley de alquileres\\Datos\\EPH\\EPH 2018Q4")
EPH2018Q4hog<-read_excel("usu_hogar_T418.xls")
EPH2018Q4ind<-read_excel("usu_individual_T418.xls")
setwd("C:\\Mis Archivos\\Facundo\\Tesis ley de alquileres\\Datos\\EPH\\EPH 2019Q1")
EPH2019Q1hog<-read_excel("usu_hogar_t119.xls")
EPH2019Q1ind<-read_excel("usu_individual_T119.xls")
setwd("C:\\Mis Archivos\\Facundo\\Tesis ley de alquileres\\Datos\\EPH\\EPH 2019Q2")
EPH2019Q2hog<-read_excel("usu_hogar_T219.xls")
EPH2019Q2ind<-read_excel("usu_individual_T219.xls")
setwd("C:\\Mis Archivos\\Facundo\\Tesis ley de alquileres\\Datos\\EPH\\EPH 2019Q3")
EPH2019Q3hog<-read_excel("usu_hogar_T319.xls")
EPH2019Q3ind<-read_excel("usu_individual_T319.xls")
setwd("C:\\Mis Archivos\\Facundo\\Tesis ley de alquileres\\Datos\\EPH\\EPH 2019Q4")
EPH2019Q4hog<-read_excel("usu_hogar_T419.xls")
EPH2019Q4ind<-read_excel("usu_individual_T419.xls")
setwd("C:\\Mis Archivos\\Facundo\\Tesis ley de alquileres\\Datos\\EPH\\EPH 2020Q1")
EPH2020Q1hog<-read_excel("usu_hogar_T120.xlsx")
EPH2020Q1ind<-read_excel("usu_individual_T120.xlsx")
setwd("C:\\Mis Archivos\\Facundo\\Tesis ley de alquileres\\Datos\\EPH\\EPH 2020Q2")
EPH2020Q2hog<-read_excel("usu_hogar_T220.xlsx")
EPH2020Q2ind<-read_excel("usu_individual_T220.xlsx")
setwd("C:\\Mis Archivos\\Facundo\\Tesis ley de alquileres\\Datos\\EPH\\EPH 2020Q3")
EPH2020Q3hog<-read_excel("usu_hogar_T320.xlsx")
EPH2020Q3ind<-read_excel("usu_individual_T320.xlsx")
setwd("C:\\Mis Archivos\\Facundo\\Tesis ley de alquileres\\Datos\\EPH\\EPH 2020Q4")
EPH2020Q4hog<-read_excel("EPH_usu_hogar_T420.xlsx")
EPH2020Q4ind<-read_excel("EPH_usu_individual_T420.xlsx")
setwd("C:\\Mis Archivos\\Facundo\\Tesis ley de alquileres\\Datos\\EPH\\EPH 2021Q1")
EPH2021Q1hog<-read_excel("usu_hogar_T121.xlsx")
EPH2021Q1ind<-read_excel("usu_individual_T121.xlsx")
setwd("C:\\Mis Archivos\\Facundo\\Tesis ley de alquileres\\Datos\\EPH\\EPH 2021Q2")
EPH2021Q2hog<-read_excel("usu_hogar_T221.xlsx")
EPH2021Q2ind<-read_excel("usu_individual_T221.xlsx")
setwd("C:\\Mis Archivos\\Facundo\\Tesis ley de alquileres\\Datos\\EPH\\EPH 2021Q3")
EPH2021Q3hog<-read_excel("usu_hogar_T321.xls.xls")
EPH2021Q3ind<-read_excel("usu_individual_T321.xls.xls")
setwd("C:\\Mis Archivos\\Facundo\\Tesis ley de alquileres\\Datos\\EPH\\EPH 2021Q4")
EPH2021Q4hog<-read_excel("usu_hogar_T421.xlsx")
EPH2021Q4ind<-read_excel("usu_individual_T421.xlsx")
setwd("C:\\Mis Archivos\\Facundo\\Tesis ley de alquileres\\Datos\\EPH\\EPH 2022Q1")
EPH2022Q1hog<-read_excel("usu_hogar_t122.xlsx.xlsx")
EPH2022Q1ind<-read_excel("usu_individual_T122.xlsx.xlsx")
setwd("C:\\Mis Archivos\\Facundo\\Tesis ley de alquileres\\Datos\\EPH\\EPH 2022Q2")
EPH2022Q2hog<-read_excel("usu_hogar_T222.xlsx")
EPH2022Q2ind<-read_excel("usu_individual_T222.xlsx")
setwd("C:\\Mis Archivos\\Facundo\\Tesis ley de alquileres\\Datos\\EPH\\EPH 2022Q3")
EPH2022Q3hog<-read_excel("usu_hogar_T322.xlsx")
EPH2022Q3ind<-read_excel("usu_individual_T322.xlsx")
setwd("C:\\Mis Archivos\\Facundo\\Tesis ley de alquileres\\Datos\\EPH\\EPH 2022Q4")
EPH2022Q4hog<-read_excel("usu_hogar_T422.xlsx.xlsx")
EPH2022Q4ind<-read_excel("usu_individual_T422.xlsx.xlsx")
setwd("C:\\Mis Archivos\\Facundo\\Tesis ley de alquileres\\Datos\\EPH\\EPH 2023Q1")
EPH2023Q1hog<-read_excel("usu_hogar_T123.xlsx")
EPH2023Q1ind<-read_excel("usu_individual_T123.xlsx")
setwd("C:\\Mis Archivos\\Facundo\\Tesis ley de alquileres\\Datos\\EPH\\EPH 2023Q2")
EPH2023Q2hog<-read_excel("usu_hogar_T223.xlsx")
EPH2023Q2ind<-read_excel("usu_individual_T223.xlsx")
setwd("C:\\Mis Archivos\\Facundo\\Tesis ley de alquileres\\Datos\\EPH\\EPH 2023Q3")
EPH2023Q3hog<-read_excel("usu_hogar_T323.xlsx")
EPH2023Q3ind<-read_excel("usu_individual_T323.xlsx")
setwd("C:\\Mis Archivos\\Facundo\\Tesis ley de alquileres\\Datos\\EPH\\EPH 2023Q4")
EPH2023Q4hog<-read_excel("usu_hogar_T423.xlsx")
EPH2023Q4ind<-read_excel("usu_individual_T423.xlsx")
setwd("C:\\Mis Archivos\\Facundo\\Tesis ley de alquileres\\Datos\\EPH\\EPH 2024Q1")
EPH2024Q1hog<-read_excel("usu_hogar_T124.xlsx")
EPH2024Q1ind<-read_excel("usu_individual_T124.xlsx")
setwd("C:\\Mis Archivos\\Facundo\\Tesis ley de alquileres\\Datos\\EPH\\EPH 2024Q2")
EPH2024Q2hog<-read_excel("usu_hogar_T224.xlsx")
EPH2024Q2ind<-read_excel("usu_individual_T224.xlsx")



#Data Manipulation

EPH2016Q4 <- EPH2016Q4hog %>%
  mutate(
    ANO4 = as.character(ANO4),
    TRIMESTRE = as.character(TRIMESTRE),
    Periodo = paste0(ANO4, "Q", TRIMESTRE),
    Regimen_tenencia = II7
  ) %>%
  filter(
    REALIZADA == 1
  ) %>%
  select(CODUSU, NRO_HOGAR, Periodo, Regimen_tenencia) %>%
  left_join(EPH2016Q4ind, by = c("CODUSU", "NRO_HOGAR")) %>%
  mutate(
    Ubicacion_trabajo = PP04G,                # Columna para ubicación de trabajo
    Informatizados = substring(as.character(PP04D_COD), 4, 4)
  ) %>%
  filter(ESTADO %in% c(1, 2)) %>%                     # Filtrar ocupados
  select(
    CODUSU, NRO_HOGAR, Periodo, PONDERA, AGLOMERADO,
    Regimen_tenencia, ESTADO, Ubicacion_trabajo, Informatizados
  ) %>%
  group_by(Periodo) %>%                       # Agrupación por período
  summarise(
    TD = sum(PONDERA[ESTADO == 2]),                        # Total de desocupados
    TO = sum(PONDERA[ESTADO == 1]),                        # Total de ocupados
    OV = sum(PONDERA[Ubicacion_trabajo == 6 & ESTADO == 1]),# Ocupados en viviendas
    OI = sum(PONDERA[Informatizados == 3 & ESTADO == 1]),   # Ocupados informatizados
    OR = sum(PONDERA[Ubicacion_trabajo == 6 & Informatizados == 3 & ESTADO == 1]), # Trabajadores remotos
    TOV = OV * 100 / TO,                      # Tasa de ocupación en viviendas
    TOI = OI * 100 / TO,                      # Tasa de ocupación informatizada
    TOR = OR * 100 / TO,                      # Tasa de ocupación remota
    Activos = sum(PONDERA[ESTADO %in% c(1, 2)]),            # PEA
    DesempleoBSAS = sum(PONDERA[ESTADO == 2 & AGLOMERADO %in% c(33, 32)]) / 
      sum(PONDERA[ESTADO %in% c(1, 2) & AGLOMERADO %in% c(33, 32)]), # Tasa de desempleo BsAs
    DesempleoCBA = sum(PONDERA[ESTADO == 2 & AGLOMERADO == 13]) / 
      sum(PONDERA[ESTADO %in% c(1, 2) & AGLOMERADO == 13]),            #Tasa de desmepleo Cordoba
    DesempleoRO = sum(PONDERA[ESTADO == 2 & AGLOMERADO == 04]) /    
      sum(PONDERA[ESTADO %in% c(1, 2) & AGLOMERADO == 04])          #Tasa de desempleo Rosario
  )




EPH2017Q1 <- EPH2017Q1hog %>%
  mutate(
    ANO4 = as.character(ANO4),
    TRIMESTRE = as.character(TRIMESTRE),
    Periodo = paste0(ANO4, "Q", TRIMESTRE),
    Regimen_tenencia = II7
  ) %>%
  filter(
    REALIZADA == 1
  ) %>%
  select(CODUSU, NRO_HOGAR, Periodo, Regimen_tenencia) %>%
  left_join(EPH2017Q1ind, by = c("CODUSU", "NRO_HOGAR")) %>%
  mutate(
    Ubicacion_trabajo = PP04G,                # Columna para ubicación de trabajo
    Informatizados = substring(as.character(PP04D_COD), 4, 4)
  ) %>%
  filter(ESTADO %in% c(1,2))%>%                     # Filtrar ocupados
  select(
    CODUSU, NRO_HOGAR, Periodo, PONDERA, AGLOMERADO,
    Regimen_tenencia, ESTADO, Ubicacion_trabajo, Informatizados
  ) %>%
  group_by(Periodo) %>%                       # Agrupación por período
  summarise(
    TD = sum(PONDERA[ESTADO == 2]),                        # Total de desocupados
    TO = sum(PONDERA[ESTADO == 1]),                        # Total de ocupados
    OV = sum(PONDERA[Ubicacion_trabajo == 6 & ESTADO == 1]),# Ocupados en viviendas
    OI = sum(PONDERA[Informatizados == 3 & ESTADO == 1]),   # Ocupados informatizados
    OR = sum(PONDERA[Ubicacion_trabajo == 6 & Informatizados == 3 & ESTADO == 1]), # Trabajadores remotos
    TOV = OV * 100 / TO,                      # Tasa de ocupación en viviendas
    TOI = OI * 100 / TO,                      # Tasa de ocupación informatizada
    TOR = OR * 100 / TO,                       # Tasa de ocupación remota
    Activos = sum(PONDERA[ESTADO %in% c(1, 2)]),            # PEA
    DesempleoBSAS = sum(PONDERA[ESTADO == 2 & AGLOMERADO %in% c(33, 32)]) / 
      sum(PONDERA[ESTADO %in% c(1, 2) & AGLOMERADO %in% c(33, 32)]), # Tasa de desempleo BsAs
    DesempleoCBA = sum(PONDERA[ESTADO == 2 & AGLOMERADO == 13]) / 
      sum(PONDERA[ESTADO %in% c(1, 2) & AGLOMERADO == 13]),            #Tasa de desmepleo Cordoba
    DesempleoRO = sum(PONDERA[ESTADO == 2 & AGLOMERADO == 04]) /    
      sum(PONDERA[ESTADO %in% c(1, 2) & AGLOMERADO == 04])          #Tasa de desempleo Rosario           
  )


EPH2017Q2 <- EPH2017Q2hog %>%
  mutate(
    ANO4 = as.character(ANO4),
    TRIMESTRE = as.character(TRIMESTRE),
    Periodo = paste0(ANO4, "Q", TRIMESTRE),
    Regimen_tenencia = II7
  ) %>%
  filter(
    REALIZADA == 1
  ) %>%
  select(CODUSU, NRO_HOGAR, Periodo, Regimen_tenencia) %>%
  left_join(EPH2017Q2ind, by = c("CODUSU", "NRO_HOGAR")) %>%
  mutate(
    Ubicacion_trabajo = PP04G,                # Columna para ubicación de trabajo
    Informatizados = substring(as.character(PP04D_COD), 4, 4)
  ) %>%
  filter(ESTADO %in% c(1,2))%>%                     # Filtrar ocupados
  select(
    CODUSU, NRO_HOGAR, Periodo, PONDERA, AGLOMERADO,
    Regimen_tenencia, ESTADO, Ubicacion_trabajo, Informatizados
  ) %>%
  group_by(Periodo) %>%                       # Agrupación por período
  summarise(
    TD = sum(PONDERA[ESTADO == 2]),                        # Total de desocupados
    TO = sum(PONDERA[ESTADO == 1]),                        # Total de ocupados
    OV = sum(PONDERA[Ubicacion_trabajo == 6 & ESTADO == 1]),# Ocupados en viviendas
    OI = sum(PONDERA[Informatizados == 3 & ESTADO == 1]),   # Ocupados informatizados
    OR = sum(PONDERA[Ubicacion_trabajo == 6 & Informatizados == 3 & ESTADO == 1]), # Trabajadores remotos
    TOV = OV * 100 / TO,                      # Tasa de ocupación en viviendas
    TOI = OI * 100 / TO,                      # Tasa de ocupación informatizada
    TOR = OR * 100 / TO ,                       # Tasa de ocupación remota
    Activos = sum(PONDERA[ESTADO %in% c(1, 2)]),            # PEA
    DesempleoBSAS = sum(PONDERA[ESTADO == 2 & AGLOMERADO %in% c(33, 32)]) / 
      sum(PONDERA[ESTADO %in% c(1, 2) & AGLOMERADO %in% c(33, 32)]), # Tasa de desempleo BsAs
    DesempleoCBA = sum(PONDERA[ESTADO == 2 & AGLOMERADO == 13]) / 
      sum(PONDERA[ESTADO %in% c(1, 2) & AGLOMERADO == 13]),            #Tasa de desmepleo Cordoba
    DesempleoRO = sum(PONDERA[ESTADO == 2 & AGLOMERADO == 04]) /    
      sum(PONDERA[ESTADO %in% c(1, 2) & AGLOMERADO == 04])          #Tasa de desempleo Rosario            
  )

EPH2017Q3 <- EPH2017Q3hog %>%
  mutate(
    ANO4 = as.character(ANO4),
    TRIMESTRE = as.character(TRIMESTRE),
    Periodo = paste0(ANO4, "Q", TRIMESTRE),
    Regimen_tenencia = II7
  ) %>%
  filter(
    REALIZADA == 1
  ) %>%
  select(CODUSU, NRO_HOGAR, Periodo, Regimen_tenencia) %>%
  left_join(EPH2017Q3ind, by = c("CODUSU", "NRO_HOGAR")) %>%
  mutate(
    Ubicacion_trabajo = PP04G,                # Columna para ubicación de trabajo
    Informatizados = substring(as.character(PP04D_COD), 4, 4)
  ) %>%
  filter(ESTADO %in% c(1,2))%>%                     # Filtrar ocupados
  select(
    CODUSU, NRO_HOGAR, Periodo, PONDERA, AGLOMERADO,
    Regimen_tenencia, ESTADO, Ubicacion_trabajo, Informatizados
  ) %>%
  group_by(Periodo) %>%                       # Agrupación por período
  summarise(
    TD = sum(PONDERA[ESTADO == 2]),                        # Total de desocupados
    TO = sum(PONDERA[ESTADO == 1]),                        # Total de ocupados
    OV = sum(PONDERA[Ubicacion_trabajo == 6 & ESTADO == 1]),# Ocupados en viviendas
    OI = sum(PONDERA[Informatizados == 3 & ESTADO == 1]),   # Ocupados informatizados
    OR = sum(PONDERA[Ubicacion_trabajo == 6 & Informatizados == 3 & ESTADO == 1]), # Trabajadores remotos
    TOV = OV * 100 / TO,                      # Tasa de ocupación en viviendas
    TOI = OI * 100 / TO,                      # Tasa de ocupación informatizada
    TOR = OR * 100 / TO,                       # Tasa de ocupación remota
    Activos = sum(PONDERA[ESTADO %in% c(1, 2)]),            # PEA
    DesempleoBSAS = sum(PONDERA[ESTADO == 2 & AGLOMERADO %in% c(33, 32)]) / 
      sum(PONDERA[ESTADO %in% c(1, 2) & AGLOMERADO %in% c(33, 32)]), # Tasa de desempleo BsAs
    DesempleoCBA = sum(PONDERA[ESTADO == 2 & AGLOMERADO == 13]) / 
      sum(PONDERA[ESTADO %in% c(1, 2) & AGLOMERADO == 13]),            #Tasa de desmepleo Cordoba
    DesempleoRO = sum(PONDERA[ESTADO == 2 & AGLOMERADO == 04]) /    
      sum(PONDERA[ESTADO %in% c(1, 2) & AGLOMERADO == 04])          #Tasa de desempleo Rosario              
  )

EPH2017Q4 <- EPH2017Q4hog %>%
  mutate(
    ANO4 = as.character(ANO4),
    TRIMESTRE = as.character(TRIMESTRE),
    Periodo = paste0(ANO4, "Q", TRIMESTRE),
    Regimen_tenencia = II7
  ) %>%
  filter(
    REALIZADA == 1
  ) %>%
  select(CODUSU, NRO_HOGAR, Periodo, Regimen_tenencia) %>%
  left_join(EPH2017Q4ind, by = c("CODUSU", "NRO_HOGAR")) %>%
  mutate(
    Ubicacion_trabajo = PP04G,                # Columna para ubicación de trabajo
    Informatizados = substring(as.character(PP04D_COD), 4, 4)
  ) %>%
  filter(ESTADO %in% c(1,2))%>%                     # Filtrar ocupados
  select(
    CODUSU, NRO_HOGAR, Periodo, PONDERA, AGLOMERADO,
    Regimen_tenencia, ESTADO, Ubicacion_trabajo, Informatizados
  ) %>%
  group_by(Periodo) %>%                       # Agrupación por período
  summarise(
    TD = sum(PONDERA[ESTADO == 2]),                        # Total de desocupados
    TO = sum(PONDERA[ESTADO == 1]),                        # Total de ocupados
    OV = sum(PONDERA[Ubicacion_trabajo == 6 & ESTADO == 1]),# Ocupados en viviendas
    OI = sum(PONDERA[Informatizados == 3 & ESTADO == 1]),   # Ocupados informatizados
    OR = sum(PONDERA[Ubicacion_trabajo == 6 & Informatizados == 3 & ESTADO == 1]), # Trabajadores remotos
    TOV = OV * 100 / TO,                      # Tasa de ocupación en viviendas
    TOI = OI * 100 / TO,                      # Tasa de ocupación informatizada
    TOR = OR * 100 / TO,                       # Tasa de ocupación remota
    Activos = sum(PONDERA[ESTADO %in% c(1, 2)]),            # PEA
    DesempleoBSAS = sum(PONDERA[ESTADO == 2 & AGLOMERADO %in% c(33, 32)]) / 
      sum(PONDERA[ESTADO %in% c(1, 2) & AGLOMERADO %in% c(33, 32)]), # Tasa de desempleo BsAs
    DesempleoCBA = sum(PONDERA[ESTADO == 2 & AGLOMERADO == 13]) / 
      sum(PONDERA[ESTADO %in% c(1, 2) & AGLOMERADO == 13]),            #Tasa de desmepleo Cordoba
    DesempleoRO = sum(PONDERA[ESTADO == 2 & AGLOMERADO == 04]) /    
      sum(PONDERA[ESTADO %in% c(1, 2) & AGLOMERADO == 04])          #Tasa de desempleo Rosario              
  )

EPH2018Q1 <- EPH2018Q1hog %>%
  mutate(
    ANO4 = as.character(ANO4),
    TRIMESTRE = as.character(TRIMESTRE),
    Periodo = paste0(ANO4, "Q", TRIMESTRE),
    Regimen_tenencia = II7
  ) %>%
  filter(
    REALIZADA == 1
  ) %>%
  select(CODUSU, NRO_HOGAR, Periodo, Regimen_tenencia) %>%
  left_join(EPH2018Q1ind, by = c("CODUSU", "NRO_HOGAR")) %>%
  mutate(
    Ubicacion_trabajo = PP04G,                # Columna para ubicación de trabajo
    Informatizados = substring(as.character(PP04D_COD), 4, 4)
  ) %>%
  filter(ESTADO %in% c(1,2))%>%                     # Filtrar ocupados
  select(
    CODUSU, NRO_HOGAR, Periodo, PONDERA, AGLOMERADO,
    Regimen_tenencia, ESTADO, Ubicacion_trabajo, Informatizados
  ) %>%
  group_by(Periodo) %>%                       # Agrupación por período
  summarise(
    TD = sum(PONDERA[ESTADO == 2]),                        # Total de desocupados
    TO = sum(PONDERA[ESTADO == 1]),                        # Total de ocupados
    OV = sum(PONDERA[Ubicacion_trabajo == 6 & ESTADO == 1]),# Ocupados en viviendas
    OI = sum(PONDERA[Informatizados == 3 & ESTADO == 1]),   # Ocupados informatizados
    OR = sum(PONDERA[Ubicacion_trabajo == 6 & Informatizados == 3 & ESTADO == 1]), # Trabajadores remotos
    TOV = OV * 100 / TO,                      # Tasa de ocupación en viviendas
    TOI = OI * 100 / TO,                      # Tasa de ocupación informatizada
    TOR = OR * 100 / TO,                       # Tasa de ocupación remota
    Activos = sum(PONDERA[ESTADO %in% c(1, 2)]),            # PEA
    DesempleoBSAS = sum(PONDERA[ESTADO == 2 & AGLOMERADO %in% c(33, 32)]) / 
      sum(PONDERA[ESTADO %in% c(1, 2) & AGLOMERADO %in% c(33, 32)]), # Tasa de desempleo BsAs
    DesempleoCBA = sum(PONDERA[ESTADO == 2 & AGLOMERADO == 13]) / 
      sum(PONDERA[ESTADO %in% c(1, 2) & AGLOMERADO == 13]),            #Tasa de desmepleo Cordoba
    DesempleoRO = sum(PONDERA[ESTADO == 2 & AGLOMERADO == 04]) /    
      sum(PONDERA[ESTADO %in% c(1, 2) & AGLOMERADO == 04])          #Tasa de desempleo Rosario              
  )

EPH2018Q2 <- EPH2018Q2hog %>%
  mutate(
    ANO4 = as.character(ANO4),
    TRIMESTRE = as.character(TRIMESTRE),
    Periodo = paste0(ANO4, "Q", TRIMESTRE),
    Regimen_tenencia = II7
  ) %>%
  filter(
    REALIZADA == 1
  ) %>%
  select(CODUSU, NRO_HOGAR, Periodo, Regimen_tenencia) %>%
  left_join(EPH2018Q2ind, by = c("CODUSU", "NRO_HOGAR")) %>%
  mutate(
    Ubicacion_trabajo = PP04G,                # Columna para ubicación de trabajo
    Informatizados = substring(as.character(PP04D_COD), 4, 4)
  ) %>%
  filter(ESTADO %in% c(1,2))%>%                     # Filtrar ocupados
  select(
    CODUSU, NRO_HOGAR, Periodo, PONDERA, AGLOMERADO,
    Regimen_tenencia, ESTADO, Ubicacion_trabajo, Informatizados
  ) %>%
  group_by(Periodo) %>%                       # Agrupación por período
  summarise(
    TD = sum(PONDERA[ESTADO == 2]),                        # Total de desocupados
    TO = sum(PONDERA[ESTADO == 1]),                        # Total de ocupados
    OV = sum(PONDERA[Ubicacion_trabajo == 6 & ESTADO == 1]),# Ocupados en viviendas
    OI = sum(PONDERA[Informatizados == 3 & ESTADO == 1]),   # Ocupados informatizados
    OR = sum(PONDERA[Ubicacion_trabajo == 6 & Informatizados == 3 & ESTADO == 1]), # Trabajadores remotos
    TOV = OV * 100 / TO,                      # Tasa de ocupación en viviendas
    TOI = OI * 100 / TO,                      # Tasa de ocupación informatizada
    TOR = OR * 100 / TO,                       # Tasa de ocupación remota
    Activos = sum(PONDERA[ESTADO %in% c(1, 2)]),            # PEA
    DesempleoBSAS = sum(PONDERA[ESTADO == 2 & AGLOMERADO %in% c(33, 32)]) / 
      sum(PONDERA[ESTADO %in% c(1, 2) & AGLOMERADO %in% c(33, 32)]), # Tasa de desempleo BsAs
    DesempleoCBA = sum(PONDERA[ESTADO == 2 & AGLOMERADO == 13]) / 
      sum(PONDERA[ESTADO %in% c(1, 2) & AGLOMERADO == 13]),            #Tasa de desmepleo Cordoba
    DesempleoRO = sum(PONDERA[ESTADO == 2 & AGLOMERADO == 04]) /    
      sum(PONDERA[ESTADO %in% c(1, 2) & AGLOMERADO == 04])          #Tasa de desempleo Rosario              
  )

EPH2018Q3 <- EPH2018Q3hog %>%
  mutate(
    ANO4 = as.character(ANO4),
    TRIMESTRE = as.character(TRIMESTRE),
    Periodo = paste0(ANO4, "Q", TRIMESTRE),
    Regimen_tenencia = II7
  ) %>%
  filter(
    REALIZADA == 1
  ) %>%
  select(CODUSU, NRO_HOGAR, Periodo, Regimen_tenencia) %>%
  left_join(EPH2018Q3ind, by = c("CODUSU", "NRO_HOGAR")) %>%
  mutate(
    Ubicacion_trabajo = PP04G,                # Columna para ubicación de trabajo
    Informatizados = substring(as.character(PP04D_COD), 4, 4)
  ) %>%
  filter(ESTADO %in% c(1,2))%>%                     # Filtrar ocupados
  select(
    CODUSU, NRO_HOGAR, Periodo, PONDERA, AGLOMERADO,
    Regimen_tenencia, ESTADO, Ubicacion_trabajo, Informatizados
  ) %>%
  group_by(Periodo) %>%                       # Agrupación por período
  summarise(
    TD = sum(PONDERA[ESTADO == 2]),                        # Total de desocupados
    TO = sum(PONDERA[ESTADO == 1]),                        # Total de ocupados
    OV = sum(PONDERA[Ubicacion_trabajo == 6 & ESTADO == 1]),# Ocupados en viviendas
    OI = sum(PONDERA[Informatizados == 3 & ESTADO == 1]),   # Ocupados informatizados
    OR = sum(PONDERA[Ubicacion_trabajo == 6 & Informatizados == 3 & ESTADO == 1]), # Trabajadores remotos
    TOV = OV * 100 / TO,                      # Tasa de ocupación en viviendas
    TOI = OI * 100 / TO,                      # Tasa de ocupación informatizada
    TOR = OR * 100 / TO,                       # Tasa de ocupación remota
    Activos = sum(PONDERA[ESTADO %in% c(1, 2)]),            # PEA
    DesempleoBSAS = sum(PONDERA[ESTADO == 2 & AGLOMERADO %in% c(33, 32)]) / 
      sum(PONDERA[ESTADO %in% c(1, 2) & AGLOMERADO %in% c(33, 32)]), # Tasa de desempleo BsAs
    DesempleoCBA = sum(PONDERA[ESTADO == 2 & AGLOMERADO == 13]) / 
      sum(PONDERA[ESTADO %in% c(1, 2) & AGLOMERADO == 13]),            #Tasa de desmepleo Cordoba
    DesempleoRO = sum(PONDERA[ESTADO == 2 & AGLOMERADO == 04]) /    
      sum(PONDERA[ESTADO %in% c(1, 2) & AGLOMERADO == 04])          #Tasa de desempleo Rosario              
  )

EPH2018Q4 <- EPH2018Q4hog %>%
  mutate(
    ANO4 = as.character(ANO4),
    TRIMESTRE = as.character(TRIMESTRE),
    Periodo = paste0(ANO4, "Q", TRIMESTRE),
    Regimen_tenencia = II7
  ) %>%
  filter(
    REALIZADA == 1
  ) %>%
  select(CODUSU, NRO_HOGAR, Periodo, Regimen_tenencia) %>%
  left_join(EPH2018Q4ind, by = c("CODUSU", "NRO_HOGAR")) %>%
  mutate(
    Ubicacion_trabajo = PP04G,                # Columna para ubicación de trabajo
    Informatizados = substring(as.character(PP04D_COD), 4, 4)
  ) %>%
  filter(ESTADO %in% c(1,2))%>%                     # Filtrar ocupados
  select(
    CODUSU, NRO_HOGAR, Periodo, PONDERA, AGLOMERADO,
    Regimen_tenencia, ESTADO, Ubicacion_trabajo, Informatizados
  ) %>%
  group_by(Periodo) %>%                       # Agrupación por período
  summarise(
    TD = sum(PONDERA[ESTADO == 2]),                        # Total de desocupados
    TO = sum(PONDERA[ESTADO == 1]),                        # Total de ocupados
    OV = sum(PONDERA[Ubicacion_trabajo == 6 & ESTADO == 1]),# Ocupados en viviendas
    OI = sum(PONDERA[Informatizados == 3 & ESTADO == 1]),   # Ocupados informatizados
    OR = sum(PONDERA[Ubicacion_trabajo == 6 & Informatizados == 3 & ESTADO == 1]), # Trabajadores remotos
    TOV = OV * 100 / TO,                      # Tasa de ocupación en viviendas
    TOI = OI * 100 / TO,                      # Tasa de ocupación informatizada
    TOR = OR * 100 / TO,                       # Tasa de ocupación remota
    Activos = sum(PONDERA[ESTADO %in% c(1, 2)]),            # PEA
    DesempleoBSAS = sum(PONDERA[ESTADO == 2 & AGLOMERADO %in% c(33, 32)]) / 
      sum(PONDERA[ESTADO %in% c(1, 2) & AGLOMERADO %in% c(33, 32)]), # Tasa de desempleo BsAs
    DesempleoCBA = sum(PONDERA[ESTADO == 2 & AGLOMERADO == 13]) / 
      sum(PONDERA[ESTADO %in% c(1, 2) & AGLOMERADO == 13]),            #Tasa de desmepleo Cordoba
    DesempleoRO = sum(PONDERA[ESTADO == 2 & AGLOMERADO == 04]) /    
      sum(PONDERA[ESTADO %in% c(1, 2) & AGLOMERADO == 04])          #Tasa de desempleo Rosario              
  )

EPH2019Q1 <- EPH2019Q1hog %>%
  mutate(
    ANO4 = as.character(ANO4),
    TRIMESTRE = as.character(TRIMESTRE),
    Periodo = paste0(ANO4, "Q", TRIMESTRE),
    Regimen_tenencia = II7
  ) %>%
  filter(
    REALIZADA == 1
  ) %>%
  select(CODUSU, NRO_HOGAR, Periodo, Regimen_tenencia) %>%
  left_join(EPH2019Q1ind, by = c("CODUSU", "NRO_HOGAR")) %>%
  mutate(
    Ubicacion_trabajo = PP04G,                # Columna para ubicación de trabajo
    Informatizados = substring(as.character(PP04D_COD), 4, 4)
  ) %>%
  filter(ESTADO %in% c(1,2))%>%                     # Filtrar ocupados
  select(
    CODUSU, NRO_HOGAR, Periodo, PONDERA, AGLOMERADO,
    Regimen_tenencia, ESTADO, Ubicacion_trabajo, Informatizados
  ) %>%
  group_by(Periodo) %>%                       # Agrupación por período
  summarise(
    TD = sum(PONDERA[ESTADO == 2]),                        # Total de desocupados
    TO = sum(PONDERA[ESTADO == 1]),                        # Total de ocupados
    OV = sum(PONDERA[Ubicacion_trabajo == 6 & ESTADO == 1]),# Ocupados en viviendas
    OI = sum(PONDERA[Informatizados == 3 & ESTADO == 1]),   # Ocupados informatizados
    OR = sum(PONDERA[Ubicacion_trabajo == 6 & Informatizados == 3 & ESTADO == 1]), # Trabajadores remotos
    TOV = OV * 100 / TO,                      # Tasa de ocupación en viviendas
    TOI = OI * 100 / TO,                      # Tasa de ocupación informatizada
    TOR = OR * 100 / TO,                       # Tasa de ocupación remota
    Activos = sum(PONDERA[ESTADO %in% c(1, 2)]),            # PEA
    DesempleoBSAS = sum(PONDERA[ESTADO == 2 & AGLOMERADO %in% c(33, 32)]) / 
      sum(PONDERA[ESTADO %in% c(1, 2) & AGLOMERADO %in% c(33, 32)]), # Tasa de desempleo BsAs
    DesempleoCBA = sum(PONDERA[ESTADO == 2 & AGLOMERADO == 13]) / 
      sum(PONDERA[ESTADO %in% c(1, 2) & AGLOMERADO == 13]),            #Tasa de desmepleo Cordoba
    DesempleoRO = sum(PONDERA[ESTADO == 2 & AGLOMERADO == 04]) /    
      sum(PONDERA[ESTADO %in% c(1, 2) & AGLOMERADO == 04])          #Tasa de desempleo Rosario              
  )

EPH2019Q2 <- EPH2019Q2hog %>%
  mutate(
    ANO4 = as.character(ANO4),
    TRIMESTRE = as.character(TRIMESTRE),
    Periodo = paste0(ANO4, "Q", TRIMESTRE),
    Regimen_tenencia = II7
  ) %>%
  filter(
    REALIZADA == 1
  ) %>%
  select(CODUSU, NRO_HOGAR, Periodo, Regimen_tenencia) %>%
  left_join(EPH2019Q2ind, by = c("CODUSU", "NRO_HOGAR")) %>%
  mutate(
    Ubicacion_trabajo = PP04G,                # Columna para ubicación de trabajo
    Informatizados = substring(as.character(PP04D_COD), 4, 4)
  ) %>%
  filter(ESTADO %in% c(1,2))%>%                     # Filtrar ocupados
  select(
    CODUSU, NRO_HOGAR, Periodo, PONDERA, AGLOMERADO,
    Regimen_tenencia, ESTADO, Ubicacion_trabajo, Informatizados
  ) %>%
  group_by(Periodo) %>%                       # Agrupación por período
  summarise(
    TD = sum(PONDERA[ESTADO == 2]),                        # Total de desocupados
    TO = sum(PONDERA[ESTADO == 1]),                        # Total de ocupados
    OV = sum(PONDERA[Ubicacion_trabajo == 6 & ESTADO == 1]),# Ocupados en viviendas
    OI = sum(PONDERA[Informatizados == 3 & ESTADO == 1]),   # Ocupados informatizados
    OR = sum(PONDERA[Ubicacion_trabajo == 6 & Informatizados == 3 & ESTADO == 1]), # Trabajadores remotos
    TOV = OV * 100 / TO,                      # Tasa de ocupación en viviendas
    TOI = OI * 100 / TO,                      # Tasa de ocupación informatizada
    TOR = OR * 100 / TO,                       # Tasa de ocupación remota
    Activos = sum(PONDERA[ESTADO %in% c(1, 2)]),            # PEA
    DesempleoBSAS = sum(PONDERA[ESTADO == 2 & AGLOMERADO %in% c(33, 32)]) / 
      sum(PONDERA[ESTADO %in% c(1, 2) & AGLOMERADO %in% c(33, 32)]), # Tasa de desempleo BsAs
    DesempleoCBA = sum(PONDERA[ESTADO == 2 & AGLOMERADO == 13]) / 
      sum(PONDERA[ESTADO %in% c(1, 2) & AGLOMERADO == 13]),            #Tasa de desmepleo Cordoba
    DesempleoRO = sum(PONDERA[ESTADO == 2 & AGLOMERADO == 04]) /    
      sum(PONDERA[ESTADO %in% c(1, 2) & AGLOMERADO == 04])          #Tasa de desempleo Rosario              
  )

EPH2019Q3 <- EPH2019Q3hog %>%
  mutate(
    ANO4 = as.character(ANO4),
    TRIMESTRE = as.character(TRIMESTRE),
    Periodo = paste0(ANO4, "Q", TRIMESTRE),
    Regimen_tenencia = II7
  ) %>%
  filter(
    REALIZADA == 1
  ) %>%
  select(CODUSU, NRO_HOGAR, Periodo, Regimen_tenencia) %>%
  left_join(EPH2019Q3ind, by = c("CODUSU", "NRO_HOGAR")) %>%
  mutate(
    Ubicacion_trabajo = PP04G,                # Columna para ubicación de trabajo
    Informatizados = substring(as.character(PP04D_COD), 4, 4)
  ) %>%
  filter(ESTADO %in% c(1,2))%>%                     # Filtrar ocupados
  select(
    CODUSU, NRO_HOGAR, Periodo, PONDERA, AGLOMERADO,
    Regimen_tenencia, ESTADO, Ubicacion_trabajo, Informatizados
  ) %>%
  group_by(Periodo) %>%                       # Agrupación por período
  summarise(
    TD = sum(PONDERA[ESTADO == 2]),                        # Total de desocupados
    TO = sum(PONDERA[ESTADO == 1]),                        # Total de ocupados
    OV = sum(PONDERA[Ubicacion_trabajo == 6 & ESTADO == 1]),# Ocupados en viviendas
    OI = sum(PONDERA[Informatizados == 3 & ESTADO == 1]),   # Ocupados informatizados
    OR = sum(PONDERA[Ubicacion_trabajo == 6 & Informatizados == 3 & ESTADO == 1]), # Trabajadores remotos
    TOV = OV * 100 / TO,                      # Tasa de ocupación en viviendas
    TOI = OI * 100 / TO,                      # Tasa de ocupación informatizada
    TOR = OR * 100 / TO,                       # Tasa de ocupación remota
    Activos = sum(PONDERA[ESTADO %in% c(1, 2)]),            # PEA
    DesempleoBSAS = sum(PONDERA[ESTADO == 2 & AGLOMERADO %in% c(33, 32)]) / 
      sum(PONDERA[ESTADO %in% c(1, 2) & AGLOMERADO %in% c(33, 32)]), # Tasa de desempleo BsAs
    DesempleoCBA = sum(PONDERA[ESTADO == 2 & AGLOMERADO == 13]) / 
      sum(PONDERA[ESTADO %in% c(1, 2) & AGLOMERADO == 13]),            #Tasa de desmepleo Cordoba
    DesempleoRO = sum(PONDERA[ESTADO == 2 & AGLOMERADO == 04]) /    
      sum(PONDERA[ESTADO %in% c(1, 2) & AGLOMERADO == 04])          #Tasa de desempleo Rosario              
  )

EPH2019Q4 <- EPH2019Q4hog %>%
  mutate(
    ANO4 = as.character(ANO4),
    TRIMESTRE = as.character(TRIMESTRE),
    Periodo = paste0(ANO4, "Q", TRIMESTRE),
    Regimen_tenencia = II7
  ) %>%
  filter(
    REALIZADA == 1
  ) %>%
  select(CODUSU, NRO_HOGAR, Periodo, Regimen_tenencia) %>%
  left_join(EPH2019Q4ind, by = c("CODUSU", "NRO_HOGAR")) %>%
  mutate(
    Ubicacion_trabajo = PP04G,                # Columna para ubicación de trabajo
    Informatizados = substring(as.character(PP04D_COD), 4, 4)
  ) %>%
  filter(ESTADO %in% c(1,2))%>%                     # Filtrar ocupados
  select(
    CODUSU, NRO_HOGAR, Periodo, PONDERA, AGLOMERADO,
    Regimen_tenencia, ESTADO, Ubicacion_trabajo, Informatizados
  ) %>%
  group_by(Periodo) %>%                       # Agrupación por período
  summarise(
    TD = sum(PONDERA[ESTADO == 2]),                        # Total de desocupados
    TO = sum(PONDERA[ESTADO == 1]),                        # Total de ocupados
    OV = sum(PONDERA[Ubicacion_trabajo == 6 & ESTADO == 1]),# Ocupados en viviendas
    OI = sum(PONDERA[Informatizados == 3 & ESTADO == 1]),   # Ocupados informatizados
    OR = sum(PONDERA[Ubicacion_trabajo == 6 & Informatizados == 3 & ESTADO == 1]), # Trabajadores remotos
    TOV = OV * 100 / TO,                      # Tasa de ocupación en viviendas
    TOI = OI * 100 / TO,                      # Tasa de ocupación informatizada
    TOR = OR * 100 / TO,                       # Tasa de ocupación remota
    Activos = sum(PONDERA[ESTADO %in% c(1, 2)]),            # PEA
    DesempleoBSAS = sum(PONDERA[ESTADO == 2 & AGLOMERADO %in% c(33, 32)]) / 
      sum(PONDERA[ESTADO %in% c(1, 2) & AGLOMERADO %in% c(33, 32)]), # Tasa de desempleo BsAs
    DesempleoCBA = sum(PONDERA[ESTADO == 2 & AGLOMERADO == 13]) / 
      sum(PONDERA[ESTADO %in% c(1, 2) & AGLOMERADO == 13]),            #Tasa de desmepleo Cordoba
    DesempleoRO = sum(PONDERA[ESTADO == 2 & AGLOMERADO == 04]) /    
      sum(PONDERA[ESTADO %in% c(1, 2) & AGLOMERADO == 04])          #Tasa de desempleo Rosario              
  )

EPH2020Q1 <- EPH2020Q1hog %>%
  mutate(
    ANO4 = as.character(ANO4),
    TRIMESTRE = as.character(TRIMESTRE),
    Periodo = paste0(ANO4, "Q", TRIMESTRE),
    Regimen_tenencia = II7
  ) %>%
  filter(
    REALIZADA == 1
  ) %>%
  select(CODUSU, NRO_HOGAR, Periodo, Regimen_tenencia) %>%
  left_join(EPH2020Q1ind, by = c("CODUSU", "NRO_HOGAR")) %>%
  mutate(
    Ubicacion_trabajo = PP04G,                # Columna para ubicación de trabajo
    Informatizados = substring(as.character(PP04D_COD), 4, 4)
  ) %>%
  filter(ESTADO %in% c(1,2))%>%                     # Filtrar ocupados
  select(
    CODUSU, NRO_HOGAR, Periodo, PONDERA, AGLOMERADO,
    Regimen_tenencia, ESTADO, Ubicacion_trabajo, Informatizados
  ) %>%
  group_by(Periodo) %>%                       # Agrupación por período
  summarise(
    TD = sum(PONDERA[ESTADO == 2]),                        # Total de desocupados
    TO = sum(PONDERA[ESTADO == 1]),                        # Total de ocupados
    OV = sum(PONDERA[Ubicacion_trabajo == 6 & ESTADO == 1]),# Ocupados en viviendas
    OI = sum(PONDERA[Informatizados == 3 & ESTADO == 1]),   # Ocupados informatizados
    OR = sum(PONDERA[Ubicacion_trabajo == 6 & Informatizados == 3 & ESTADO == 1]), # Trabajadores remotos
    TOV = OV * 100 / TO,                      # Tasa de ocupación en viviendas
    TOI = OI * 100 / TO,                      # Tasa de ocupación informatizada
    TOR = OR * 100 / TO,                       # Tasa de ocupación remota
    Activos = sum(PONDERA[ESTADO %in% c(1, 2)]),            # PEA
    DesempleoBSAS = sum(PONDERA[ESTADO == 2 & AGLOMERADO %in% c(33, 32)]) / 
      sum(PONDERA[ESTADO %in% c(1, 2) & AGLOMERADO %in% c(33, 32)]), # Tasa de desempleo BsAs
    DesempleoCBA = sum(PONDERA[ESTADO == 2 & AGLOMERADO == 13]) / 
      sum(PONDERA[ESTADO %in% c(1, 2) & AGLOMERADO == 13]),            #Tasa de desmepleo Cordoba
    DesempleoRO = sum(PONDERA[ESTADO == 2 & AGLOMERADO == 04]) /    
      sum(PONDERA[ESTADO %in% c(1, 2) & AGLOMERADO == 04])          #Tasa de desempleo Rosario              
  )

EPH2020Q2 <- EPH2020Q2hog %>%
  mutate(
    ANO4 = as.character(ANO4),
    TRIMESTRE = as.character(TRIMESTRE),
    Periodo = paste0(ANO4, "Q", TRIMESTRE),
    Regimen_tenencia = II7
  ) %>%
  filter(
    REALIZADA == 1
  ) %>%
  select(CODUSU, NRO_HOGAR, Periodo, Regimen_tenencia) %>%
  left_join(EPH2020Q2ind, by = c("CODUSU", "NRO_HOGAR")) %>%
  mutate(
    Ubicacion_trabajo = PP04G,                # Columna para ubicación de trabajo
    Informatizados = substring(as.character(PP04D_COD), 4, 4)
  ) %>%
  filter(ESTADO %in% c(1,2))%>%                     # Filtrar ocupados
  select(
    CODUSU, NRO_HOGAR, Periodo, PONDERA, AGLOMERADO,
    Regimen_tenencia, ESTADO, Ubicacion_trabajo, Informatizados
  ) %>%
  group_by(Periodo) %>%                       # Agrupación por período
  summarise(
    TD = sum(PONDERA[ESTADO == 2]),                        # Total de desocupados
    TO = sum(PONDERA[ESTADO == 1]),                        # Total de ocupados
    OV = sum(PONDERA[Ubicacion_trabajo == 6 & ESTADO == 1]),# Ocupados en viviendas
    OI = sum(PONDERA[Informatizados == 3 & ESTADO == 1]),   # Ocupados informatizados
    OR = sum(PONDERA[Ubicacion_trabajo == 6 & Informatizados == 3 & ESTADO == 1]), # Trabajadores remotos
    TOV = OV * 100 / TO,                      # Tasa de ocupación en viviendas
    TOI = OI * 100 / TO,                      # Tasa de ocupación informatizada
    TOR = OR * 100 / TO,                       # Tasa de ocupación remota
    Activos = sum(PONDERA[ESTADO %in% c(1, 2)]),            # PEA
    DesempleoBSAS = sum(PONDERA[ESTADO == 2 & AGLOMERADO %in% c(33, 32)]) / 
      sum(PONDERA[ESTADO %in% c(1, 2) & AGLOMERADO %in% c(33, 32)]), # Tasa de desempleo BsAs
    DesempleoCBA = sum(PONDERA[ESTADO == 2 & AGLOMERADO == 13]) / 
      sum(PONDERA[ESTADO %in% c(1, 2) & AGLOMERADO == 13]),            #Tasa de desmepleo Cordoba
    DesempleoRO = sum(PONDERA[ESTADO == 2 & AGLOMERADO == 04]) /    
      sum(PONDERA[ESTADO %in% c(1, 2) & AGLOMERADO == 04])          #Tasa de desempleo Rosario              
  )

EPH2020Q3 <- EPH2020Q3hog %>%
  mutate(
    ANO4 = as.character(ANO4),
    TRIMESTRE = as.character(TRIMESTRE),
    Periodo = paste0(ANO4, "Q", TRIMESTRE),
    Regimen_tenencia = II7
  ) %>%
  filter(
    REALIZADA == 1
  ) %>%
  select(CODUSU, NRO_HOGAR, Periodo, Regimen_tenencia) %>%
  left_join(EPH2020Q3ind, by = c("CODUSU", "NRO_HOGAR")) %>%
  mutate(
    Ubicacion_trabajo = PP04G,                # Columna para ubicación de trabajo
    Informatizados = substring(as.character(PP04D_COD), 4, 4)
  ) %>%
  filter(ESTADO %in% c(1,2))%>%                     # Filtrar ocupados
  select(
    CODUSU, NRO_HOGAR, Periodo, PONDERA, AGLOMERADO,
    Regimen_tenencia, ESTADO, Ubicacion_trabajo, Informatizados
  ) %>%
  group_by(Periodo) %>%                       # Agrupación por período
  summarise(
    TD = sum(PONDERA[ESTADO == 2]),                        # Total de desocupados
    TO = sum(PONDERA[ESTADO == 1]),                        # Total de ocupados
    OV = sum(PONDERA[Ubicacion_trabajo == 6 & ESTADO == 1]),# Ocupados en viviendas
    OI = sum(PONDERA[Informatizados == 3 & ESTADO == 1]),   # Ocupados informatizados
    OR = sum(PONDERA[Ubicacion_trabajo == 6 & Informatizados == 3 & ESTADO == 1]), # Trabajadores remotos
    TOV = OV * 100 / TO,                      # Tasa de ocupación en viviendas
    TOI = OI * 100 / TO,                      # Tasa de ocupación informatizada
    TOR = OR * 100 / TO,                       # Tasa de ocupación remota
    Activos = sum(PONDERA[ESTADO %in% c(1, 2)]),            # PEA
    DesempleoBSAS = sum(PONDERA[ESTADO == 2 & AGLOMERADO %in% c(33, 32)]) / 
      sum(PONDERA[ESTADO %in% c(1, 2) & AGLOMERADO %in% c(33, 32)]), # Tasa de desempleo BsAs
    DesempleoCBA = sum(PONDERA[ESTADO == 2 & AGLOMERADO == 13]) / 
      sum(PONDERA[ESTADO %in% c(1, 2) & AGLOMERADO == 13]),            #Tasa de desmepleo Cordoba
    DesempleoRO = sum(PONDERA[ESTADO == 2 & AGLOMERADO == 04]) /    
      sum(PONDERA[ESTADO %in% c(1, 2) & AGLOMERADO == 04])          #Tasa de desempleo Rosario              
  )

EPH2020Q4 <- EPH2020Q4hog %>%
  mutate(
    ANO4 = as.character(ANO4),
    TRIMESTRE = as.character(TRIMESTRE),
    Periodo = paste0(ANO4, "Q", TRIMESTRE),
    Regimen_tenencia = II7
  ) %>%
  filter(
    REALIZADA == 1
  ) %>%
  select(CODUSU, NRO_HOGAR, Periodo, Regimen_tenencia) %>%
  left_join(EPH2020Q4ind, by = c("CODUSU", "NRO_HOGAR")) %>%
  mutate(
    Ubicacion_trabajo = PP04G,                # Columna para ubicación de trabajo
    Informatizados = substring(as.character(PP04D_COD), 4, 4)
  ) %>%
  filter(ESTADO %in% c(1,2))%>%                     # Filtrar ocupados
  select(
    CODUSU, NRO_HOGAR, Periodo, PONDERA, AGLOMERADO,
    Regimen_tenencia, ESTADO, Ubicacion_trabajo, Informatizados
  ) %>%
  group_by(Periodo) %>%                       # Agrupación por período
  summarise(
    TD = sum(PONDERA[ESTADO == 2]),                        # Total de desocupados
    TO = sum(PONDERA[ESTADO == 1]),                        # Total de ocupados
    OV = sum(PONDERA[Ubicacion_trabajo == 6 & ESTADO == 1]),# Ocupados en viviendas
    OI = sum(PONDERA[Informatizados == 3 & ESTADO == 1]),   # Ocupados informatizados
    OR = sum(PONDERA[Ubicacion_trabajo == 6 & Informatizados == 3 & ESTADO == 1]), # Trabajadores remotos
    TOV = OV * 100 / TO,                      # Tasa de ocupación en viviendas
    TOI = OI * 100 / TO,                      # Tasa de ocupación informatizada
    TOR = OR * 100 / TO,                       # Tasa de ocupación remota
    Activos = sum(PONDERA[ESTADO %in% c(1, 2)]),            # PEA
    DesempleoBSAS = sum(PONDERA[ESTADO == 2 & AGLOMERADO %in% c(33, 32)]) / 
      sum(PONDERA[ESTADO %in% c(1, 2) & AGLOMERADO %in% c(33, 32)]), # Tasa de desempleo BsAs
    DesempleoCBA = sum(PONDERA[ESTADO == 2 & AGLOMERADO == 13]) / 
      sum(PONDERA[ESTADO %in% c(1, 2) & AGLOMERADO == 13]),            #Tasa de desmepleo Cordoba
    DesempleoRO = sum(PONDERA[ESTADO == 2 & AGLOMERADO == 04]) /    
      sum(PONDERA[ESTADO %in% c(1, 2) & AGLOMERADO == 04])          #Tasa de desempleo Rosario              
  )

EPH2021Q1 <- EPH2021Q1hog %>%
  mutate(
    ANO4 = as.character(ANO4),
    TRIMESTRE = as.character(TRIMESTRE),
    Periodo = paste0(ANO4, "Q", TRIMESTRE),
    Regimen_tenencia = II7
  ) %>%
  filter(
    REALIZADA == 1
  ) %>%
  select(CODUSU, NRO_HOGAR, Periodo, Regimen_tenencia) %>%
  left_join(EPH2021Q1ind, by = c("CODUSU", "NRO_HOGAR")) %>%
  mutate(
    Ubicacion_trabajo = PP04G,                # Columna para ubicación de trabajo
    Informatizados = substring(as.character(PP04D_COD), 4, 4)
  ) %>%
  filter(ESTADO %in% c(1,2))%>%                     # Filtrar ocupados
  select(
    CODUSU, NRO_HOGAR, Periodo, PONDERA, AGLOMERADO,
    Regimen_tenencia, ESTADO, Ubicacion_trabajo, Informatizados
  ) %>%
  group_by(Periodo) %>%                       # Agrupación por período
  summarise(
    TD = sum(PONDERA[ESTADO == 2]),                        # Total de desocupados
    TO = sum(PONDERA[ESTADO == 1]),                        # Total de ocupados
    OV = sum(PONDERA[Ubicacion_trabajo == 6 & ESTADO == 1]),# Ocupados en viviendas
    OI = sum(PONDERA[Informatizados == 3 & ESTADO == 1]),   # Ocupados informatizados
    OR = sum(PONDERA[Ubicacion_trabajo == 6 & Informatizados == 3 & ESTADO == 1]), # Trabajadores remotos
    TOV = OV * 100 / TO,                      # Tasa de ocupación en viviendas
    TOI = OI * 100 / TO,                      # Tasa de ocupación informatizada
    TOR = OR * 100 / TO,                       # Tasa de ocupación remota
    Activos = sum(PONDERA[ESTADO %in% c(1, 2)]),            # PEA
    DesempleoBSAS = sum(PONDERA[ESTADO == 2 & AGLOMERADO %in% c(33, 32)]) / 
      sum(PONDERA[ESTADO %in% c(1, 2) & AGLOMERADO %in% c(33, 32)]), # Tasa de desempleo BsAs
    DesempleoCBA = sum(PONDERA[ESTADO == 2 & AGLOMERADO == 13]) / 
      sum(PONDERA[ESTADO %in% c(1, 2) & AGLOMERADO == 13]),            #Tasa de desmepleo Cordoba
    DesempleoRO = sum(PONDERA[ESTADO == 2 & AGLOMERADO == 04]) /    
      sum(PONDERA[ESTADO %in% c(1, 2) & AGLOMERADO == 04])          #Tasa de desempleo Rosario              
  )

EPH2021Q2 <- EPH2021Q2hog %>%
  mutate(
    ANO4 = as.character(ANO4),
    TRIMESTRE = as.character(TRIMESTRE),
    Periodo = paste0(ANO4, "Q", TRIMESTRE),
    Regimen_tenencia = II7
  ) %>%
  filter(
    REALIZADA == 1
  ) %>%
  select(CODUSU, NRO_HOGAR, Periodo, Regimen_tenencia) %>%
  left_join(EPH2021Q2ind, by = c("CODUSU", "NRO_HOGAR")) %>%
  mutate(
    Ubicacion_trabajo = PP04G,                # Columna para ubicación de trabajo
    Informatizados = substring(as.character(PP04D_COD), 4, 4)
  ) %>%
  filter(ESTADO %in% c(1,2))%>%                     # Filtrar ocupados
  select(
    CODUSU, NRO_HOGAR, Periodo, PONDERA, AGLOMERADO,
    Regimen_tenencia, ESTADO, Ubicacion_trabajo, Informatizados
  ) %>%
  group_by(Periodo) %>%                       # Agrupación por período
  summarise(
    TD = sum(PONDERA[ESTADO == 2]),                        # Total de desocupados
    TO = sum(PONDERA[ESTADO == 1]),                        # Total de ocupados
    OV = sum(PONDERA[Ubicacion_trabajo == 6 & ESTADO == 1]),# Ocupados en viviendas
    OI = sum(PONDERA[Informatizados == 3 & ESTADO == 1]),   # Ocupados informatizados
    OR = sum(PONDERA[Ubicacion_trabajo == 6 & Informatizados == 3 & ESTADO == 1]), # Trabajadores remotos
    TOV = OV * 100 / TO,                      # Tasa de ocupación en viviendas
    TOI = OI * 100 / TO,                      # Tasa de ocupación informatizada
    TOR = OR * 100 / TO,                       # Tasa de ocupación remota
    Activos = sum(PONDERA[ESTADO %in% c(1, 2)]),            # PEA
    DesempleoBSAS = sum(PONDERA[ESTADO == 2 & AGLOMERADO %in% c(33, 32)]) / 
      sum(PONDERA[ESTADO %in% c(1, 2) & AGLOMERADO %in% c(33, 32)]), # Tasa de desempleo BsAs
    DesempleoCBA = sum(PONDERA[ESTADO == 2 & AGLOMERADO == 13]) / 
      sum(PONDERA[ESTADO %in% c(1, 2) & AGLOMERADO == 13]),            #Tasa de desmepleo Cordoba
    DesempleoRO = sum(PONDERA[ESTADO == 2 & AGLOMERADO == 04]) /    
      sum(PONDERA[ESTADO %in% c(1, 2) & AGLOMERADO == 04])          #Tasa de desempleo Rosario              
  )

EPH2021Q3 <- EPH2021Q3hog %>%
  mutate(
    ANO4 = as.character(ANO4),
    TRIMESTRE = as.character(TRIMESTRE),
    Periodo = paste0(ANO4, "Q", TRIMESTRE),
    Regimen_tenencia = II7
  ) %>%
  filter(
    REALIZADA == 1
  ) %>%
  select(CODUSU, NRO_HOGAR, Periodo, Regimen_tenencia) %>%
  left_join(EPH2021Q3ind, by = c("CODUSU", "NRO_HOGAR")) %>%
  mutate(
    Ubicacion_trabajo = PP04G,                # Columna para ubicación de trabajo
    Informatizados = substring(as.character(PP04D_COD), 4, 4)
  ) %>%
  filter(ESTADO %in% c(1,2))%>%                     # Filtrar ocupados
  select(
    CODUSU, NRO_HOGAR, Periodo, PONDERA, AGLOMERADO,
    Regimen_tenencia, ESTADO, Ubicacion_trabajo, Informatizados
  ) %>%
  group_by(Periodo) %>%                       # Agrupación por período
  summarise(
    TD = sum(PONDERA[ESTADO == 2]),                        # Total de desocupados
    TO = sum(PONDERA[ESTADO == 1]),                        # Total de ocupados
    OV = sum(PONDERA[Ubicacion_trabajo == 6 & ESTADO == 1]),# Ocupados en viviendas
    OI = sum(PONDERA[Informatizados == 3 & ESTADO == 1]),   # Ocupados informatizados
    OR = sum(PONDERA[Ubicacion_trabajo == 6 & Informatizados == 3 & ESTADO == 1]), # Trabajadores remotos
    TOV = OV * 100 / TO,                      # Tasa de ocupación en viviendas
    TOI = OI * 100 / TO,                      # Tasa de ocupación informatizada
    TOR = OR * 100 / TO,                       # Tasa de ocupación remota
    Activos = sum(PONDERA[ESTADO %in% c(1, 2)]),            # PEA
    DesempleoBSAS = sum(PONDERA[ESTADO == 2 & AGLOMERADO %in% c(33, 32)]) / 
      sum(PONDERA[ESTADO %in% c(1, 2) & AGLOMERADO %in% c(33, 32)]), # Tasa de desempleo BsAs
    DesempleoCBA = sum(PONDERA[ESTADO == 2 & AGLOMERADO == 13]) / 
      sum(PONDERA[ESTADO %in% c(1, 2) & AGLOMERADO == 13]),            #Tasa de desmepleo Cordoba
    DesempleoRO = sum(PONDERA[ESTADO == 2 & AGLOMERADO == 04]) /    
      sum(PONDERA[ESTADO %in% c(1, 2) & AGLOMERADO == 04])          #Tasa de desempleo Rosario              
  )

EPH2021Q4 <- EPH2021Q4hog %>%
  mutate(
    ANO4 = as.character(ANO4),
    TRIMESTRE = as.character(TRIMESTRE),
    Periodo = paste0(ANO4, "Q", TRIMESTRE),
    Regimen_tenencia = II7
  ) %>%
  filter(
    REALIZADA == 1
  ) %>%
  select(CODUSU, NRO_HOGAR, Periodo, Regimen_tenencia) %>%
  left_join(EPH2021Q4ind, by = c("CODUSU", "NRO_HOGAR")) %>%
  mutate(
    Ubicacion_trabajo = PP04G,                # Columna para ubicación de trabajo
    Informatizados = substring(as.character(PP04D_COD), 4, 4)
  ) %>%
  filter(ESTADO %in% c(1,2))%>%                     # Filtrar ocupados
  select(
    CODUSU, NRO_HOGAR, Periodo, PONDERA, AGLOMERADO,
    Regimen_tenencia, ESTADO, Ubicacion_trabajo, Informatizados
  ) %>%
  group_by(Periodo) %>%                       # Agrupación por período
  summarise(
    TD = sum(PONDERA[ESTADO == 2]),                        # Total de desocupados
    TO = sum(PONDERA[ESTADO == 1]),                        # Total de ocupados
    OV = sum(PONDERA[Ubicacion_trabajo == 6 & ESTADO == 1]),# Ocupados en viviendas
    OI = sum(PONDERA[Informatizados == 3 & ESTADO == 1]),   # Ocupados informatizados
    OR = sum(PONDERA[Ubicacion_trabajo == 6 & Informatizados == 3 & ESTADO == 1]), # Trabajadores remotos
    TOV = OV * 100 / TO,                      # Tasa de ocupación en viviendas
    TOI = OI * 100 / TO,                      # Tasa de ocupación informatizada
    TOR = OR * 100 / TO,                       # Tasa de ocupación remota
    Activos = sum(PONDERA[ESTADO %in% c(1, 2)]),            # PEA
    DesempleoBSAS = sum(PONDERA[ESTADO == 2 & AGLOMERADO %in% c(33, 32)]) / 
      sum(PONDERA[ESTADO %in% c(1, 2) & AGLOMERADO %in% c(33, 32)]), # Tasa de desempleo BsAs
    DesempleoCBA = sum(PONDERA[ESTADO == 2 & AGLOMERADO == 13]) / 
      sum(PONDERA[ESTADO %in% c(1, 2) & AGLOMERADO == 13]),            #Tasa de desmepleo Cordoba
    DesempleoRO = sum(PONDERA[ESTADO == 2 & AGLOMERADO == 04]) /    
      sum(PONDERA[ESTADO %in% c(1, 2) & AGLOMERADO == 04])          #Tasa de desempleo Rosario              
  )

EPH2022Q1 <- EPH2022Q1hog %>%
  mutate(
    ANO4 = as.character(ANO4),
    TRIMESTRE = as.character(TRIMESTRE),
    Periodo = paste0(ANO4, "Q", TRIMESTRE),
    Regimen_tenencia = II7
  ) %>%
  filter(
    REALIZADA == 1
  ) %>%
  select(CODUSU, NRO_HOGAR, Periodo, Regimen_tenencia) %>%
  left_join(EPH2022Q1ind, by = c("CODUSU", "NRO_HOGAR")) %>%
  mutate(
    Ubicacion_trabajo = PP04G,                # Columna para ubicación de trabajo
    Informatizados = substring(as.character(PP04D_COD), 4, 4)
  ) %>%
  filter(ESTADO %in% c(1,2))%>%                     # Filtrar ocupados
  select(
    CODUSU, NRO_HOGAR, Periodo, PONDERA, AGLOMERADO,
    Regimen_tenencia, ESTADO, Ubicacion_trabajo, Informatizados
  ) %>%
  group_by(Periodo) %>%                       # Agrupación por período
  summarise(
    TD = sum(PONDERA[ESTADO == 2]),                        # Total de desocupados
    TO = sum(PONDERA[ESTADO == 1]),                        # Total de ocupados
    OV = sum(PONDERA[Ubicacion_trabajo == 6 & ESTADO == 1]),# Ocupados en viviendas
    OI = sum(PONDERA[Informatizados == 3 & ESTADO == 1]),   # Ocupados informatizados
    OR = sum(PONDERA[Ubicacion_trabajo == 6 & Informatizados == 3 & ESTADO == 1]), # Trabajadores remotos
    TOV = OV * 100 / TO,                      # Tasa de ocupación en viviendas
    TOI = OI * 100 / TO,                      # Tasa de ocupación informatizada
    TOR = OR * 100 / TO,                       # Tasa de ocupación remota
    Activos = sum(PONDERA[ESTADO %in% c(1, 2)]),            # PEA
    DesempleoBSAS = sum(PONDERA[ESTADO == 2 & AGLOMERADO %in% c(33, 32)]) / 
      sum(PONDERA[ESTADO %in% c(1, 2) & AGLOMERADO %in% c(33, 32)]), # Tasa de desempleo BsAs
    DesempleoCBA = sum(PONDERA[ESTADO == 2 & AGLOMERADO == 13]) / 
      sum(PONDERA[ESTADO %in% c(1, 2) & AGLOMERADO == 13]),            #Tasa de desmepleo Cordoba
    DesempleoRO = sum(PONDERA[ESTADO == 2 & AGLOMERADO == 04]) /    
      sum(PONDERA[ESTADO %in% c(1, 2) & AGLOMERADO == 04])          #Tasa de desempleo Rosario              
  )

EPH2022Q2 <- EPH2022Q2hog %>%
  mutate(
    ANO4 = as.character(ANO4),
    TRIMESTRE = as.character(TRIMESTRE),
    Periodo = paste0(ANO4, "Q", TRIMESTRE),
    Regimen_tenencia = II7
  ) %>%
  filter(
    REALIZADA == 1
  ) %>%
  select(CODUSU, NRO_HOGAR, Periodo, Regimen_tenencia) %>%
  left_join(EPH2022Q2ind, by = c("CODUSU", "NRO_HOGAR")) %>%
  mutate(
    Ubicacion_trabajo = PP04G,                # Columna para ubicación de trabajo
    Informatizados = substring(as.character(PP04D_COD), 4, 4)
  ) %>%
  filter(ESTADO %in% c(1,2))%>%                     # Filtrar ocupados
  select(
    CODUSU, NRO_HOGAR, Periodo, PONDERA, AGLOMERADO,
    Regimen_tenencia, ESTADO, Ubicacion_trabajo, Informatizados
  ) %>%
  group_by(Periodo) %>%                       # Agrupación por período
  summarise(
    TD = sum(PONDERA[ESTADO == 2]),                        # Total de desocupados
    TO = sum(PONDERA[ESTADO == 1]),                        # Total de ocupados
    OV = sum(PONDERA[Ubicacion_trabajo == 6 & ESTADO == 1]),# Ocupados en viviendas
    OI = sum(PONDERA[Informatizados == 3 & ESTADO == 1]),   # Ocupados informatizados
    OR = sum(PONDERA[Ubicacion_trabajo == 6 & Informatizados == 3 & ESTADO == 1]), # Trabajadores remotos
    TOV = OV * 100 / TO,                      # Tasa de ocupación en viviendas
    TOI = OI * 100 / TO,                      # Tasa de ocupación informatizada
    TOR = OR * 100 / TO,                       # Tasa de ocupación remota
    Activos = sum(PONDERA[ESTADO %in% c(1, 2)]),            # PEA
    DesempleoBSAS = sum(PONDERA[ESTADO == 2 & AGLOMERADO %in% c(33, 32)]) / 
      sum(PONDERA[ESTADO %in% c(1, 2) & AGLOMERADO %in% c(33, 32)]), # Tasa de desempleo BsAs
    DesempleoCBA = sum(PONDERA[ESTADO == 2 & AGLOMERADO == 13]) / 
      sum(PONDERA[ESTADO %in% c(1, 2) & AGLOMERADO == 13]),            #Tasa de desmepleo Cordoba
    DesempleoRO = sum(PONDERA[ESTADO == 2 & AGLOMERADO == 04]) /    
      sum(PONDERA[ESTADO %in% c(1, 2) & AGLOMERADO == 04])          #Tasa de desempleo Rosario              
  )

EPH2022Q3 <- EPH2022Q3hog %>%
  mutate(
    ANO4 = as.character(ANO4),
    TRIMESTRE = as.character(TRIMESTRE),
    Periodo = paste0(ANO4, "Q", TRIMESTRE),
    Regimen_tenencia = II7
  ) %>%
  filter(
    REALIZADA == 1
  ) %>%
  select(CODUSU, NRO_HOGAR, Periodo, Regimen_tenencia) %>%
  left_join(EPH2022Q3ind, by = c("CODUSU", "NRO_HOGAR")) %>%
  mutate(
    Ubicacion_trabajo = PP04G,                # Columna para ubicación de trabajo
    Informatizados = substring(as.character(PP04D_COD), 4, 4)
  ) %>%
  filter(ESTADO %in% c(1,2))%>%                     # Filtrar ocupados
  select(
    CODUSU, NRO_HOGAR, Periodo, PONDERA, AGLOMERADO,
    Regimen_tenencia, ESTADO, Ubicacion_trabajo, Informatizados
  ) %>%
  group_by(Periodo) %>%                       # Agrupación por período
  summarise(
    TD = sum(PONDERA[ESTADO == 2]),                        # Total de desocupados
    TO = sum(PONDERA[ESTADO == 1]),                        # Total de ocupados
    OV = sum(PONDERA[Ubicacion_trabajo == 6 & ESTADO == 1]),# Ocupados en viviendas
    OI = sum(PONDERA[Informatizados == 3 & ESTADO == 1]),   # Ocupados informatizados
    OR = sum(PONDERA[Ubicacion_trabajo == 6 & Informatizados == 3 & ESTADO == 1]), # Trabajadores remotos
    TOV = OV * 100 / TO,                      # Tasa de ocupación en viviendas
    TOI = OI * 100 / TO,                      # Tasa de ocupación informatizada
    TOR = OR * 100 / TO,                       # Tasa de ocupación remota
    Activos = sum(PONDERA[ESTADO %in% c(1, 2)]),            # PEA
    DesempleoBSAS = sum(PONDERA[ESTADO == 2 & AGLOMERADO %in% c(33, 32)]) / 
      sum(PONDERA[ESTADO %in% c(1, 2) & AGLOMERADO %in% c(33, 32)]), # Tasa de desempleo BsAs
    DesempleoCBA = sum(PONDERA[ESTADO == 2 & AGLOMERADO == 13]) / 
      sum(PONDERA[ESTADO %in% c(1, 2) & AGLOMERADO == 13]),            #Tasa de desmepleo Cordoba
    DesempleoRO = sum(PONDERA[ESTADO == 2 & AGLOMERADO == 04]) /    
      sum(PONDERA[ESTADO %in% c(1, 2) & AGLOMERADO == 04])          #Tasa de desempleo Rosario              
  )

EPH2022Q4 <- EPH2022Q4hog %>%
  mutate(
    ANO4 = as.character(ANO4),
    TRIMESTRE = as.character(TRIMESTRE),
    Periodo = paste0(ANO4, "Q", TRIMESTRE),
    Regimen_tenencia = II7
  ) %>%
  filter(
    REALIZADA == 1
  ) %>%
  select(CODUSU, NRO_HOGAR, Periodo, Regimen_tenencia) %>%
  left_join(EPH2022Q4ind, by = c("CODUSU", "NRO_HOGAR")) %>%
  mutate(
    Ubicacion_trabajo = PP04G,                # Columna para ubicación de trabajo
    Informatizados = substring(as.character(PP04D_COD), 4, 4)
  ) %>%
  filter(ESTADO %in% c(1,2))%>%                     # Filtrar ocupados
  select(
    CODUSU, NRO_HOGAR, Periodo, PONDERA, AGLOMERADO,
    Regimen_tenencia, ESTADO, Ubicacion_trabajo, Informatizados
  ) %>%
  group_by(Periodo) %>%                       # Agrupación por período
  summarise(
    TD = sum(PONDERA[ESTADO == 2]),                        # Total de desocupados
    TO = sum(PONDERA[ESTADO == 1]),                        # Total de ocupados
    OV = sum(PONDERA[Ubicacion_trabajo == 6 & ESTADO == 1]),# Ocupados en viviendas
    OI = sum(PONDERA[Informatizados == 3 & ESTADO == 1]),   # Ocupados informatizados
    OR = sum(PONDERA[Ubicacion_trabajo == 6 & Informatizados == 3 & ESTADO == 1]), # Trabajadores remotos
    TOV = OV * 100 / TO,                      # Tasa de ocupación en viviendas
    TOI = OI * 100 / TO,                      # Tasa de ocupación informatizada
    TOR = OR * 100 / TO,                       # Tasa de ocupación remota
    Activos = sum(PONDERA[ESTADO %in% c(1, 2)]),            # PEA
    DesempleoBSAS = sum(PONDERA[ESTADO == 2 & AGLOMERADO %in% c(33, 32)]) / 
      sum(PONDERA[ESTADO %in% c(1, 2) & AGLOMERADO %in% c(33, 32)]), # Tasa de desempleo BsAs
    DesempleoCBA = sum(PONDERA[ESTADO == 2 & AGLOMERADO == 13]) / 
      sum(PONDERA[ESTADO %in% c(1, 2) & AGLOMERADO == 13]),            #Tasa de desmepleo Cordoba
    DesempleoRO = sum(PONDERA[ESTADO == 2 & AGLOMERADO == 04]) /    
      sum(PONDERA[ESTADO %in% c(1, 2) & AGLOMERADO == 04])          #Tasa de desempleo Rosario              
  )

EPH2023Q1 <- EPH2023Q1hog %>%
  mutate(
    ANO4 = as.character(ANO4),
    TRIMESTRE = as.character(TRIMESTRE),
    Periodo = paste0(ANO4, "Q", TRIMESTRE),
    Regimen_tenencia = II7
  ) %>%
  filter(
    REALIZADA == 1
  ) %>%
  select(CODUSU, NRO_HOGAR, Periodo, Regimen_tenencia) %>%
  left_join(EPH2023Q1ind, by = c("CODUSU", "NRO_HOGAR")) %>%
  mutate(
    Ubicacion_trabajo = PP04G,                # Columna para ubicación de trabajo
    Informatizados = substring(as.character(PP04D_COD), 4, 4)
  ) %>%
  filter(ESTADO %in% c(1,2))%>%                     # Filtrar ocupados
  select(
    CODUSU, NRO_HOGAR, Periodo, PONDERA, AGLOMERADO,
    Regimen_tenencia, ESTADO, Ubicacion_trabajo, Informatizados
  ) %>%
  group_by(Periodo) %>%                       # Agrupación por período
  summarise(
    TD = sum(PONDERA[ESTADO == 2]),                        # Total de desocupados
    TO = sum(PONDERA[ESTADO == 1]),                        # Total de ocupados
    OV = sum(PONDERA[Ubicacion_trabajo == 6 & ESTADO == 1]),# Ocupados en viviendas
    OI = sum(PONDERA[Informatizados == 3 & ESTADO == 1]),   # Ocupados informatizados
    OR = sum(PONDERA[Ubicacion_trabajo == 6 & Informatizados == 3 & ESTADO == 1]), # Trabajadores remotos
    TOV = OV * 100 / TO,                      # Tasa de ocupación en viviendas
    TOI = OI * 100 / TO,                      # Tasa de ocupación informatizada
    TOR = OR * 100 / TO,                       # Tasa de ocupación remota
    Activos = sum(PONDERA[ESTADO %in% c(1, 2)]),            # PEA
    DesempleoBSAS = sum(PONDERA[ESTADO == 2 & AGLOMERADO %in% c(33, 32)]) / 
      sum(PONDERA[ESTADO %in% c(1, 2) & AGLOMERADO %in% c(33, 32)]), # Tasa de desempleo BsAs
    DesempleoCBA = sum(PONDERA[ESTADO == 2 & AGLOMERADO == 13]) / 
      sum(PONDERA[ESTADO %in% c(1, 2) & AGLOMERADO == 13]),            #Tasa de desmepleo Cordoba
    DesempleoRO = sum(PONDERA[ESTADO == 2 & AGLOMERADO == 04]) /    
      sum(PONDERA[ESTADO %in% c(1, 2) & AGLOMERADO == 04])          #Tasa de desempleo Rosario              
  )

EPH2023Q2 <- EPH2023Q2hog %>%
  mutate(
    ANO4 = as.character(ANO4),
    TRIMESTRE = as.character(TRIMESTRE),
    Periodo = paste0(ANO4, "Q", TRIMESTRE),
    Regimen_tenencia = II7
  ) %>%
  filter(
    REALIZADA == 1
  ) %>%
  select(CODUSU, NRO_HOGAR, Periodo, Regimen_tenencia) %>%
  left_join(EPH2023Q2ind, by = c("CODUSU", "NRO_HOGAR")) %>%
  mutate(
    Ubicacion_trabajo = PP04G,                # Columna para ubicación de trabajo
    Informatizados = substring(as.character(PP04D_COD), 4, 4)
  ) %>%
  filter(ESTADO %in% c(1,2))%>%                     # Filtrar ocupados
  select(
    CODUSU, NRO_HOGAR, Periodo, PONDERA, AGLOMERADO,
    Regimen_tenencia, ESTADO, Ubicacion_trabajo, Informatizados
  ) %>%
  group_by(Periodo) %>%                       # Agrupación por período
  summarise(
    TD = sum(PONDERA[ESTADO == 2]),                        # Total de desocupados
    TO = sum(PONDERA[ESTADO == 1]),                        # Total de ocupados
    OV = sum(PONDERA[Ubicacion_trabajo == 6 & ESTADO == 1]),# Ocupados en viviendas
    OI = sum(PONDERA[Informatizados == 3 & ESTADO == 1]),   # Ocupados informatizados
    OR = sum(PONDERA[Ubicacion_trabajo == 6 & Informatizados == 3 & ESTADO == 1]), # Trabajadores remotos
    TOV = OV * 100 / TO,                      # Tasa de ocupación en viviendas
    TOI = OI * 100 / TO,                      # Tasa de ocupación informatizada
    TOR = OR * 100 / TO,                       # Tasa de ocupación remota
    Activos = sum(PONDERA[ESTADO %in% c(1, 2)]),            # PEA
    DesempleoBSAS = sum(PONDERA[ESTADO == 2 & AGLOMERADO %in% c(33, 32)]) / 
      sum(PONDERA[ESTADO %in% c(1, 2) & AGLOMERADO %in% c(33, 32)]), # Tasa de desempleo BsAs
    DesempleoCBA = sum(PONDERA[ESTADO == 2 & AGLOMERADO == 13]) / 
      sum(PONDERA[ESTADO %in% c(1, 2) & AGLOMERADO == 13]),            #Tasa de desmepleo Cordoba
    DesempleoRO = sum(PONDERA[ESTADO == 2 & AGLOMERADO == 04]) /    
      sum(PONDERA[ESTADO %in% c(1, 2) & AGLOMERADO == 04])          #Tasa de desempleo Rosario              
  )

EPH2023Q3 <- EPH2023Q3hog %>%
  mutate(
    ANO4 = as.character(ANO4),
    TRIMESTRE = as.character(TRIMESTRE),
    Periodo = paste0(ANO4, "Q", TRIMESTRE),
    Regimen_tenencia = II7
  ) %>%
  filter(
    REALIZADA == 1
  ) %>%
  select(CODUSU, NRO_HOGAR, Periodo, Regimen_tenencia) %>%
  left_join(EPH2023Q3ind, by = c("CODUSU", "NRO_HOGAR")) %>%
  mutate(
    Ubicacion_trabajo = PP04G,                # Columna para ubicación de trabajo
    Informatizados = substring(as.character(PP04D_COD), 4, 4)
  ) %>%
  filter(ESTADO %in% c(1,2))%>%                     # Filtrar ocupados
  select(
    CODUSU, NRO_HOGAR, Periodo, PONDERA, AGLOMERADO,
    Regimen_tenencia, ESTADO, Ubicacion_trabajo, Informatizados
  ) %>%
  group_by(Periodo) %>%                       # Agrupación por período
  summarise(
    TD = sum(PONDERA[ESTADO == 2]),                        # Total de desocupados
    TO = sum(PONDERA[ESTADO == 1]),                        # Total de ocupados
    OV = sum(PONDERA[Ubicacion_trabajo == 6 & ESTADO == 1]),# Ocupados en viviendas
    OI = sum(PONDERA[Informatizados == 3 & ESTADO == 1]),   # Ocupados informatizados
    OR = sum(PONDERA[Ubicacion_trabajo == 6 & Informatizados == 3 & ESTADO == 1]), # Trabajadores remotos
    TOV = OV * 100 / TO,                      # Tasa de ocupación en viviendas
    TOI = OI * 100 / TO,                      # Tasa de ocupación informatizada
    TOR = OR * 100 / TO,                       # Tasa de ocupación remota
    Activos = sum(PONDERA[ESTADO %in% c(1, 2)]),            # PEA
    DesempleoBSAS = sum(PONDERA[ESTADO == 2 & AGLOMERADO %in% c(33, 32)]) / 
      sum(PONDERA[ESTADO %in% c(1, 2) & AGLOMERADO %in% c(33, 32)]), # Tasa de desempleo BsAs
    DesempleoCBA = sum(PONDERA[ESTADO == 2 & AGLOMERADO == 13]) / 
      sum(PONDERA[ESTADO %in% c(1, 2) & AGLOMERADO == 13]),            #Tasa de desmepleo Cordoba
    DesempleoRO = sum(PONDERA[ESTADO == 2 & AGLOMERADO == 04]) /    
      sum(PONDERA[ESTADO %in% c(1, 2) & AGLOMERADO == 04])          #Tasa de desempleo Rosario              
  )

EPH2023Q <- EPH2023Q3hog %>%
  mutate(
    ANO4 = as.character(ANO4),
    TRIMESTRE = as.character(TRIMESTRE),
    Periodo = paste0(ANO4, "Q", TRIMESTRE),
    Regimen_tenencia = II7
  ) %>%
  filter(
    REALIZADA == 1
  ) %>%
  select(CODUSU, NRO_HOGAR, Periodo, Regimen_tenencia) %>%
  left_join(EPH2023Q3ind, by = c("CODUSU", "NRO_HOGAR")) %>%
  mutate(
    Ubicacion_trabajo = PP04G,                # Columna para ubicación de trabajo
    Informatizados = substring(as.character(PP04D_COD), 4, 4)
  ) %>%
  filter(ESTADO %in% c(1,2))%>%                     # Filtrar ocupados
  select(
    CODUSU, NRO_HOGAR, Periodo, PONDERA, AGLOMERADO,
    Regimen_tenencia, ESTADO, Ubicacion_trabajo, Informatizados
  ) %>%
  group_by(Periodo) %>%                       # Agrupación por período
  summarise(
    TD = sum(PONDERA[ESTADO == 2]),                        # Total de desocupados
    TO = sum(PONDERA[ESTADO == 1]),                        # Total de ocupados
    OV = sum(PONDERA[Ubicacion_trabajo == 6 & ESTADO == 1]),# Ocupados en viviendas
    OI = sum(PONDERA[Informatizados == 3 & ESTADO == 1]),   # Ocupados informatizados
    OR = sum(PONDERA[Ubicacion_trabajo == 6 & Informatizados == 3 & ESTADO == 1]), # Trabajadores remotos
    TOV = OV * 100 / TO,                      # Tasa de ocupación en viviendas
    TOI = OI * 100 / TO,                      # Tasa de ocupación informatizada
    TOR = OR * 100 / TO,                       # Tasa de ocupación remota
    Activos = sum(PONDERA[ESTADO %in% c(1, 2)]),            # PEA
    DesempleoBSAS = sum(PONDERA[ESTADO == 2 & AGLOMERADO %in% c(33, 32)]) / 
      sum(PONDERA[ESTADO %in% c(1, 2) & AGLOMERADO %in% c(33, 32)]), # Tasa de desempleo BsAs
    DesempleoCBA = sum(PONDERA[ESTADO == 2 & AGLOMERADO == 13]) / 
      sum(PONDERA[ESTADO %in% c(1, 2) & AGLOMERADO == 13]),            #Tasa de desmepleo Cordoba
    DesempleoRO = sum(PONDERA[ESTADO == 2 & AGLOMERADO == 04]) /    
      sum(PONDERA[ESTADO %in% c(1, 2) & AGLOMERADO == 04])          #Tasa de desempleo Rosario              
  )

EPH2023Q4 <- EPH2023Q4hog %>%
  mutate(
    ANO4 = as.character(ANO4),
    TRIMESTRE = as.character(TRIMESTRE),
    Periodo = paste0(ANO4, "Q", TRIMESTRE),
    Regimen_tenencia = II7
  ) %>%
  filter(
    REALIZADA == 1
  ) %>%
  select(CODUSU, NRO_HOGAR, Periodo, Regimen_tenencia) %>%
  left_join(EPH2023Q4ind, by = c("CODUSU", "NRO_HOGAR")) %>%
  mutate(
    Ubicacion_trabajo = PP04G,                # Columna para ubicación de trabajo
    Informatizados = substring(as.character(PP04D_COD), 4, 4)
  ) %>%
  filter(ESTADO %in% c(1,2))%>%                     # Filtrar ocupados
  select(
    CODUSU, NRO_HOGAR, Periodo, PONDERA, AGLOMERADO,
    Regimen_tenencia, ESTADO, Ubicacion_trabajo, Informatizados
  ) %>%
  group_by(Periodo) %>%                       # Agrupación por período
  summarise(
    TD = sum(PONDERA[ESTADO == 2]),                        # Total de desocupados
    TO = sum(PONDERA[ESTADO == 1]),                        # Total de ocupados
    OV = sum(PONDERA[Ubicacion_trabajo == 6 & ESTADO == 1]),# Ocupados en viviendas
    OI = sum(PONDERA[Informatizados == 3 & ESTADO == 1]),   # Ocupados informatizados
    OR = sum(PONDERA[Ubicacion_trabajo == 6 & Informatizados == 3 & ESTADO == 1]), # Trabajadores remotos
    TOV = OV * 100 / TO,                      # Tasa de ocupación en viviendas
    TOI = OI * 100 / TO,                      # Tasa de ocupación informatizada
    TOR = OR * 100 / TO,                       # Tasa de ocupación remota
    Activos = sum(PONDERA[ESTADO %in% c(1, 2)]),            # PEA
    DesempleoBSAS = sum(PONDERA[ESTADO == 2 & AGLOMERADO %in% c(33, 32)]) / 
      sum(PONDERA[ESTADO %in% c(1, 2) & AGLOMERADO %in% c(33, 32)]), # Tasa de desempleo BsAs
    DesempleoCBA = sum(PONDERA[ESTADO == 2 & AGLOMERADO == 13]) / 
      sum(PONDERA[ESTADO %in% c(1, 2) & AGLOMERADO == 13]),            #Tasa de desmepleo Cordoba
    DesempleoRO = sum(PONDERA[ESTADO == 2 & AGLOMERADO == 04]) /    
      sum(PONDERA[ESTADO %in% c(1, 2) & AGLOMERADO == 04])          #Tasa de desempleo Rosario              
  )

EPH2024Q1 <- EPH2024Q1hog %>%
  mutate(
    ANO4 = as.character(ANO4),
    TRIMESTRE = as.character(TRIMESTRE),
    Periodo = paste0(ANO4, "Q", TRIMESTRE),
    Regimen_tenencia = II7
  ) %>%
  filter(
    REALIZADA == 1
  ) %>%
  select(CODUSU, NRO_HOGAR, Periodo, Regimen_tenencia) %>%
  left_join(EPH2024Q1ind, by = c("CODUSU", "NRO_HOGAR")) %>%
  mutate(
    Ubicacion_trabajo = PP04G,                # Columna para ubicación de trabajo
    Informatizados = substring(as.character(PP04D_COD), 4, 4)
  ) %>%
  filter(ESTADO %in% c(1,2))%>%                     # Filtrar ocupados
  select(
    CODUSU, NRO_HOGAR, Periodo, PONDERA, AGLOMERADO,
    Regimen_tenencia, ESTADO, Ubicacion_trabajo, Informatizados
  ) %>%
  group_by(Periodo) %>%                       # Agrupación por período
  summarise(
    TD = sum(PONDERA[ESTADO == 2]),                        # Total de desocupados
    TO = sum(PONDERA[ESTADO == 1]),                        # Total de ocupados
    OV = sum(PONDERA[Ubicacion_trabajo == 6 & ESTADO == 1]),# Ocupados en viviendas
    OI = sum(PONDERA[Informatizados == 3 & ESTADO == 1]),   # Ocupados informatizados
    OR = sum(PONDERA[Ubicacion_trabajo == 6 & Informatizados == 3 & ESTADO == 1]), # Trabajadores remotos
    TOV = OV * 100 / TO,                      # Tasa de ocupación en viviendas
    TOI = OI * 100 / TO,                      # Tasa de ocupación informatizada
    TOR = OR * 100 / TO,                       # Tasa de ocupación remota
    Activos = sum(PONDERA[ESTADO %in% c(1, 2)]),            # PEA
    DesempleoBSAS = sum(PONDERA[ESTADO == 2 & AGLOMERADO %in% c(33, 32)]) / 
      sum(PONDERA[ESTADO %in% c(1, 2) & AGLOMERADO %in% c(33, 32)]), # Tasa de desempleo BsAs
    DesempleoCBA = sum(PONDERA[ESTADO == 2 & AGLOMERADO == 13]) / 
      sum(PONDERA[ESTADO %in% c(1, 2) & AGLOMERADO == 13]),            #Tasa de desmepleo Cordoba
    DesempleoRO = sum(PONDERA[ESTADO == 2 & AGLOMERADO == 04]) /    
      sum(PONDERA[ESTADO %in% c(1, 2) & AGLOMERADO == 04])          #Tasa de desempleo Rosario              
  )

EPH2024Q2 <- EPH2024Q2hog %>%
  mutate(
    ANO4 = as.character(ANO4),
    TRIMESTRE = as.character(TRIMESTRE),
    Periodo = paste0(ANO4, "Q", TRIMESTRE),
    Regimen_tenencia = II7
  ) %>%
  filter(
    REALIZADA == 1
  ) %>%
  select(CODUSU, NRO_HOGAR, Periodo, Regimen_tenencia) %>%
  left_join(EPH2024Q2ind, by = c("CODUSU", "NRO_HOGAR")) %>%
  mutate(
    Ubicacion_trabajo = PP04G,                # Columna para ubicación de trabajo
    Informatizados = substring(as.character(PP04D_COD), 4, 4)
  ) %>%
  filter(ESTADO %in% c(1,2))%>%                     # Filtrar ocupados
  select(
    CODUSU, NRO_HOGAR, Periodo, PONDERA, AGLOMERADO,
    Regimen_tenencia, ESTADO, Ubicacion_trabajo, Informatizados
  ) %>%
  group_by(Periodo) %>%                       # Agrupación por período
  summarise(
    TD = sum(PONDERA[ESTADO == 2]),                        # Total de desocupados
    TO = sum(PONDERA[ESTADO == 1]),                        # Total de ocupados
    OV = sum(PONDERA[Ubicacion_trabajo == 6 & ESTADO == 1]),# Ocupados en viviendas
    OI = sum(PONDERA[Informatizados == 3 & ESTADO == 1]),   # Ocupados informatizados
    OR = sum(PONDERA[Ubicacion_trabajo == 6 & Informatizados == 3 & ESTADO == 1]), # Trabajadores remotos
    TOV = OV * 100 / TO,                      # Tasa de ocupación en viviendas
    TOI = OI * 100 / TO,                      # Tasa de ocupación informatizada
    TOR = OR * 100 / TO,                       # Tasa de ocupación remota
    Activos = sum(PONDERA[ESTADO %in% c(1, 2)]),            # PEA
    DesempleoBSAS = sum(PONDERA[ESTADO == 2 & AGLOMERADO %in% c(33, 32)]) / 
      sum(PONDERA[ESTADO %in% c(1, 2) & AGLOMERADO %in% c(33, 32)]), # Tasa de desempleo BsAs
    DesempleoCBA = sum(PONDERA[ESTADO == 2 & AGLOMERADO == 13]) / 
      sum(PONDERA[ESTADO %in% c(1, 2) & AGLOMERADO == 13]),            #Tasa de desmepleo Cordoba
    DesempleoRO = sum(PONDERA[ESTADO == 2 & AGLOMERADO == 04]) /    
      sum(PONDERA[ESTADO %in% c(1, 2) & AGLOMERADO == 04])          #Tasa de desempleo Rosario              
  )

Variables <- EPH2016Q4 %>% 
  full_join(EPH2017Q1) %>% 
  full_join(EPH2017Q2) %>% 
  full_join(EPH2017Q3) %>% 
  full_join(EPH2017Q4) %>% 
  full_join(EPH2018Q1) %>% 
  full_join(EPH2018Q2) %>% 
  full_join(EPH2018Q3) %>%   
  full_join(EPH2018Q4) %>% 
  full_join(EPH2019Q1) %>% 
  full_join(EPH2019Q2) %>% 
  full_join(EPH2019Q3) %>% 
  full_join(EPH2019Q4) %>% 
  full_join(EPH2020Q1) %>% 
  full_join(EPH2020Q2) %>% 
  full_join(EPH2020Q3) %>% 
  full_join(EPH2020Q4) %>% 
  full_join(EPH2021Q1) %>% 
  full_join(EPH2021Q2) %>% 
  full_join(EPH2021Q3) %>% 
  full_join(EPH2021Q4) %>% 
  full_join(EPH2022Q1) %>% 
  full_join(EPH2022Q2) %>%
  full_join(EPH2022Q3) %>% 
  full_join(EPH2022Q4) %>% 
  full_join(EPH2023Q1) %>% 
  full_join(EPH2023Q2) %>% 
  full_join(EPH2023Q3) %>% 
  full_join(EPH2023Q4) %>% 
  select(Periodo, TOR, TOI, TOV, OV, OI, OR, TO, DesempleoBSAS, DesempleoCBA, DesempleoRO)


Variables_independientes <- Variables_independientes %>% 
  full_join(Variables, by="Periodo")


