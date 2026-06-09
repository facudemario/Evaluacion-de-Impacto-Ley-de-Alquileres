library(tidyverse)
library(readr)
library(dplyr)
library(igraph)
library(readxl)
library(ggplot2)
library(Synth)
library(readxl)
library(openxlsx)
library(zoo)
library(CausalImpact)
library(seasonal)


#Datos de Variables independientes
setwd("G:\\Mi unidad\\Carpeta sin título\\Mis Archivos\\Facundo\\Tesis ley de alquileres\\Datos\\Datos Mensual\\Independientes")
 ActividadCba <-read_excel("serie-informe-de-coyuntura.xlsx")
 percapita <- read_csv2("percapita.csv")
 nose <- read_excel("Exchange rate, new LCU per USD extended backward, period average.xlsx") 
 Exports <- read_excel("Exports Merchandise, Customs, constant 2010 US$, millions, seas. adj..xlsx") 
 Imports <- read_excel("Imports Merchandise, Customs, constant 2010 US$, millions, seas. adj..xlsx") 
 TCN <- read_excel("Nominal Effective Exchange Rate.xlsx") 
 TCN_Oficial <- read_excel("Official exchange rate, LCU per USD, period average.xlsx") 
 TCR <- read_excel("Real Effective Exchange Rate.xlsx") 
 Stocks <- read_excel("Stock Markets, US$.xlsx") 
 ToT <- read_excel("Terms of Trade.xlsx") 
 Unemployment <- read_excel("Unemployment Rate, seas. adj..xlsx") 
 Trafico <- read_excel("Actividad MELI.xlsx") 
 EMAE <- read_excel("sh_emae_mensual_base2004.xls") 
 Inflacion <- read_excel("Inflacion.xlsx")
 RIPTE <- read_csv2("RIPTE.csv")
 FormalEmpleo <- read_excel("Empleo formal.xlsx")
 ProxyConsumo <- read_excel("SupermercadoIndec.xlsx")
 Badlar <- read_excel("BADLAR.xlsx")
 Plazofijo <- read_excel("Plazofijo.xlsx")
 ICC <- read_excel("ICC.xlsx")
 Dolares <- read_excel("Dolares.xlsx")

 
#Mensual
Años <- c(2017, 2018,2019,2020,2021,2022,2023,2024,2025)

patron <- paste0(Años, collapse="|")

ICC <- ICC %>% 
  filter(grepl(patron, Periodo)) %>% 
  select(Periodo,`Var CBA`,VariacionGBA)

FormalEmpleo <- FormalEmpleo %>% 
  mutate(Periodo=Período) %>% 
  filter(grepl(patron, Periodo)) 
  

RIPTE <- RIPTE %>% 
  filter(grepl(patron, indice_tiempo)) %>%
  mutate(Periodo=indice_tiempo) 

percapita <- percapita %>% 
  filter(grepl(patron, Periodo)) %>%
  mutate(percapita = `pib_precios_2004 (dividido por poblacion)`) %>% 
  select(Periodo, percapita) 

EMAE <- EMAE %>%
  filter(grepl(patron, Periodo)) %>%
  mutate(EMAE = `Variacion % GBA`) %>% 
  mutate(IMAE = `Variacion % Sta Fe`) %>% 
  mutate(ICAE = `Variacion % Cba`) %>% 
  select(Periodo, EMAE, IMAE, ICAE) 

Exports <- Exports %>% 
  filter(grepl(patron, Periodo))%>% 
  mutate("Exports"=Argentina) %>% 
  select(Periodo, Exports)

Imports <- Imports %>% 
  select(Periodo, Argentina) %>% 
  filter(grepl(patron, Periodo))%>% 
  mutate("Imports"=Argentina) %>%
  select(Periodo, Imports)

Stocks <- Stocks %>% 
  select(Periodo, Argentina) %>% 
  filter(grepl(patron, Periodo))%>% 
  mutate("Stocks"=Argentina) %>%  
  select(Periodo, Stocks)

TCN <- TCN %>% 
  select(Periodo, Argentina) %>% 
  filter(grepl(patron, Periodo))%>% 
  mutate("TCN"=Argentina) %>%    
  select(Periodo, TCN)

TCN_Oficial <- TCN_Oficial %>% 
  select(Periodo, Argentina) %>% 
  filter(grepl(patron, Periodo))%>% 
  mutate("TCN_Oficial"=Argentina) %>%    
  select(Periodo, TCN_Oficial)

TCR <- TCR %>% 
  select(Periodo, Argentina) %>% 
  filter(grepl(patron, Periodo))%>% 
  mutate("TCR"=Argentina) %>%    
  select(Periodo, TCR)

ToT <- ToT %>% 
  select(Periodo, Argentina) %>% 
  filter(grepl(patron, Periodo))%>% 
  mutate("ToT"=Argentina) %>%    
  select(Periodo, ToT)

Unemployment <- Unemployment %>% 
  select(Periodo, Argentina) %>% 
  filter(grepl(patron, Periodo))%>% 
  mutate("Unemployment"=Argentina) %>%    
  select(Periodo, Unemployment)

nose <- nose %>% 
  select(Periodo, Argentina) %>% 
  filter(grepl(patron, Periodo))%>% 
  mutate("nose"=Argentina) %>%    
  select(Periodo, nose)

ProxyConsumo <- ProxyConsumo %>% 
  filter(grepl(patron, Periodo))
  
  
Inflacion <- Inflacion %>% 
    filter(grepl(patron, Periodo)) 




#Variables dependientes
setwd("G:\\Mi unidad\\Carpeta sin título\\Mis Archivos\\Facundo\\Tesis ley de alquileres\\Datos\\Datos Mensual\\Dependientes")
Oferta_AlqBSAS <- read_excel("alquileresagrupacion_202402.xlsx") 
Oferta_AlqInterior <- read_excel("alquileresmunicipios_202412_interior.xlsx") 
Precio_AlqBSAS <- read_excel("alquileresagrupacion_202511.xlsx") 
Precio_AlqInterior <- read_excel("alquileresmunicipios_202511_interior.xlsx") 

Oferta_AlqInterior <- Oferta_AlqInterior %>%
  mutate(Mes = gsub("-", "M", Mes))
Precio_AlqInterior <- Precio_AlqInterior %>%
  mutate(Mes = gsub("-", "M", Mes))

# Precios

Precio_AMBA_Departamento <- Precio_AlqBSAS %>% 
  filter(Inmueble=="Departamento") %>%
  filter(Aglomerado=="AMBA") %>% 
  mutate(P_ambadpto = Mediana.por.m2.a.precios.constantes) %>% 
  mutate(Mes = sub("-", "M",Mes)) %>% 
  select(Mes, P_ambadpto)
  
Precio_AMBA_Casa <- Precio_AlqBSAS %>% 
  filter(Inmueble=="Casa") %>%
  filter(Aglomerado=="AMBA") %>% 
  mutate(P_ambacasa = Mediana.por.m2.a.precios.constantes) %>% 
  mutate(Mes = sub("-", "M",Mes)) %>% 
  select(Mes, P_ambacasa) 

Precio_CABA_Departamento <- Precio_AlqBSAS %>%
  filter(Aglomerado=="CABA") %>% 
  filter(Inmueble=="Departamento") %>% 
   mutate(P_cabadpto = Mediana.por.m2.a.precios.constantes) %>% 
   mutate(Mes = sub("-", "M",Mes)) %>% 
   select(Mes, P_cabadpto) 

Precio_CABA_Casa <- Precio_AlqBSAS %>% 
  filter(Aglomerado=="CABA") %>% 
  filter(Inmueble=="Casa") %>% 
  mutate(P_cabacasa = Mediana.por.m2.a.precios.constantes) %>% 
  mutate(Mes = sub("-", "M",Mes)) %>% 
  select(Mes, P_cabacasa)
  

#Interior del pais


Precio_cordoba_casa <- Precio_AlqInterior %>% 
  filter(Municipio=="Cordoba") %>% 
  filter(Inmueble=="Casa") %>% 
  mutate(P_cbacasa = Mediana.por.m2.a.precios.constantes) %>% 
  select(Mes, P_cbacasa)

Precio_cordoba_dpto  <- Precio_AlqInterior %>% 
  filter(Municipio=="Cordoba")%>% 
  filter(Inmueble=="Departamento") %>% 
  mutate(P_cbadpto = Mediana.por.m2.a.precios.constantes) %>% 
  select(Mes, P_cbadpto)

Precio_rosario_casa  <- Precio_AlqInterior %>% 
  filter(Municipio=="Rosario")%>% 
  filter(Inmueble=="Casa") %>% 
  mutate(P_roscasa = Mediana.por.m2.a.precios.constantes) %>% 
  select(Mes, P_roscasa)

Precio_rosario_dpto  <- Precio_AlqInterior %>% 
  filter(Municipio=="Rosario")%>% 
  filter(Inmueble=="Departamento") %>% 
  mutate(P_rosdpto = Mediana.por.m2.a.precios.constantes) %>% 
  select(Mes, P_rosdpto)



# Oferta

#AMBA Y CABA

Oferta_AMBA_Departamento <- Oferta_AlqBSAS %>% 
  filter(Inmueble=="Departamento") %>%
  filter(Aglomerado=="AMBA") %>% 
  mutate(Q_ambadpto = Oferta) %>% 
  mutate(Mes = sub("-", "M",Mes)) %>% 
  select(Mes, Q_ambadpto)
  
Oferta_AMBA_Casa <- Oferta_AlqBSAS %>% 
  filter(Inmueble=="Casa") %>%
  filter(Aglomerado=="AMBA") %>% 
  mutate(Q_ambacasa = Oferta) %>% 
  mutate(Mes = sub("-", "M",Mes)) %>% 
  select(Mes, Q_ambacasa) 

Oferta_CABA_Departamento <- Oferta_AlqBSAS %>%
  filter(Aglomerado=="CABA") %>% 
  filter(Inmueble=="Departamento") %>% 
   mutate(Q_cabadto = Oferta) %>% 
   mutate(Mes = sub("-", "M",Mes)) %>% 
   select(Mes, Q_cabadto) 

 Oferta_CABA_Casa <- Oferta_AlqBSAS %>% 
  filter(Aglomerado=="CABA") %>% 
  filter(Inmueble=="Casa") %>% 
  mutate(Q_cabacasa = Oferta) %>% 
  mutate(Mes = sub("-", "M",Mes)) %>% 
  select(Mes, Q_cabacasa)
  

#Interior del pais



Oferta_cordoba_casa <- Oferta_AlqInterior %>% 
  filter(Municipio=="Cordoba") %>% 
  filter(Inmueble=="Casa") %>% 
  mutate(Q_cbacasa = Oferta) %>% 
  select(Mes, Q_cbacasa)

Oferta_cordoba_dpto  <- Oferta_AlqInterior %>% 
  filter(Municipio=="Cordoba")%>% 
  filter(Inmueble=="Departamento") %>% 
  mutate(Q_cbadpto = Oferta) %>% 
  select(Mes, Q_cbadpto)

Oferta_rosario_casa  <- Oferta_AlqInterior %>% 
  filter(Municipio=="Rosario")%>% 
  filter(Inmueble=="Casa") %>% 
  mutate(Q_roscasa = Oferta) %>% 
  select(Mes, Q_roscasa)

Oferta_rosario_dpto  <- Oferta_AlqInterior %>% 
  filter(Municipio=="Rosario")%>% 
  filter(Inmueble=="Departamento") %>% 
  mutate(Q_rosdpto = Oferta) %>% 
  select(Mes, Q_rosdpto)





#Pasamos a un dataframe, convertimos todo a numerico y luegotime series

Independientes <- EMAE %>% 
  left_join(Stocks)%>% 
  left_join(TCN) %>% 
  left_join(TCN_Oficial) %>% 
  left_join(TCR, by ="Periodo")%>% 
  left_join(ToT)%>% 
  left_join(Unemployment)%>% 
  left_join(nose)%>% 
  left_join(Inflacion) %>% 
  left_join(Trafico) %>% 
  left_join(percapita) %>% 
  left_join(ProxyConsumo) %>%
  left_join(FormalEmpleo) %>%
  left_join(RIPTE) %>%
  left_join(Badlar) %>% 
  left_join(Plazofijo) %>% 
  left_join(ICC) %>% 
  left_join(Dolares) %>% 
  select(-...6,-...7,-...8,-...9,-...11) %>% 
  filter(!grepl("2017",Periodo)) %>% 
  mutate(across(where(\(x) is.character(x) && all(grepl("^-?[0-9\\.]+$", x) | x == "" | is.na(x))), 
                \(x) as.numeric(x)))
  #mutate(across(where(is.numeric), scale)) #esto mejora la performance del modelo


Dependientes <-  Precio_cordoba_casa %>% 
  left_join(Oferta_AMBA_Departamento) %>%
  left_join(Oferta_AMBA_Casa) %>% 
  left_join(Oferta_CABA_Departamento) %>% 
  left_join(Oferta_CABA_Casa) %>% 
  left_join(Oferta_cordoba_dpto) %>% 
  left_join(Oferta_rosario_casa) %>% 
  left_join(Oferta_rosario_dpto) %>% 
  left_join(Precio_AMBA_Departamento) %>%
  left_join(Precio_AMBA_Casa) %>% 
  left_join(Precio_CABA_Departamento) %>% 
  left_join(Precio_CABA_Casa) %>% 
  left_join(Oferta_cordoba_casa) %>% 
  left_join(Precio_cordoba_dpto) %>% 
  left_join(Precio_rosario_casa) %>% 
  left_join(Precio_rosario_dpto) %>% 
  arrange(Mes)
  

Independientes_IPC <- EMAE %>% 
  left_join(Stocks, by = "Periodo") %>% 
  left_join(TCN, by = "Periodo") %>% 
  left_join(TCN_Oficial, by = "Periodo") %>% 
  left_join(TCR, by = "Periodo") %>% 
  left_join(ToT, by = "Periodo") %>% 
  left_join(Unemployment, by = "Periodo") %>% 
  left_join(nose, by = "Periodo") %>% 
  left_join(Inflacion, by = "Periodo") %>% 
  left_join(Trafico, by = "Periodo") %>% 
  left_join(percapita, by = "Periodo") %>% 
  left_join(ProxyConsumo, by = "Periodo") %>% 
  left_join(FormalEmpleo, by = "Periodo") %>% 
  left_join(RIPTE, by = "Periodo") %>% 
  left_join(Badlar, by = "Periodo") %>% 
  left_join(Plazofijo, by = "Periodo") %>% 
  left_join(ICC, by = "Periodo") %>% 
  left_join(Dolares, by = "Periodo") %>% 
  select(-...6, -...7, -...8, -...9, -...11)
  
  
  #filter(!grepl("2025", Periodo))
#Declaramos los time points
time.points <- seq.Date(as.Date("2017-1-1"), by="month", length.out = 105) 
print(time.points)

class(time.points)

#Time series IPC

end_period=c(2025,09)

IPC_Alquileres <- ts(Independientes_IPC$`Alquileres Deflactado`, start = c(2017, 1), frequency = 12)
IPC_Alquileres <- window(IPC_Alquileres, end = end_period)

EMAE_IPC <- ts(Independientes_IPC$EMAE, start = c(2017, 1), frequency = 12)
EMAE_IPC <- window(EMAE_IPC, end = end_period)

ConsumoGBA_IPC <- ts(Independientes_IPC$`Cantidad de operaciones en unidades (GBA)`, start = c(2017, 1), frequency = 12)
ConsumoGBA_IPC <- window(ConsumoGBA_IPC, end = end_period)

percapita_IPC <- ts(Independientes_IPC$percapita, start = c(2017, 1), frequency = 12)
percapita_IPC <- seas(percapita_IPC)
percapita_IPC <- final(percapita_IPC)
percapita_IPC <- window(percapita_IPC, end = end_period)

Unemployment_IPC <- ts(Independientes_IPC$Unemployment, start = c(2017, 1), frequency = 12)
Unemployment_IPC <- window(Unemployment_IPC, end = end_period)

RIPTE_IPC <- ts(Independientes_IPC$ripte, start = c(2017, 1), frequency = 12)
RIPTE_IPC <- seas(RIPTE_IPC)
RIPTE_IPC <- final(RIPTE_IPC)
RIPTE_IPC <- window(RIPTE_IPC, end = end_period)

Plazofijo_IPC <- ts(Independientes_IPC$Plazofijo, start = c(2017, 1), frequency = 12)
Plazofijo_IPC <- window(Plazofijo_IPC, end = end_period)

Inflacion_IPC <- ts(Independientes_IPC$`Variacion General`, start = c(2017, 1), frequency = 12)
Inflacion_IPC <- window(Inflacion_IPC, end = end_period)

TCN_Oficial_IPC <- ts(Independientes_IPC$TCN_Oficial, start = c(2017, 1), frequency = 12)
TCN_Oficial_IPC <- window(TCN_Oficial_IPC, end = end_period)

TCN_IPC <- ts(Independientes_IPC$TCN, start = c(2017, 1), frequency = 12)
TCN_IPC <- window(TCN_IPC, end = end_period)

TCR_IPC <- ts(Independientes_IPC$TCR, start = c(2017, 1), frequency = 12)
TCR_IPC <- window(TCR_IPC, end = end_period)

Stocks_IPC <- ts(Independientes_IPC$Stocks, start = c(2017, 1), frequency = 12)
Stocks_IPC <- window(Stocks_IPC, end = end_period)

ICCGBA_IPC <- ts(Independientes$VariacionGBA, start = c(2017, 1), frequency = 12)
ICCGBA_IPC <- window(ICCGBA_IPC, end = end_period)


#IPC Alquileres

IPC_Alquileres <- zoo(
  cbind(
    as.numeric(IPC_Alquileres),
    as.numeric(percapita_IPC),
    as.numeric(Unemployment_IPC),
    as.numeric(EMAE_IPC),
    as.numeric(ConsumoGBA_IPC),
    #as.numeric(RIPTE_IPC),
    #as.numeric(ventasStaFe),
    as.numeric(Plazofijo_IPC),
    #as.numeric(EFStaFe),
    as.numeric(Inflacion_IPC),
    as.numeric(ICCGBA_IPC),
    as.numeric(TCR_IPC),
    as.numeric(TCN_IPC),
    as.numeric(Stocks_IPC)
  ),
  time.points
)

pre.period <- c(1, 42)  # Pre-intervention period
post.period <- c(43, 84)  # Post-intervention period

IPC_Alquileres <- IPC_Alquileres |>
  
  lapply(\(x) as.numeric(as.character(x))) |>
  as.data.frame()

IPC_Alquileres <- CausalImpact(IPC_Alquileres, pre.period, post.period)
plot(IPC_Alquileres)

summary(IPC_Alquileres)

IPC_Alquileres

time.points  


#OFERTA (varian los timepoints con respecto a Precios)

#Definimos las time-series con su ventana temporal correspondiente (hasta septiembre)

end_period <- c(2025, 09)

CCLvar <- ts(Independientes$`Var CCL`,start = c(2018, 1), frequency = 12)
CCLvar <- seas(CCLvar)
CCLvar <- final(CCLvar)
CCLvar <- window(CCLvar, end = end_period)

ICCGBA <- ts(Independientes$VariacionGBA, start = c(2018, 1), frequency = 12)
ICCGBA <- seas(ICCGBA)
ICCGBA <- final(ICCGBA)
ICCGBA <- window(ICCGBA, end = end_period)

ICCCBA <- ts(Independientes$`Var CBA`, start = c(2018, 1), frequency = 12)
ICCCBA <- seas(ICCCBA)
ICCCBA <- final(ICCCBA)
ICCCBA <- window(ICCCBA, end = end_period)

Plazofijo <- ts(Independientes$Plazofijo, start = c(2018, 1), frequency = 12)
Plazofijo <- window(Plazofijo, end = end_period)

EFCABA <- ts(Independientes$CABA, start = c(2018, 1), frequency = 12)
EFCABA <- window(EFCABA, end = end_period)

EFGBA <- ts(Independientes$`BUENOS AIRES`, start = c(2018, 1), frequency = 12)
EFGBA <- window(EFGBA, end = end_period)

EFCba <- ts(Independientes$CÓRDOBA, start = c(2018, 1), frequency = 12)
EFCba <- window(EFCba, end = end_period)

EFStaFe <- ts(Independientes$`SANTA FE`, start = c(2018, 1), frequency = 12)
EFStaFe <- window(EFStaFe, end = end_period)

RIPTE <- ts(Independientes$ripte, start = c(2018, 1), frequency = 12)
RIPTE <- seas(RIPTE)
RIPTE <- final(RIPTE)
RIPTE <- window(RIPTE, end = end_period)

ventasCABA <- ts(Independientes$`Ventas por M2 (CABA)`, start = c(2018, 1), frequency = 12)
ventasCABA <- seas(ventasCABA)
ventasCABA <- final(ventasCABA)
ventasCABA <- window(ventasCABA, end = end_period)

ventasGBA <- ts(Independientes$`Ventas por M2 (GBA)`, start = c(2018, 1), frequency = 12)
ventasGBA <- seas(ventasGBA)
ventasGBA <- final(ventasGBA)
ventasGBA <- window(ventasGBA, end = end_period)

ventasCba <- ts(Independientes$`Ventas por M2 (Cba)`, start = c(2018, 1), frequency = 12)
ventasCba <- seas(ventasCba)
ventasCba <- final(ventasCba)
ventasCba <- window(ventasCba, end = end_period)

ventasStaFe <- ts(Independientes$`Cantidad de operaciones en unidades (Sta Fe)`, start = c(2018, 1), frequency = 12)
ventasStaFe <- seas(ventasStaFe)
ventasStaFe <- final(ventasStaFe)
ventasStaFe <- window(ventasStaFe, end = end_period)

percapita <- ts(Independientes$percapita, start = c(2018, 1), frequency = 12)
percapita <- seas(percapita)
percapita <- final(percapita)
percapita <- window(percapita, end = end_period)

ICAE <- ts(Independientes$ICAE, start = c(2018,1), frequency = 12)
ICAE <- seas(ICAE)
ICAE <- final(ICAE)
ICAE <- window(ICAE, end = end_period)

EMAE <- ts(Independientes$EMAE, start = c(2018, 1), frequency = 12)
EMAE <- seas(EMAE)
EMAE <- final(EMAE)
EMAE <- window(EMAE, end = end_period)

IMAE <- ts(Independientes$IMAE, start = c(2018, 1), frequency = 12)
IMAE <- seas(IMAE)
IMAE <- final(IMAE)
IMAE <- window(IMAE, end = end_period)

Stocks <- ts(Independientes$Stocks, start = c(2018, 1), frequency = 12)
Stocks <- seas(Stocks)
Stocks <- final(Stocks)
Stocks <- window(Stocks, end = end_period)

TCN_Oficial <- ts(Independientes$TCN_Oficial, start = c(2018, 1), frequency = 12)
TCN_Oficial <- window(TCN_Oficial, end = end_period)

TCN <- ts(Independientes$TCN, start = c(2018, 1), frequency = 12)
TCN <- window(TCN, end = end_period)


TCR <- ts(Independientes$TCR, start = c(2018, 1), frequency = 12)
TCR <- window(TCR, end = end_period)

Unemployment <- ts(Independientes$Unemployment, start = c(2018, 1), frequency = 12)
Unemployment <- window(Unemployment, end = end_period)

Inflacion <- ts(Independientes$`Variacion General`, start = c(2018, 1), frequency = 12)
Inflacion <- window(Inflacion, end = end_period)

Trafico <- ts(Independientes$Mercadolibre.com.ar, start = c(2018, 1), frequency = 12)
Trafico <- seas(Trafico)
Trafico <- final(Trafico)
Trafico <- window(Trafico, end = end_period)

nose <- ts(Independientes$nose, start = c(2018, 1), frequency = 12)
nose <- window(nose, end = end_period)

ConsumoCABA <- ts(Independientes$`Cantidad de operaciones en unidades (CABA)`, start = c(2018, 1), frequency = 12)
ConsumoCABA <- seas(ConsumoCABA)
ConsumoCABA <- final(ConsumoCABA)
ConsumoCABA <- window(ConsumoCABA, end = end_period)

ConsumoGBA <- ts(Independientes$`Cantidad de operaciones en unidades (GBA)`, start = c(2018, 1), frequency = 12)
ConsumoGBA <- seas(ConsumoGBA)
ConsumoGBA <- final(ConsumoGBA)
ConsumoGBA <- window(ConsumoGBA, end = end_period)

ConsumoRO <- ts(Independientes$`Cantidad de operaciones en unidades (Sta Fe)`, start = c(2018, 1), frequency = 12)
ConsumoRO <- seas(ConsumoRO)
ConsumoRO <- final(ConsumoRO)
ConsumoRO <- window(ConsumoRO, end = end_period)

ConsumoCba <- ts(Independientes$`Cantidad de operaciones en unidades (Cba)`, start = c(2018, 1), frequency = 12)
ConsumoCba <- seas(ConsumoCba)
ConsumoCba <- final(ConsumoCba)
ConsumoCba <- window(ConsumoCba, end = end_period)

CCL <- ts(Independientes$CCL, start = c(2018, 1), frequency = 12)
CCL <- seas(CCL)
CCL <- final(CCL)
CCL <- window(CCL, end = end_period)

#Time series Rosario
precio_casa_Rosario <- ts(Dependientes$P_roscasa, start = c(2018,1), frequency = 12)
precio_casa_Rosario <- window(precio_casa_Rosario, end = end_period)

precio_dpto_Rosario <- ts(Dependientes$P_rosdpto, start = c(2018,1), frequency = 12)
precio_dpto_Rosario <- window(precio_dpto_Rosario, end = end_period)

Precio_casa_Cordoba <- ts(Dependientes$P_cbacasa, start = c(2018,1), frequency = 12)
Precio_casa_Cordoba <- window(Precio_casa_Cordoba, end = end_period)

Precio_dpto_Cordoba <- ts(Dependientes$P_cbadpto, start = c(2018,1), frequency = 12)
Precio_dpto_Cordoba <- window(Precio_dpto_Cordoba, end = end_period)



#Time series Cordoba
oferta_casa_Rosario <- ts(Dependientes$Q_roscasa, start = c(2018,1), frequency = 12)
oferta_casa_Rosario <- window(oferta_casa_Rosario, end = end_period)

oferta_dpto_Rosario <- ts(Dependientes$Q_rosdpto, start = c(2018,1), frequency = 12)
oferta_dpto_Rosario <- window(oferta_dpto_Rosario, end = end_period)

oferta_casa_Cordoba <- ts(Dependientes$Q_cbacasa, start = c(2018,1), frequency = 12)
oferta_casa_Cordoba <- window(oferta_casa_Cordoba, end = end_period)

oferta_dpto_Cordoba <- ts(Dependientes$Q_cbadpto, start = c(2018,1), frequency = 12)
oferta_dpto_Cordoba <- window(oferta_dpto_Cordoba, end = end_period)

#Time series AMBA y CABA
oferta_dpto_amba <- ts(Dependientes$Q_ambadpto, start = c(2018, 1), frequency = 12)
oferta_dpto_amba<- window(oferta_dpto_amba, end = end_period)

oferta_casa_amba <- ts(Dependientes$Q_ambacasa, start = c(2018, 1), frequency = 12)
oferta_casa_amba <- window(oferta_casa_amba, end = end_period)

oferta_dpto_CABA <- ts(Dependientes$Q_cabadto, start = c(2018, 1), frequency = 12)
oferta_dpto_CABA <- window(oferta_dpto_CABA, end = end_period)

oferta_casa_CABA <- ts(Dependientes$Q_cabacasa, start = c(2018, 1), frequency = 12)
oferta_casa_CABA <- window(oferta_casa_CABA, end = end_period)

Precio_dpto_amba <- ts(Dependientes$P_ambadpto, start = c(2018, 1), frequency = 12)
Precio_dpto_amba <- window(Precio_dpto_amba, end = end_period)

Precio_casa_amba <- ts(Dependientes$P_ambacasa, start = c(2018, 1), frequency = 12)
Precio_casa_amba <- window(Precio_casa_amba, end = end_period)

Precio_dpto_CABA <- ts(Dependientes$P_cabadpto, start = c(2018, 1), frequency = 12)
Precio_dpto_CABA <- window(Precio_dpto_CABA, end = end_period)

Precio_casa_CABA <- ts(Dependientes$P_cabacasa, start = c(2018, 1), frequency = 12)
Precio_casa_CABA <- window(Precio_casa_CABA, end = end_period)

plot(Precio_casa_CABA)

#OFERTA
time.points <- seq.Date(as.Date("2018-1-1"), by="month", length.out = 74) 
print(time.points)

#Oferta casa AMBA



oferta_casa_amba <- zoo(
  cbind(
    as.numeric(oferta_casa_amba),
    as.numeric(percapita),
    as.numeric(Unemployment),
    as.numeric(Trafico),
    as.numeric(EMAE),
    as.numeric(ConsumoGBA),
    as.numeric(RIPTE),
    #as.numeric(ventasStaFe),
    as.numeric(Plazofijo),
    #as.numeric(EFStaFe),
    as.numeric(Inflacion),
    as.numeric(ICCGBA),
    as.numeric(TCR),
    as.numeric(TCN),
    as.numeric(Stocks),
    as.numeric(TCN_Oficial)  
    ),
  time.points
)

oferta_casa_amba <- as.data.frame(oferta_casa_amba)

pre.period <- c(1, 29)  # Pre-intervention period
post.period <- c(30, 72)  # Post-intervention period

oferta_casa_amba <- oferta_casa_amba |>
  
  lapply(\(x) as.numeric(as.character(x))) |>
  as.data.frame()

oferta_casa_amba <- CausalImpact(oferta_casa_amba, pre.period, post.period)
plot(oferta_casa_amba)

summary(oferta_casa_amba)

#Oferta dpto AMBA

oferta_dpto_amba <- zoo(
  cbind(
    as.numeric(oferta_dpto_amba),
    as.numeric(percapita),
    as.numeric(Unemployment),
    as.numeric(Trafico),
    as.numeric(EMAE),
    as.numeric(ConsumoGBA),
    as.numeric(RIPTE),
    #as.numeric(ventasStaFe),
    as.numeric(Plazofijo),
    #as.numeric(EFStaFe),
    as.numeric(Inflacion),
    as.numeric(ICCGBA),
    as.numeric(TCR),
    as.numeric(TCN),
    as.numeric(Stocks),
    as.numeric(TCN_Oficial)
  ),
  time.points
)

oferta_dpto_amba <- as.data.frame(oferta_dpto_amba)

pre.period <- c(1, 29)  # Pre-intervention period
post.period <- c(30, 72)  # Post-intervention period

oferta_dpto_amba <- oferta_dpto_amba |>
  
  lapply(\(x) as.numeric(as.character(x))) |>
  as.data.frame()

oferta_dpto_amba <- CausalImpact(oferta_dpto_amba, pre.period, post.period)
plot(oferta_dpto_amba)

summary(oferta_dpto_amba)

#Oferta dpto CABA
#OFERTA
time.points <- seq.Date(as.Date("2018-1-1"), by="month", length.out = 74) 
print(time.points)

oferta_dpto_CABA <- zoo(
  cbind(
    as.numeric(oferta_dpto_CABA),
    as.numeric(percapita),
    as.numeric(Unemployment),
    as.numeric(Trafico),
    as.numeric(EMAE),
    as.numeric(ConsumoGBA),
    as.numeric(RIPTE),
    #as.numeric(ventasStaFe),
    as.numeric(Plazofijo),
    #as.numeric(EFStaFe),
    as.numeric(Inflacion),
    #as.numeric(ICCGBA),
    as.numeric(TCR),
    as.numeric(TCN),
    as.numeric(Stocks),
    as.numeric(TCN_Oficial)
  ),
  time.points
)

oferta_dpto_CABA <- as.data.frame(oferta_dpto_CABA)

pre.period <- c(1, 29)  # Pre-intervention period
post.period <- c(30, 72)  # Post-intervention period

oferta_dpto_CABA <- oferta_dpto_CABA |>
  
  lapply(\(x) as.numeric(as.character(x))) |>
  as.data.frame()

oferta_dpto_CABA <- CausalImpact(oferta_dpto_CABA, pre.period, post.period)
plot(oferta_dpto_CABA)

summary(oferta_dpto_CABA)


#Oferta casa CABA

oferta_casa_CABA <- zoo(
  cbind(
    as.numeric(oferta_casa_CABA), #
    as.numeric(percapita), # 
    as.numeric(Unemployment), #
    as.numeric(Trafico), #
    as.numeric(EMAE), #
    as.numeric(ConsumoCABA), #
    as.numeric(RIPTE), #
    #as.numeric(ventasStaFe), 
    as.numeric(Plazofijo), #
    #as.numeric(EFStaFe),
    as.numeric(Inflacion), #
    #as.numeric(ICCGBA),
    as.numeric(TCR), #
    as.numeric(TCN), #
    as.numeric(Stocks), #
    as.numeric(TCN_Oficial) #
  ),
  time.points
)

oferta_casa_CABA <- as.data.frame(oferta_casa_CABA)

pre.period <- c(1, 29)  # Pre-intervention period
post.period <- c(30, 72)  # Post-intervention period

oferta_casa_CABA <- oferta_casa_CABA |>
  lapply(\(x) as.numeric(as.character(x))) |>
  as.data.frame()

oferta_casa_CABA <- CausalImpact(oferta_casa_CABA, pre.period, post.period)
plot(oferta_casa_CABA)

summary(oferta_casa_CABA)


#Oferta Casa rosario

time.points <- seq.Date(as.Date("2018-1-1"), by="month", length.out = 82) 

oferta_casa_Rosario <- zoo(
   cbind(
     as.numeric(oferta_casa_Rosario),
     as.numeric(percapita),
     as.numeric(Unemployment),
     as.numeric(Trafico),
     as.numeric(IMAE),
     as.numeric(ConsumoRO),
     as.numeric(RIPTE),
     #as.numeric(ventasStaFe),
     as.numeric(Plazofijo),
     #as.numeric(EFStaFe),
     as.numeric(Inflacion),
     as.numeric(ICCGBA),
     as.numeric(TCR),
     as.numeric(TCN),
     as.numeric(Stocks),
     as.numeric(TCN_Oficial)
   ),
   time.points
 )

oferta_casa_Rosario <- as.data.frame(oferta_casa_Rosario)

pre.period <- c(1, 29)  # Pre-intervention period
post.period <- c(30, 72)  # Post-intervention period

oferta_casa_Rosario <- oferta_casa_Rosario |>
  
  lapply(\(x) as.numeric(as.character(x))) |>
  as.data.frame()

oferta_casa_Rosario <- CausalImpact(oferta_casa_Rosario, pre.period, post.period)
plot(oferta_casa_Rosario)


summary(oferta_casa_Rosario)

#Rosario dpto
oferta_dpto_Rosario <- zoo(
  cbind(
    as.numeric(oferta_dpto_Rosario),
    as.numeric(percapita),
    as.numeric(Unemployment),
    as.numeric(Trafico),
    as.numeric(IMAE),
    as.numeric(ConsumoRO),
    as.numeric(RIPTE),
    #as.numeric(ventasStaFe),
    as.numeric(Plazofijo),
    #as.numeric(EFStaFe),
    as.numeric(Inflacion),
    #as.numeric(ICCGBA),
    as.numeric(TCR),
    as.numeric(TCN),
    as.numeric(Stocks),
    as.numeric(TCN_Oficial)
  ),
  time.points
)
oferta_dpto_Rosario <- as.data.frame(oferta_dpto_Rosario)

pre.period <- c(1, 29)  # Pre-intervention period
post.period <- c(30, 72)  # Post-intervention period

oferta_dpto_Rosario <- oferta_dpto_Rosario |>
  lapply(\(x) as.numeric(as.character(x))) |>
  as.data.frame()

oferta_dpto_Rosario <- CausalImpact(oferta_dpto_Rosario, pre.period, post.period)
plot(oferta_dpto_Rosario)

summary(oferta_dpto_Rosario)



#Habria que chequear del metodo si agregar variables que correlacionen Genera problemas en la
#Estimacion. Lo digo porque si pones el empleo formal en santafe en vez de unemployment nacional,
#Logras una estimacion con una menorvarianza que si estuviesen los dos al mismo tiempo

time.points <- seq.Date(as.Date("2018-1-1"), by="month", length.out = 82) 

#Cordoba Casa
oferta_casa_Cordoba <- zoo(
  cbind(
    as.numeric(oferta_casa_Cordoba),
    as.numeric(EMAE),
    as.numeric(percapita),
    as.numeric(Unemployment),
    as.numeric(Trafico),
    as.numeric(ConsumoCba),
    as.numeric(RIPTE),
    as.numeric(Plazofijo),
    #as.numeric(EFStaFe),
    as.numeric(Inflacion),
    as.numeric(ICCCBA),
    as.numeric(TCR),
    as.numeric(TCN),
    as.numeric(Stocks),
    as.numeric(TCN_Oficial)
  ),
  time.points
)
oferta_casa_Cordoba <- as.data.frame(oferta_casa_Cordoba)
oferta_casa_Cordoba <- oferta_casa_Cordoba |>
  lapply(\(x) as.numeric(as.character(x))) |>
  as.data.frame()
pre.period <- c(1, 29)  # Pre-intervention period
post.period <- c(30, 72)  # Post-intervention period
oferta_casa_Cordoba <- CausalImpact(oferta_casa_Cordoba, pre.period, post.period)
plot(oferta_casa_Cordoba)


summary(oferta_casa_Cordoba)

# #Cordoba departamento
oferta_dpto_Cordoba <- zoo(
  cbind(
    as.numeric(oferta_dpto_Cordoba),
    as.numeric(percapita),
    as.numeric(Unemployment),
    as.numeric(Trafico),
    as.numeric(ICAE),
    as.numeric(ConsumoCba),
    as.numeric(RIPTE),
    #as.numeric(ventasStaFe),
    as.numeric(Plazofijo),
    #as.numeric(EFStaFe),
    as.numeric(Inflacion),
    as.numeric(ICCCBA),
    as.numeric(TCR),
    as.numeric(TCN),
    as.numeric(Stocks),
    as.numeric(TCN_Oficial)
  ),
  time.points
)
oferta_dpto_Cordoba <- as.data.frame(oferta_dpto_Cordoba)
oferta_dpto_Cordoba <- oferta_dpto_Cordoba |>
  lapply(\(x) as.numeric(as.character(x))) |>
  as.data.frame()
pre.period <- c(1, 29)  # Pre-intervention period
post.period <- c(30, 72)  # Post-intervention period
oferta_dpto_Cordoba <- CausalImpact(oferta_dpto_Cordoba, pre.period, post.period)
plot(oferta_dpto_Cordoba)

summary(oferta_dpto_Cordoba)

#PRECIOS (varian los timepoints con respecto a Oferta)

time.points <- seq.Date(as.Date("2018-1-1"), by="month", length.out = 93) 

time.points

#casas AMBA 
Precio_casa_amba <- zoo(
  cbind(
    as.numeric(Precio_casa_amba),
    as.numeric(percapita),
    as.numeric(Unemployment),
    as.numeric(Trafico),
    as.numeric(EMAE),
    as.numeric(ConsumoGBA),
    as.numeric(RIPTE),
    #as.numeric(ventasStaFe),
    as.numeric(Plazofijo),
    #as.numeric(EFStaFe),
    as.numeric(Inflacion),
    #as.numeric(ICCGBA),
    as.numeric(TCR),
    as.numeric(TCN),
    as.numeric(Stocks)
  ),
  time.points
)
Precio_casa_amba <- as.data.frame(Precio_casa_amba)

pre.period <- c(1, 29)  # Pre-intervention period
post.period <- c(30, 72)  # Post-intervention period

Precio_casa_amba <- Precio_casa_amba |>
  lapply(\(x) as.numeric(as.character(x))) |>
  as.data.frame()

Precio_casa_amba <- CausalImpact(Precio_casa_amba, pre.period, post.period)
plot(Precio_casa_amba)

summary(Precio_casa_amba)

#dptos AMBA

Precio_dpto_amba <- zoo(
  cbind(
    as.numeric(Precio_dpto_amba),
    as.numeric(percapita),
    as.numeric(Unemployment),
    as.numeric(Trafico),
    as.numeric(EMAE),
    as.numeric(ConsumoGBA),
    as.numeric(RIPTE),
    #as.numeric(ventasStaFe),
    as.numeric(Plazofijo),
    #as.numeric(EFStaFe),
    as.numeric(Inflacion),
    #as.numeric(ICCGBA),
    as.numeric(TCR),
    as.numeric(TCN),
    as.numeric(Stocks)
  ),
  time.points
)
Precio_dpto_amba <- as.data.frame(Precio_dpto_amba)

pre.period <- c(1, 29)  # Pre-intervention period
post.period <- c(30, 72)  # Post-intervention period

Precio_dpto_amba <- Precio_dpto_amba |>
  lapply(\(x) as.numeric(as.character(x))) |>
  as.data.frame()

Precio_dpto_amba <- CausalImpact(Precio_dpto_amba, pre.period, post.period)
plot(Precio_dpto_amba)

summary(Precio_dpto_amba)

#Rosario casa precios
precio_casa_Rosario <- zoo(
  cbind(
    as.numeric(precio_casa_Rosario),
    as.numeric(percapita),
    as.numeric(Unemployment),
    as.numeric(Trafico),
    as.numeric(IMAE),
    as.numeric(ConsumoRO),
    as.numeric(RIPTE),
    #as.numeric(ventasStaFe),
    as.numeric(Plazofijo),
    #as.numeric(EFStaFe),
    as.numeric(Inflacion),
    #as.numeric(ICCGBA),
    as.numeric(TCR),
    as.numeric(TCN),
    as.numeric(Stocks)
  ),
  time.points
)
precio_casa_Rosario <- as.data.frame(precio_casa_Rosario)

pre.period <- c(1, 29)  # Pre-intervention period
post.period <- c(30, 72)  # Post-intervention period

precio_casa_Rosario <- precio_casa_Rosario |>
  lapply(\(x) as.numeric(as.character(x))) |>
  as.data.frame()

precio_casa_Rosario <- CausalImpact(precio_casa_Rosario, pre.period, post.period)
plot(precio_casa_Rosario)

summary(precio_casa_Rosario)


#Rosario dpto precios
precio_dpto_Rosario <- zoo(
  cbind(
    as.numeric(precio_dpto_Rosario),
    as.numeric(percapita),
    as.numeric(Unemployment),
    as.numeric(Trafico),
    as.numeric(IMAE),
    as.numeric(ConsumoRO),
    as.numeric(RIPTE),
    #as.numeric(ventasStaFe),
    as.numeric(Plazofijo),
    #as.numeric(EFStaFe),
    as.numeric(Inflacion),
    #as.numeric(ICCGBA),
    as.numeric(TCR),
    as.numeric(TCN),
    as.numeric(Stocks)
  ),
  time.points
)
precio_dpto_Rosario <- as.data.frame(precio_dpto_Rosario)

pre.period <- c(1, 29)  # Pre-intervention period
post.period <- c(30, 72)  # Post-intervention period

precio_dpto_Rosario <- precio_dpto_Rosario |>
  lapply(\(x) as.numeric(as.character(x))) |>
  as.data.frame()

precio_dpto_Rosario <- CausalImpact(precio_dpto_Rosario, pre.period, post.period)
plot(precio_dpto_Rosario)




summary(precio_dpto_Rosario)


#Cordoba Casa
Precio_casa_Cordoba <- zoo(
  cbind(
    as.numeric(Precio_casa_Cordoba),
    as.numeric(percapita),
    as.numeric(Unemployment),
    as.numeric(Trafico),
    as.numeric(EMAE),
    as.numeric(ConsumoCba),
    as.numeric(RIPTE),
    #as.numeric(ventasStaFe),
    as.numeric(Plazofijo),
    #as.numeric(EFStaFe),
    as.numeric(Inflacion),
    #as.numeric(ICCCBA),
    as.numeric(TCR),
    as.numeric(TCN),
    as.numeric(Stocks)
  ),
  time.points
)


Precio_casa_Cordoba <- as.data.frame(Precio_casa_Cordoba)
Precio_casa_Cordoba <- Precio_casa_Cordoba |>
     lapply(\(x) as.numeric(as.character(x))) |>
     as.data.frame()
pre.period <- c(1, 29)  # Pre-intervention period
post.period <- c(30, 72)  # Post-intervention period
Precio_casa_Cordoba <- CausalImpact(Precio_casa_Cordoba, pre.period, post.period)
plot(Precio_casa_Cordoba)

summary(Precio_casa_Cordoba)



#Cordoba departamento
Precio_dpto_Cordoba <- zoo(
  cbind(
    as.numeric(Precio_dpto_Cordoba),
    as.numeric(percapita),
    as.numeric(Unemployment),
    as.numeric(Trafico),
    as.numeric(EMAE),
    as.numeric(ConsumoCba),
    as.numeric(RIPTE),
    #as.numeric(ventasStaFe),
    as.numeric(Plazofijo),
    #as.numeric(EFStaFe),
    as.numeric(Inflacion),
    #as.numeric(ICCCBA),
    as.numeric(TCR),
    as.numeric(TCN),
    as.numeric(Stocks)
  ),
  time.points
)

Precio_dpto_Cordoba <- as.data.frame(Precio_dpto_Cordoba)
Precio_dpto_Cordoba <- Precio_dpto_Cordoba |>
        lapply(\(x) as.numeric(as.character(x))) |>
        as.data.frame()
pre.period <- c(1, 29)  # Pre-intervention period
post.period <- c(30, 72)  # Post-intervention period
Precio_dpto_Cordoba <- CausalImpact(Precio_dpto_Cordoba, pre.period, post.period)
plot(Precio_dpto_Cordoba)

summary(Precio_dpto_Cordoba)

#Precios de casas en CABA

Precio_casa_CABA <- zoo(
  cbind(
    as.numeric(Precio_casa_CABA),
    as.numeric(percapita),
    as.numeric(Unemployment),
    as.numeric(Trafico),
    as.numeric(EMAE),
    as.numeric(ConsumoCABA),
    as.numeric(RIPTE),
    #as.numeric(ventasStaFe),
    as.numeric(Plazofijo),
    #as.numeric(EFStaFe),
    as.numeric(Inflacion),
    #as.numeric(ICCGBA),
    as.numeric(TCR),
    as.numeric(TCN),
    as.numeric(Stocks)
  ),
  time.points
)
Precio_casa_CABA <- as.data.frame(Precio_casa_CABA)
Precio_casa_CABA <- Precio_casa_CABA |>
  lapply(\(x) as.numeric(as.character(x))) |>
  as.data.frame()
pre.period <- c(1, 29)  # Pre-intervention period
post.period <- c(30, 72)  # Post-intervention period
Precio_casa_CABA <- CausalImpact(Precio_casa_CABA, pre.period, post.period)
plot(Precio_casa_CABA)

summary(Precio_casa_CABA)

#Precio de departamentos en CABA

Precio_dpto_CABA <- zoo(
  cbind(
    as.numeric(Precio_dpto_CABA),
    as.numeric(percapita),
    as.numeric(Unemployment),
    as.numeric(Trafico),
    as.numeric(EMAE),
    as.numeric(ConsumoCABA),
    as.numeric(RIPTE),
    #as.numeric(ventasStaFe),
    as.numeric(Plazofijo),
    #as.numeric(EFStaFe),
    as.numeric(Inflacion),
    #as.numeric(ICCGBA),
    as.numeric(TCR),
    as.numeric(TCN),
    as.numeric(Stocks)
  ),
  time.points
)
Precio_dpto_CABA <- as.data.frame(Precio_dpto_CABA)
Precio_dpto_CABA <- Precio_dpto_CABA |>
  lapply(\(x) as.numeric(as.character(x))) |>
  as.data.frame()
pre.period <- c(1, 29)  # Pre-intervention period
post.period <- c(30, 72)  # Post-intervention period
Precio_dpto_CABA <- CausalImpact(Precio_dpto_CABA, pre.period, post.period)
plot(Precio_dpto_CABA)

summary(Precio_dpto_CABA)
