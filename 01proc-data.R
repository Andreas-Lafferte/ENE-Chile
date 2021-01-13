# Code 1: Process ENE 

# 1. Cargar librerias ----
pacman::p_load(tidyverse, rvest, xml2, lubridate, openxlsx, readxl, ggrepel, tibble, writexl, haven,
               dplyr, car, summarytools, ggpubr, sjmisc, sjlabelled, stargazer, srvyr)
options(scipen=999)

# 2. Descargar bases de datos ----
download.file("https://www.ine.cl/docs/default-source/ocupacion-y-desocupacion/bbdd/2010/formato-csv/ene-2010-11.csv?sfvrsn=ab729ef3_4&download=true",destfile = "input/ene2010_11.csv",method = "libcurl")
download.file("https://www.ine.cl/docs/default-source/ocupacion-y-desocupacion/bbdd/2011/formato-csv/ene-2011-11.csv?sfvrsn=ab729ef3_4&download=true",destfile = "input/ene2011_11.csv",method = "libcurl")
download.file("https://www.ine.cl/docs/default-source/ocupacion-y-desocupacion/bbdd/2012/formato-csv/ene-2012-11.csv?sfvrsn=ab729ef3_4&download=true",destfile = "input/ene2012_11.csv",method = "libcurl")
download.file("https://www.ine.cl/docs/default-source/ocupacion-y-desocupacion/bbdd/2013/formato-csv/ene-2013-11.csv?sfvrsn=ab729ef3_4&download=true",destfile = "input/ene2013_11.csv",method = "libcurl")
download.file("https://www.ine.cl/docs/default-source/ocupacion-y-desocupacion/bbdd/2014/formato-csv/ene-2014-11.csv?sfvrsn=ab729ef3_4&download=true",destfile = "input/ene2014_11.csv",method = "libcurl")
download.file("https://www.ine.cl/docs/default-source/ocupacion-y-desocupacion/bbdd/2015/formato-csv/ene-2015-11.csv?sfvrsn=ab729ef3_4&download=true",destfile = "input/ene2015_11.csv",method = "libcurl")
download.file("https://www.ine.cl/docs/default-source/ocupacion-y-desocupacion/bbdd/2016/formato-csv/ene-2016-11.csv?sfvrsn=ab729ef3_4&download=true",destfile = "input/ene2016_11.csv",method = "libcurl")
download.file("https://www.ine.cl/docs/default-source/ocupacion-y-desocupacion/bbdd/2017/formato-csv/ene-2017-11.csv?sfvrsn=ab729ef3_4&download=true",destfile = "input/ene2017_11.csv",method = "libcurl")
download.file("https://www.ine.cl/docs/default-source/ocupacion-y-desocupacion/bbdd/2018/formato-csv/ene-2018-11.csv?sfvrsn=ab729ef3_4&download=true",destfile = "input/ene2018_11.csv",method = "libcurl")
download.file("https://www.ine.cl/docs/default-source/ocupacion-y-desocupacion/bbdd/2019/formato-csv/ene-2019-11.csv?sfvrsn=ab729ef3_4&download=true",destfile = "input/ene2019_11.csv",method = "libcurl")

# 3. Cargar bases de datos ---- 
ene0 <- read.csv("input/ene2010_11.csv")
ene1 <- read.csv("input/ene2011_11.csv")
ene2 <- read.csv("input/ene2012_11.csv")
ene3 <- read.csv("input/ene2013_11.csv")
ene4 <- read.csv("input/ene2014_11.csv")
ene5 <- read.csv("input/ene2015_11.csv")
ene6 <- read.csv("input/ene2016_11.csv")
ene7 <- read.csv("input/ene2017_11.csv")
ene8 <- read.csv("input/ene2018_11.csv")
ene9 <- read.csv("input/ene2019_11.csv")

# 4. Descriptivos generales ----


# 5. Procesamiento ----

# 5.1. ----- Recodificar rama actividad ecÃ³nomica ---- 

ene1$b14_rev4cl_caenes <- as_factor(ene1$b14_rev4cl_caenes)
ene1 <- ene1 %>% mutate(b14_rev4cl_caenes=case_when(b14_rev4cl_caenes=="Agricultura, ganadería, silvicultura y pesca" ~ "Agricultura y pesca",
                                                    b14_rev4cl_caenes=="Explotación de minas y canteras" ~ "Minería",
                                                    b14_rev4cl_caenes=="Industrias manufactureras" ~ "Industria",
                                                    b14_rev4cl_caenes=="Suministro de electricidad, gas, vapor y aire acondicionado" ~ "Electricidad,  gas  y  agua",
                                                    b14_rev4cl_caenes=="Suministro de agua" ~ "Electricidad,  gas  y  agua", 
                                                    b14_rev4cl_caenes=="Construcción" ~ "Construcción",
                                                    b14_rev4cl_caenes=="Comercio al por mayor y al por menor" ~ "Comercio",
                                                    b14_rev4cl_caenes=="Transporte y almacenamiento" ~ "Transporte y comunicaciones",
                                                    b14_rev4cl_caenes=="Actividades de alojamiento y de servicio de comidas" ~ "Servicios",
                                                    b14_rev4cl_caenes=="Información y comunicaciones" ~ "Transporte y comunicaciones",
                                                    b14_rev4cl_caenes=="Actividades financieras y de seguros" ~ "Establecimientos financieros",
                                                    b14_rev4cl_caenes=="Actividades inmobiliarias" ~ "Establecimientos financieros",
                                                    b14_rev4cl_caenes=="Actividades profesionales, científicas y técnicas" ~ "Servicios", 
                                                    b14_rev4cl_caenes=="Actividades de servicios administrativos y de apoyo" ~ "Servicios",
                                                    b14_rev4cl_caenes=="Administración pública y defensa" ~ "Servicios", 
                                                    b14_rev4cl_caenes=="Enseñanza" ~ "Servicios", 
                                                    b14_rev4cl_caenes=="Actividades de atención de la salud humana y de asistencia social" ~ "Servicios",
                                                    b14_rev4cl_caenes=="Actividades artósticas, de entretenimiento y recreativas" ~ "Actividades no especificadas",
                                                    b14_rev4cl_caenes=="Otras actividades de servicios" ~ "Actividades no especificadas",
                                                    b14_rev4cl_caenes=="Actividades de los hogares como empleadores" ~ "Actividades no especificadas"))

# group by b2 con b5 
b <- ene1 %>% group_by(b14_rev4cl_caenes) %>% select(`b2`,
                                                     `b5`, 
                                                     `activ`)

c <- b %>% filter(b5==2 | b5==3 & b2==2 | b2==3)
class(b$b14_rev4cl_caenes)
freq(b$b2)
freq(c$b14_rev4cl_caenes)

c %>% count(b14_rev4cl_caenes)

cuadro1 <- c %>% add_count(b14_rev4cl_caenes) #Funciona pero para agrupar por rama se realiza un rbind

cuadro2 <- rbind(
  c('Actividades no especificadas', 2474),
  c('Agricultura y pesca', 3735),
  c('Comercio', 4712),
  c('Construcci?n', 2150),
  c('Electricidad,  gas  y  agua', 466),
  c('Establecimientos financieros',  779),
  c('Industria', 2723),
  c('Miner?a', 1282),
  c('Servicios', 5614),
  c('Transporte y comunicaciones', 2151),
  c('Total', 26086))%>% as.data.frame()

names(cuadro2)<-c("Rama.Actividad.Econ?mica","asalariados.privados.tot")

