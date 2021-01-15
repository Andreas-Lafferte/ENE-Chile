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

# 4. Procesamiento ----

# 2010 
ene0$b2<- as.factor(ene0$b2)
ene0$b5 <- as.factor(ene0$b5)
ene0$activ <- as.factor(ene0$activ)
ene0$b14 <- as_factor(ene0$b14)

# 2011
ene1$b2<- as.factor(ene1$b2)
ene1$b5 <- as.factor(ene1$b5)
ene1$activ <- as.factor(ene1$activ)
ene1$b14 <- as_factor(ene1$b14)

# 2012
ene2$b2<- as.factor(ene2$b2)
ene2$b5 <- as.factor(ene2$b5)
ene2$activ <- as.factor(ene2$activ)
ene2$b14 <- as_factor(ene2$b14)

# 2013
ene3$b2<- as.factor(ene3$b2)
ene3$b5 <- as.factor(ene3$b5)
ene3$activ <- as.factor(ene3$activ)
ene3$b14_rev4cl_caenes <- as_factor(ene3$b14_rev4cl_caenes)

# 2014
ene4$b2<- as.factor(ene4$b2)
ene4$b5 <- as.factor(ene4$b5)
ene4$activ <- as.factor(ene4$activ)
ene4$b14_rev4cl_caenes <- as_factor(ene4$b14_rev4cl_caenes)

# 2015
ene5$b2<- as.factor(ene5$b2)
ene5$b5 <- as.factor(ene5$b5)
ene5$activ <- as.factor(ene5$activ)
ene5$b14_rev4cl_caenes <- as_factor(ene5$b14_rev4cl_caenes)

# 2016
ene6$b2<- as.factor(ene6$b2)
ene6$b5 <- as.factor(ene6$b5)
ene6$activ <- as.factor(ene6$activ)
ene6$b14_rev4cl_caenes <- as_factor(ene6$b14_rev4cl_caenes)

# 2017
ene7$b2<- as.factor(ene7$b2)
ene7$b5 <- as.factor(ene7$b5)
ene7$activ <- as.factor(ene7$activ)
ene7$b14_rev4cl_caenes <- as_factor(ene7$b14_rev4cl_caenes)

# 2018
ene8$b2<- as.factor(ene8$b2)
ene8$b5 <- as.factor(ene8$b5)
ene8$activ <- as.factor(ene8$activ)
ene8$b14_rev4cl_caenes <- as_factor(ene8$b14_rev4cl_caenes)

# 2019
ene9$b2<- as.factor(ene9$b2)
ene9$b5 <- as.factor(ene9$b5)
ene9$activ <- as.factor(ene9$activ)
ene9$b14_rev4cl_caenes <- as_factor(ene9$b14_rev4cl_caenes)

# 4.1. Recodificar rama actividad económica ---- 

# 2010
ene0 <- ene0 %>% mutate(b14=case_when(b14==1 ~ "Agricultura y pesca",
                                      b14==2 ~ "Agricultura y pesca",
                                      b14==3 ~ "Minería",
                                      b14==4 ~ "Industria",
                                      b14==5 ~ "Electricidad,  gas  y  agua",
                                      b14==6 ~ "Construcción",
                                      b14==7 ~ "Comercio",
                                      b14==8 ~ "Comercio",
                                      b14==9 ~ "Transporte y comunicaciones",
                                      b14==10 ~ "Establecimientos financieros",
                                      b14==11 ~ "Establecimientos financieros",
                                      b14==12 ~ "Servicios",
                                      b14==13 ~ "Servicios",
                                      b14==14 ~ "Servicios",
                                      b14==15 ~ "Servicios",
                                      b14==16 ~ "Servicios",
                                      b14==17 ~ "Servicios"))

# 2011
ene1 <- ene1 %>% mutate(b14=case_when(b14==1 ~ "Agricultura y pesca",
                                      b14==2 ~ "Agricultura y pesca",
                                      b14==3 ~ "Minería",
                                      b14==4 ~ "Industria",
                                      b14==5 ~ "Electricidad,  gas  y  agua",
                                      b14==6 ~ "Construcción",
                                      b14==7 ~ "Comercio",
                                      b14==8 ~ "Comercio",
                                      b14==9 ~ "Transporte y comunicaciones",
                                      b14==10 ~ "Establecimientos financieros",
                                      b14==11 ~ "Establecimientos financieros",
                                      b14==12 ~ "Servicios",
                                      b14==13 ~ "Servicios",
                                      b14==14 ~ "Servicios",
                                      b14==15 ~ "Servicios",
                                      b14==16 ~ "Servicios",
                                      b14==17 ~ "Servicios"))

# 2012 
ene2 <- ene2 %>% mutate(b14=case_when(b14==1 ~ "Agricultura y pesca",
                                      b14==2 ~ "Agricultura y pesca",
                                      b14==3 ~ "Minería",
                                      b14==4 ~ "Industria",
                                      b14==5 ~ "Electricidad,  gas  y  agua",
                                      b14==6 ~ "Construcción",
                                      b14==7 ~ "Comercio",
                                      b14==8 ~ "Comercio",
                                      b14==9 ~ "Transporte y comunicaciones",
                                      b14==10 ~ "Establecimientos financieros",
                                      b14==11 ~ "Establecimientos financieros",
                                      b14==12 ~ "Servicios",
                                      b14==13 ~ "Servicios",
                                      b14==14 ~ "Servicios",
                                      b14==15 ~ "Servicios",
                                      b14==16 ~ "Servicios",
                                      b14==17 ~ "Servicios"))

# 2013 
ene3 <- ene3 %>% mutate(b14_rev4cl_caenes=case_when(b14_rev4cl_caenes==1 ~ "Agricultura y pesca",
                                                    b14_rev4cl_caenes==2 ~ "Minería",
                                                    b14_rev4cl_caenes==3 ~ "Industria",
                                                    b14_rev4cl_caenes==4 ~ "Electricidad,  gas  y  agua",
                                                    b14_rev4cl_caenes==5 ~ "Electricidad,  gas  y  agua", 
                                                    b14_rev4cl_caenes==6 ~ "Construcción",
                                                    b14_rev4cl_caenes==7 ~ "Comercio",
                                                    b14_rev4cl_caenes==8 ~ "Transporte y comunicaciones",
                                                    b14_rev4cl_caenes==9 ~ "Servicios",
                                                    b14_rev4cl_caenes==10 ~ "Transporte y comunicaciones",
                                                    b14_rev4cl_caenes==11 ~ "Establecimientos financieros",
                                                    b14_rev4cl_caenes==12 ~ "Establecimientos financieros",
                                                    b14_rev4cl_caenes==13 ~ "Servicios", 
                                                    b14_rev4cl_caenes==14 ~ "Servicios",
                                                    b14_rev4cl_caenes==15 ~ "Servicios", 
                                                    b14_rev4cl_caenes==16 ~ "Servicios", 
                                                    b14_rev4cl_caenes==17 ~ "Servicios",
                                                    b14_rev4cl_caenes==18 ~ "Servicios",
                                                    b14_rev4cl_caenes==19 ~ "Servicios",
                                                    b14_rev4cl_caenes==20 ~ "Servicios",
                                                    b14_rev4cl_caenes==21 ~ "Servicios"))


# 2014 
ene4 <- ene4 %>% mutate(b14_rev4cl_caenes=case_when(b14_rev4cl_caenes==1 ~ "Agricultura y pesca",
                                                    b14_rev4cl_caenes==2 ~ "Minería",
                                                    b14_rev4cl_caenes==3 ~ "Industria",
                                                    b14_rev4cl_caenes==4 ~ "Electricidad,  gas  y  agua",
                                                    b14_rev4cl_caenes==5 ~ "Electricidad,  gas  y  agua", 
                                                    b14_rev4cl_caenes==6 ~ "Construcción",
                                                    b14_rev4cl_caenes==7 ~ "Comercio",
                                                    b14_rev4cl_caenes==8 ~ "Transporte y comunicaciones",
                                                    b14_rev4cl_caenes==9 ~ "Servicios",
                                                    b14_rev4cl_caenes==10 ~ "Transporte y comunicaciones",
                                                    b14_rev4cl_caenes==11 ~ "Establecimientos financieros",
                                                    b14_rev4cl_caenes==12 ~ "Establecimientos financieros",
                                                    b14_rev4cl_caenes==13 ~ "Servicios", 
                                                    b14_rev4cl_caenes==14 ~ "Servicios",
                                                    b14_rev4cl_caenes==15 ~ "Servicios", 
                                                    b14_rev4cl_caenes==16 ~ "Servicios", 
                                                    b14_rev4cl_caenes==17 ~ "Servicios",
                                                    b14_rev4cl_caenes==18 ~ "Servicios",
                                                    b14_rev4cl_caenes==19 ~ "Servicios",
                                                    b14_rev4cl_caenes==20 ~ "Servicios",
                                                    b14_rev4cl_caenes==21 ~ "Servicios"))


# 2015
ene5 <- ene5 %>% mutate(b14_rev4cl_caenes=case_when(b14_rev4cl_caenes==1 ~ "Agricultura y pesca",
                                                    b14_rev4cl_caenes==2 ~ "Minería",
                                                    b14_rev4cl_caenes==3 ~ "Industria",
                                                    b14_rev4cl_caenes==4 ~ "Electricidad,  gas  y  agua",
                                                    b14_rev4cl_caenes==5 ~ "Electricidad,  gas  y  agua", 
                                                    b14_rev4cl_caenes==6 ~ "Construcción",
                                                    b14_rev4cl_caenes==7 ~ "Comercio",
                                                    b14_rev4cl_caenes==8 ~ "Transporte y comunicaciones",
                                                    b14_rev4cl_caenes==9 ~ "Servicios",
                                                    b14_rev4cl_caenes==10 ~ "Transporte y comunicaciones",
                                                    b14_rev4cl_caenes==11 ~ "Establecimientos financieros",
                                                    b14_rev4cl_caenes==12 ~ "Establecimientos financieros",
                                                    b14_rev4cl_caenes==13 ~ "Servicios", 
                                                    b14_rev4cl_caenes==14 ~ "Servicios",
                                                    b14_rev4cl_caenes==15 ~ "Servicios", 
                                                    b14_rev4cl_caenes==16 ~ "Servicios", 
                                                    b14_rev4cl_caenes==17 ~ "Servicios",
                                                    b14_rev4cl_caenes==18 ~ "Servicios",
                                                    b14_rev4cl_caenes==19 ~ "Servicios",
                                                    b14_rev4cl_caenes==20 ~ "Servicios",
                                                    b14_rev4cl_caenes==21 ~ "Servicios"))


# 2016 
ene6 <- ene6 %>% mutate(b14_rev4cl_caenes=case_when(b14_rev4cl_caenes==1 ~ "Agricultura y pesca",
                                                    b14_rev4cl_caenes==2 ~ "Minería",
                                                    b14_rev4cl_caenes==3 ~ "Industria",
                                                    b14_rev4cl_caenes==4 ~ "Electricidad,  gas  y  agua",
                                                    b14_rev4cl_caenes==5 ~ "Electricidad,  gas  y  agua", 
                                                    b14_rev4cl_caenes==6 ~ "Construcción",
                                                    b14_rev4cl_caenes==7 ~ "Comercio",
                                                    b14_rev4cl_caenes==8 ~ "Transporte y comunicaciones",
                                                    b14_rev4cl_caenes==9 ~ "Servicios",
                                                    b14_rev4cl_caenes==10 ~ "Transporte y comunicaciones",
                                                    b14_rev4cl_caenes==11 ~ "Establecimientos financieros",
                                                    b14_rev4cl_caenes==12 ~ "Establecimientos financieros",
                                                    b14_rev4cl_caenes==13 ~ "Servicios", 
                                                    b14_rev4cl_caenes==14 ~ "Servicios",
                                                    b14_rev4cl_caenes==15 ~ "Servicios", 
                                                    b14_rev4cl_caenes==16 ~ "Servicios", 
                                                    b14_rev4cl_caenes==17 ~ "Servicios",
                                                    b14_rev4cl_caenes==18 ~ "Servicios",
                                                    b14_rev4cl_caenes==19 ~ "Servicios",
                                                    b14_rev4cl_caenes==20 ~ "Servicios",
                                                    b14_rev4cl_caenes==21 ~ "Servicios"))


# 2017 
ene7 <- ene7 %>% mutate(b14_rev4cl_caenes=case_when(b14_rev4cl_caenes==1 ~ "Agricultura y pesca",
                                                    b14_rev4cl_caenes==2 ~ "Minería",
                                                    b14_rev4cl_caenes==3 ~ "Industria",
                                                    b14_rev4cl_caenes==4 ~ "Electricidad,  gas  y  agua",
                                                    b14_rev4cl_caenes==5 ~ "Electricidad,  gas  y  agua", 
                                                    b14_rev4cl_caenes==6 ~ "Construcción",
                                                    b14_rev4cl_caenes==7 ~ "Comercio",
                                                    b14_rev4cl_caenes==8 ~ "Transporte y comunicaciones",
                                                    b14_rev4cl_caenes==9 ~ "Servicios",
                                                    b14_rev4cl_caenes==10 ~ "Transporte y comunicaciones",
                                                    b14_rev4cl_caenes==11 ~ "Establecimientos financieros",
                                                    b14_rev4cl_caenes==12 ~ "Establecimientos financieros",
                                                    b14_rev4cl_caenes==13 ~ "Servicios", 
                                                    b14_rev4cl_caenes==14 ~ "Servicios",
                                                    b14_rev4cl_caenes==15 ~ "Servicios", 
                                                    b14_rev4cl_caenes==16 ~ "Servicios", 
                                                    b14_rev4cl_caenes==17 ~ "Servicios",
                                                    b14_rev4cl_caenes==18 ~ "Servicios",
                                                    b14_rev4cl_caenes==19 ~ "Servicios",
                                                    b14_rev4cl_caenes==20 ~ "Servicios",
                                                    b14_rev4cl_caenes==21 ~ "Servicios"))

# 2018 
ene8 <- ene8 %>% mutate(b14_rev4cl_caenes=case_when(b14_rev4cl_caenes==1 ~ "Agricultura y pesca",
                                                    b14_rev4cl_caenes==2 ~ "Minería",
                                                    b14_rev4cl_caenes==3 ~ "Industria",
                                                    b14_rev4cl_caenes==4 ~ "Electricidad,  gas  y  agua",
                                                    b14_rev4cl_caenes==5 ~ "Electricidad,  gas  y  agua", 
                                                    b14_rev4cl_caenes==6 ~ "Construcción",
                                                    b14_rev4cl_caenes==7 ~ "Comercio",
                                                    b14_rev4cl_caenes==8 ~ "Transporte y comunicaciones",
                                                    b14_rev4cl_caenes==9 ~ "Servicios",
                                                    b14_rev4cl_caenes==10 ~ "Transporte y comunicaciones",
                                                    b14_rev4cl_caenes==11 ~ "Establecimientos financieros",
                                                    b14_rev4cl_caenes==12 ~ "Establecimientos financieros",
                                                    b14_rev4cl_caenes==13 ~ "Servicios", 
                                                    b14_rev4cl_caenes==14 ~ "Servicios",
                                                    b14_rev4cl_caenes==15 ~ "Servicios", 
                                                    b14_rev4cl_caenes==16 ~ "Servicios", 
                                                    b14_rev4cl_caenes==17 ~ "Servicios",
                                                    b14_rev4cl_caenes==18 ~ "Servicios",
                                                    b14_rev4cl_caenes==19 ~ "Servicios",
                                                    b14_rev4cl_caenes==20 ~ "Servicios",
                                                    b14_rev4cl_caenes==21 ~ "Servicios"))

# 2019 
ene9 <- ene9 %>% mutate(b14_rev4cl_caenes=case_when(b14_rev4cl_caenes==1 ~ "Agricultura y pesca",
                                                    b14_rev4cl_caenes==2 ~ "Minería",
                                                    b14_rev4cl_caenes==3 ~ "Industria",
                                                    b14_rev4cl_caenes==4 ~ "Electricidad,  gas  y  agua",
                                                    b14_rev4cl_caenes==5 ~ "Electricidad,  gas  y  agua", 
                                                    b14_rev4cl_caenes==6 ~ "Construcción",
                                                    b14_rev4cl_caenes==7 ~ "Comercio",
                                                    b14_rev4cl_caenes==8 ~ "Transporte y comunicaciones",
                                                    b14_rev4cl_caenes==9 ~ "Servicios",
                                                    b14_rev4cl_caenes==10 ~ "Transporte y comunicaciones",
                                                    b14_rev4cl_caenes==11 ~ "Establecimientos financieros",
                                                    b14_rev4cl_caenes==12 ~ "Establecimientos financieros",
                                                    b14_rev4cl_caenes==13 ~ "Servicios", 
                                                    b14_rev4cl_caenes==14 ~ "Servicios",
                                                    b14_rev4cl_caenes==15 ~ "Servicios", 
                                                    b14_rev4cl_caenes==16 ~ "Servicios", 
                                                    b14_rev4cl_caenes==17 ~ "Servicios",
                                                    b14_rev4cl_caenes==18 ~ "Servicios",
                                                    b14_rev4cl_caenes==19 ~ "Servicios",
                                                    b14_rev4cl_caenes==20 ~ "Servicios",
                                                    b14_rev4cl_caenes==21 ~ "Servicios"))

# 4.2. Diseño muestral ----

# 2010
ene0 %>% filter(b5==2 | b5==3 & (b2==2|b2==3)) %>% select(fact_cal) %>% sum() # CÁLCULO SIN AGRUPAR POR RAMA

ene0_pond <- ene0  %>% as_survey_design(ids = 1, strata = estrato, weights = fact_cal) # FACTORES DE EXPANSION

h <- ene0_pond %>% 
  group_by(activ) %>%  filter(b5==2 | b5==3 & (b2==2|b2==3)) %>%
  summarize(frecuencias = survey_total(na.rm = T),
            proporcion = survey_mean(vartype = "ci", level = 0.95, na.rm = TRUE),
            n = unweighted(n())) # COMPROBAR 

h <- ene0_pond %>% 
  group_by(b14) %>%  filter(b5==2 | b5==3 & (b2==2|b2==3)) %>%
  summarize(frecuencias = survey_total(na.rm = T),
            proporcion = survey_mean(vartype = "ci", level = 0.95, na.rm = TRUE),
            n = unweighted(n())) # CÁLCULO AGRUPANDO POR RAMAS

# 2011
ene1 %>% filter(b5==2 | b5==3 & (b2==2|b2==3)) %>% select(fact_cal) %>% sum() # CÁLCULO SIN AGRUPAR POR RAMA

ene1_pond <- ene1  %>% as_survey_design(ids = 1, strata = estrato, weights = fact_cal) # FACTORES DE EXPANSION

i <- ene1_pond %>% 
  group_by(activ) %>%  filter(b5==2 | b5==3 & (b2==2|b2==3)) %>%
  summarize(frecuencias = survey_total(na.rm = T),
            proporcion = survey_mean(vartype = "ci", level = 0.95, na.rm = TRUE),
            n = unweighted(n())) # COMPROBAR 

i <- ene1_pond %>% 
  group_by(b14) %>%  filter(b5==2 | b5==3 & (b2==2|b2==3)) %>%
  summarize(frecuencias = survey_total(na.rm = T),
            proporcion = survey_mean(vartype = "ci", level = 0.95, na.rm = TRUE),
            n = unweighted(n())) # CÁLCULO AGRUPANDO POR RAMAS

# 2012
ene2 %>% filter(b5==2 | b5==3 & (b2==2|b2==3)) %>% select(fact_cal) %>% sum() # CÁLCULO SIN AGRUPAR POR RAMA

ene2_pond <- ene2  %>% as_survey_design(ids = 1, strata = estrato, weights = fact_cal) # FACTORES DE EXPANSION

j <- ene2_pond %>% 
  group_by(activ) %>%  filter(b5==2 | b5==3 & (b2==2|b2==3)) %>%
  summarize(frecuencias = survey_total(na.rm = T),
            proporcion = survey_mean(vartype = "ci", level = 0.95, na.rm = TRUE),
            n = unweighted(n())) # COMPROBAR 

j <- ene2_pond %>% 
  group_by(b14) %>%  filter(b5==2 | b5==3 & (b2==2|b2==3)) %>%
  summarize(frecuencias = survey_total(na.rm = T),
            proporcion = survey_mean(vartype = "ci", level = 0.95, na.rm = TRUE),
            n = unweighted(n())) # CÁLCULO AGRUPANDO POR RAMAS

# 2013
ene3 %>% filter(b5==2 | b5==3 & (b2==2|b2==3)) %>% select(fact_cal) %>% sum() # CÁLCULO SIN AGRUPAR POR RAMA

ene3_pond <- ene3  %>% as_survey_design(ids = 1, strata = estrato, weights = fact_cal) # FACTORES DE EXPANSION

a <- ene3_pond %>% 
  group_by(activ) %>%  filter(b5==2 | b5==3 & (b2==2|b2==3)) %>%
  summarize(frecuencias = survey_total(na.rm = T),
            proporcion = survey_mean(vartype = "ci", level = 0.95, na.rm = TRUE),
            n = unweighted(n())) # COMPROBAR 

a <- ene3_pond %>% 
  group_by(b14_rev4cl_caenes) %>%  filter(b5==2 | b5==3 & (b2==2|b2==3)) %>%
  summarize(frecuencias = survey_total(na.rm = T),
            proporcion = survey_mean(vartype = "ci", level = 0.95, na.rm = TRUE),
            n = unweighted(n())) # CÁLCULO AGRUPANDO POR RAMAS

# 2014
ene4 %>% filter(b5==2 | b5==3 & (b2==2|b2==3)) %>% select(fact_cal) %>% sum() # CÁLCULO SIN AGRUPAR POR RAMA

ene4_pond <- ene4  %>% as_survey_design(ids = 1, strata = estrato, weights = fact_cal) # FACTORES DE EXPANSION

b <- ene4_pond %>% 
  group_by(activ) %>%  filter(b5==2 | b5==3 & (b2==2|b2==3)) %>%
  summarize(frecuencias = survey_total(na.rm = T),
            proporcion = survey_mean(vartype = "ci", level = 0.95, na.rm = TRUE),
            n = unweighted(n())) # COMPROBAR 

b <- ene4_pond %>% 
  group_by(b14_rev4cl_caenes) %>%  filter(b5==2 | b5==3 & (b2==2|b2==3)) %>%
  summarize(frecuencias = survey_total(na.rm = T),
            proporcion = survey_mean(vartype = "ci", level = 0.95, na.rm = TRUE),
            n = unweighted(n())) # CÁLCULO AGRUPANDO POR RAMAS

# 2015
ene5 %>% filter(b5==2 | b5==3 & (b2==2|b2==3)) %>% select(fact_cal) %>% sum() # CÁLCULO SIN AGRUPAR POR RAMA

ene5_pond <- ene5  %>% as_survey_design(ids = 1, strata = estrato, weights = fact_cal) # FACTORES DE EXPANSION

c <- ene5_pond %>% 
  group_by(activ) %>%  filter(b5==2 | b5==3 & (b2==2|b2==3)) %>%
  summarize(frecuencias = survey_total(na.rm = T),
            proporcion = survey_mean(vartype = "ci", level = 0.95, na.rm = TRUE),
            n = unweighted(n())) # COMPROBAR 

c <- ene5_pond %>% 
  group_by(b14_rev4cl_caenes) %>%  filter(b5==2 | b5==3 & (b2==2|b2==3)) %>%
  summarize(frecuencias = survey_total(na.rm = T),
            proporcion = survey_mean(vartype = "ci", level = 0.95, na.rm = TRUE),
            n = unweighted(n())) # CÁLCULO AGRUPANDO POR RAMAS

# 2016
ene6 %>% filter(b5==2 | b5==3 & (b2==2|b2==3)) %>% select(fact_cal) %>% sum() # CÁLCULO SIN AGRUPAR POR RAMA

ene6_pond <- ene6  %>% as_survey_design(ids = 1, strata = estrato, weights = fact_cal) # FACTORES DE EXPANSION

d <- ene6_pond %>% 
  group_by(activ) %>%  filter(b5==2 | b5==3 & (b2==2|b2==3)) %>%
  summarize(frecuencias = survey_total(na.rm = T),
            proporcion = survey_mean(vartype = "ci", level = 0.95, na.rm = TRUE),
            n = unweighted(n())) # COMPROBAR 

d <- ene6_pond %>% 
  group_by(b14_rev4cl_caenes) %>%  filter(b5==2 | b5==3 & (b2==2|b2==3)) %>%
  summarize(frecuencias = survey_total(na.rm = T),
            proporcion = survey_mean(vartype = "ci", level = 0.95, na.rm = TRUE),
            n = unweighted(n())) # CÁLCULO AGRUPANDO POR RAMAS

# 2017
ene7 %>% filter(b5==2 | b5==3 & (b2==2|b2==3)) %>% select(fact_cal) %>% sum() # CÁLCULO SIN AGRUPAR POR RAMA

ene7_pond <- ene7  %>% as_survey_design(ids = 1, strata = estrato, weights = fact_cal) # FACTORES DE EXPANSION

e <- ene7_pond %>% 
  group_by(activ) %>%  filter(b5==2 | b5==3 & (b2==2|b2==3)) %>%
  summarize(frecuencias = survey_total(na.rm = T),
            proporcion = survey_mean(vartype = "ci", level = 0.95, na.rm = TRUE),
            n = unweighted(n())) # COMPROBAR 

e <- ene7_pond %>% 
  group_by(b14_rev4cl_caenes) %>%  filter(b5==2 | b5==3 & (b2==2|b2==3)) %>%
  summarize(frecuencias = survey_total(na.rm = T),
            proporcion = survey_mean(vartype = "ci", level = 0.95, na.rm = TRUE),
            n = unweighted(n())) # CÁLCULO AGRUPANDO POR RAMAS

# 2018
ene8 %>% filter(b5==2 | b5==3 & (b2==2|b2==3)) %>% select(fact_cal) %>% sum() # CÁLCULO SIN AGRUPAR POR RAMA

ene8_pond <- ene8  %>% as_survey_design(ids = 1, strata = estrato, weights = fact_cal) # FACTORES DE EXPANSION

f <- ene8_pond %>% 
  group_by(activ) %>%  filter(b5==2 | b5==3 & (b2==2|b2==3)) %>%
  summarize(frecuencias = survey_total(na.rm = T),
            proporcion = survey_mean(vartype = "ci", level = 0.95, na.rm = TRUE),
            n = unweighted(n())) # COMPROBAR 

f <- ene8_pond %>% 
  group_by(b14_rev4cl_caenes) %>%  filter(b5==2 | b5==3 & (b2==2|b2==3)) %>%
  summarize(frecuencias = survey_total(na.rm = T),
            proporcion = survey_mean(vartype = "ci", level = 0.95, na.rm = TRUE),
            n = unweighted(n())) # CÁLCULO AGRUPANDO POR RAMAS

# 2019
ene9 %>% filter(b5==2 | b5==3 & (b2==2|b2==3)) %>% select(fact_cal) %>% sum() # CÁLCULO SIN AGRUPAR POR RAMA

ene9_pond <- ene9  %>% as_survey_design(ids = 1, strata = estrato, weights = fact_cal) # FACTORES DE EXPANSION

g <- ene9_pond %>% 
  group_by(activ) %>%  filter(b5==2 | b5==3 & (b2==2|b2==3)) %>%
  summarize(frecuencias = survey_total(na.rm = T),
            proporcion = survey_mean(vartype = "ci", level = 0.95, na.rm = TRUE),
            n = unweighted(n())) # COMPROBAR 

g <- ene9_pond %>% 
  group_by(b14_rev4cl_caenes) %>%  filter(b5==2 | b5==3 & (b2==2|b2==3)) %>%
  summarize(frecuencias = survey_total(na.rm = T),
            proporcion = survey_mean(vartype = "ci", level = 0.95, na.rm = TRUE),
            n = unweighted(n())) # CÁLCULO AGRUPANDO POR RAMAS

# 5. Unir data ---- 

# 5.1. Limpiar data ponderada ----
a <- a[-c(3,4,5,6,7)]
b <- b[-c(3,4,5,6,7)]
c <- c[-c(3,4,5,6,7)]
d <- d[-c(3,4,5,6,7)]
e <- e[-c(3,4,5,6,7)]
f <- f[-c(3,4,5,6,7)]
g <- g[-c(3,4,5,6,7)] 
# AÑOS 2010 A 2012 
h <- h[-c(3,4,5,6,7)]
i <- i[-c(3,4,5,6,7)]
j <- j[-c(3,4,5,6,7)]

# 5.2. Names ---- 
names(a)<-c("Rama.Actividad.Económica","2013")
names(b)<-c("Rama.Actividad.Económica","2014")
names(c)<-c("Rama.Actividad.Económica","2015")
names(d)<-c("Rama.Actividad.Económica","2016")
names(e)<-c("Rama.Actividad.Económica","2017")
names(f)<-c("Rama.Actividad.Económica","2018")
names(g)<-c("Rama.Actividad.Económica","2019")
# AÑOS 2010 A 2012
names(h)<-c("Rama.Actividad.Económica","2010")
names(i)<-c("Rama.Actividad.Económica","2011")
names(j)<-c("Rama.Actividad.Económica","2012")

# 5.3. Merge ----
cuadro2013_2014 <- merge(a, b, by = "Rama.Actividad.Económica", all.x = T)
cuadro2015_2016 <- merge(c, d, by = "Rama.Actividad.Económica", all.x = T)
cuadro2017_2018 <- merge(e, f, by = "Rama.Actividad.Económica", all.x = T)
cuadro2013_2019 <- merge(cuadro2013_2014, cuadro2015_2016, by = "Rama.Actividad.Económica", all.x = T)
cuadro2013_2019 <- merge(cuadro2013_2019, cuadro2017_2018, by = "Rama.Actividad.Económica", all.x = T)
cuadro2013_2019 <- merge(cuadro2013_2019, g, by = "Rama.Actividad.Económica", all.x = T)
# AÑOS 2010 A 2012
cuadro2010_2011 <- merge(h, i, by = "Rama.Actividad.Económica", all.x = T)
cuadro2010_2012 <- merge(cuadro2010_2011, j, by = "Rama.Actividad.Económica", all.x = T)
cuadro2010_2019 <- merge(cuadro2010_2012, cuadro2013_2019, by = "Rama.Actividad.Económica", all.x = T)

cuadro2010_2019<- cuadro2010_2019 %>% add_row(Rama.Actividad.Económica="Total",
                                   `2010`=sum(.$`2010`[1]+.$`2010`[2]+.$`2010`[3]+.$`2010`[4]+.$`2010`[5]+.$`2010`[6]+.$`2010`[7]+.$`2010`[8]+.$`2010`[9]),         
                                   `2011`=sum(.$`2011`[1]+.$`2011`[2]+.$`2011`[3]+.$`2011`[4]+.$`2011`[5]+.$`2011`[6]+.$`2011`[7]+.$`2011`[8]+.$`2011`[9]),
                                   `2012`=sum(.$`2012`[1]+.$`2012`[2]+.$`2012`[3]+.$`2012`[4]+.$`2012`[5]+.$`2012`[6]+.$`2012`[7]+.$`2012`[8]+.$`2012`[9]),
                                   `2013`=sum(.$`2013`[1]+.$`2013`[2]+.$`2013`[3]+.$`2013`[4]+.$`2013`[5]+.$`2013`[6]+.$`2013`[7]+.$`2013`[8]+.$`2013`[9]),
                                   `2014`=sum(.$`2014`[1]+.$`2014`[2]+.$`2014`[3]+.$`2014`[4]+.$`2014`[5]+.$`2014`[6]+.$`2014`[7]+.$`2014`[8]+.$`2014`[9]),
                                   `2015`=sum(.$`2015`[1]+.$`2015`[2]+.$`2015`[3]+.$`2015`[4]+.$`2015`[5]+.$`2015`[6]+.$`2015`[7]+.$`2015`[8]+.$`2015`[9]),
                                   `2016`=sum(.$`2016`[1]+.$`2016`[2]+.$`2016`[3]+.$`2016`[4]+.$`2016`[5]+.$`2016`[6]+.$`2016`[7]+.$`2016`[8]+.$`2016`[9]),
                                   `2017`=sum(.$`2017`[1]+.$`2017`[2]+.$`2017`[3]+.$`2017`[4]+.$`2017`[5]+.$`2017`[6]+.$`2017`[7]+.$`2017`[8]+.$`2017`[9]),
                                   `2018`=sum(.$`2018`[1]+.$`2018`[2]+.$`2018`[3]+.$`2018`[4]+.$`2018`[5]+.$`2018`[6]+.$`2018`[7]+.$`2018`[8]+.$`2018`[9]),
                                   `2019`=sum(.$`2019`[1]+.$`2019`[2]+.$`2019`[3]+.$`2019`[4]+.$`2019`[5]+.$`2019`[6]+.$`2019`[7]+.$`2019`[8]+.$`2019`[9]))

# 6. Export ----
save(cuadro2010_2019, file= "input/cuadro2010_2019.RData")
write_xlsx(cuadro2010_2019,"output/cuadro2010_2019.xlsx", col_names = TRUE,format_headers = TRUE)