# Code 3: Process ENE by sex 

# 1. Cargar librerias ----
pacman::p_load(tidyverse, rvest, xml2, lubridate, openxlsx, readxl, ggrepel, tibble, writexl, haven,
               dplyr, car, summarytools, ggpubr, sjmisc, sjlabelled, stargazer, srvyr)
options(scipen=999)

# 2.Cargar bases de datos ---- 
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

# 3. Procesamiento ----

# 2010 
ene0$sexo <- as.factor(ene0$sexo)
ene0$b2<- as.factor(ene0$b2)
ene0$b5 <- as.factor(ene0$b5)
ene0$activ <- as.factor(ene0$activ)
ene0$b14 <- as_factor(ene0$b14)

# 2011
ene1$sexo <- as.factor(ene1$sexo)
ene1$b2<- as.factor(ene1$b2)
ene1$b5 <- as.factor(ene1$b5)
ene1$activ <- as.factor(ene1$activ)
ene1$b14 <- as_factor(ene1$b14)

# 2012
ene2$sexo <- as.factor(ene2$sexo)
ene2$b2<- as.factor(ene2$b2)
ene2$b5 <- as.factor(ene2$b5)
ene2$activ <- as.factor(ene2$activ)
ene2$b14 <- as_factor(ene2$b14)

# 2013
ene3$sexo <- as.factor(ene3$sexo)
ene3$b2<- as.factor(ene3$b2)
ene3$b5 <- as.factor(ene3$b5)
ene3$activ <- as.factor(ene3$activ)
ene3$b14_rev4cl_caenes <- as_factor(ene3$b14_rev4cl_caenes)

# 2014
ene4$sexo <- as.factor(ene4$sexo)
ene4$b2<- as.factor(ene4$b2)
ene4$b5 <- as.factor(ene4$b5)
ene4$activ <- as.factor(ene4$activ)
ene4$b14_rev4cl_caenes <- as_factor(ene4$b14_rev4cl_caenes)

# 2015
ene5$sexo <- as.factor(ene5$sexo)
ene5$b2<- as.factor(ene5$b2)
ene5$b5 <- as.factor(ene5$b5)
ene5$activ <- as.factor(ene5$activ)
ene5$b14_rev4cl_caenes <- as_factor(ene5$b14_rev4cl_caenes)

# 2016
ene6$sexo <- as.factor(ene6$sexo)
ene6$b2<- as.factor(ene6$b2)
ene6$b5 <- as.factor(ene6$b5)
ene6$activ <- as.factor(ene6$activ)
ene6$b14_rev4cl_caenes <- as_factor(ene6$b14_rev4cl_caenes)

# 2017
ene7$sexo <- as.factor(ene7$sexo)
ene7$b2<- as.factor(ene7$b2)
ene7$b5 <- as.factor(ene7$b5)
ene7$activ <- as.factor(ene7$activ)
ene7$b14_rev4cl_caenes <- as_factor(ene7$b14_rev4cl_caenes)

# 2018
ene8$sexo <- as.factor(ene8$sexo)
ene8$b2<- as.factor(ene8$b2)
ene8$b5 <- as.factor(ene8$b5)
ene8$activ <- as.factor(ene8$activ)
ene8$b14_rev4cl_caenes <- as_factor(ene8$b14_rev4cl_caenes)

# 2019
ene9$sexo <- as.factor(ene9$sexo)
ene9$b2<- as.factor(ene9$b2)
ene9$b5 <- as.factor(ene9$b5)
ene9$activ <- as.factor(ene9$activ)
ene9$b14_rev4cl_caenes <- as_factor(ene9$b14_rev4cl_caenes)

# 3.1. Recodificar rama actividad económica ---- 

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

# 4. Diseño muestral ----

# 4.1. Mujeres

# 2010
ene0 %>% filter(b5==2 | b5==3 & (b2==2|b2==3)) %>% filter(sexo==2) %>% select(fact_cal) %>% sum()

ene0_pond <- ene0 %>% as_survey_design(ids = 1, strata = estrato, weights = fact_cal) # FACTORES DE EXPANSION

h <- ene0_pond %>% 
  group_by(activ) %>%  filter(b5==2 | b5==3 & (b2==2|b2==3)) %>% filter(sexo==2) %>%
  summarize(frecuencias = survey_total(na.rm = T),
            proporcion = survey_mean(vartype = "ci", level = 0.95, na.rm = TRUE),
            n = unweighted(n())) # COMPROBAR 

h <- ene0_pond %>% 
  group_by(b14) %>%  filter(b5==2 | b5==3 & (b2==2|b2==3)) %>% filter(sexo==2) %>%
  summarize(frecuencias = survey_total(na.rm = T),
            proporcion = survey_mean(vartype = "ci", level = 0.95, na.rm = TRUE),
            n = unweighted(n())) # CÁLCULO AGRUPANDO POR RAMAS

# 2011
ene1 %>% filter(b5==2 | b5==3 & (b2==2|b2==3)) %>% filter(sexo==2) %>% select(fact_cal) %>% sum() # CÁLCULO SIN AGRUPAR POR RAMA

ene1_pond <- ene1  %>% as_survey_design(ids = 1, strata = estrato, weights = fact_cal) # FACTORES DE EXPANSION

i <- ene1_pond %>% 
  group_by(activ) %>%  filter(b5==2 | b5==3 & (b2==2|b2==3)) %>% filter(sexo==2) %>%
  summarize(frecuencias = survey_total(na.rm = T),
            proporcion = survey_mean(vartype = "ci", level = 0.95, na.rm = TRUE),
            n = unweighted(n())) # COMPROBAR 

i <- ene1_pond %>% 
  group_by(b14) %>%  filter(b5==2 | b5==3 & (b2==2|b2==3)) %>% filter(sexo==2) %>%
  summarize(frecuencias = survey_total(na.rm = T),
            proporcion = survey_mean(vartype = "ci", level = 0.95, na.rm = TRUE),
            n = unweighted(n())) # CÁLCULO AGRUPANDO POR RAMAS

# 2012
ene2 %>% filter(b5==2 | b5==3 & (b2==2|b2==3)) %>% filter(sexo==2) %>% select(fact_cal) %>% sum() # CÁLCULO SIN AGRUPAR POR RAMA

ene2_pond <- ene2  %>% as_survey_design(ids = 1, strata = estrato, weights = fact_cal) # FACTORES DE EXPANSION

j <- ene2_pond %>% 
  group_by(activ) %>%  filter(b5==2 | b5==3 & (b2==2|b2==3)) %>% filter(sexo==2) %>%
  summarize(frecuencias = survey_total(na.rm = T),
            proporcion = survey_mean(vartype = "ci", level = 0.95, na.rm = TRUE),
            n = unweighted(n())) # COMPROBAR 

j <- ene2_pond %>% 
  group_by(b14) %>%  filter(b5==2 | b5==3 & (b2==2|b2==3)) %>% filter(sexo==2) %>%
  summarize(frecuencias = survey_total(na.rm = T),
            proporcion = survey_mean(vartype = "ci", level = 0.95, na.rm = TRUE),
            n = unweighted(n())) # CÁLCULO AGRUPANDO POR RAMAS

# 2013
ene3 %>% filter(b5==2 | b5==3 & (b2==2|b2==3)) %>% filter(sexo==2) %>% select(fact_cal) %>% sum() # CÁLCULO SIN AGRUPAR POR RAMA

ene3_pond <- ene3  %>% as_survey_design(ids = 1, strata = estrato, weights = fact_cal) # FACTORES DE EXPANSION

a <- ene3_pond %>% 
  group_by(activ) %>%  filter(b5==2 | b5==3 & (b2==2|b2==3)) %>% filter(sexo==2) %>%
  summarize(frecuencias = survey_total(na.rm = T),
            proporcion = survey_mean(vartype = "ci", level = 0.95, na.rm = TRUE),
            n = unweighted(n())) # COMPROBAR 

a <- ene3_pond %>% 
  group_by(b14_rev4cl_caenes) %>%  filter(b5==2 | b5==3 & (b2==2|b2==3)) %>% filter(sexo==2) %>%
  summarize(frecuencias = survey_total(na.rm = T),
            proporcion = survey_mean(vartype = "ci", level = 0.95, na.rm = TRUE),
            n = unweighted(n())) # CÁLCULO AGRUPANDO POR RAMAS

# 2014
ene4 %>% filter(b5==2 | b5==3 & (b2==2|b2==3)) %>% filter(sexo==2) %>% select(fact_cal) %>% sum() # CÁLCULO SIN AGRUPAR POR RAMA

ene4_pond <- ene4  %>% as_survey_design(ids = 1, strata = estrato, weights = fact_cal) # FACTORES DE EXPANSION

b <- ene4_pond %>% 
  group_by(activ) %>%  filter(b5==2 | b5==3 & (b2==2|b2==3)) %>% filter(sexo==2) %>%
  summarize(frecuencias = survey_total(na.rm = T),
            proporcion = survey_mean(vartype = "ci", level = 0.95, na.rm = TRUE),
            n = unweighted(n())) # COMPROBAR 

b <- ene4_pond %>% 
  group_by(b14_rev4cl_caenes) %>%  filter(b5==2 | b5==3 & (b2==2|b2==3)) %>% filter(sexo==2) %>%
  summarize(frecuencias = survey_total(na.rm = T),
            proporcion = survey_mean(vartype = "ci", level = 0.95, na.rm = TRUE),
            n = unweighted(n())) # CÁLCULO AGRUPANDO POR RAMAS

# 2015
ene5 %>% filter(b5==2 | b5==3 & (b2==2|b2==3)) %>% filter(sexo==2) %>% select(fact_cal) %>% sum() # CÁLCULO SIN AGRUPAR POR RAMA

ene5_pond <- ene5  %>% as_survey_design(ids = 1, strata = estrato, weights = fact_cal) # FACTORES DE EXPANSION

c <- ene5_pond %>% 
  group_by(activ) %>%  filter(b5==2 | b5==3 & (b2==2|b2==3)) %>% filter(sexo==2) %>%
  summarize(frecuencias = survey_total(na.rm = T),
            proporcion = survey_mean(vartype = "ci", level = 0.95, na.rm = TRUE),
            n = unweighted(n())) # COMPROBAR 

c <- ene5_pond %>% 
  group_by(b14_rev4cl_caenes) %>%  filter(b5==2 | b5==3 & (b2==2|b2==3)) %>% filter(sexo==2) %>%
  summarize(frecuencias = survey_total(na.rm = T),
            proporcion = survey_mean(vartype = "ci", level = 0.95, na.rm = TRUE),
            n = unweighted(n())) # CÁLCULO AGRUPANDO POR RAMAS

# 2016
ene6 %>% filter(b5==2 | b5==3 & (b2==2|b2==3)) %>% filter(sexo==2) %>% select(fact_cal) %>% sum() # CÁLCULO SIN AGRUPAR POR RAMA

ene6_pond <- ene6  %>% as_survey_design(ids = 1, strata = estrato, weights = fact_cal) # FACTORES DE EXPANSION

d <- ene6_pond %>% 
  group_by(activ) %>%  filter(b5==2 | b5==3 & (b2==2|b2==3)) %>% filter(sexo==2) %>%
  summarize(frecuencias = survey_total(na.rm = T),
            proporcion = survey_mean(vartype = "ci", level = 0.95, na.rm = TRUE),
            n = unweighted(n())) # COMPROBAR 

d <- ene6_pond %>% 
  group_by(b14_rev4cl_caenes) %>%  filter(b5==2 | b5==3 & (b2==2|b2==3)) %>% filter(sexo==2) %>%
  summarize(frecuencias = survey_total(na.rm = T),
            proporcion = survey_mean(vartype = "ci", level = 0.95, na.rm = TRUE),
            n = unweighted(n())) # CÁLCULO AGRUPANDO POR RAMAS

# 2017
ene7 %>% filter(b5==2 | b5==3 & (b2==2|b2==3)) %>% filter(sexo==2) %>% select(fact_cal) %>% sum() # CÁLCULO SIN AGRUPAR POR RAMA

ene7_pond <- ene7  %>% as_survey_design(ids = 1, strata = estrato, weights = fact_cal) # FACTORES DE EXPANSION

e <- ene7_pond %>% 
  group_by(activ) %>%  filter(b5==2 | b5==3 & (b2==2|b2==3)) %>% filter(sexo==2) %>%
  summarize(frecuencias = survey_total(na.rm = T),
            proporcion = survey_mean(vartype = "ci", level = 0.95, na.rm = TRUE),
            n = unweighted(n())) # COMPROBAR 

e <- ene7_pond %>% 
  group_by(b14_rev4cl_caenes) %>%  filter(b5==2 | b5==3 & (b2==2|b2==3)) %>% filter(sexo==2) %>%
  summarize(frecuencias = survey_total(na.rm = T),
            proporcion = survey_mean(vartype = "ci", level = 0.95, na.rm = TRUE),
            n = unweighted(n())) # CÁLCULO AGRUPANDO POR RAMAS

# 2018
ene8 %>% filter(b5==2 | b5==3 & (b2==2|b2==3)) %>% filter(sexo==2) %>% select(fact_cal) %>% sum() # CÁLCULO SIN AGRUPAR POR RAMA

ene8_pond <- ene8  %>% as_survey_design(ids = 1, strata = estrato, weights = fact_cal) # FACTORES DE EXPANSION

f <- ene8_pond %>% 
  group_by(activ) %>%  filter(b5==2 | b5==3 & (b2==2|b2==3)) %>% filter(sexo==2) %>%
  summarize(frecuencias = survey_total(na.rm = T),
            proporcion = survey_mean(vartype = "ci", level = 0.95, na.rm = TRUE),
            n = unweighted(n())) # COMPROBAR 

f <- ene8_pond %>% 
  group_by(b14_rev4cl_caenes) %>%  filter(b5==2 | b5==3 & (b2==2|b2==3)) %>% filter(sexo==2) %>%
  summarize(frecuencias = survey_total(na.rm = T),
            proporcion = survey_mean(vartype = "ci", level = 0.95, na.rm = TRUE),
            n = unweighted(n())) # CÁLCULO AGRUPANDO POR RAMAS

# 2019
ene9 %>% filter(b5==2 | b5==3 & (b2==2|b2==3)) %>% filter(sexo==2) %>% select(fact_cal) %>% sum() # CÁLCULO SIN AGRUPAR POR RAMA

ene9_pond <- ene9  %>% as_survey_design(ids = 1, strata = estrato, weights = fact_cal) # FACTORES DE EXPANSION

g <- ene9_pond %>% 
  group_by(activ) %>%  filter(b5==2 | b5==3 & (b2==2|b2==3)) %>% filter(sexo==2) %>%
  summarize(frecuencias = survey_total(na.rm = T),
            proporcion = survey_mean(vartype = "ci", level = 0.95, na.rm = TRUE),
            n = unweighted(n())) # COMPROBAR 

g <- ene9_pond %>% 
  group_by(b14_rev4cl_caenes) %>%  filter(b5==2 | b5==3 & (b2==2|b2==3)) %>% filter(sexo==2) %>%
  summarize(frecuencias = survey_total(na.rm = T),
            proporcion = survey_mean(vartype = "ci", level = 0.95, na.rm = TRUE),
            n = unweighted(n())) # CÁLCULO AGRUPANDO POR RAMAS

# 4.2. Hombres

# 2010
ene0 %>% filter(b5==2 | b5==3 & (b2==2|b2==3)) %>% filter(sexo==1) %>% select(fact_cal) %>% sum()

ene0_pond <- ene0 %>% as_survey_design(ids = 1, strata = estrato, weights = fact_cal) # FACTORES DE EXPANSION

k <- ene0_pond %>% 
  group_by(activ) %>%  filter(b5==2 | b5==3 & (b2==2|b2==3)) %>% filter(sexo==1) %>%
  summarize(frecuencias = survey_total(na.rm = T),
            proporcion = survey_mean(vartype = "ci", level = 0.95, na.rm = TRUE),
            n = unweighted(n())) # COMPROBAR 

k <- ene0_pond %>% 
  group_by(b14) %>%  filter(b5==2 | b5==3 & (b2==2|b2==3)) %>% filter(sexo==1) %>%
  summarize(frecuencias = survey_total(na.rm = T),
            proporcion = survey_mean(vartype = "ci", level = 0.95, na.rm = TRUE),
            n = unweighted(n())) # CÁLCULO AGRUPANDO POR RAMAS

# 2011
ene1 %>% filter(b5==2 | b5==3 & (b2==2|b2==3)) %>% filter(sexo==1) %>% select(fact_cal) %>% sum() # CÁLCULO SIN AGRUPAR POR RAMA

ene1_pond <- ene1  %>% as_survey_design(ids = 1, strata = estrato, weights = fact_cal) # FACTORES DE EXPANSION

l <- ene1_pond %>% 
  group_by(activ) %>%  filter(b5==2 | b5==3 & (b2==2|b2==3)) %>% filter(sexo==1) %>%
  summarize(frecuencias = survey_total(na.rm = T),
            proporcion = survey_mean(vartype = "ci", level = 0.95, na.rm = TRUE),
            n = unweighted(n())) # COMPROBAR 

l <- ene1_pond %>% 
  group_by(b14) %>%  filter(b5==2 | b5==3 & (b2==2|b2==3)) %>% filter(sexo==1) %>%
  summarize(frecuencias = survey_total(na.rm = T),
            proporcion = survey_mean(vartype = "ci", level = 0.95, na.rm = TRUE),
            n = unweighted(n())) # CÁLCULO AGRUPANDO POR RAMAS

# 2012
ene2 %>% filter(b5==2 | b5==3 & (b2==2|b2==3)) %>% filter(sexo==1) %>% select(fact_cal) %>% sum() # CÁLCULO SIN AGRUPAR POR RAMA

ene2_pond <- ene2  %>% as_survey_design(ids = 1, strata = estrato, weights = fact_cal) # FACTORES DE EXPANSION

m <- ene2_pond %>% 
  group_by(activ) %>%  filter(b5==2 | b5==3 & (b2==2|b2==3)) %>% filter(sexo==1) %>%
  summarize(frecuencias = survey_total(na.rm = T),
            proporcion = survey_mean(vartype = "ci", level = 0.95, na.rm = TRUE),
            n = unweighted(n())) # COMPROBAR 

m <- ene2_pond %>% 
  group_by(b14) %>%  filter(b5==2 | b5==3 & (b2==2|b2==3)) %>% filter(sexo==1) %>%
  summarize(frecuencias = survey_total(na.rm = T),
            proporcion = survey_mean(vartype = "ci", level = 0.95, na.rm = TRUE),
            n = unweighted(n())) # CÁLCULO AGRUPANDO POR RAMAS

# 2013
ene3 %>% filter(b5==2 | b5==3 & (b2==2|b2==3)) %>% filter(sexo==1) %>% select(fact_cal) %>% sum() # CÁLCULO SIN AGRUPAR POR RAMA

ene3_pond <- ene3  %>% as_survey_design(ids = 1, strata = estrato, weights = fact_cal) # FACTORES DE EXPANSION

n <- ene3_pond %>% 
  group_by(activ) %>%  filter(b5==2 | b5==3 & (b2==2|b2==3)) %>% filter(sexo==1) %>%
  summarize(frecuencias = survey_total(na.rm = T),
            proporcion = survey_mean(vartype = "ci", level = 0.95, na.rm = TRUE),
            n = unweighted(n())) # COMPROBAR 

n <- ene3_pond %>% 
  group_by(b14_rev4cl_caenes) %>%  filter(b5==2 | b5==3 & (b2==2|b2==3)) %>% filter(sexo==1) %>%
  summarize(frecuencias = survey_total(na.rm = T),
            proporcion = survey_mean(vartype = "ci", level = 0.95, na.rm = TRUE),
            n = unweighted(n())) # CÁLCULO AGRUPANDO POR RAMAS

# 2014
ene4 %>% filter(b5==2 | b5==3 & (b2==2|b2==3)) %>% filter(sexo==1) %>% select(fact_cal) %>% sum() # CÁLCULO SIN AGRUPAR POR RAMA

ene4_pond <- ene4  %>% as_survey_design(ids = 1, strata = estrato, weights = fact_cal) # FACTORES DE EXPANSION

o <- ene4_pond %>% 
  group_by(activ) %>%  filter(b5==2 | b5==3 & (b2==2|b2==3)) %>% filter(sexo==1) %>%
  summarize(frecuencias = survey_total(na.rm = T),
            proporcion = survey_mean(vartype = "ci", level = 0.95, na.rm = TRUE),
            n = unweighted(n())) # COMPROBAR 

o <- ene4_pond %>% 
  group_by(b14_rev4cl_caenes) %>%  filter(b5==2 | b5==3 & (b2==2|b2==3)) %>% filter(sexo==1) %>%
  summarize(frecuencias = survey_total(na.rm = T),
            proporcion = survey_mean(vartype = "ci", level = 0.95, na.rm = TRUE),
            n = unweighted(n())) # CÁLCULO AGRUPANDO POR RAMAS

# 2015
ene5 %>% filter(b5==2 | b5==3 & (b2==2|b2==3)) %>% filter(sexo==1) %>% select(fact_cal) %>% sum() # CÁLCULO SIN AGRUPAR POR RAMA

ene5_pond <- ene5  %>% as_survey_design(ids = 1, strata = estrato, weights = fact_cal) # FACTORES DE EXPANSION

p <- ene5_pond %>% 
  group_by(activ) %>%  filter(b5==2 | b5==3 & (b2==2|b2==3)) %>% filter(sexo==1) %>%
  summarize(frecuencias = survey_total(na.rm = T),
            proporcion = survey_mean(vartype = "ci", level = 0.95, na.rm = TRUE),
            n = unweighted(n())) # COMPROBAR 

p <- ene5_pond %>% 
  group_by(b14_rev4cl_caenes) %>%  filter(b5==2 | b5==3 & (b2==2|b2==3)) %>% filter(sexo==1) %>%
  summarize(frecuencias = survey_total(na.rm = T),
            proporcion = survey_mean(vartype = "ci", level = 0.95, na.rm = TRUE),
            n = unweighted(n())) # CÁLCULO AGRUPANDO POR RAMAS

# 2016
ene6 %>% filter(b5==2 | b5==3 & (b2==2|b2==3)) %>% filter(sexo==1) %>% select(fact_cal) %>% sum() # CÁLCULO SIN AGRUPAR POR RAMA

ene6_pond <- ene6  %>% as_survey_design(ids = 1, strata = estrato, weights = fact_cal) # FACTORES DE EXPANSION

q <- ene6_pond %>% 
  group_by(activ) %>%  filter(b5==2 | b5==3 & (b2==2|b2==3)) %>% filter(sexo==1) %>%
  summarize(frecuencias = survey_total(na.rm = T),
            proporcion = survey_mean(vartype = "ci", level = 0.95, na.rm = TRUE),
            n = unweighted(n())) # COMPROBAR 

q <- ene6_pond %>% 
  group_by(b14_rev4cl_caenes) %>%  filter(b5==2 | b5==3 & (b2==2|b2==3)) %>% filter(sexo==1) %>%
  summarize(frecuencias = survey_total(na.rm = T),
            proporcion = survey_mean(vartype = "ci", level = 0.95, na.rm = TRUE),
            n = unweighted(n())) # CÁLCULO AGRUPANDO POR RAMAS

# 2017
ene7 %>% filter(b5==2 | b5==3 & (b2==2|b2==3)) %>% filter(sexo==1) %>% select(fact_cal) %>% sum() # CÁLCULO SIN AGRUPAR POR RAMA

ene7_pond <- ene7  %>% as_survey_design(ids = 1, strata = estrato, weights = fact_cal) # FACTORES DE EXPANSION

r <- ene7_pond %>% 
  group_by(activ) %>%  filter(b5==2 | b5==3 & (b2==2|b2==3)) %>% filter(sexo==1) %>%
  summarize(frecuencias = survey_total(na.rm = T),
            proporcion = survey_mean(vartype = "ci", level = 0.95, na.rm = TRUE),
            n = unweighted(n())) # COMPROBAR 

r <- ene7_pond %>% 
  group_by(b14_rev4cl_caenes) %>%  filter(b5==2 | b5==3 & (b2==2|b2==3)) %>% filter(sexo==1) %>%
  summarize(frecuencias = survey_total(na.rm = T),
            proporcion = survey_mean(vartype = "ci", level = 0.95, na.rm = TRUE),
            n = unweighted(n())) # CÁLCULO AGRUPANDO POR RAMAS

# 2018
ene8 %>% filter(b5==2 | b5==3 & (b2==2|b2==3)) %>% filter(sexo==1) %>% select(fact_cal) %>% sum() # CÁLCULO SIN AGRUPAR POR RAMA

ene8_pond <- ene8  %>% as_survey_design(ids = 1, strata = estrato, weights = fact_cal) # FACTORES DE EXPANSION

s <- ene8_pond %>% 
  group_by(activ) %>%  filter(b5==2 | b5==3 & (b2==2|b2==3)) %>% filter(sexo==1) %>%
  summarize(frecuencias = survey_total(na.rm = T),
            proporcion = survey_mean(vartype = "ci", level = 0.95, na.rm = TRUE),
            n = unweighted(n())) # COMPROBAR 

s <- ene8_pond %>% 
  group_by(b14_rev4cl_caenes) %>%  filter(b5==2 | b5==3 & (b2==2|b2==3)) %>% filter(sexo==1) %>%
  summarize(frecuencias = survey_total(na.rm = T),
            proporcion = survey_mean(vartype = "ci", level = 0.95, na.rm = TRUE),
            n = unweighted(n())) # CÁLCULO AGRUPANDO POR RAMAS

# 2019
ene9 %>% filter(b5==2 | b5==3 & (b2==2|b2==3)) %>% filter(sexo==1) %>% select(fact_cal) %>% sum() # CÁLCULO SIN AGRUPAR POR RAMA

ene9_pond <- ene9  %>% as_survey_design(ids = 1, strata = estrato, weights = fact_cal) # FACTORES DE EXPANSION

t <- ene9_pond %>% 
  group_by(activ) %>%  filter(b5==2 | b5==3 & (b2==2|b2==3)) %>% filter(sexo==1) %>%
  summarize(frecuencias = survey_total(na.rm = T),
            proporcion = survey_mean(vartype = "ci", level = 0.95, na.rm = TRUE),
            n = unweighted(n())) # COMPROBAR 

t <- ene9_pond %>% 
  group_by(b14_rev4cl_caenes) %>%  filter(b5==2 | b5==3 & (b2==2|b2==3)) %>% filter(sexo==1) %>%
  summarize(frecuencias = survey_total(na.rm = T),
            proporcion = survey_mean(vartype = "ci", level = 0.95, na.rm = TRUE),
            n = unweighted(n())) # CÁLCULO AGRUPANDO POR RAMAS

# 5. Unir data ---- 

# 5.1. Limpiar data ponderada ----
#MUJERES
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
# HOMBRES
k <- k[-c(3,4,5,6,7)]
l <- l[-c(3,4,5,6,7)]
m <- m[-c(3,4,5,6,7)]
n <- n[-c(3,4,5,6,7)]
o <- o[-c(3,4,5,6,7)]
p <- p[-c(3,4,5,6,7)]
q <- q[-c(3,4,5,6,7)]
r <- r[-c(3,4,5,6,7)]
s <- s[-c(3,4,5,6,7)]
t <- t[-c(3,4,5,6,7)]

# 5.2. Names ---- 
# MUJERES
names(a)<-c("Rama.Actividad.Económica","MUJ_2013")
names(b)<-c("Rama.Actividad.Económica","MUJ_2014")
names(c)<-c("Rama.Actividad.Económica","MUJ_2015")
names(d)<-c("Rama.Actividad.Económica","MUJ_2016")
names(e)<-c("Rama.Actividad.Económica","MUJ_2017")
names(f)<-c("Rama.Actividad.Económica","MUJ_2018")
names(g)<-c("Rama.Actividad.Económica","MUJ_2019")
# AÑOS 2010 A 2012
names(h)<-c("Rama.Actividad.Económica","MUJ_2010")
names(i)<-c("Rama.Actividad.Económica","MUJ_2011")
names(j)<-c("Rama.Actividad.Económica","MUJ_2012")

#HOMBRES 
# 5.2. Names ---- 
names(k)<-c("Rama.Actividad.Económica","HOMB_2010")
names(l)<-c("Rama.Actividad.Económica","HOMB_2011")
names(m)<-c("Rama.Actividad.Económica","HOMB_2012")
names(n)<-c("Rama.Actividad.Económica","HOMB_2013")
names(o)<-c("Rama.Actividad.Económica","HOMB_2014")
names(p)<-c("Rama.Actividad.Económica","HOMB_2015")
names(q)<-c("Rama.Actividad.Económica","HOMB_2016")
names(r)<-c("Rama.Actividad.Económica","HOMB_2017")
names(s)<-c("Rama.Actividad.Económica","HOMB_2018")
names(t)<-c("Rama.Actividad.Económica","HOMB_2019")

# 5.3. Merge ----
