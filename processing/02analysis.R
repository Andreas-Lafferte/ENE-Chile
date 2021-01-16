# Code 2: Analysis ENE-DT

# 1. Cargar librerias ----
pacman::p_load(tidyverse, rvest, xml2, lubridate, openxlsx, readxl, ggrepel, tibble, writexl, haven,
               dplyr, car, summarytools, ggpubr, ggplot2, sjmisc, sjlabelled, stargazer, srvyr)
options(scipen=999)

# 2. Cargar bases de datos ----

# ENE
cuadro2010-2019 <- load("input/cuadro2010_2019.RData")
ene <- cuadro2010_2019

# OOSS DT
# 2010
a <-read.xlsx("input/1. OOSS.xlsx",sheet = 14,rows=c(3:13),cols=c(1:10),na.strings = "**",colNames = FALSE)

# 2011 a 2017
b <- cbind(
  read.xlsx("input/1. OOSS.xlsx",sheet = 15,rows=c(4:22),cols=c(1:7),na.strings = "**",colNames = FALSE),
  read.xlsx("input/1. OOSS.xlsx",sheet = 16,rows=c(4:22),cols=c(2:7),na.strings = "**",colNames = FALSE),
  read.xlsx("input/1. OOSS.xlsx",sheet = 17,rows=c(4:22),cols=c(2:7),na.strings = "**",colNames = FALSE),
  read.xlsx("input/1. OOSS.xlsx",sheet = 18,rows=c(4:22),cols=c(2:4),na.strings = "**",colNames = FALSE))

# 2018
c <- read.xlsx("input/1. OOSS.xlsx",sheet = 19,rows=c(3:26),cols=c(1:4),na.strings = "**")

# 3. Procesamiento ----

# ENE
ene <- ene[-c(11)]

# 2010
names(a) <- c("Rama.Actividad.Económica", "h8","m8","t8","h9","m9","t9","h10","m10","t10")
a <- a[-c(2,3,4,5,6,7)]
a_tot <- a[-c(2,3)]
names(a_tot) <- c("Rama.Actividad.Económica", "2010")

# 2011 a 2017
names(b) <- c("Rama.Actividad.Económica","h11","m11","t11","h12","m12","t12","h13","m13","t13","h14","m14","t14",
              "h15","m15","t15","h16","m16","t16","h17","m17","t17")
b_tot <- b %>% select(1,4,7,10,13,16,19,22) # SOLO TOTALES
names(b_tot) <- c("Rama.Actividad.Económica", "2011", "2012", "2013", "2014", "2015", "2016", "2017")

# 2018 
names(c) <- c("Rama.Actividad.Económica","h18","m18","t18")
c_tot <- c %>% select(1,4) # SOLO TOTALES
names(c_tot) <- c("Rama.Actividad.Económica", "2018")

# 3.1. Ramas ----
#2010
a_tot <- a_tot %>% add_row(Rama.Actividad.Económica="Agricultura y pesca",
                           `2010`=sum(.$`2010`[1])) %>%
  
  add_row(Rama.Actividad.Económica="Transporte y comunicaciones",
          `2010`=sum(.$`2010`[7])) %>%
  
  add_row(Rama.Actividad.Económica="Establecimientos financieros",
          `2010`=sum(.$`2010`[8])) %>%
  
  add_row(Rama.Actividad.Económica="Actividades  no  especificadas",
          `2010`=sum(.$`2010`[10]))

a_tot <- a_tot[-c(8,10),]

# 2011 a 2017
cuadro1 <- b_tot %>% add_row(Rama.Actividad.Económica="Agricultura y pesca",
                                 `2011`=sum(.$`2011`[1]+.$`2011`[2]),
                                 `2012`=sum(.$`2012`[1]+.$`2012`[2]),
                                 `2013`=sum(.$`2013`[1]+.$`2013`[2]),
                                 `2014`=sum(.$`2014`[1]+.$`2014`[2]),
                                 `2015`=sum(.$`2015`[1]+.$`2015`[2]),
                                 `2016`=sum(.$`2016`[1]+.$`2016`[2]),
                                 `2017`=sum(.$`2017`[1]+.$`2017`[2])) %>% 
  
  add_row(Rama.Actividad.Económica="Minería",
          `2011`=sum(.$`2011`[3]),
          `2012`=sum(.$`2012`[3]),
          `2013`=sum(.$`2013`[3]),
          `2014`=sum(.$`2014`[3]),
          `2015`=sum(.$`2015`[3]),
          `2016`=sum(.$`2016`[3]),
          `2017`=sum(.$`2017`[3])) %>%
  
  add_row(Rama.Actividad.Económica="Industria",
          `2011`=sum(.$`2011`[4]),
          `2012`=sum(.$`2012`[4]),
          `2013`=sum(.$`2013`[4]),
          `2014`=sum(.$`2014`[4]),
          `2015`=sum(.$`2015`[4]),
          `2016`=sum(.$`2016`[4]),
          `2017`=sum(.$`2017`[4])) %>%
  
  add_row(Rama.Actividad.Económica="Electricidad,  gas  y  agua",
          `2011`=sum(.$`2011`[5]),
          `2012`=sum(.$`2012`[5]),
          `2013`=sum(.$`2013`[5]),
          `2014`=sum(.$`2014`[5]),
          `2015`=sum(.$`2015`[5]),
          `2016`=sum(.$`2016`[5]),
          `2017`=sum(.$`2017`[5])) %>%
  
  add_row(Rama.Actividad.Económica="Construcción",
          `2011`=sum(.$`2011`[6]),
          `2012`=sum(.$`2012`[6]),
          `2013`=sum(.$`2013`[6]),
          `2014`=sum(.$`2014`[6]),
          `2015`=sum(.$`2015`[6]),
          `2016`=sum(.$`2016`[6]),
          `2017`=sum(.$`2017`[6])) %>%
  
  add_row(Rama.Actividad.Económica="Comercio",
          `2011`=sum(.$`2011`[7]+.$`2011`[8]),
          `2012`=sum(.$`2012`[7]+.$`2012`[8]),
          `2013`=sum(.$`2013`[7]+.$`2013`[8]),
          `2014`=sum(.$`2014`[7]+.$`2014`[8]),
          `2015`=sum(.$`2015`[7]+.$`2015`[8]),
          `2016`=sum(.$`2016`[7]+.$`2016`[8]),
          `2017`=sum(.$`2017`[7]+.$`2017`[8])) %>%
  
  add_row(Rama.Actividad.Económica="Transporte y comunicaciones",
          `2011`=sum(.$`2011`[9]),
          `2012`=sum(.$`2012`[9]),
          `2013`=sum(.$`2013`[9]),
          `2014`=sum(.$`2014`[9]),
          `2015`=sum(.$`2015`[9]),
          `2016`=sum(.$`2016`[9]),
          `2017`=sum(.$`2017`[9])) %>%
  
  add_row(Rama.Actividad.Económica="Establecimientos financieros",
          `2011`=sum(.$`2011`[10]+.$`2011`[11]),
          `2012`=sum(.$`2012`[10]+.$`2012`[11]),
          `2013`=sum(.$`2013`[10]+.$`2013`[11]),
          `2014`=sum(.$`2014`[10]+.$`2014`[11]),
          `2015`=sum(.$`2015`[10]+.$`2015`[11]),
          `2016`=sum(.$`2016`[10]+.$`2016`[11]),
          `2017`=sum(.$`2017`[10]+.$`2017`[11])) %>%
  
  add_row(Rama.Actividad.Económica="Servicios",
          `2011`=sum(.$`2011`[12]+.$`2011`[13]+.$`2011`[14]+.$`2011`[15]+.$`2011`[17]),
          `2012`=sum(.$`2012`[12]+.$`2012`[13]+.$`2012`[14]+.$`2012`[15]+.$`2012`[16]+.$`2012`[17]),
          `2013`=sum(.$`2013`[12]+.$`2013`[13]+.$`2013`[14]+.$`2013`[15]+.$`2013`[16]+.$`2013`[17]),
          `2014`=sum(.$`2014`[12]+.$`2014`[13]+.$`2014`[14]+.$`2014`[15]+.$`2014`[16]+.$`2014`[17]),
          `2015`=sum(.$`2015`[12]+.$`2015`[13]+.$`2015`[14]+.$`2015`[15]+.$`2015`[16]+.$`2015`[17]),
          `2016`=sum(.$`2016`[12]+.$`2016`[13]+.$`2016`[14]+.$`2016`[15]+.$`2016`[16]+.$`2016`[17]),
          `2017`=sum(.$`2017`[12]+.$`2017`[13]+.$`2017`[14]+.$`2017`[15]+.$`2017`[16]+.$`2017`[17])) %>%
  
  add_row(Rama.Actividad.Económica="Actividades  no  especificadas",
          `2011`=sum(.$`2011`[18]),
          `2012`=sum(.$`2012`[18]),
          `2013`=sum(.$`2013`[18]),
          `2014`=sum(.$`2014`[18]),
          `2015`=sum(.$`2015`[18]),
          `2016`=sum(.$`2016`[18]),
          `2017`=sum(.$`2017`[18]))

cuadro1 <- cuadro1[-c(1:18),] %>% as.data.frame()

# 2018
cuadro2 <- c_tot %>% add_row(Rama.Actividad.Económica="Agricultura y pesca",
                                 `2018`=sum(.$`2018`[1])) %>% 
  
  add_row(Rama.Actividad.Económica="Minería",
          `2018`=sum(.$`2018`[2])) %>%
  
  add_row(Rama.Actividad.Económica="Industria",
          `2018`=sum(.$`2018`[3])) %>%
  
  add_row(Rama.Actividad.Económica="Electricidad,  gas  y  agua",
          `2018`=sum(.$`2018`[4]+.$`2018`[5])) %>%
  
  add_row(Rama.Actividad.Económica="Construcción",
          `2018`=sum(.$`2018`[6])) %>%
  
  add_row(Rama.Actividad.Económica="Comercio",
          `2018`=sum(.$`2018`[7]+.$`2018`[9])) %>%
  
  add_row(Rama.Actividad.Económica="Transporte y comunicaciones",
          `2018`=sum(.$`2018`[8]+.$`2018`[10])) %>%
  
  add_row(Rama.Actividad.Económica="Establecimientos financieros",
          `2018`=sum(.$`2018`[11]+.$`2018`[12])) %>%
  
  add_row(Rama.Actividad.Económica="Servicios",
          `2018`=sum(.$`2018`[13]+.$`2018`[14]+.$`2018`[15]+.$`2018`[16]+.$`2018`[17]+.$`2018`[18]+.$`2018`[19]+.$`2018`[20]+.$`2018`[21])) %>%
  
  add_row(Rama.Actividad.Económica="Actividades  no  especificadas",
          `2018`=sum(.$`2018`[22]))

cuadro2 <- cuadro2[-c(1:22),]

# 3.2. Merge ---- 
afi_tot <- merge(a_tot, cuadro1, by = "Rama.Actividad.Económica", all.x = T)
afi_tot <- merge(afi_tot, cuadro2, by = "Rama.Actividad.Económica", all.x = T)

# 3.3. Tasa sindicalización privada ---- 
tasa_sindi <- merge(ene, afi_tot, by = "Rama.Actividad.Económica", all.x = T)

names(tasa_sindi) <- c("Rama.Actividad.Económica", "ocup_2010", "ocup_2011", "ocup_2012", "ocup_2013", "ocup_2014", "ocup_2015",
                       "ocup_2016", "ocup_2017", "ocup_2018", "afi_2010", "afi_2011", "afi_2012", "afi_2013", "afi_2014", "afi_2015", 
                       "afi_2016", "afi_2017", "afi_2018")

tasa_sindi <- tasa_sindi %>% mutate(tasa_sindi_2010=afi_2010*100/(ocup_2010),
                                    tasa_sindi_2011=afi_2011*100/(ocup_2011),
                                    tasa_sindi_2012=afi_2012*100/(ocup_2012),
                                    tasa_sindi_2013=afi_2013*100/(ocup_2013),
                                    tasa_sindi_2014=afi_2014*100/(ocup_2014),
                                    tasa_sindi_2015=afi_2015*100/(ocup_2015),
                                    tasa_sindi_2016=afi_2016*100/(ocup_2016),
                                    tasa_sindi_2017=afi_2017*100/(ocup_2017),
                                    tasa_sindi_2018=afi_2018*100/(ocup_2018)) %>% as.data.frame()

tasa_sindi_INE <- tasa_sindi[-c(2:19)]
tasa_sindi_INE[,-1] <-round(tasa_sindi_INE[,-1],2)

# 4. Export ----
write_xlsx(tasa_sindi,"output/tasas_sindi_privados_rama.xlsx", col_names = TRUE,format_headers = TRUE)
write_xlsx(tasa_sindi_INE,"output/tasas_privados_INE.xlsx", col_names = TRUE,format_headers = TRUE)