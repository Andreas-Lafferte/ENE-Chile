# Code 4: Analysis ENE-DT by sex

# 1. Cargar librerias ----
pacman::p_load(tidyverse, rvest, xml2, lubridate, openxlsx, readxl, ggrepel, tibble, writexl, haven,
               dplyr, car, summarytools, ggpubr, ggplot2, sjmisc, sjlabelled, stargazer, srvyr)
options(scipen=999)

# 2. Cargar bases de datos ----

# ENE
totsex2010-2019 <- load("input/totsex2010_2019.RData")
ene <- totsex2010_2019

# OOSS DT
# 2010
a <- read.xlsx("input/1. OOSS.xlsx",sheet = 14,rows=c(3:13),cols=c(1:10),na.strings = "**",colNames = FALSE)

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
ene <- ene[-c(11,21)]

# 2010
names(a) <- c("Rama.Actividad.Económica", "h8","m8","t8","h9","m9","t9","h10","m10","t10")
a <- a[-c(2,3,4,5,6,7)]
a_sex <- a[-c(4)] # SOLO POR SEXO

# 2011 a 2017
names(b) <- c("Rama.Actividad.Económica","h11","m11","t11","h12","m12","t12","h13","m13","t13","h14","m14","t14",
              "h15","m15","t15","h16","m16","t16","h17","m17","t17")
b_sex <- b[-c(4,7,10,13,16,19,22)] # SOLO POR SEXO

# 2018 
names(c) <- c("Rama.Actividad.Económica","h18","m18","t18")
c_sex <- c[-c(4)] # SOLO POR SEXO

# 3.1. Ramas ----

#2010
a_sex <- a_sex %>% add_row(Rama.Actividad.Económica="Agricultura y pesca",
                           `h10`=sum(.$`h10`[1]),
                           `m10`=sum(.$`m10`[1])) %>%
  
  add_row(Rama.Actividad.Económica="Transporte y comunicaciones",
          `h10`=sum(.$`h10`[7]),
          `m10`=sum(.$`m10`[7])) %>%
  
  add_row(Rama.Actividad.Económica="Establecimientos financieros",
          `h10`=sum(.$`h10`[8]),
          `m10`=sum(.$`m10`[8])) %>%
  
  add_row(Rama.Actividad.Económica="Actividades  no  especificadas",
          `h10`=sum(.$`h10`[10]),
          `m10`=sum(.$`m10`[10]))

a_sex <- a_sex[-c(1,7,8,10),]

# 2011 a 2017
cuadro1 <- b_sex %>% add_row(Rama.Actividad.Económica="Agricultura y pesca",
                             `h11`=sum(.$`h11`[1]+.$`h11`[2]),
                             `m11`=sum(.$`m11`[1]+.$`m11`[2]),
                             `h12`=sum(.$`h12`[1]+.$`h12`[2]),
                             `m12`=sum(.$`m12`[1]+.$`m12`[2]),
                             `h13`=sum(.$`h13`[1]+.$`h13`[2]),
                             `m13`=sum(.$`m13`[1]+.$`m13`[2]),
                             `h14`=sum(.$`h14`[1]+.$`h14`[2]),
                             `m14`=sum(.$`m14`[1]+.$`m14`[2]),
                             `h15`=sum(.$`h15`[1]+.$`h15`[2]),
                             `m15`=sum(.$`m15`[1]+.$`m15`[2]),
                             `h16`=sum(.$`h16`[1]+.$`h16`[2]),
                             `m16`=sum(.$`m16`[1]+.$`m16`[2]),
                             `h17`=sum(.$`h17`[1]+.$`h17`[2]),
                             `m17`=sum(.$`m17`[1]+.$`m17`[2])) %>% 
  
  add_row(Rama.Actividad.Económica="Minería",
          `h11`=sum(.$`h11`[3]),
          `m11`=sum(.$`m11`[3]),
          `h12`=sum(.$`h12`[3]),
          `m12`=sum(.$`m12`[3]),
          `h13`=sum(.$`h13`[3]),
          `m13`=sum(.$`m13`[3]),
          `h14`=sum(.$`h14`[3]),
          `m14`=sum(.$`m14`[3]),
          `h15`=sum(.$`h15`[3]),
          `m15`=sum(.$`m15`[3]),
          `h16`=sum(.$`h16`[3]),
          `m16`=sum(.$`m16`[3]),
          `h17`=sum(.$`h17`[3]),
          `m17`=sum(.$`m17`[3])) %>%
  
  add_row(Rama.Actividad.Económica="Industria",
          `h11`=sum(.$`h11`[4]),
          `m11`=sum(.$`m11`[4]),
          `h12`=sum(.$`h12`[4]),
          `m12`=sum(.$`m12`[4]),
          `h13`=sum(.$`h13`[4]),
          `m13`=sum(.$`m13`[4]),
          `h14`=sum(.$`h14`[4]),
          `m14`=sum(.$`m14`[4]),
          `h15`=sum(.$`h15`[4]),
          `m15`=sum(.$`m15`[4]),
          `h16`=sum(.$`h16`[4]),
          `m16`=sum(.$`m16`[4]),
          `h17`=sum(.$`h17`[4]),
          `m17`=sum(.$`m17`[4])) %>%
  
  add_row(Rama.Actividad.Económica="Electricidad,  gas  y  agua",
          `h11`=sum(.$`h11`[5]),
          `m11`=sum(.$`m11`[5]),
          `h12`=sum(.$`h12`[5]),
          `m12`=sum(.$`m12`[5]),
          `h13`=sum(.$`h13`[5]),
          `m13`=sum(.$`m13`[5]),
          `h14`=sum(.$`h14`[5]),
          `m14`=sum(.$`m14`[5]),
          `h15`=sum(.$`h15`[5]),
          `m15`=sum(.$`m15`[5]),
          `h16`=sum(.$`h16`[5]),
          `m16`=sum(.$`m16`[5]),
          `h17`=sum(.$`h17`[5]),
          `m17`=sum(.$`m17`[5])) %>%
  
  add_row(Rama.Actividad.Económica="Construcción",
          `h11`=sum(.$`h11`[6]),
          `m11`=sum(.$`m11`[6]),
          `h12`=sum(.$`h12`[6]),
          `m12`=sum(.$`m12`[6]),
          `h13`=sum(.$`h13`[6]),
          `m13`=sum(.$`m13`[6]),
          `h14`=sum(.$`h14`[6]),
          `m14`=sum(.$`m14`[6]),
          `h15`=sum(.$`h15`[6]),
          `m15`=sum(.$`m15`[6]),
          `h16`=sum(.$`h16`[6]),
          `m16`=sum(.$`m16`[6]),
          `h17`=sum(.$`h17`[6]),
          `m17`=sum(.$`m17`[6])) %>%
  
  add_row(Rama.Actividad.Económica="Comercio",
          `h11`=sum(.$`h11`[7]+.$`h11`[8]),
          `m11`=sum(.$`m11`[7]+.$`m11`[8]),
          `h12`=sum(.$`h12`[7]+.$`h12`[8]),
          `m12`=sum(.$`m12`[7]+.$`m12`[8]),
          `h13`=sum(.$`h13`[7]+.$`h13`[8]),
          `m13`=sum(.$`m13`[7]+.$`m13`[8]),
          `h14`=sum(.$`h14`[7]+.$`h14`[8]),
          `m14`=sum(.$`m14`[7]+.$`m14`[8]),
          `h15`=sum(.$`h15`[7]+.$`h15`[8]),
          `m15`=sum(.$`m15`[7]+.$`m15`[8]),
          `h16`=sum(.$`h16`[7]+.$`h16`[8]),
          `m16`=sum(.$`m16`[7]+.$`m16`[8]),
          `h17`=sum(.$`h17`[7]+.$`h17`[8]),
          `m17`=sum(.$`m17`[7]+.$`m17`[8])) %>%
  
  add_row(Rama.Actividad.Económica="Transporte y comunicaciones",
          `h11`=sum(.$`h11`[9]),
          `m11`=sum(.$`m11`[9]),
          `h12`=sum(.$`h12`[9]),
          `m12`=sum(.$`m12`[9]),
          `h13`=sum(.$`h13`[9]),
          `m13`=sum(.$`m13`[9]),
          `h14`=sum(.$`h14`[9]),
          `m14`=sum(.$`m14`[9]),
          `h15`=sum(.$`h15`[9]),
          `m15`=sum(.$`m15`[9]),
          `h16`=sum(.$`h16`[9]),
          `m16`=sum(.$`m16`[9]),
          `h17`=sum(.$`h17`[9]),
          `m17`=sum(.$`m17`[9])) %>%
  
  add_row(Rama.Actividad.Económica="Establecimientos financieros",
          `h11`=sum(.$`h11`[10]+.$`h11`[11]),
          `m11`=sum(.$`m11`[10]+.$`m11`[11]),
          `h12`=sum(.$`h12`[10]+.$`h12`[11]),
          `m12`=sum(.$`m12`[10]+.$`m12`[11]),
          `h13`=sum(.$`h13`[10]+.$`h13`[11]),
          `m13`=sum(.$`m13`[10]+.$`m13`[11]),
          `h14`=sum(.$`h14`[10]+.$`h14`[11]),
          `m14`=sum(.$`m14`[10]+.$`m14`[11]),
          `h15`=sum(.$`h15`[10]+.$`h15`[11]),
          `m15`=sum(.$`m15`[10]+.$`m15`[11]),
          `h16`=sum(.$`h16`[10]+.$`h16`[11]),
          `m16`=sum(.$`m16`[10]+.$`m16`[11]),
          `h17`=sum(.$`h17`[10]+.$`h17`[11]),
          `m17`=sum(.$`m17`[10]+.$`m17`[11])) %>%
          
  add_row(Rama.Actividad.Económica="Servicios",
          `h11`=sum(.$`h11`[12]+.$`h11`[13]+.$`h11`[14]+.$`h11`[15]+.$`h11`[17]),
          `m11`=sum(.$`m11`[12]+.$`m11`[13]+.$`m11`[14]+.$`m11`[15]+.$`m11`[17]),
          `h12`=sum(.$`h12`[12]+.$`h12`[13]+.$`h12`[14]+.$`h12`[15]+.$`h12`[16]+.$`h12`[17]),
          `m12`=sum(.$`m12`[12]+.$`m12`[13]+.$`m12`[14]+.$`m12`[15]+.$`m12`[16]+.$`m12`[17]),
          `h13`=sum(.$`h13`[12]+.$`h13`[13]+.$`h13`[14]+.$`h13`[15]+.$`h13`[16]+.$`h13`[17]),
          `m13`=sum(.$`m13`[12]+.$`m13`[13]+.$`m13`[14]+.$`m13`[15]+.$`m13`[16]+.$`m13`[17]),
          `h14`=sum(.$`h14`[12]+.$`h14`[13]+.$`h14`[14]+.$`h14`[15]+.$`h14`[16]+.$`h14`[17]),
          `m14`=sum(.$`m14`[12]+.$`m14`[13]+.$`m14`[14]+.$`m14`[15]+.$`m14`[16]+.$`m14`[17]),
          `h15`=sum(.$`h15`[12]+.$`h15`[13]+.$`h15`[14]+.$`h15`[15]+.$`h15`[16]+.$`h15`[17]),
          `m15`=sum(.$`m15`[12]+.$`m15`[13]+.$`m15`[14]+.$`m15`[15]+.$`m15`[16]+.$`m15`[17]),
          `h16`=sum(.$`h16`[12]+.$`h16`[13]+.$`h16`[14]+.$`h16`[15]+.$`h16`[16]+.$`h16`[17]),
          `m16`=sum(.$`m16`[12]+.$`m16`[13]+.$`m16`[14]+.$`m16`[15]+.$`m16`[16]+.$`m16`[17]),
          `h17`=sum(.$`h17`[12]+.$`h17`[13]+.$`h17`[14]+.$`h17`[15]+.$`h17`[16]+.$`h17`[17]),
          `m17`=sum(.$`m17`[12]+.$`m17`[13]+.$`m17`[14]+.$`m17`[15]+.$`m17`[16]+.$`m17`[17])) %>%
  
  add_row(Rama.Actividad.Económica="Actividades  no  especificadas",
          `h11`=sum(.$`h11`[18]),
          `m11`=sum(.$`m11`[18]),
          `h12`=sum(.$`h12`[18]),
          `m12`=sum(.$`m12`[18]),
          `h13`=sum(.$`h13`[18]),
          `m13`=sum(.$`m13`[18]),
          `h14`=sum(.$`h14`[18]),
          `m14`=sum(.$`m14`[18]),
          `h15`=sum(.$`h15`[18]),
          `m15`=sum(.$`m15`[18]),
          `h16`=sum(.$`h16`[18]),
          `m16`=sum(.$`m16`[18]),
          `h17`=sum(.$`h17`[18]),
          `m17`=sum(.$`m17`[18]))

cuadro1 <- cuadro1[-c(1:18),] %>% as.data.frame()

# 2018
cuadro2 <- c_sex %>% add_row(Rama.Actividad.Económica="Agricultura y pesca",
                             `h18`=sum(.$`h18`[1]),
                             `m18`=sum(.$`m18`[1])) %>% 
  
  add_row(Rama.Actividad.Económica="Minería",
          `h18`=sum(.$`h18`[2]),
          `m18`=sum(.$`m18`[2])) %>%
  
  add_row(Rama.Actividad.Económica="Industria",
          `h18`=sum(.$`h18`[3]),
          `m18`=sum(.$`m18`[3])) %>%
  
  add_row(Rama.Actividad.Económica="Electricidad,  gas  y  agua",
          `h18`=sum(.$`h18`[4]+.$`h18`[5]),
          `m18`=sum(.$`m18`[4]+.$`m18`[5])) %>%
  
  add_row(Rama.Actividad.Económica="Construcción",
          `h18`=sum(.$`h18`[6]),
          `m18`=sum(.$`m18`[6])) %>%
  
  add_row(Rama.Actividad.Económica="Comercio",
          `h18`=sum(.$`h18`[7]+.$`h18`[9]),
          `m18`=sum(.$`m18`[7]+.$`m18`[9])) %>%
  
  add_row(Rama.Actividad.Económica="Transporte y comunicaciones",
          `h18`=sum(.$`h18`[8]+.$`h18`[10]),
          `m18`=sum(.$`m18`[8]+.$`m18`[10])) %>%
  
  add_row(Rama.Actividad.Económica="Establecimientos financieros",
          `h18`=sum(.$`h18`[11]+.$`h18`[12]),
          `m18`=sum(.$`m18`[11]+.$`m18`[12])) %>%
  
  add_row(Rama.Actividad.Económica="Servicios",
          `h18`=sum(.$`h18`[13]+.$`h18`[14]+.$`h18`[15]+.$`h18`[16]+.$`h18`[17]+.$`h18`[18]+.$`h18`[19]+.$`h18`[20]+.$`h18`[21]),
          `m18`=sum(.$`m18`[13]+.$`m18`[14]+.$`m18`[15]+.$`m18`[16]+.$`m18`[17]+.$`m18`[18]+.$`m18`[19]+.$`m18`[20]+.$`m18`[21])) %>%
  
  add_row(Rama.Actividad.Económica="Actividades  no  especificadas",
          `h18`=sum(.$`h18`[22]),
          `m18`=sum(.$`m18`[22]))

cuadro2 <- cuadro2[-c(1:22),]

# 3.2. Merge ---- 
afi_sex <- merge(a_sex, cuadro1, by = "Rama.Actividad.Económica", all.x = T)
afi_sex <- merge(afi_sex, cuadro2, by = "Rama.Actividad.Económica", all.x = T)

# 3.3. Tasa sindicalización privada ---- 
tasa_sindi_sex <- merge(ene, afi_sex, by = "Rama.Actividad.Económica", all.x = T)

tasa_sindi_sex <- tasa_sindi_sex %>% mutate(TASAHOMB_2010=h10*100/(HOMB_2010),
                                            TASAMUJ_2010=m10*100/(MUJ_2010),
                                            TASAHOMB_2011=h11*100/(HOMB_2011),
                                            TASAMUJ_2011=m11*100/(MUJ_2011),
                                            TASAHOMB_2012=h12*100/(HOMB_2012),
                                            TASAMUJ_2012=m12*100/(MUJ_2012),
                                            TASAHOMB_2013=h13*100/(HOMB_2013),
                                            TASAMUJ_2013=m13*100/(MUJ_2013),
                                            TASAHOMB_2014=h14*100/(HOMB_2014),
                                            TASAMUJ_2014=m14*100/(MUJ_2014),
                                            TASAHOMB_2015=h15*100/(HOMB_2015),
                                            TASAMUJ_2015=m15*100/(MUJ_2015),
                                            TASAHOMB_2016=h16*100/(HOMB_2016),
                                            TASAMUJ_2016=m16*100/(MUJ_2016),
                                            TASAHOMB_2017=h17*100/(HOMB_2017),
                                            TASAMUJ_2017=m17*100/(MUJ_2017),
                                            TASAHOMB_2018=h18*100/(HOMB_2018),
                                            TASAMUJ_2018=m18*100/(MUJ_2018)) %>% as.data.frame()
sindi_sex_INE <- tasa_sindi_sex[-c(2:37)]
sindi_sex_INE[,-1] <-round(sindi_sex_INE[,-1],2)

# 4. Export ----
write_xlsx(tasa_sindi_sex,"output/sindiprivados_sexorama.xlsx", col_names = TRUE,format_headers = TRUE)
write_xlsx(sindi_sex_INE,"output/tasas_privados_sexo_rama_INE.xlsx", col_names = TRUE,format_headers = TRUE)