# Code 2: Analysis ENE-DT

# 1. Cargar librerias ----
pacman::p_load(tidyverse, rvest, xml2, lubridate, openxlsx, readxl, ggrepel, tibble, writexl, haven,
               dplyr, car, summarytools, ggpubr, ggplot2, sjmisc, sjlabelled, stargazer, srvyr)
options(scipen=999)

# 2. Cargar bases de datos ----

# ENE
cuadro2010_2019 <- load("input/cuadro2010_2019.RData")
ene <- cuadro2010_2019

# OOSS DT
# 2010
a <-read.xlsx("input/1. OOSS.xlsx",sheet = 14,rows=c(3:13),cols=c(1:10),na.strings = "**",colNames = FALSE)
names(a) <- c("Rama.Actividad.Económica", "h8","m8","t8","h9","m9","t9","h10","m10","t10")
a <- a[-c(2,3,4,5,6,7)]

# 2011 a 2017
b <- cbind(
  read.xlsx("input/1. OOSS.xlsx",sheet = 15,rows=c(4:22),cols=c(1:7),na.strings = "**",colNames = FALSE),
  read.xlsx("input/1. OOSS.xlsx",sheet = 16,rows=c(4:22),cols=c(2:7),na.strings = "**",colNames = FALSE),
  read.xlsx("input/1. OOSS.xlsx",sheet = 17,rows=c(4:22),cols=c(2:7),na.strings = "**",colNames = FALSE),
  read.xlsx("input/1. OOSS.xlsx",sheet = 18,rows=c(4:22),cols=c(2:4),na.strings = "**",colNames = FALSE))

names(b) <- c("Rama.Actividad.Económica","h11","m11","t11","h12","m12","t12","h13","m13","t13","h14","m14","t14",
                         "h15","m15","t15","h16","m16","t16","h17","m17","t17")

c <- b %>% select(1,4,7,10,13,16,19,22) # SOLO TOTALES

# 2018
d <- read.xlsx("input/1. OOSS.xlsx",sheet = 19,rows=c(3:26),cols=c(1:4),na.strings = "**")
names(d)<-c("Rama.Actividad.Económica","h18","m18","t18")

d_tot <- d %>% select(1,4) # SOLO TOTALES

# 3. Procesamiento ----

