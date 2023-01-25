library(readr)
library(tidyr)
library(tidyverse)

ENFR_2018_Base_usuario_2 <- read.csv("Desktop/ENFR 2018 - Base usuario 2.txt", 
                                       sep = "|" )
View(ENFR_2018_Base_usuario_2)
datos<-ENFR_2018_Base_usuario_2
View(datos)
datos$bisg02
datos$bisg03
datos$bisg04
datos$bisg05
datos$bisg06




a<-t(matrix(    c(table(datos$bisg02),table(datos$bisg03),table(datos$bisg04),table(datos$bisg05),table(datos$bisg06)), nrow  = 3))


install.packages("chorddiag")
library(chorddiag)
library(devtools)
install_github("mattflor/chorddiag")

# Create dummy data



b<-datos[, c("bisg02","bisg03","bisg04","bisg05","bisg06")]

recode
b$bisg02<-dplyr::recode(b$bisg02, "1" = "0", "3" = "1","2" = "1")
b$bisg03<-dplyr::recode(b$bisg03, "1" = "0", "3" = "1","2" = "1")
b$bisg04<-dplyr::recode(b$bisg04, "1" = "0", "3" = "1","2" = "1")
b$bisg06<-dplyr::recode(b$bisg06, "1" = "0", "3" = "1","2" = "1")
b$bisg05<-dplyr::recode(b$bisg05, "1" = "0", "3" = "1","2" = "1")


input_names = names(b)[-1]

b2<-b %>% count_(input_names) %>% unite_("ComboVar",input_names,sep="")

library(expss)
fre(b)
c<-unite(b, datos, sep = " ")

library(UpSetR)


upset(fromExpression(c2), 
      nintersects = 40, 
      nsets = 6, 
      order.by = "freq", 
      decreasing = T, 
      mb.ratio = c(0.6, 0.4),
      number.angles = 0, 
      text.scale = 1.1, 
      point.size = 2.8, 
      line.size = 1
)


df %>% count_(input_names) %>% unite_("ComboVar",input_names,sep="")



table(b)

c<- bind_cols(c,b[,1:5])
c2<-c(
Movilidad = length(  which( c$bisg02 == 1))
,Cuidados = length(  which( c$bisg03 == 1))
,Actividades = length(  which( c$bisg04 == 1))
,Dolor = length(  which( c$bisg05 == 1))
,Ansiedad = length(  which( c$bisg06 == 1))
,"Movilidad" = length(which(c$datos=="1 0 0 0 0"))
,"Movilidad&Cuidados" = length(which(c$datos=="1 1 0 0 0"))
,"Movilidad&Cuidados&Actividad" = length(which(c$datos=="1 1 1 0 0"))
,"Movilidad&Cuidados&Actividad&Dolor" = length(which(c$datos=="1 1 1 1 0"))
,"Movilidad&Cuidados&Actividad&Ansiedad" = length(which(c$datos=="1 1 1 1 1"))
,"Movilidad&Actividad" = length(which(c$datos=="1 0 1 0 0"))
,"Movilidad&Actividad&Dolor" = length(which(c$datos=="1 0 1 1 0"))
,"Movilidad&Actividad&Dolor&Ansiedad" = length(which(c$datos=="1 0 1 1 1"))
,"Movilidad&Dolor" =length(which(c$datos=="1 0 0 1 0"))
,"Movilidad&Dolor&Ansiedad" =length(which(c$datos=="1 0 0 1 1"))
  ,"Movilidad&Ansiedad" =length(which(c$datos=="1 0 0 0 1"))
  ,"Cuidados&Actividad" =3
  ,  "Cuidados&Dolor" =3
  ,"Cuidados&Ansiedad" =3
  ,  "Actividad&Dolor" =3
  ,"Actividad&Ansiedad" =3
  ,"Dolor&Ansiedad" =3

  )





