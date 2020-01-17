ls()
rm(list = ls())
load("D:/Usuarios/jlcebrian/Documents/2019/COMPARATIVA_RIM/NVDPS.RData")
ls()
install.packages("data.table")
library(data.table)
install.packages("tidyverse")
library (tidyverse)
library (lubridate)
library (dplyr)
library( readxl)
library(tidyr)

NVDT<-data.table(NVDP2018_cadiz)
head (NVDT)
NVDT$ESPECIE<-NVDT$SCIENTIFIC_NAME
NVDT$FECHA_DESEMBARQUE<-NVDT$FcDesembarque_corta
NVDT$PUERTO_DESEMBARQUE<-NVDT$Descripcion
length(unique(NVDT$IdDiario))
length(unique(NVDT$IDMAREA))

NVDP2018_COMPARATIVA<-NVDT[, .(CONSUM=round(sum(PesoConsumo,1)),DP=sum(PesoCapturado),
                               NV=sum(PesoNotaVenta),DESEM=sum(PesoDesembarcado)),
                           by = c("IDMAREA","IdDiario","CODIGO_UE","ESTRATO_RIM","METIER_DCF","LABORATORIO","DIVICES","FECHA_DESEMBARQUE",
                                  "PUERTO_DESEMBARQUE", "ESPECIE")]

head (NVDP2018_COMPARATIVA)
subset(NVDP2018_COMPARATIVA, IDMAREA=="18319244" & ESPECIE =="Merluccius merluccius")
as.data.frame(subset(NVDP2018_COMPARATIVA, IDMAREA=="18627944" ))
 #lo pasamos a formato corto
NVDP2018_COMPARATIVA.m1 = melt(NVDP2018_COMPARATIVA, id.vars = c(1:10),
             measure.vars = c("CONSUM", "DP", "NV", "DESEM"),
             variable.name = "TIPO_PESO", value.name = "PESO")

NVDP2018_COMPARATIVA.m1<-NVDP2018_COMPARATIVA.m1[order(IDMAREA,ESPECIE)]
head (NVDP2018_COMPARATIVA.m1)
gc()

muestreos18 <- readRDS("D:/Usuarios/jlcebrian/Documents/2019/COMPARATIVA_RIM/muestreos_2018.rds")
head (muestreos18)

muestreoscat<-muestreos18$catches
head (muestreoscat)
tail (muestreoscat)
table(muestreoscat$PUERTO)
muestreos_lonja<-subset(muestreoscat, COD_TIPO_MUE %in% c(1,2,4,6) )
ls()
rm(NVDT, db3, con3, muestreos18, muestreoscat, nvdp_ventas2)
dim(muestreos_lonja)
head (muestreos_lonja)
length(unique(muestreos_lonja$COD_ID))#3614
length(unique(muestreos_lonja$COD_TIPO_MUE))
str(muestreos_lonja)
muestreos_lonja$FECHA <- dmy(muestreos_lonja$FECHA_MUE)
dim(muestreos_lonja)#28939    48
muestreos_lonja<- muestreos_lonja[complete.cases(muestreos_lonja[c( "CODSGPM")]),]
dim(muestreos_lonja)
muestreos_lonja<-arrange(muestreos_lonja, FECHA, CODSGPM)

muestreos<-muestreos_lonja%>%
        group_by(YEAR,IDMAREA, COD_ID,FECHA,FECHA_MUE, PROCEDENCIA,ESTRATO_RIM, METIER_DCF,
                 PUERTO, COD_TIPO_MUE,BARCO, CODSGPM, COD_BARCO,ESPECIE=ESP_MUE)%>%
        summarise(PESO=sum(P_DESEM))%>%
        mutate(TIPO_PESO="RIM")

as.data.frame(head(muestreos))

length(unique(muestreos$COD_ID))#3614
muestreos1<-subset(muestreos, IDMAREA>0)

length(unique(muestreos1$COD_ID))#3614
muestreos1<-spread(muestreos1,PESO,TIPO_PESO )%>%distinct()
as.data.frame(head (muestreos1))
length(unique(muestreos1$COD_ID))  #3570
as.data.frame(head(NVDP2018_COMPARATIVA))
str(NVDP2018_COMPARATIVA)
NVDP2018_COMPARATIVA2<-gather(NVDP2018_COMPARATIVA,TIPO_PESO, PESO, 11:14 )
NVDP2018_COMPARATIVA2<-arrange(NVDP2018_COMPARATIVA2, IDMAREA, ESPECIE)
as.data.frame(head(NVDP2018_COMPARATIVA2))
str(NVDP2018_COMPARATIVA2)
class(NVDP2018_COMPARATIVA.m1)
muestreosDT<-data.table (muestreos1)
cruce1<-merge(NVDP2018_COMPARATIVA.m1[,c(1,10,11,12)],muestreosDT, all.y=TRUE)
cruce1<-cruce1[order(IDMAREA,ESPECIE)]
dim(cruce1)#53176     7
length(unique(cruce1$COD_ID))#3570
length(unique(cruce1$IDMAREA))#3570
as.data.frame(head(cruce1),10)
as.data.frame(subset(cruce1,IDMAREA=="18000042"))
as.data.frame(subset(NVDP2018_COMPARATIVA.m1,IDMAREA=="18000042"))
cruce1<-data.table(cruce1)
NVDP2018_COMPARATIVA.m1<-data.table(NVDP2018_COMPARATIVA.m1)
cruce2<-merge(cruce1[,c(1,5)],NVDP2018_COMPARATIVA.m1, all.x = TRUE)  #, by=c("IDMAREA",  "ESPECIE", "PESO", "TIPO_PESO"))
cruce2<-unique(cruce2)
cruce2<-cruce2[order(IDMAREA,ESPECIE)]
as.data.frame(head(cruce2),10)
head (cruce2)
as.data.frame(head(subset(cruce1,IDMAREA=="18000042"),10))
length(unique(cruce1$COD_ID))#3570
cruce3<-rbind(cruce1[,c(1,2,3,4,5)], cruce2[,c(1,11,12,13,2)])%>%arrange(IDMAREA, ESPECIE)%>%distinct()
cruce3<-cruce3[order(IDMAREA,ESPECIE)]
cruce3<-full_join(cruce1, cruce2, by="IDMAREA", "TIPO_PESO")
as.data.frame(head(subset(cruce3,IDMAREA=="18000042"),10))
cruce3<-data.table(cruce3)
cruce1<-data.table(cruce1)
head (cruce1[,c(1,6:16)])
head (cruce2)
str(cruce3)
str (cruce1)
cruce1$IDMAREA<-as.factor(cruce1$IDMAREA)
cruce3$IDMAREA<-as.factor(cruce3$IDMAREA)
cruce4<-full_join(cruce3, cruce1[,c(1,6:16)])%>%distinct()
        #"IDMAREA", "COD_ID", "FECHA", "FECHA_MUE","PROCEDENCIA","ESTRATO_RIM",
                              #  "METIER_DCF", "PUERTO","COD_TIPO_MUE", "BARCO", "CODSGPM", "COD_BARCO")])
cruce4<-cruce4[order(IDMAREA,ESPECIE)]
as.data.frame(head(subset(cruce4,IDMAREA=="18000042"),10))
as.data.frame(tail(cruce4,10))
length(unique(cruce4$COD_ID))#3570
cruce2$IDMAREA<-as.factor(cruce2$IDMAREA)
cruce5<-full_join(cruce4, cruce2[,c(1,3,8,9,10)])%>%distinct()
 cruce5<-cruce5[order(IDMAREA,ESPECIE)]
 as.data.frame(head(subset(cruce5,IDMAREA=="18000042"),10))