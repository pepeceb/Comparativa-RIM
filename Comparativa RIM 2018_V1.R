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

NVDP2018_COMPARATIVA<-NVDP2018_cadiz%>%
        group_by(IDMAREA,CODIGO_UE, ESTRATO_RIM, METIER_DCF, LABORATORIO, DIVICES, FECHA=FcDesembarque_corta,
                 PUERTO_DESEMBARQUE=Descripcion, ESPECIE=SCIENTIFIC_NAME)%>%
        summarise(
                CONSUM=sum(PesoConsumo),
                DP=sum(PesoCapturado),
                NV=sum(PesoNotaVenta),
                DESEM=sum(PesoDesembarcado)
        )
NVDT<-data.table(NVDP2018_cadiz)
head (NVDT)
NVDT$ESPECIE<-NVDT$SCIENTIFIC_NAME
NVDT$FECHA_DESEMBARQUE<-NVDT$FcDesembarque_corta
NVDT$PUERTO_DESEMBARQUE<-NVDT$Descripcion
length(unique(NVDT$IdDiario))
length(unique(NVDT$IDMAREA))
table(NVDT$ESPECIE)
gc()
NVDP2018_COMPARATIVA<-NVDT[, .(CONSUM=round(sum(PesoConsumo,1)),DP=sum(PesoCapturado),
                               NV=sum(PesoNotaVenta),DESEM=sum(PesoDesembarcado)),
                           by = c("IDMAREA","CODIGO_UE","ESTRATO_RIM","METIER_DCF","LABORATORIO","DIVICES","FECHA_DESEMBARQUE",
                                  "PUERTO_DESEMBARQUE", "ESPECIE")]

head (NVDP2018_COMPARATIVA)
subset(NVDP2018_COMPARATIVA, IDMAREA=="18319244" & ESPECIE =="Merluccius merluccius")
as.data.frame(subset(NVDP2018_COMPARATIVA, IDMAREA=="18627944" ))

NVDP2018_COMPARATIVA.m1 = data.table::melt(NVDP2018_COMPARATIVA, id.vars = c(1:9),
             measure.vars = c("CONSUM", "DP", "NV", "DESEM"),
             variable.name = "TIPO_PESO", value.name = "PESO")

NVDP2018_COMPARATIVA.m1<-NVDP2018_COMPARATIVA.m1[order(IDMAREA,ESPECIE)] 
head (NVDP2018_COMPARATIVA.m1)
gc()
memory.limit()
memory.limit(size=7000)
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
ls()
length(unique(muestreos$COD_ID))#3614
muestreos1<-subset(muestreos, IDMAREA>0)
muestreosDT<-data.table(muestreos1)
length(unique(muestreosDT$COD_ID))#3614
muestreos1<-spread(muestreos1,PESO,TIPO_PESO )%>%distinct()
as.data.frame(head (muestreos1))
length(unique(muestreos1$COD_ID))  #3570              
str (NVDP2018_COMPARATIVA.m1)
cruce1<-merge(NVDP2018_COMPARATIVA.m1[,c(1,9,10,11)],muestreosDT, all.y=TRUE)
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
cruce3<-rbind(cruce1[,c(1,2,3,4,5)], cruce2[,c(1,10,11,12,2)])%>%arrange(IDMAREA, ESPECIE)%>%distinct()
cruce3<-cruce3[order(IDMAREA,ESPECIE)]

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
cruce5<-full_join(cruce4, cruce2[,c(1,3,8,9,10)], by="IDMAREA")%>%distinct()
install.packages("purrrlyr")
library(purrrlyr) 
df1 <- mutate_if(cruce4, is.factor, as.character)
df2 <- dmap(cruce2[,c(1,3,8,9,10)], as.vector)
cruce6<-full_join(df1,df2, by = "IDMAREA") %>% 
        mutate_at(vars(matches("ESPECIE")), funs(replace(., . == "", NA))) %>% 
        #mutate(ESPECIE2 = coalesce(ESPECIE.y, ESPECIE.x)) %>% 
        select(-contains(".y"))%>%distinct()
cruce6<-full_join(cruce4,cruce2[,c(1,3,8,9,10)], cty = FECHA_DESEMBARQUE, st = CODIGO_UE)%>%
        mutate(
                FECHA_DESEMBARQUE         = ifelse(nzchar(FECHA_DESEMBARQUE), FECHA_DESEMBARQUE, cty), 
                CODIGO_UE = ifelse(grepl("[^\\s]", CODIGO_UE), CODIGO_UE, st)
        ) %>%
        select(-cty, -st)


 cruce5<-cruce5[order(IDMAREA,ESPECIE)]
 as.data.frame(head(subset(cruce5,IDMAREA=="18000042"),10))
 
 subset(cruce6, IDMAREA=="18319244")
 cruce6<-distinct(cruce6)
subset(cruce6, IDMAREA== "18342767")
head (cruce6)
 ## write to working directory
 install.packages("openxlsx")
 library(openxlsx)
 options("openxlsx.borderColour" = "#4F80BD") ## set default border colour
 write.xlsx(cruce5, file = "matrix1.xlsx", colNames = TRUE, borders = "columns")
 write.xlsx(cruce5, file = "matrix2.xlsx", colNames = TRUE, borders = "surrounding")
 
 write.xlsx(cruce6, file = "matrix3.xlsx", colNames = TRUE, borders = "rows")
 hs <- createStyle(textDecoration = "BOLD", fontColour = "#FFFFFF", fontSize=12,
                   fontName="Arial Narrow", fgFill = "#4F80BD")
 
 write.xlsx(cruce6, file = "MATRIX.xlsx", colNames = TRUE, borders = "rows", headerStyle = hs)
 class(cruce6)
 ## Lists elements are written to individual worksheets, using list names as sheet names if available
 l <- list("IRIS" = iris, "MTCATS" = mtcars, matrix(runif(1000), ncol = 5))
 write.xlsx(l, "writeList1.xlsx", colWidths = c(NA, "auto", "auto"))
 
 ## different sheets can be given different parameters
 write.xlsx(l, "writeList2.xlsx", startCol = c(1,2,3), startRow = 2,
            asTable = c(TRUE, TRUE, FALSE), withFilter = c(TRUE, FALSE, FALSE))
 
 write.xlsx(cruce6, "MATRIX.xlsx", startCol = c(1), startRow = 2,
            asTable = c(TRUE), withFilter = c(TRUE))
 
 
 
 
 
 
 
 
 
 
ls()
rm( "na" , "napuerto" , "NVDP2018_COMPARATIVA" )
install.packages("xlsx", dependencies = T)
library("xlsx")
exportar <- function(datos, archivo){
        wb <- createWorkbook(type="xlsx")
        
        # Estilos de celdas
        # Estilos de titulos y subtitulos
        titulo <- CellStyle(wb)+ Font(wb,  heightInPoints=16, isBold=TRUE)
        
        subtitulo <- CellStyle(wb) + Font(wb,  heightInPoints=12,
                                          isItalic=TRUE, isBold=FALSE)
        
        # Estilo de tablas
        filas <- CellStyle(wb) + Font(wb, isBold=TRUE)
        
        columnas <- CellStyle(wb) + Font(wb, isBold=TRUE) +
                Alignment(vertical="VERTICAL_CENTER",wrapText=TRUE, horizontal="ALIGN_CENTER") +
                Border(color="black", position=c("TOP", "BOTTOM"),
                       pen=c("BORDER_THICK", "BORDER_THICK"))+Fill(foregroundColor = "lightblue", pattern = "SOLID_FOREGROUND")
        
        # Crear una hoja
        sheet <- createSheet(wb, sheetName = "Información - R Users Group - Ecuador")
        
        # Funcion linea (agregar texto)
        linea<-function(sheet, rowIndex, title, titleStyle){
                rows <- createRow(sheet, rowIndex=rowIndex)
                sheetTitle <- createCell(rows, colIndex=1)
                setCellValue(sheetTitle[[1,1]], title)
                setCellStyle(sheetTitle[[1,1]], titleStyle)
        }
        
        # Agregamos titulos,  subtitulos, etc.
        linea(sheet, rowIndex=8,
              title=paste("Fecha:", format(Sys.Date(), format="%Y/%m/%d")),
              titleStyle = subtitulo)
        
        linea(sheet, rowIndex=9,
              title="Elaborado por: R Users Group - Ecuador",
              titleStyle = subtitulo)
        
        linea(sheet, rowIndex=11,
              paste("Información de prueba"),
              titleStyle = titulo)
        
        # Tablas
        addDataFrame(datos,
                     sheet, startRow=13, startColumn=1,
                     colnamesStyle = columnas,
                     rownamesStyle = filas,
                     row.names = F)
        
        # Ancho de columnas
        setColumnWidth(sheet, colIndex=c(1:ncol(datos)), colWidth=15)
        
        # Imagen    Cambia la ruta por la que de tu imagen
        #addPicture("C:/final5x5.png", sheet, scale=0.75, startRow = 1, startColumn = 1)
        
        # Guardar
        saveWorkbook(wb, archivo)
}
class(cruce5)
cruce5<-as.data.frame(cruce5)
getwd()
exportar(cruce5, "matrix1")
save.xlsx <- function (file, ...)
{
        require(xlsx, quietly = TRUE)
        objects <- list(...)
        fargs <- as.list(match.call(expand.dots = TRUE))
        objnames <- as.character(fargs)[-c(1, 2)]
        nobjects <- length(objects)
        for (i in 1:nobjects) {
                if (i == 1)
                        write.xlsx(objects[[i]], file, sheetName = objnames[i])
                else write.xlsx(objects[[i]], file, sheetName = objnames[i],
                                append = TRUE)
        }
        print(paste("Workbook", file, "has", nobjects, "worksheets."))
}

save.xlsx("matrix1.xlsx",cruce5)
ls()
getwd()
rm(nvdp, nvdp_ventas2,NVDP2018_COMPARATIVA,NVDP2018_COMPARATIVA2, cruce1, cruce2, cruce3)
cruce6$FUENTE<-cruce6$TIPO_PESO
cruce6$ESPECIE<-cruce6$ESPECIE.x
head (cruce6)
Datos<-cruce6
head (Datos)
library(Hmisc)
Datos$ESTRATO_RIM<-as.character (Datos$ESTRATO_RIM)
substring2(Datos$ESTRATO_RIM,"BACA_CN") <- "ARRASTRE_CN"
substring2(Datos$ESTRATO_RIM,"JURELERA_CN") <- "ARRASTRE_CN"
substring2(Datos$ESTRATO_RIM,"Raj*") <- "Rajidae"
Datos$PUERTO<-as.character (Datos$PUERTO)
Datos$PUERTO[Datos$PUERTO==  "Avilés"] <- "Aviles-Gijon"
Datos$PUERTO[Datos$PUERTO==  "Gijón"] <- "Aviles-Gijon"
head (Datos)
#para calcular la CPUE por ESTRATO_RIM para el Arrastre
Datos1<-Datos %>%
        group_by(  ESTRATO_RIM,PUERTO,FUENTE, COD_TIPO_MUE) %>%
        dplyr::mutate(MAREAS=length(unique(COD_ID)))   %>%
        group_by( ESTRATO_RIM, PUERTO,FUENTE,ESPECIE, MAREAS, COD_TIPO_MUE ) %>%
        dplyr::summarise (PESO= sum(PESO)) %>%
        group_by(  ESTRATO_RIM,PUERTO, FUENTE,ESPECIE,COD_TIPO_MUE ) %>%
        dplyr::mutate(CPUE = round(PESO/MAREAS,2)) %>%  
        subset (ESTRATO_RIM %in% c("ARRASTRE_CN")) %>%	
        #subset (PUERTO %in% c( "Santa Eugenia de Ribeira")) %>%
        #subset (TIPO_MUESTREO %in% c("1","2")) %>%
        #•subset (ESPECIE %in% c(ESPECIES$ESPECIE))  %>%
        subset(ESPECIE %in% c("Merluccius merluccius", "Micromesistius poutassou","Lepidorhombus spp","Eledone spp",
"Ommastrephidae","Lophius spp","Triglidae", "Trachurus spp","Scomber colias", "Boops boops",
                 "Scyliorhinus canicula", "Trisopterus spp", "Scomber scombrus", "Nephrops norvegicus")) %>%
        as.data.frame() 
head (Datos1)
ESPECIES<-subset(Datos1,
                 subset = ( CPUE> 100 ))  
ESPECIES<-subset(Datos1,FUENTE %in% c("RIM") & CPUE>75)

ESPECIES<- as.data.frame (ESPECIES)
head (ESPECIES)
library(ggplot2)
library( ggalt)
library(ggthemes)
install.packages("Hmisc")
library(Hmisc)
head(Datos1)
Datos2<-filter(Datos1, COD_TIPO_MUE %in% c(1,2) & FUENTE %in% c("RIM", "DP", "NV") & !(PUERTO  %in% ("Vigo")))
p<-ggplot(Datos2, aes(x=reorder(ESPECIE,-CPUE), y=CPUE, fill=FUENTE))

p + geom_bar(stat="identity",position="dodge", size=1, color="black",width =NULL)  +
        labs(title= Datos1$ESTRATO_RIM, subtitle= Datos1$PUERTO)+  
        facet_grid(PUERTO~COD_TIPO_MUE)   +
        theme_bw(14) +
        theme(axis.text=element_text(angle=45,size=12, hjust=1)) +
        scale_fill_manual(values = c("dodgerblue4", "darkorchid3","orange"))  +
        labs(x="",y="CPUE (KG/MAREA)")    +
        
        theme(strip.text.x = element_text(size=11, angle=0,face="bold"),
              strip.text.y = element_text(size=9, face="bold"),
              strip.background = element_rect(colour="darkblue", fill="#CCCCFF"))     +
        #ylim(0,3000)  +
        
        
        
        geom_text(data=subset(Datos2,subset=(FUENTE=="RIM")),aes(x=5, y=1000, label=paste(" Mareas Muestreadas=",MAREAS)),
                  size=3, col="darkblue")   + #ylim(0,10000)+
        
        
        geom_text(aes(label = round(CPUE,0)),
                  position=position_dodge(width=1), hjust=.6,vjust=-0.5,parse = TRUE,size=3)  +
        

        scale_y_sqrt() +
        geom_text(data=subset(Datos1,subset=(FUENTE=="RIM")),aes(x=5, y=12000, label=""))

table (Datos$ESTRATO_RIM, Datos$FUENTE)
Datos$COD_TIPO_MUE[Datos$COD_TIPO_MUE==  "1"] <- "MT1"
Datos$COD_TIPO_MUE[Datos$COD_TIPO_MUE==  "2"] <- "MT2"
Datos$COD_TIPO_MUE[Datos$COD_TIPO_MUE==  "4"] <- "MT4"
Datos$COD_TIPO_MUE[Datos$COD_TIPO_MUE==  "6"] <- "MT6"
head (Datos1)
str(Datos1)
library(dplyr)
library(ggplot2)
Datos2<-filter(Datos1, COD_TIPO_MUE %in% c("MT1","MT2") & FUENTE %in% c("CONSUM","RIM", "DP", "NV") & 
                       (PUERTO  %in% ("A Coruña")))
Datos2 = subset(Datos2, !(ESPECIE %in% c('Nephrops norvegicus', "Scomber colias")))
head (Datos2)
table(Datos2$ESPECIE)
lbls_cor<-c( "Trachurus\n spp","\n Scomber\n scombrus", "Micromesisitius\n poutassou","\n Merluccius\n merluccius",
             "Lepidorhombus\n spp","\n Ommastreph","Lophius\n spp","Boops\n boops",
             "Eledone\n spp",        "Scyliorhinus\n canicula",   "Triglidae","Trisopterus\n spp" )
#lbls_cor2  <- paste(c("", "\n"), levels(factor(lbls_cor)))
#lbls_cor  <- paste(c("", "\n"), levels(reorder(Datos2$ESPECIE, -Datos2$CPUE)))
ARCOR<-ggplot(Datos2, aes(y=CPUE, x=factor(reorder(ESPECIE,-CPUE), labels = lbls_cor),  fill=factor(FUENTE)))+
 geom_bar(position="dodge",stat="identity" ,size=1, color="black")  +
        labs(subtitle=Datos2$PUERTO, title= Datos2$ESTRATO_RIM)  +
        facet_wrap(.~COD_TIPO_MUE, ncol = 1)   +
        theme(axis.text.x=element_text(angle=0,size=10, face="bold.italic"))  +
        #scale_fill_manual(values = c("lightsteelblue","cornflowerblue", "lightsteelblue", "darkslateblue"))+
        labs(x="",y="CPUE")   + theme(plot.title = element_text(lineheight=.8, face="bold"))+
        theme(strip.text.x = element_text(size=11, angle=0,face="bold.italic"),
              strip.text.y = element_text(size=9, face="bold"),
              strip.background = element_blank())   +
        scale_fill_manual(values = c("lightsteelblue","dodgerblue4", "darkorchid3","orange"))   +
        #strip.background = element_rect(colour="darkblue", fill="#CCCCFF")     +
        #ylim(0,3500)  +
        theme(legend.position="top", legend.box = "horizontal")+
        theme(legend.title=element_text( size = 11, face = "bold", colour="black"),
              legend.text = element_text( size = 10, face = "italic"))+
        guides(fill=guide_legend("FUENTE", override.aes = list(size=6)))+
            geom_text(data=subset(Datos2,subset=(FUENTE=="RIM")),aes(x=5, y=500, label=paste(" Mareas Muestreadas=",MAREAS)),
                      fontface="bold",    size=4, col="darkblue")   + #ylim(0,10000)+
            
            geom_text(aes(label = round(CPUE,0)),
                      position=position_dodge(width=1), hjust=.6,vjust=-0.5,parse = TRUE,size=2.8) 
        ARCOR
  
  #####arrastre Avilés
        Datos1<-Datos %>%
                group_by(  ESTRATO_RIM,PUERTO,FUENTE, COD_TIPO_MUE) %>%
                dplyr::mutate(MAREAS=length(unique(COD_ID)))   %>%
                group_by( ESTRATO_RIM, PUERTO,FUENTE,ESPECIE, MAREAS, COD_TIPO_MUE ) %>%
                dplyr::summarise (PESO= sum(PESO)) %>%
                group_by(  ESTRATO_RIM,PUERTO, FUENTE,ESPECIE,COD_TIPO_MUE ) %>%
                dplyr::mutate(CPUE = round(PESO/MAREAS,2)) %>%  
                subset (ESTRATO_RIM %in% c("ARRASTRE_CN")) %>%	
                #subset (PUERTO %in% c( "Santa Eugenia de Ribeira")) %>%
                #subset (TIPO_MUESTREO %in% c("1","2")) %>%
                #•subset (ESPECIE %in% c(ESPECIES$ESPECIE))  %>%
                subset(ESPECIE %in% c("Merluccius merluccius", "Micromesistius poutassou","Lepidorhombus spp","Eledone spp",
                                      "Ommastrephidae","Lophius spp","Triglidae", "Trachurus spp","Scomber colias", "Boops boops",
                                      "Scyliorhinus canicula", "Trisopterus spp", "Scomber scombrus", "Nephrops norvegicus")) %>%
                as.data.frame() 
  Datos2<-filter(Datos1, COD_TIPO_MUE %in% c("MT1") & FUENTE %in% c("CONSUM","RIM", "DP", "NV") &
                         (PUERTO  %in% ("Avilés")))
  Datos2 = subset(Datos2, !(ESPECIE %in% c('Eledone spp',"Trisopterus spp", "Scomber colias")))
  head (Datos2)
  table(Datos2$ESPECIE)
 # lbls_avil2  <- paste(c("", "\n"), levels(factor(lbls_avil)))
  #lbls_avil  <- paste(c("", "\n"), levels(reorder(Datos2$ESPECIE, -Datos2$CPUE)))
  lbls_avil<-c( "Lophius\n spp","\n Lepidorhombus\n spp","Trachurus\n spp","\n Merluccius\n merluccius",
                "Scyliorhinus\n canicula","   Ommastrephid", "\n Micromesistius\n   poutassou","Boops\n boops",
                "Scomber\n scombrus", "Triglidae" )
  ARAVI<-ggplot(Datos2, aes(y=CPUE, x=factor(reorder(ESPECIE,-CPUE), labels = lbls_avil),  fill=factor(FUENTE)))+
          #ggplot(Datos2, aes(y=CPUE, x=factor(ESPECIE), fill=factor(FUENTE)))+
          geom_bar(position="dodge",stat="identity" ,size=1, color="black")  +
          labs(subtitle=Datos2$PUERTO, title= Datos2$ESTRATO_RIM)  +
          facet_wrap(.~COD_TIPO_MUE, ncol = 1)   +
          theme(axis.text.x=element_text(angle=0,size=10, face="bold.italic"))  +
          #scale_fill_manual(values = c("cornflowerblue", "lightsteelblue", "darkslateblue"))+
          labs(x="",y="CPUE")   + theme(plot.title = element_text(lineheight=.8, face="bold"))+
  
   theme(strip.text.x = element_text(size=11, angle=0,face="bold"),
                  strip.text.y = element_text(size=9, face="bold"),
                  strip.background = element_blank())   +
          scale_fill_manual(values = c("lightsteelblue","dodgerblue4", "darkorchid3","orange"))   +
          #strip.background = element_rect(colour="darkblue", fill="#CCCCFF")     +
          #ylim(0,3500)  +
          theme(legend.position="top", legend.box = "horizontal")+
          theme(legend.title=element_text( size = 11, face = "bold", colour="black"),
                legend.text = element_text( size = 10, face = "italic"))+
          guides(fill=guide_legend("FUENTE", override.aes = list(size=4)))+
          geom_text(data=subset(Datos2,subset=(FUENTE=="RIM")),aes(x=5, y=250, label=paste(" Mareas Muestreadas=",MAREAS)),
                    fontface="bold",  size=4.5, col="darkblue")   + #ylim(0,10000)+
          
          
          geom_text(aes(label = round(CPUE,0)),
                    position=position_dodge(width=1), hjust=.6,vjust=-0.5,parse = TRUE,size=2.8) 
  ARAVI
  
  #####arrastre Gijon
  head (Datos1)
  Datos2<-filter(Datos1, COD_TIPO_MUE %in% c("MT2") & FUENTE %in% c("RIM", "DP", "NV", "CONSUM") & (PUERTO  %in% ("Gijón")))
  Datos2 = subset(Datos2, !(ESPECIE %in% c("Eledone spp", "Trisopterus spp")))

 
  lbls_gij<-c( "Scomber\n scombrus", "Trachurus\n spp", \n "Scomber\n colias  ","Lepidorhombus\n spp", "\n Boops\n boops",
               "Lophius\n spp","Scyliorhinus\n canicula","\n Merluccius\n merluccius","Ommastrephidae   ",
               "\n Triglidae" ,  "Micromesistius\n poutassou")
 
  #lbls_gij  <- paste(c("", "\n"), levels(reorder(Datos2$ESPECIE, -Datos2$CPUE)))
  ARGIJ<-ggplot(Datos2, aes(y=CPUE, x=factor(reorder(ESPECIE,-CPUE), labels = lbls_gij),  fill=factor(FUENTE)))+
          #ggplot(Datos2, aes(y=CPUE, x=factor(ESPECIE), fill=factor(FUENTE)))+
          geom_bar(position="dodge",stat="identity" ,size=1, color="black")  +
          labs(subtitle=Datos2$PUERTO, title= Datos2$ESTRATO_RIM)  +
          facet_wrap(.~COD_TIPO_MUE, ncol = 1)   +
          theme(axis.text.x=element_text(angle=0,size=10, face="bold.italic"))  +
     
          labs(x="",y="CPUE")   + 
          theme(plot.title = element_text(lineheight=.8, face="bold"))+
  
   theme(strip.text.x = element_text(size=11, angle=0,face="bold"),
                  strip.text.y = element_text(size=9, face="bold"),
                  strip.background = element_blank())   +
          scale_fill_manual(values = c( "lightsteelblue","dodgerblue4", "darkorchid3","orange"))   +
          #strip.background = element_rect(colour="darkblue", fill="#CCCCFF")     +
          #ylim(0,3500)  +
          theme(legend.position="top", legend.box = "horizontal")+
          theme(legend.title=element_text( size = 11, face = "bold", colour="black"),
                legend.text = element_text( size = 10, face = "italic"))+
          guides(fill=guide_legend("FUENTE", override.aes = list(size=5)))+
          geom_text(data=subset(Datos2,subset=(FUENTE=="RIM")),aes(x=5, y=700, label=paste(" Mareas Muestreadas=",MAREAS)),
                    fontface="bold",  size=4.2, col="darkblue")   + #ylim(0,10000)+
          
          geom_text(aes(label = round(CPUE,0)),
                    position=position_dodge(width=1), hjust=.6,vjust=-0.5,parse = TRUE,size=3.2) 
  ARGIJ
  gc()
  
  #####arrastre Burela
  Datos1<-Datos %>%
          group_by(  ESTRATO_RIM,PUERTO,FUENTE, COD_TIPO_MUE) %>%
          dplyr::mutate(MAREAS=length(unique(COD_ID)))   %>%
          group_by( ESTRATO_RIM, PUERTO,FUENTE,ESPECIE, MAREAS, COD_TIPO_MUE ) %>%
          dplyr::summarise (PESO= sum(PESO)) %>%
          group_by(  ESTRATO_RIM,PUERTO, FUENTE,ESPECIE,COD_TIPO_MUE ) %>%
          dplyr::mutate(CPUE = round(PESO/MAREAS,2)) %>%  
          subset (ESTRATO_RIM %in% c("ARRASTRE_CN")) %>%	
          #subset (PUERTO %in% c( "Santa Eugenia de Ribeira")) %>%
          #subset (TIPO_MUESTREO %in% c("1","2")) %>%
          #•subset (ESPECIE %in% c(ESPECIES$ESPECIE))  %>%
          subset(ESPECIE %in% c("Merluccius merluccius", "Micromesistius poutassou","Lepidorhombus spp","Eledone spp",
                                "Ommastrephidae","Lophius spp","Triglidae", "Trachurus spp","Scomber colias", "Boops boops",
                                "Scyliorhinus canicula", "Trisopterus spp", "Scomber scombrus", "Nephrops norvegicus")) %>%
          as.data.frame()
  head (Datos1)
  Datos2<-filter(Datos1, COD_TIPO_MUE %in% c("MT1","MT2") & FUENTE %in% c("RIM", "DP", "NV", "CONSUM") & (PUERTO  %in% ("Burela")))
  Datos2 = subset(Datos2, !(ESPECIE %in% c('Nephrops norvegicus', "Triglidae", "Trisopterus spp", "Eledone spp")))
  head (Datos2)

  lbls_bur<-c( "Scomber\n scombrus", "Trachurus\n spp" ,"Boops\n boops","Lepidorhombus\n spp","\n Merluccius\n merluccius",
               "Scyliorhinus\n canicula","\n Scomber\n colias", "Ommastrephidae","\n Lophius\n spp",
               "Micromesistius\n  poutassou")
 
  #lbls_bur  <- paste(c("", "\n"), levels(reorder(Datos2$ESPECIE, -Datos2$CPUE)))
  ARBUR<-ggplot(Datos2, aes(y=CPUE, x=factor(reorder(ESPECIE, -CPUE), labels = lbls_bur),  fill=factor(FUENTE)))+
          geom_bar(position="dodge",stat="identity" ,size=1, color="black")  +
          labs(subtitle=Datos2$PUERTO, title= Datos2$ESTRATO_RIM)  +
          facet_wrap(.~COD_TIPO_MUE, ncol = 1)   +
          theme(axis.text.x=element_text(angle=0,size=10, face="bold.italic"))  +
          #scale_fill_manual(values = c("cornflowerblue", "lightsteelblue", "darkslateblue"))+
          labs(x="",y="CPUE")   + theme(plot.title = element_text(lineheight=.8, face="bold"))+
          
          theme(strip.text.x = element_text(size=11, angle=0,face="bold.italic"),
                strip.text.y = element_text(size=11, face="bold"),
                strip.background = element_blank())   +
          scale_fill_manual(values = c( "lightsteelblue","dodgerblue4", "darkorchid3","orange"))   +
          #strip.background = element_rect(colour="darkblue", fill="#CCCCFF")     +
          #ylim(0,3500)  +
          theme(legend.position="top", legend.box = "horizontal")+
          theme(legend.title=element_text( size = 11, face = "bold", colour="black"),
                legend.text = element_text( size = 12))+
          guides(fill=guide_legend("FUENTE", override.aes = list(size=6)))+
          geom_text(data=subset(Datos2,subset=(FUENTE=="RIM")),aes(x=5, y=900, label=paste(" Mareas Muestreadas=",MAREAS)),
                   fontface="bold", size=4.2, col="darkblue")   + #ylim(0,10000)+
          
          geom_text(aes(label = round(CPUE,0)),
                    position=position_dodge(width=1), hjust=.6,vjust=-0.5,parse = TRUE,size=3.2) 
  ARBUR
  
  
  
  #####arrastre Ribeira
  table(Datos1$PUERTO, Datos1$FUENTE)
  Datos1$PUERTO[Datos1$PUERTO==  "Santa Eugenia de Ribeira"] <- "Ribeira"
  head (Datos1)
  Datos2<-subset(Datos1, PUERTO=="Ribeira"  &
                         FUENTE %in% c("RIM", "DP", "NV", "CONSUM"))
  
  head (Datos2)
  Datos2<-filter(Datos1, #COD_TIPO_MUE %in% c("MT1","MT2") &
                         FUENTE %in% c("RIM", "DP", "NV", "CONSUM") &  
                         (PUERTO  %in% ("Ribeira")))
  Datos2 = subset(Datos2, !(ESPECIE %in%  c("Trisopterus spp",'Nephrops norvegicus', 
                                   "Triglidae", "Scyliorhinus canicula", "Eledone spp")))
  table(Datos2$ESPECIE)
  head (Datos2)


  lbls_rib<-c( "Trachurus\n spp" ,"Scomber\n scombrus","\n  Micromesistius\n poutassou","Scomber colias", 
               "\n Merluccius\n merluccius", "Boops boops", "\n Ommastrephidae","Lepidorhombus\n spp","Lophius\n spp"
               
  )
 # lbls_rib2  <- paste(c("", "\n"), levels(reorder(Datos2$ESPECIE, -Datos2$CPUE)))
  ARRIB<-ggplot(Datos2, aes( x=factor(reorder(ESPECIE, -CPUE), labels = lbls_rib),y= CPUE,  fill=factor(FUENTE)))+
          #ggplot(Datos2, aes(x=factor(ESPECIE), y= CPUE, fill=factor(FUENTE)))+
          #ggplot(Datos2, aes(x=reorder(ESPECIE,-CPUE, labels=lbls_rib2), y=CPUE, fill=FUENTE))  +
          geom_bar(position="dodge",stat="identity" ,size=1, color="black")  +
          labs(subtitle=Datos2$PUERTO, title= Datos2$ESTRATO_RIM)  +
          facet_wrap(.~COD_TIPO_MUE, ncol = 1, scales="free_x")   +
          theme(axis.text.x=element_text(angle=0,size=10, face="bold.italic"))  +
          #scale_fill_manual(values = c("cornflowerblue", "lightsteelblue", "darkslateblue"))+
          labs(x="",y="CPUE")   + theme(plot.title = element_text(lineheight=.8, face="bold")) +
          
          theme(strip.text.x = element_text(size=11, angle=0,face="bold"),
                strip.text.y = element_text(size=9, face="bold"),
                strip.background = element_blank())   +
          scale_fill_manual(values = c( "lightsteelblue","dodgerblue4", "darkorchid3","orange"))   +
          #strip.background = element_rect(colour="darkblue", fill="#CCCCFF")     +
          #ylim(0,3500)  +
          theme(legend.position="top", legend.box = "horizontal")+
          theme(legend.title=element_text( size = 12, face = "bold", colour="black"),
                legend.text = element_text( size = 10, face = "italic"))+
          guides(fill=guide_legend("FUENTE", override.aes = list(size=6)))+
          geom_text(data=subset(Datos2,subset=(FUENTE=="RIM")),aes(x=5, y=2000, label=paste(" Mareas Muestreadas=",MAREAS)),
                    fontface="bold", size=4, col="darkblue")   + #ylim(0,10000)+
          
          geom_text(aes(label = round(CPUE,0)),
                    position=position_dodge(width=1), hjust=.6,vjust=-0.5,parse = TRUE,size=3.2) +
          scale_y_sqrt(breaks=c(100,500,1000,2000,3000))
  ARRIB
  ls()
  rm("tabla.destino", "Tabla.rel", "miss"  )
  ######Marín
  Datos2<-filter(Datos1, COD_TIPO_MUE %in% c("MT1","MT2") & FUENTE %in% c("RIM", "DP", "NV", "CONSUM") & 
                         (PUERTO  %in% ("Marín")))
  Datos2 = subset(Datos2, !(ESPECIE %in% c("Scyliorhinus canicula","Scomber colias", "Nephrops norvegicus")))
  head (Datos2)
  #lbls_mar  <- paste(c("", "\n"), levels(factor(Datos2$ESPECIE)))
  lbls_mar<-c( "Ommastrephidae","\n Scomber\n scombrus","Lepidorhombus\n spp","\n Merluccius\n  merluccius",
               "Micromesistius\n poutassou", "Lophius\n spp",
               "Trachurus\n spp","Eledone\n spp", "Triglidae" , "Trisopterus\n spp")
 # lbls_mar2  <- paste(c("", "\n"), levels(factor(lbls_mar)))
  #lbls_mar  <- paste(c("", "\n"), levels(reorder(Datos2$ESPECIE, -Datos2$CPUE)))
  ARMAR<-ggplot(Datos2, aes(y=CPUE, x=factor(reorder(ESPECIE,-CPUE), labels = lbls_mar),  fill=factor(FUENTE)))+
          geom_bar(position="dodge",stat="identity" ,size=1, color="black")  +
          labs(subtitle=Datos2$PUERTO, title= Datos2$ESTRATO_RIM)  +
          facet_wrap(.~COD_TIPO_MUE, ncol = 1)   +
          theme(axis.text.x=element_text(angle=0,size=10, face="bold.italic"))  +
          #scale_fill_manual(values = c("cornflowerblue", "lightsteelblue", "darkslateblue"))+
          labs(x="",y="CPUE")   + theme(plot.title = element_text(lineheight=.8, face="bold"))+
          
          theme(strip.text.x = element_text(size=11, angle=0,face="bold"),
                strip.text.y = element_text(size=9, face="bold"),
                strip.background = element_blank())   +
          scale_fill_manual(values = c( "lightsteelblue","dodgerblue4", "darkorchid3","orange"))   +
          #strip.background = element_rect(colour="darkblue", fill="#CCCCFF")     +
          #ylim(0,3500)  +
          theme(legend.position="top", legend.box = "horizontal")+
          theme(legend.title=element_text( size = 11, face = "bold", colour="black"),
                legend.text = element_text( size = 10, face = "italic"))+
          guides(fill=guide_legend("FUENTE", override.aes = list(size=6)))+
          geom_text(data=subset(Datos2,subset=(FUENTE=="RIM")),aes(x=4.5, y=600, label=paste(" Mareas Muestreadas=",MAREAS)),
                    fontface="bold", size=4.2, col="darkblue")   + #ylim(0,10000)+
          
          geom_text(aes(label = round(CPUE,0)),
                    position=position_dodge(width=1), hjust=.6,vjust=-0.5,parse = TRUE,size=3) 
  ARMAR
  
  
  head (Datos)
  substring2(Datos$ESTRATO_RIM,"BACA_CN") <- "ARRASTRE_CN"
  substring2(Datos$ESTRATO_RIM,"JURELERA_CN") <- "ARRASTRE_CN"
  head (Datos)
  table(Datos$COD_TIPO_MUE)
  mareas<-subset(Datos,COD_TIPO_MUE %in% c("MT2", "MT1") & ESTRATO_RIM %in% c("ARRASTRE_CN")  
                        & FUENTE %in% c("NV", "DP", "RIM")&
                   
                          #PUERTO %in%  c("A Coruña", "Avilés", "Gijón","Finisterre", "Muros") &
                          ESPECIE %in% c("Merluccius merluccius", "Micromesistius poutassou","Lepidorhombus spp",
                                         "Ommastrephidae","Lophius spp","Triglidae", "Trachurus spp","Boops boops", "Scomber colias",
                                         "Scyliorhinus canicula", "Trisopterus spp", "Scomber scombrus", "Nephrops norvegicus"))
head (mareas)
table(mareas$ESPECIE)
table(mareas$PUERTO) 
  esptal2$PUERTO[esptal2$PUERTO==  "Avilés"] <- "Aviles-Gijon"
  esptal2$PUERTO[esptal2$PUERTO==  "Gijón"] <- "Aviles-Gijon"
  esptal2$PUERTO[esptal2$PUERTO==  "Santa Eugenia de Ribeira"] <- "Ribeira "
  head (as.data.frame(esptal2))
  table(esptal2$PUERTO)
  n_fun <- function(x){
          return(data.frame(y =-500,
                            label = length(x)))
  }
  
  mareas1<-subset(mareas, PUERTO %in% c("A Coruña") & PESO>5)
  mareas1 = subset(mareas1, !(ESPECIE %in% c('Nephrops norvegicus', "Scomber colias")))
  lbls  <- paste(c("", "\n"), levels(factor(mareas1$ESPECIE)))

ARCOR2<-ggplot(mareas1,
               aes(x=reorder(factor(ESPECIE), -PESO),y=PESO), fill=FUENTE)   +
        geom_point(aes(fill=ESPECIE,size=PESO), shape=21,
                   position=position_jitter(width=0.2, height=0.2)) +
        scale_size(range = c(2,8)) +
        stat_boxplot(geom ='errorbar', width = 0.26)    +
        geom_boxplot(width = 0.5, aes(fill = ESPECIE),alpha=0.4, show.legend=FALSE) +
        
        guides(size=FALSE,fill=guide_legend(override.aes=list(size=1)))  +
        labs(title=paste("ARRASTRE CN",mareas1$PUERTO), subtitle="Pesos por Marea",caption = "MT1 +MT2",
             x=NULL)  +
        theme_bw()+
        facet_wrap(.~FUENTE, scales="free_x")  +
        expand_limits(y = -550) +
        stat_summary(fun.data = n_fun, geom = "text", hjust = 0.5, fontface="bold", size=3) +
        
        theme(plot.title = element_text(lineheight=.8, face="bold"))+
        theme(axis.text.x=element_text(angle=45,size=10, hjust=1,face= "bold.italic"))   +
        theme(strip.text.x = element_text(family = "sans",size=18, angle=0,face=c("italic")),
              strip.text.y = element_text(size=12, face="bold"),
              strip.background = element_blank())+
        theme(legend.position="top")+
        theme(legend.title=element_text( size = 12, face = "bold", colour="blue"),
              legend.text = element_text( size = 10, face = "italic"))+
        guides(fill=guide_legend("Especie", override.aes = list(size=6))) +
        scale_fill_manual(values=col) #+scale_y_sqrt()
  ARCOR2
  
  
  
  mareas1<-subset(mareas, PUERTO %in% c("Avilés"))
  n_fun <- function(x){
          return(data.frame(y =-300,
                            label = length(x)))
  }
  head (mareas1)
  lbls  <- paste(c("", "\n"), levels(factor(mareas1$ESPECIE)))
  ARAVI2<-ggplot(mareas1,
                 aes(x=reorder(factor(ESPECIE), -PESO),y=PESO), fill=FUENTE)   +
          geom_point(aes(fill=ESPECIE,size=PESO), shape=21,
                     position=position_jitter(width=0.2, height=0.2)) +
          scale_size(range = c(2,8)) +
          stat_boxplot(geom ='errorbar', width = 0.26)    +
          geom_boxplot(width = 0.5, aes(fill = ESPECIE),alpha=0.4, show.legend=FALSE) +
          
          guides(size=FALSE,fill=guide_legend(override.aes=list(size=1)))  +
          labs(title=paste("ARRASTRE CN",mareas1$PUERTO), subtitle="Pesos por Marea",
               x=NULL)  +
          theme_bw()+
          facet_wrap(.~FUENTE, scales="free_x")  +
          stat_summary(fun.data = n_fun, geom = "text", hjust = 0.5, fontface="bold", size=3) +
          expand_limits(y = -110) +
          theme(plot.title = element_text(lineheight=.8, face="bold"))+
          theme(axis.text.x=element_text(angle=45,size=10, hjust=1,face= "bold.italic"))   +
          theme(strip.text.x = element_text(family = "sans",size=18, angle=0,face=c("italic")),
                strip.text.y = element_text(size=12, face="bold"),
                strip.background = element_blank())+
          theme(legend.position="top")+
          theme(legend.title=element_text( size = 12, face = "bold", colour="blue"),
                legend.text = element_text( size = 10, face = "italic"))+
          guides(fill=guide_legend("Especie", override.aes = list(size=6))) +
          scale_fill_manual(values=col) #+scale_y_sqrt()
  ARAVI2
  
  ####gijon
  mareas1<-subset(mareas, PUERTO %in% c("Gijón") & PESO>2)
  head (mareas1)
  lbls  <- paste(c("", "\n"), levels(factor(mareas1$ESPECIE)))
  n_fun <- function(x){
          return(data.frame(y =-300,
                            label = length(x)))
  }
 mareas1 = subset(mareas1, !(ESPECIE %in% c('Eledone spp',"Trisopterus spp")))                          
  ARAGIJ2<-ggplot(mareas1,
                 aes(x=reorder(factor(ESPECIE), -PESO),y=PESO), fill=FUENTE)   +
          geom_point(aes(fill=ESPECIE,size=PESO), shape=21,
                     position=position_jitter(width=0.2, height=0.2)) +
          scale_size(range = c(2,8)) +
          stat_boxplot(geom ='errorbar', width = 0.26)    +
          geom_boxplot(width = 0.5, aes(fill = ESPECIE),alpha=0.4, show.legend=FALSE) +
          
          guides(size=FALSE,fill=guide_legend(override.aes=list(size=1)))  +
          labs(title=paste("ARRASTRE CN",mareas1$PUERTO), subtitle="Pesos por Marea",
               x=NULL)  +
          theme_bw()+
          facet_wrap(.~FUENTE, scales="free_x")  +
          stat_summary(fun.data = n_fun, geom = "text", hjust = 0.5, fontface="bold", size=3) +
          expand_limits(y = -110) +
          theme(plot.title = element_text(lineheight=.8, face="bold"))+
          theme(axis.text.x=element_text(angle=45,size=10, hjust=1,face= "bold.italic"))   +
          theme(strip.text.x = element_text(family = "sans",size=18, angle=0,face=c("italic")),
                strip.text.y = element_text(size=12, face="bold"),
                strip.background = element_blank())+
          theme(legend.position="top")+
          theme(legend.title=element_text( size = 12, face = "bold", colour="blue"),
                legend.text = element_text( size = 10, face = "italic"))+
          guides(fill=guide_legend("Especie", override.aes = list(size=6))) +
          scale_fill_manual(values=col) #+scale_y_sqrt()
  ARAGIJ2
  
  
  
  rm(ARAGIJ3)
  
  ####Burela
  mareas1<-subset(mareas, PUERTO %in% c("Burela"))
  mareas1 = subset(mareas1, !(ESPECIE %in% c('Nephrops norvegicus', "Triglidae", "Trisopterus spp", "Eledone spp")))
  head (mareas1)
  lbls  <- paste(c("", "\n"), levels(factor(mareas1$ESPECIE)))
  n_fun <- function(x){
          return(data.frame(y =-400,
                            label = length(x)))
  }
  
  ARABUL2<-ggplot(mareas1,
                  aes(x=reorder(factor(ESPECIE), -PESO),y=PESO), fill=FUENTE)   +
          geom_point(aes(fill=ESPECIE,size=PESO), shape=21,
                     position=position_jitter(width=0.2, height=0.2)) +
          scale_size(range = c(2,8)) +
          stat_boxplot(geom ='errorbar', width = 0.26)    +
          geom_boxplot(width = 0.5, aes(fill = ESPECIE),alpha=0.4, show.legend=FALSE) +
          
          guides(size=FALSE,fill=guide_legend(override.aes=list(size=1)))  +
          labs(title=paste("ARRASTRE CN",mareas1$PUERTO), subtitle="Pesos por Marea",
               x=NULL)  +
          theme_bw()+
          facet_wrap(.~FUENTE, scales="free_x")  +
          stat_summary(fun.data = n_fun, geom = "text", hjust = 0.5, fontface="bold", size=3) +
          expand_limits(y = -110) +
          theme(plot.title = element_text(lineheight=.8, face="bold"))+
          theme(axis.text.x=element_text(angle=45,size=10, hjust=1,face= "bold.italic"))   +
          theme(strip.text.x = element_text(family = "sans",size=18, angle=0,face=c("italic")),
                strip.text.y = element_text(size=12, face="bold"),
                strip.background = element_blank())+
          theme(legend.position="top")+
          theme(legend.title=element_text( size = 12, face = "bold", colour="blue"),
                legend.text = element_text( size = 10, face = "italic"))+
          guides(fill=guide_legend("Especie", override.aes = list(size=6))) +
          scale_fill_manual(values=col)# +scale_y_sqrt()
  ARABUL2
  grid.draw(ARABUL2g) 
 
 rm(ARABUL3)
  
  ########RIBEIRA
  table(Datos1$PUERTO, Datos1$FUENTE)
  Datos1$PUERTO[Datos1$PUERTO==  "Santa Eugenia de Ribeira"] <- "Ribeira"
  mareas$PUERTO[mareas$PUERTO==  "Santa Eugenia de Ribeira"] <- "Ribeira "
  
  mareas1<-subset(mareas, PUERTO %in% c("Santa Eugenia de Ribeira") & FUENTE %in% c("NV", "DP", "RIM"))
  mareas1 = subset(mareas1, !(ESPECIE %in% c('Nephrops norvegicus', "Triglidae", "Scyliorhinus canicula", "Eledone spp")))
  head (mareas1)
  table(mareas$PUERTO)
  lbls  <- paste(c("", "\n"), levels(factor(mareas1$ESPECIE)))
  n_fun <- function(x){
          return(data.frame(y =-1000,
                            label = length(x)))
  }
  
  ARARIB2<-ggplot(mareas1,
                  aes(x=reorder(factor(ESPECIE), -PESO),y=PESO), fill=FUENTE)   +
          geom_point(aes(fill=ESPECIE,size=PESO), shape=21,
                     position=position_jitter(width=0.2, height=0.2)) +
          scale_size(range = c(2,8)) +
          stat_boxplot(geom ='errorbar', width = 0.26)    +
          geom_boxplot(width = 0.5, aes(fill = ESPECIE),alpha=0.4, show.legend=FALSE) +
          
          guides(size=FALSE,fill=guide_legend(override.aes=list(size=1)))  +
          labs(title=paste("ARRASTRE CN",mareas1$PUERTO), subtitle="Pesos por Marea",
               x=NULL)  +
          theme_bw()+
          facet_wrap(.~FUENTE, scales="free_x")  +
          stat_summary(fun.data = n_fun, geom = "text", hjust = 0.5, fontface="bold", size=3) +
          expand_limits(y = -310) +
          theme(plot.title = element_text(lineheight=.8, face="bold"))+
          theme(axis.text.x=element_text(angle=45,size=10, hjust=1,face= "bold.italic"))   +
          theme(strip.text.x = element_text(family = "sans",size=18, angle=0,face=c("italic")),
                strip.text.y = element_text(size=12, face="bold"),
                strip.background = element_blank())+
          theme(legend.position="top")+
          theme(legend.title=element_text( size = 12, face = "bold", colour="blue"),
                legend.text = element_text( size = 10, face = "italic"))+
          guides(fill=guide_legend("Especie", override.aes = list(size=6))) +
          scale_fill_manual(values=col) #+scale_y_sqrt()
  ARARIB2
  ARARIB2
  ARARIB2g2<- grid.draw(ARARIB2g) 
  ARARIB3<-ggplot(subset(mareas1,PESO>5),
                  aes(x=reorder(factor(ESPECIE), -PESO),y=PESO), fill=FUENTE)   +
          geom_point(aes(fill=ESPECIE,size=PESO), shape=21,
                     position=position_jitter(width=0.2, height=0.2)) +
          scale_size(range = c(2,8)) +
          stat_boxplot(geom ='errorbar', width = 0.26)    +
          geom_boxplot(width = 0.5, aes(fill = ESPECIE),alpha=0.4, show.legend=FALSE) +
          
          guides(size=FALSE,fill=guide_legend(override.aes=list(size=1)))  +
          labs(title=paste("ARRASTRE CN",mareas1$PUERTO), subtitle="Pesos por Marea",
               x=NULL)  +
          theme_bw()+
          facet_wrap(.~FUENTE, scales="free_x")  +
          #stat_summary(fun.data = n_fun, geom = "text", hjust = 0.5, fontface="bold", size=3) +
          expand_limits(y = -310) +
          theme(plot.title = element_text(lineheight=.8, face="bold"))+
          theme(axis.text.x=element_text(angle=45,size=10, hjust=1,face= "bold.italic"))   +
          theme(strip.text.x = element_text(family = "sans",size=18, angle=0,face=c("italic")),
                strip.text.y = element_text(size=12, face="bold"),
                strip.background = element_blank())+
          theme(legend.position="top")+
          theme(legend.title=element_text( size = 12, face = "bold", colour="blue"),
                legend.text = element_text( size = 10, face = "italic"))+
          guides(fill=guide_legend("Especie", override.aes = list(size=6))) +
          scale_fill_manual(values=col) +scale_y_sqrt(breaks=c(50,500,1000,5000,10000,25000,40000))+
          geom_abline(aes(slope = 0, intercept = 1), lty = "dotted", lwd = 1.5)
  ARARIB3
  
  ####MARIN
  mareas1<-subset(mareas, PUERTO %in% c("Marín") & FUENTE %in% c("NV", "DP", "RIM"))
  mareas1 = subset(mareas1, !(ESPECIE %in% c("Scomber colias")))
  head (mareas1)
  table(mareas$PUERTO)
  lbls  <- paste(c("", "\n"), levels(factor(mareas1$ESPECIE)))
  n_fun <- function(x){
          return(data.frame(y =-500,
                            label = length(x)))
  }
  
  ARAMAR2<-ggplot(subset(mareas1,PESO<7000),
                  aes(x=reorder(factor(ESPECIE), -PESO),y=PESO), fill=FUENTE)   +
          geom_point(aes(fill=ESPECIE,size=PESO), shape=21,
                     position=position_jitter(width=0.2, height=0.2)) +
          scale_size(range = c(2,8)) +
          stat_boxplot(geom ='errorbar', width = 0.26)    +
          geom_boxplot(width = 0.5, aes(fill = ESPECIE),alpha=0.4, show.legend=FALSE) +
          
          guides(size=FALSE,fill=guide_legend(override.aes=list(size=1)))  +
          labs(title=paste("ARRASTRE CN",mareas1$PUERTO), subtitle="Pesos por Marea",
               x=NULL)  +
          theme_bw()+
          facet_wrap(.~FUENTE, scales="free_x")  +
          stat_summary(fun.data = n_fun, geom = "text", hjust = 0.5, fontface="bold", size=3) +
          #expand_limits(y = -310) +
          theme(plot.title = element_text(lineheight=.8, face="bold"))+
          theme(axis.text.x=element_text(angle=45,size=10, hjust=1,face= "bold.italic"))   +
          theme(strip.text.x = element_text(family = "sans",size=18, angle=0,face=c("italic")),
                strip.text.y = element_text(size=12, face="bold"),
                strip.background = element_blank())+
          theme(legend.position="top")+
          theme(legend.title=element_text( size = 12, face = "bold", colour="blue"),
                legend.text = element_text( size = 10, face = "italic"))+
          guides(fill=guide_legend("Especie", override.aes = list(size=6))) +
          scale_fill_manual(values=col)# +scale_y_sqrt()
  ARAMAR2
  ARAMAR3<-ggplot(subset(mareas1,PESO>5 & PESO<8000),
                  aes(x=reorder(factor(ESPECIE), -PESO),y=PESO), fill=FUENTE)   +
          geom_point(aes(fill=ESPECIE,size=PESO), shape=21,
                     position=position_jitter(width=0.2, height=0.2)) +
          scale_size(range = c(2,10)) +
          stat_boxplot(geom ='errorbar', width = 0.26)    +
          geom_boxplot(width = 0.5, aes(fill = ESPECIE),alpha=0.4, show.legend=FALSE) +
          
          guides(size=FALSE,fill=guide_legend(override.aes=list(size=1)))  +
          labs(title=paste("ARRASTRE CN",mareas1$PUERTO), subtitle="Pesos por Marea",
               x=NULL)  +
          theme_bw()+
          facet_wrap(.~FUENTE, scales="free_x")  +
          #stat_summary(fun.data = n_fun, geom = "text", hjust = 0.5, fontface="bold", size=3) +
          #expand_limits(y = -310) +
          theme(plot.title = element_text(lineheight=.8, face="bold"))+
          theme(axis.text.x=element_text(angle=45,size=10, hjust=1,face= "bold.italic"))   +
          theme(strip.text.x = element_text(family = "sans",size=18, angle=0,face=c("italic")),
                strip.text.y = element_text(size=12, face="bold"),
                strip.background = element_blank())+
          theme(legend.position="top")+
          theme(legend.title=element_text( size = 12, face = "bold", colour="blue"),
                legend.text = element_text( size = 10, face = "italic"))+
          guides(fill=guide_legend("Especie", override.aes = list(size=6))) +
          scale_fill_manual(values=col) +scale_y_sqrt(breaks=c(50,500,1000,2000,5000))+
          geom_abline(aes(slope = 0, intercept = 1), lty = "dotted", lwd = 1.5)
  ARAMAR3
  ls()


ls()

 # save.image("~/2019/COMPARATIVA_RIM/rmarkdown arrastre.RData")
  rm(flr, miss, df1, df2, cruce1, curce2, curce3, cruce4)
  ARBUR
  ARABUL2

  ARGIJ
  ARAGIJ2
 
  ARMAR
  ARAMAR2
  
  ARRIB
  ARARIB2
 
  ARAVI
  ARAVI2
  
  ARCOR
  ARCOR2
  
  
  
  Datos1<-Datos %>%
          group_by(  ESTRATO_RIM,PUERTO,FUENTE, COD_TIPO_MUE) %>%
          dplyr::mutate(MAREAS=length(unique(COD_ID)))   %>%
          group_by( ESTRATO_RIM, PUERTO,FUENTE,ESPECIE, MAREAS, COD_TIPO_MUE ) %>%
          dplyr::summarise (PESO= sum(PESO)) %>%
          group_by(  ESTRATO_RIM,PUERTO, FUENTE,ESPECIE,COD_TIPO_MUE ) %>%
          dplyr::mutate(CPUE = round(PESO/MAREAS,2)) %>%  
          subset (ESTRATO_RIM %in% c("PAREJA_CN")) %>%
          #filter(CPUE>10)
          #subset (PUERTO %in% c( "Santa Eugenia de Ribeira")) %>%
          #subset (TIPO_MUESTREO %in% c("1","2")) %>%
          #•subset (ESPECIE %in% c(ESPECIES$ESPECIE))  %>%
          subset(ESPECIE %in% c("Ommastrephidae", "Scomber scombrus", "Merluccius merluccius", "Trachurus spp",
                                "Scomber scombrus","Merluccius merluccius", "Lophius spp", "Micromesistius poutassou"))

ls()
head (Datos1)
table(Datos2$PUERTO)
table(Datos1$ESPECIE)
Datos2<-filter(Datos1, COD_TIPO_MUE %in% c("MT1","MT2") & FUENTE %in% c("RIM", "DP", "NV", "CONSUM") & 
PUERTO  %in% c ("A Coruña" ))

head (Datos2)
lblsCOR <- paste(c("", "\n"), levels(factor(Datos2$ESPECIE)))
lblsCOR  <- paste(c("", "\n"), levels(reorder(Datos2$ESPECIE, -Datos2$CPUE)))
PARCOR<-ggplot(Datos2, aes(y=CPUE, x=factor(reorder(ESPECIE,-CPUE), labels = lblsCOR),  fill=factor(FUENTE)))+
        geom_bar(position="dodge",stat="identity" ,size=1, color="black")  +
        labs(subtitle=Datos2$PUERTO, title= Datos2$ESTRATO_RIM)  +
        facet_wrap(.~COD_TIPO_MUE, ncol = 1)   +
        theme(axis.text.x=element_text(angle=0,size=12, face="bold.italic"))  +
        #scale_fill_manual(values = c("cornflowerblue", "lightsteelblue", "darkslateblue"))+
        labs(x="",y="CPUE")   + theme(plot.title = element_text(lineheight=.8, face="bold"))+
        
        theme(strip.text.x = element_text(size=11, angle=0,face="bold"),
              strip.text.y = element_text(size=9, face="bold"),
              strip.background = element_blank())   +
        scale_fill_manual(values = c( "lightsteelblue","dodgerblue4", "darkorchid3","orange"))   +
        #strip.background = element_rect(colour="darkblue", fill="#CCCCFF")     +
        #ylim(0,3500)  +
        theme(legend.position="top", legend.box = "horizontal")+
        theme(legend.title=element_text( size = 11, face = "bold", colour="black"),
              legend.text = element_text( size = 10, face = "italic"))+
        guides(fill=guide_legend("FUENTE", override.aes = list(size=6)))+
        geom_text(data=subset(Datos2,subset=(FUENTE=="RIM")),aes(x=4, y=5000, label=paste(" Mareas Muestreadas=",MAREAS)),
                  fontface="bold",  size=4, col="darkblue")   + #ylim(0,10000)+
        
        geom_text(aes(label = round(CPUE,0)),
                  position=position_dodge(width=1), hjust=.6,vjust=-0.5,parse = TRUE,size=3.2)  +
        scale_y_sqrt(breaks=c(100,500,1000,2500,5000))
PARCOR


PARRIB
Datos2<-filter(Datos1, COD_TIPO_MUE %in% c("MT1","MT2") & FUENTE %in% c("RIM", "DP", "NV", "CONSUM") & 
                       PUERTO  %in% c ("Ribeira" ))
table(Datos2$ESPECIE)
head (Datos2)
lbls_parib <- c("Lophius spp", "Merluccius\n merluccius", "Micromesistius\n poutassou",
                "Ommastrephidae", "Trachurus\n spp")
lbls_parib <- paste(c("", "\n"), levels(reorder(Datos2$ESPECIE, -Datos2$CPUE)))
PARRIB<-ggplot(Datos2, aes(y=CPUE, x=factor(reorder(ESPECIE, -CPUE), labels = lbls_parib),  fill=factor(FUENTE)))+
        geom_bar(position="dodge",stat="identity" ,size=1, color="black")  +
        labs(subtitle=Datos2$PUERTO, title= Datos2$ESTRATO_RIM)  +
        facet_wrap(.~COD_TIPO_MUE, ncol = 1)   +
        theme(axis.text.x=element_text(angle=0,size=12, face="bold.italic"))  +
        #scale_fill_manual(values = c("cornflowerblue", "lightsteelblue", "darkslateblue"))+
        labs(x="",y="CPUE")   + theme(plot.title = element_text(lineheight=.8, face="bold"))+
        
        theme(strip.text.x = element_text(size=11, angle=0,face="bold"),
              strip.text.y = element_text(size=9, face="bold"),
              strip.background = element_blank())   +
        scale_fill_manual(values = c( "lightsteelblue","dodgerblue4", "darkorchid3","orange"))   +
        #strip.background = element_rect(colour="darkblue", fill="#CCCCFF")     +
        #ylim(0,3500)  +
        theme(legend.position="top", legend.box = "horizontal")+
        theme(legend.title=element_text( size = 12, face = "bold", colour="black"),
              legend.text = element_text( size = 10, face = "italic"))+
        guides(fill=guide_legend("FUENTE", override.aes = list(size=6)))+
        geom_text(data=subset(Datos2,subset=(FUENTE=="RIM")),aes(x=4, y=5000, label=paste(" Mareas Muestreadas=",MAREAS)),
                  fontface="bold",  size=4, col="darkblue")   + #ylim(0,10000)+
        
        geom_text(aes(label = round(CPUE,0)),
                  position=position_dodge(width=1), hjust=.6,vjust=-0.5,parse = TRUE,size=3)  +
        scale_y_sqrt()


PARRIB


PARAVI

Datos2<-filter(Datos1, COD_TIPO_MUE %in% c("MT1","MT2") & FUENTE %in% c("RIM", "DP", "NV", "CONSUM") & 
                       PUERTO  %in% c ("Avilés" ))

head (Datos2)
table(Datos2$ESPECIE)
lbls_paravi <- c("Lophius spp", "Merluccius\n merluccius", "Micromesistius\n poutassou",
                "Ommastrephidae", "Scomber\n scombrus", "Trachurus\n spp")
lbls_paravi <- paste(c("", "\n"), levels(reorder(Datos2$ESPECIE, -Datos2$CPUE)))
PARAVI<-ggplot(Datos2, aes(y=CPUE, x=factor(reorder(ESPECIE, -CPUE), labels = lbls_paravi),  fill=factor(FUENTE)))+
        geom_bar(position="dodge",stat="identity" ,size=1, color="black")  +
        labs(subtitle=Datos2$PUERTO, title= Datos2$ESTRATO_RIM)  +
        facet_wrap(.~COD_TIPO_MUE, ncol = 1)   +
        theme(axis.text.x=element_text(angle=0,size=12, face="bold.italic"))  +
        #scale_fill_manual(values = c("cornflowerblue", "lightsteelblue", "darkslateblue"))+
        labs(x="",y="CPUE")   + theme(plot.title = element_text(lineheight=.8, face="bold"))+
        
        theme(strip.text.x = element_text(size=11, angle=0,face="bold"),
              strip.text.y = element_text(size=9, face="bold"),
              strip.background = element_blank())   +
        scale_fill_manual(values = c( "lightsteelblue","dodgerblue4", "darkorchid3","orange"))   +
        #strip.background = element_rect(colour="darkblue", fill="#CCCCFF")     +
        #ylim(0,3500)  +
        theme(legend.position="top", legend.box = "horizontal")+
        theme(legend.title=element_text( size = 12, face = "bold", colour="black"),
              legend.text = element_text( size = 10, face = "italic"))+
        guides(fill=guide_legend("FUENTE", override.aes = list(size=5)))+
        geom_text(data=subset(Datos2,subset=(FUENTE=="RIM")),aes(x=4, y=5000, label=paste(" Mareas Muestreadas=",MAREAS)),
                  fontface="bold",  size=4, col="darkblue")   + #ylim(0,10000)+
        
        geom_text(aes(label = round(CPUE,0)),
                  position=position_dodge(width=1), hjust=.6,vjust=-0.5,parse = TRUE,size=3.2)  +
        scale_y_sqrt(breaks=c(100,500,1500,2500,4000,6000))
PARAVI

#BACA_AC
Datos1<-Datos %>%
        group_by(  ESTRATO_RIM,PUERTO,FUENTE, COD_TIPO_MUE) %>%
        dplyr::mutate(MAREAS=length(unique(COD_ID)))   %>%
        group_by( ESTRATO_RIM, PUERTO,FUENTE,ESPECIE, MAREAS, COD_TIPO_MUE ) %>%
        dplyr::summarise (PESO= sum(PESO)) %>%
        group_by(  ESTRATO_RIM,PUERTO, FUENTE,ESPECIE,COD_TIPO_MUE ) %>%
        dplyr::mutate(CPUE = round(PESO/MAREAS,2)) %>%  
        subset (ESTRATO_RIM %in% c("BACA_AC")) %>%
        #filter(CPUE>10)
        #subset (PUERTO %in% c( "Santa Eugenia de Ribeira")) %>%
        #subset (TIPO_MUESTREO %in% c("1","2")) %>%
        #•subset (ESPECIE %in% c(ESPECIES$ESPECIE))  %>%
        subset(ESPECIE %in% c("Conger conger", "Triglidae", "Merluccius merluccius", "Trachurus spp","Lepidorhombus spp",
                              "Zeus faber", "Rajidae", "Scyliorhinus canicula", "Merluccius merluccius", "Lophius spp", "Eledone spp"))
Datos2<-filter(Datos1, COD_TIPO_MUE %in% c("MT1","MT2") & FUENTE %in% c("RIM", "DP", "NV", "CONSUM") & 
                       PUERTO  %in% c ("Gijón" ))

lbls_BACA_AC
lbls_BACA_AC  <- paste(c("", "\n"), levels(reorder(Datos2$ESPECIE, -Datos2$CPUE)))
lbls_BACA_AC<-c("Lophius spp" ,"\n Lepidorhombus\n spp" ,"Scyliorhinus canicula","\n Merluccius\n merluccius",
                "Conger conger" ,"\n Rajidae" , "Triglidae" ,"\n Trachurus\n spp",
                " Zeus faber" ,"\n Eledone spp" )
CPUE_BACA_AC<-ggplot(Datos2, aes(y=CPUE, x=factor(reorder(ESPECIE, -CPUE), labels = lbls_BACA_AC),  fill=factor(FUENTE)))+
        geom_bar(position="dodge",stat="identity" ,size=1, color="black")  +
        labs(subtitle=Datos2$PUERTO, title= Datos2$ESTRATO_RIM)  +
        facet_wrap(.~COD_TIPO_MUE, ncol = 1)   +
        theme(axis.text.x=element_text(angle=0,size=12, face="bold.italic"))  +
        #scale_fill_manual(values = c("cornflowerblue", "lightsteelblue", "darkslateblue"))+
        labs(x="",y="CPUE")   + theme(plot.title = element_text(lineheight=.8, face="bold"))+
        
        theme(strip.text.x = element_text(size=11, angle=0,face="bold"),
              strip.text.y = element_text(size=9, face="bold"),
              strip.background = element_blank())   +
        scale_fill_manual(values = c( "lightsteelblue","dodgerblue4", "darkorchid3","orange"))   +
        #strip.background = element_rect(colour="darkblue", fill="#CCCCFF")     +
        #ylim(0,3500)  +
        theme(legend.position="top", legend.box = "horizontal")+
        theme(legend.title=element_text( size = 10, face = "bold", colour="black"),
              legend.text = element_text( size = 10, face = "italic"))+
        guides(fill=guide_legend("FUENTE", override.aes = list(size=6)))+
        geom_text(data=subset(Datos2,subset=(FUENTE=="RIM")),aes(x=4, y=5000, label=paste(" Mareas Muestreadas=",MAREAS)),
                  fontface="bold",  size=4, col="darkblue")   + #ylim(0,10000)+
        
        geom_text(aes(label = round(CPUE,0)),
                  position=position_dodge(width=1), hjust=.6,vjust=-0.5,parse = TRUE,size=3) # +
CPUE_BACA_AC

#BACA_APN
Datos1<-Datos %>%
        group_by(  ESTRATO_RIM,PUERTO,FUENTE, COD_TIPO_MUE) %>%
        dplyr::mutate(MAREAS=length(unique(COD_ID)))   %>%
        group_by( ESTRATO_RIM, PUERTO,FUENTE,ESPECIE, MAREAS, COD_TIPO_MUE ) %>%
        dplyr::summarise (PESO= sum(PESO)) %>%
        group_by(  ESTRATO_RIM,PUERTO, FUENTE,ESPECIE,COD_TIPO_MUE ) %>%
        dplyr::mutate(CPUE = round(PESO/MAREAS,2)) %>%  
        subset (ESTRATO_RIM %in% c("BACA_APN")) %>%
        #filter(CPUE>10)
        #subset (PUERTO %in% c( "Santa Eugenia de Ribeira")) %>%
        #subset (TIPO_MUESTREO %in% c("1","2")) %>%
        #•subset (ESPECIE %in% c(ESPECIES$ESPECIE))  %>%
        subset(ESPECIE %in% c("Galeus melastomus", "Triglidae", "Merluccius merluccius", "Solea solea","Lepidorhombus spp",
                              "Ommastrephidae", "Phycis blennoides", "Rajidae", "Scyliorhinus canicula", "Merluccius merluccius",
                              "Lophius spp", "Eledone spp",  "Trisopterus spp"))
Datos2<-filter(Datos1, COD_TIPO_MUE %in% c("MT1","MT2") & FUENTE %in% c("RIM", "DP", "NV", "CONSUM") & 
                       PUERTO  %in% c ("Vigo" ))

head (Datos2)
#lbls_APN  <- paste(c("", "\n"), levels(factor(Datos1$ESPECIE)))
#lbls_APN  <- paste(c("", "\n"), levels(reorder(Datos2$ESPECIE, -Datos2$CPUE)))
lbls_APN<-c("Rajidae", "\n Merluccius\n merluccius" ,"Ommastrephidae", "\n Scyliorhinus\n canicula",
            "Lepidorhombus\n spp" , "\n Lophius\n spp", " Galeus melastomus" ,"\n Trisopterus\n  spp",
            "Triglidae" , "\n Eledone\n spp","Solea solea" , "\n Phycis\n blennoides")
CPUE_BACA_APN<-ggplot(Datos2, aes(y=CPUE, x=factor(reorder(ESPECIE, -CPUE), labels = lbls_APN),  fill=factor(FUENTE)))+
        geom_bar(position="dodge",stat="identity" ,size=1, color="black")  +
        labs(subtitle=Datos2$PUERTO, title= Datos2$ESTRATO_RIM)  +
        facet_wrap(.~COD_TIPO_MUE, ncol = 1)   +
        theme(axis.text.x=element_text(angle=0,size=11, face="bold.italic"))  +
        #scale_fill_manual(values = c("cornflowerblue", "lightsteelblue", "darkslateblue"))+
        labs(x="",y="CPUE")   + theme(plot.title = element_text(lineheight=.8, face="bold"))+
        
        theme(strip.text.x = element_text(size=11, angle=0,face="bold"),
              strip.text.y = element_text(size=9, face="bold"),
              strip.background = element_blank())   +
        scale_fill_manual(values = c( "lightsteelblue","dodgerblue4", "darkorchid3","orange"))   +
        #strip.background = element_rect(colour="darkblue", fill="#CCCCFF")     +
        #ylim(0,3500)  +
        theme(legend.position="top", legend.box = "horizontal")+
        theme(legend.title=element_text( size = 11, face = "bold", colour="black"),
              legend.text = element_text( size = 10, face = "italic"))+
        guides(fill=guide_legend("FUENTE", override.aes = list(size=6)))+
        geom_text(data=subset(Datos2,subset=(FUENTE=="RIM")),aes(x=6, y=1500, label=paste(" Mareas Muestreadas=",MAREAS)),
                  fontface="bold",  size=4.5, col="darkblue")   + #ylim(0,10000)+
        
        geom_text(aes(label = round(CPUE,0)),
                  position=position_dodge(width=1), hjust=.6,vjust=-0.5,parse = TRUE,size=3) # +
CPUE_BACA_APN

#enmalle_ac

Datos1<-Datos %>%
        group_by(  ESTRATO_RIM,PUERTO,FUENTE, COD_TIPO_MUE) %>%
        dplyr::mutate(MAREAS=length(unique(COD_ID)))   %>%
        group_by( ESTRATO_RIM, PUERTO,FUENTE,ESPECIE, MAREAS, COD_TIPO_MUE ) %>%
        dplyr::summarise (PESO= sum(PESO)) %>%
        group_by(  ESTRATO_RIM,PUERTO, FUENTE,ESPECIE,COD_TIPO_MUE ) %>%
        dplyr::mutate(CPUE = round(PESO/MAREAS,2)) %>%  
        subset (ESTRATO_RIM %in% c("ENMALLE_AC")) %>%
        #filter(CPUE>10)
        #subset (PUERTO %in% c( "Santa Eugenia de Ribeira")) %>%
        #subset (TIPO_MUESTREO %in% c("1","2")) %>%
        #•subset (ESPECIE %in% c(ESPECIES$ESPECIE))  %>%
        subset(ESPECIE %in% c( "Merluccius merluccius","Molva macrophthalma", "Pollachius pollachius",
                              "Lophius spp"))
Datos2<-filter(Datos1, COD_TIPO_MUE %in% c("MT1","MT2") & FUENTE %in% c("RIM", "DP", "NV", "CONSUM")) 
                       

head (Datos2)
lbls_APN  <- paste(c("", "\n"), levels(factor(Datos1$ESPECIE)))
lbls_ENMALLE  <- paste(c("", "\n"), levels(reorder(Datos2$ESPECIE, -Datos2$CPUE)))
CPUE_ENMALLE<-ggplot(Datos2, aes(y=CPUE, x=factor(reorder(ESPECIE, -CPUE), labels = lbls_ENMALLE),  fill=factor(FUENTE)))+
        geom_bar(position="dodge",stat="identity" ,size=1, color="black")  +
        labs(subtitle=Datos2$PUERTO, title= Datos2$ESTRATO_RIM)  +
        facet_wrap(.~COD_TIPO_MUE, ncol = 1)   +
        theme(axis.text.x=element_text(angle=0,size=12, face="bold.italic"))  +
        #scale_fill_manual(values = c("cornflowerblue", "lightsteelblue", "darkslateblue"))+
        labs(x="",y="CPUE")   + theme(plot.title = element_text(lineheight=.8, face="bold"))+
        
        theme(strip.text.x = element_text(size=11, angle=0,face="bold"),
              strip.text.y = element_text(size=9, face="bold"),
              strip.background = element_blank())   +
        scale_fill_manual(values = c( "lightsteelblue","dodgerblue4", "darkorchid3","orange"))   +
        #strip.background = element_rect(colour="darkblue", fill="#CCCCFF")     +
        #ylim(0,3500)  +
        theme(legend.position="top", legend.box = "horizontal")+
        theme(legend.title=element_text( size = 11, face = "bold", colour="black"),
              legend.text = element_text( size = 10, face = "italic"))+
        guides(fill=guide_legend("FUENTE", override.aes = list(size=6)))+
        geom_text(data=subset(Datos2,subset=(FUENTE=="RIM")),aes(x=3.5, y=15000, label=paste(" Mareas Muestreadas=",MAREAS)),
                  fontface="bold",  size=4.5, col="darkblue")   + #ylim(0,10000)+
        
        geom_text(aes(label = round(CPUE,0)),
                  position=position_dodge(width=1), hjust=.6,vjust=-0.5,parse = TRUE,size=3)  +
        scale_y_sqrt(breaks=c(100,500,1000,2500,5000,10000,15000,25000))
CPUE_ENMALLE

lbls_ENMALLE

#######################

#####MERLUCER_AC

Datos1<-Datos %>%
        group_by(  ESTRATO_RIM,PUERTO,FUENTE, COD_TIPO_MUE) %>%
        dplyr::mutate(MAREAS=length(unique(COD_ID)))   %>%
        group_by( ESTRATO_RIM, PUERTO,FUENTE,ESPECIE, MAREAS, COD_TIPO_MUE ) %>%
        dplyr::summarise (PESO= sum(PESO)) %>%
        group_by(  ESTRATO_RIM,PUERTO, FUENTE,ESPECIE,COD_TIPO_MUE ) %>%
        dplyr::mutate(CPUE = round(PESO/MAREAS,2)) %>%  
        subset (ESTRATO_RIM %in% c("MERLUCER_AC")) %>%
        #filter(CPUE>10)
        #subset (PUERTO %in% c( "Santa Eugenia de Ribeira")) %>%
        #subset (TIPO_MUESTREO %in% c("1","2")) %>%
        #•subset (ESPECIE %in% c(ESPECIES$ESPECIE))  %>%
        subset(ESPECIE %in% c( "Merluccius merluccius","Molva macrophthalma","Ommastrephidae" , "Helicolenus dac.",
                               "Phycis blennoides","Glyptocephalus cynoglossus", "Lophius spp", "Nephrops norvegicus"))
Datos2<-filter(Datos1, COD_TIPO_MUE %in% c("MT1","MT2") & FUENTE %in% c("RIM", "DP", "NV", "CONSUM")) 

head (Datos2)
#lbls_MERLUCER_AC  <- paste(c("", "\n"), levels(factor(Datos2$ESPECIE)))
lbls_MERLUCER_AC<-c("Merluccius\n merluccius","Lophius\n  spp" ," Nephrops\n norvegicus", "\n Phycis\n blennoides",       
        "\n Glyptocephalus\n cynoglossus", "Ommastrephidae", "\n Helicolenus\n dactylopterus" , "Molva\n   macrophthalma")

#lbls_MERLUCER_AC<-paste(c("", "\n"), levels(reorder(Datos2$ESPECIE, -Datos2$CPUE)))
CPUE_MERLUCER_AC<-ggplot(Datos2, aes(y=CPUE, x=factor(reorder(ESPECIE, -CPUE), labels = lbls_MERLUCER_AC),  fill=factor(FUENTE)))+
        geom_bar(position="dodge",stat="identity" ,size=1, color="black")  +
        labs(subtitle=Datos2$PUERTO, title= Datos2$ESTRATO_RIM)  +
        facet_wrap(.~COD_TIPO_MUE, ncol = 1)   +
        theme(axis.text.x=element_text(angle=0,size=11, face="bold.italic"))  +
        #scale_fill_manual(values = c("cornflowerblue", "lightsteelblue", "darkslateblue"))+
        labs(x="",y="CPUE")   + theme(plot.title = element_text(lineheight=.8, face="bold"))+
        
        theme(strip.text.x = element_text(size=11, angle=0,face="bold"),
              strip.text.y = element_text(size=9, face="bold"),
              strip.background = element_blank())   +
        scale_fill_manual(values = c( "lightsteelblue","dodgerblue4", "darkorchid3","orange"))   +
        #strip.background = element_rect(colour="darkblue", fill="#CCCCFF")     +
        #ylim(0,3500)  +
        theme(legend.position="top", legend.box = "horizontal")+
        theme(legend.title=element_text( size = 11, face = "bold", colour="black"),
              legend.text = element_text( size = 10, face = "italic"))+
        guides(fill=guide_legend("FUENTE", override.aes = list(size=5)))+
        geom_text(data=subset(Datos2,subset=(FUENTE=="RIM")),aes(x=4, y=15000, label=paste(" Mareas Muestreadas=",MAREAS)),
                  fontface="bold",  size=4.5, col="darkblue")   + #ylim(0,10000)+
        
        geom_text(aes(label = round(CPUE,0)),
                  position=position_dodge(width=1), hjust=.6,vjust=-0.5,parse = TRUE,size=3)  +
        scale_y_sqrt(breaks=c(100,500,1000,2500,5000,10000,20000,30000))
CPUE_MERLUCER_AC


#####RAPANTER_AC

Datos1<-Datos %>%
        group_by(  ESTRATO_RIM,FUENTE, COD_TIPO_MUE) %>%
        dplyr::mutate(MAREAS=length(unique(COD_ID)))   %>%
        group_by( ESTRATO_RIM, FUENTE,ESPECIE, MAREAS, COD_TIPO_MUE ) %>%
        dplyr::summarise (PESO= sum(PESO)) %>%
        group_by(  ESTRATO_RIM, FUENTE,ESPECIE,COD_TIPO_MUE ) %>%
        dplyr::mutate(CPUE = round(PESO/MAREAS,2)) %>%  
        subset (ESTRATO_RIM %in% c("RAPANTER_AC")) %>%	
        #subset (PUERTO %in% c( "Santa Eugenia de Ribeira")) %>%
        subset (COD_TIPO_MUE %in% c("MT1", "MT2")) %>%
        #?subset (ESPECIE %in% c(ESPECIES$ESPECIE))  %>%
        subset(ESPECIE %in% c("Lophius spp", "Merluccius merluccius", "Rajidae", "Molva molva",
                              "Ommastrephidae",	"Microstomus kitt",  "Lepidorhombus spp", "Glyptocephalus cynoglossus","Helicolenus dactylopterus",
                              "Nephrops norvegicus","Zeus faber" , "Glyptocephalus cyn."
        )) %>%
        as.data.frame() 


Datos2<-filter(Datos1,  FUENTE %in% c("CONSUM" ,  "RIM", "DP", "NV") & !(COD_TIPO_MUE  %in% ("MT4")))
head (Datos2)

lbls_RAPA<-c("Lophius\n spp","\n Lepidorhombus\n spp", "Merluccius merluccius", "\n \n Ommastrephidae" ,
             "Glyptocephalus\n cynoglossus", "\n Rajidae" ,"Nephrops\n norvegicus", 
             "Microstomus\n kitt",   "Zeus\n faber",        "Molva\n molva"   	  
)
#lbls_RAPA<-paste(c("", "\n"), levels(reorder(Datos2$ESPECIE, -Datos2$CPUE)))

CPUE_RAPA<- ggplot(Datos2, aes(y=CPUE, x=factor(reorder(ESPECIE, -CPUE), labels = lbls_RAPA),  fill=factor(FUENTE)))+
        geom_bar(position="dodge",stat="identity" ,size=1, color="black")  +
        labs(subtitle="Vigo", title= Datos2$ESTRATO_RIM)  +
        facet_wrap(.~COD_TIPO_MUE, ncol=1)   +
        theme(axis.text.x=element_text(angle=0,size=10, face="bold.italic"))  +
        #scale_fill_manual(values = c("cornflowerblue", "lightsteelblue", "darkslateblue"))+
        labs(x="",y="CPUE")   + theme(plot.title = element_text(lineheight=.8, face="bold"))+
        
        theme(strip.text.x = element_text(size=11, angle=0,face="bold"),
              strip.text.y = element_text(size=9, face="bold"),
              strip.background = element_blank())   +
        scale_fill_manual(values = c( "lightsteelblue","dodgerblue4", "darkorchid3","orange"))   +
        #strip.background = element_rect(colour="darkblue", fill="#CCCCFF")     +
        #ylim(0,3500)  +
        theme(legend.position="top")+
        theme(legend.title=element_text( size = 11, face = "bold", colour="black"),
              legend.text = element_text( size = 10, face = "italic"))+
        guides(fill=guide_legend("FUENTE", override.aes = list(size=5)))+
        geom_text(data=subset(Datos2,subset=(FUENTE=="RIM")),aes(x=8, y=max(0.8*CPUE), label=paste(" Mareas Muestreadas=",MAREAS)),
                  fontface="bold",  size=4, col="darkblue")   + #ylim(0,10000)+
        
        geom_text(aes(label = round(CPUE,0)),
                  position=position_dodge(width=1), hjust=.6,vjust=-0.5,parse = TRUE,size=3) 


CPUE_RAPA
##############PALANGRE_AC

Datos1<-Datos %>%
        group_by(  ESTRATO_RIM,PUERTO,FUENTE, COD_TIPO_MUE) %>%
        dplyr::mutate(MAREAS=length(unique(COD_ID)))   %>%
        group_by( ESTRATO_RIM, PUERTO,FUENTE,ESPECIE, MAREAS, COD_TIPO_MUE ) %>%
        dplyr::summarise (PESO= sum(PESO)) %>%
        group_by(  ESTRATO_RIM,PUERTO, FUENTE,ESPECIE,COD_TIPO_MUE ) %>%
        dplyr::mutate(CPUE = round(PESO/MAREAS,2)) %>%  
        subset (ESTRATO_RIM %in% c("PALANGRE_AC")) %>%
        #filter(CPUE>10)
        #subset (PUERTO %in% c( "Santa Eugenia de Ribeira")) %>%
        #subset (TIPO_MUESTREO %in% c("1","2")) %>%
        #•subset (ESPECIE %in% c(ESPECIES$ESPECIE))  %>%
        subset(ESPECIE %in% c( "Merluccius merluccius","Molva molva", "Helicolenus dac.","Phycis blennoides",
                               "Pagellus bogaraveo", "Conger conger"))
Datos2<-filter(Datos1, COD_TIPO_MUE %in% c("MT1","MT2") & FUENTE %in% c("RIM", "DP", "NV", "CONSUM")) 

table(Datos2$ESPECIE)
head (Datos2)
lbls_PALANGRE_AC  <- paste(c("", "\n"), levels(reorder(Datos2$ESPECIE, -Datos2$CPUE)))
lbls_PALANGRE_AC<- c("\n Merluccius\n merluccius","Helicolenus\n dactylop", "\n Phycis\n blennoides","Pagellus\n bogaraveo",
                     "Conger\n conger",    "Molva\n molva"  )
paste(c("", "\n"), levels(reorder(Datos2$ESPECIE, -Datos2$CPUE)))

PUERTO_names <- c(
        `Cillero` = "Celeiro",
        `Avilés` = "Avilés",
        "MT1" ="MT1",
        "MT2"="MT2"
)

CPUE_PALANGRE_AC<-ggplot(Datos2, aes(y=CPUE, x=factor(reorder(ESPECIE, -CPUE), labels = lbls_PALANGRE_AC),  fill=factor(FUENTE)))+
        geom_bar(position="dodge",stat="identity" ,size=1, color="black")  +
        labs(subtitle="Por Puerto y Tipo de Muestreo", title= Datos2$ESTRATO_RIM)  +
        facet_grid(PUERTO~COD_TIPO_MUE, labeller = as_labeller(PUERTO_names))   +
        theme(axis.text.x=element_text(angle=0,size=11, face="bold.italic"))  +
        #scale_fill_manual(values = c("cornflowerblue", "lightsteelblue", "darkslateblue"))+
        labs(x="",y="CPUE")   + theme(plot.title = element_text(lineheight=.8, face="bold"))+
        
        theme(strip.text.x = element_text(size=11, angle=0,face="bold"),
              strip.text.y = element_text(size=11, face="bold"),
              strip.background = element_blank())   +
        scale_fill_manual(values = c( "lightsteelblue","dodgerblue4", "darkorchid3","orange"))   +
        #strip.background = element_rect(colour="darkblue", fill="#CCCCFF")     +
        #ylim(0,3500)  +
        theme(legend.position="top", legend.box = "horizontal")+
        theme(legend.title=element_text( size = 12, face = "bold", colour="black"),
              legend.text = element_text( size = 10, face = "italic"))+
        guides(fill=guide_legend("FUENTE", override.aes = list(size=5)))+
        geom_text(data=subset(Datos2,subset=(FUENTE=="RIM")),aes(x=4.5, y=15000, label=paste(" Mareas Muestreadas=",MAREAS)),
                  fontface="bold",  size=4.5, col="darkblue")   + #ylim(0,10000)+
        
        geom_text(aes(label = round(CPUE,0)),
                  position=position_dodge(width=1), hjust=.6,vjust=-0.5,parse = TRUE,size=3)  +
        scale_y_sqrt(breaks=c(100,500,1000,2500,5000,10000,15000))
CPUE_PALANGRE_AC

rm(lbls_RAPA)


table(Datos$PUERTO)
mareas1<-subset(Datos, PUERTO %in% c("A Coruña") & FUENTE %in% c("NV", "DP", "RIM") & ESTRATO_RIM==c("PAREJA_CN") &
                        ESPECIE %in% c("Ommastrephidae", "Scomber scombrus", "Merluccius merluccius", "Trachurus spp",
                                       "Merluccius merluccius", "Lophius spp", "Micromesistius poutassou"))
mareas1 = subset(mareas1, !(ESPECIE %in% c("Scomber colias")))
head (mareas1)

n_fun <- function(x){
        return(data.frame(y =-500,
                          label = length(x)))
}

PARCOR2<-ggplot(subset(mareas1, PESO>1 ),
                aes(x=reorder(factor(ESPECIE), -PESO),y=PESO), fill=FUENTE)   +
        geom_point(aes(fill=ESPECIE,size=PESO), shape=21,
                   position=position_jitter(width=0.2, height=0.2)) +
        scale_size(range = c(2,8)) +
        stat_boxplot(geom ='errorbar', width = 0.26)    +
        geom_boxplot(width = 0.5, aes(fill = ESPECIE),alpha=0.4, show.legend=FALSE) +
        
        guides(size=FALSE,fill=guide_legend(override.aes=list(size=1)))  +
        labs(title=paste(mareas1$ESTRATO_RIM,mareas1$PUERTO), subtitle="Pesos por Marea",
             x=NULL)  +
        theme_bw()+
        facet_wrap(.~FUENTE)  +
        stat_summary(fun.data = n_fun, geom = "text", hjust = 0.5, fontface="bold", size=3) +
        #expand_limits(y = -310) +
        theme(plot.title = element_text(lineheight=.8, face="bold"))+
        theme(axis.text.x=element_text(angle=45,size=10, hjust=1,face= "bold.italic"))   +
        theme(strip.text.x = element_text(family = "sans",size=18, angle=0,face=c("italic")),
              strip.text.y = element_text(size=12, face="bold"),
              strip.background = element_blank())+
        theme(legend.position="top")+
        theme(legend.title=element_text( size = 12, face = "bold", colour="blue"),
              legend.text = element_text( size = 10, face = "italic"))+
        guides(fill=guide_legend("Especie", override.aes = list(size=6))) +
        scale_fill_manual(values=col)# +scale_y_sqrt()
PARRIB2
PARRIB4
PARCOR2
PARAVI2
rm(PARACOR3)
PARRIB3<-ggplot(subset(mareas1, PESO>1),
                aes(x=reorder(factor(ESPECIE), -PESO),y=PESO), fill=FUENTE)   +
        geom_point(aes(fill=ESPECIE,size=PESO), shape=21,
                   position=position_jitter(width=0.2, height=0.2)) +
        scale_size(range = c(2,10)) +
        stat_boxplot(geom ='errorbar', width = 0.26)    +
        geom_boxplot(width = 0.5, aes(fill = ESPECIE),alpha=0.4, show.legend=FALSE) +
        
        guides(size=FALSE,fill=guide_legend(override.aes=list(size=1)))  +
        labs(title=paste(mareas1$ESTRATO_RIM,mareas1$PUERTO), subtitle="Pesos por Marea",
             x=NULL)  +
        theme_bw()+
        facet_wrap(.~FUENTE, scales="free_x")  +
        #stat_summary(fun.data = n_fun, geom = "text", hjust = 0.5, fontface="bold", size=3) +
        #expand_limits(y = -310) +
        theme(plot.title = element_text(lineheight=.8, face="bold"))+
        theme(axis.text.x=element_text(angle=45,size=10, hjust=1,face= "bold.italic"))   +
        theme(strip.text.x = element_text(family = "sans",size=18, angle=0,face=c("italic")),
              strip.text.y = element_text(size=12, face="bold"),
              strip.background = element_blank())+
        theme(legend.position="top")+
        theme(legend.title=element_text( size = 12, face = "bold", colour="blue"),
              legend.text = element_text( size = 10, face = "italic"))+
        guides(fill=guide_legend("Especie", override.aes = list(size=6))) +
        scale_fill_manual(values=col) +scale_y_sqrt(breaks=c(50,500,1000,2000,5000,10000,15000,20000))+
        geom_abline(aes(slope = 0, intercept = 1), lty = "dotted", lwd = 1.5)




ls()
col <- c("darkorange1","#FF61CC", "darkslateblue", "violet", "mediumorchid",  "firebrick1","chartreuse3",  "pink","brown","orange", "goldenrod", "violet",
         "tomato","chocolate", "magenta", "black", "seagreen3", "#F8766D", "mediumslateblue", "aliceblue", "navy", "red",
         "cadetblue", "#00BA38", "darkorchid3", "slategray3","#619CFF", "darkgreen", "darkmagenta", "#F8766D", "purple", "royalblue",
         "hotpink", "violetred", "gray96", "lemonchiffon2", "indianred2", "lightsalmon1", "blue3" , "darkseagreen", "cyan3", "lightblue","darkgoldenrod2",
         "tan1", "cornsilk2", "deeppink2", "darkblue", "darkorchid3", "mediumaquamarine", "Chartreuse", "blue", "LavenderBlush", "cornsilk","forestgreen",
         "darkmagenta","#D39200",  "mediumslateblue","#FF61CC", "darkslateblue", "darkseagreen", "mediumorchid","#619CFF","Chartreuse" ,"#FF61CC",
         "goldenrod","deeppink2","firebrick1","darkorange3","greenyellow","mediumturquoise","thistle1","skyblue4","springgreen3","violetred2","sandybrown",
         "lightslateblue","lightsalmon2","lightsalmon3", "lightpink", "ivory4","hotpink4", "green1", "khaki1","gold","floralwhite", "darkseagreen",
         "red2")

names(col)<-c("Merluccius merluccius","Trachurus spp","Micromesistius poutassou", "Scomber scombrus","Lophius spp", "Ommastrephidae", "Lepidorhombus spp",
              "Rajidae", "Eledone spp","Triglidae", "Trisopterus spp.","Conger conger", "Molva molva", "Nephrops norvegicus" , "Brama brama",
              "Helicolenus dactylopterus", "Sparus aurata", "Boops boops", "Sardina pilchardus", "Engraulis encrasicolus", "Scomber colias",
              "S. canicula", "Octopus vulgaris",   "Phycis blennoides", "Phycis phycis", "Mullus spp","Pagellus acarne", "Scorpaeniformes",
              "Molva macrophthalma", "Sepia spp.", "Squilla mantis", "Loligo spp", "Parepenaeus longirostris", "Stichopus spp" , "Gadus morhua",
              "Zeus faber", "Glyptocephalus cynoglossus", "Microstomus kitt", "Beryx spp.", "Dicentrarchus labrax", "Mora moro", "Pollachius pollachius",
              "Galeus melastomus", "Solea solea", "Gadus morhua", "Polyprion americanus", "Pagellus bogaraveo", "Squaliformes", "Penaeus kerathurus","OTH",
              "Argyrosomus regius", "Parapenaeus longirostris", "Aphanopus carbo", "Pomatomus saltatrix", "Pagellus erythrinus", "Scyliorhinus canicula",
              "Pagellus spp", "Lophius budegassa", "Lophius piscatorius",        "Lepidorhombus boscii", "Lepidorhombus whiffiagonis",
              "Trachurus mediterraneus", "Trachurus picturatus", "Trachurus trachurus", "Trisopterus luscus","Mullus surmuletus",
              "Chelidonichthys lucerna", "Raja clavata", "Eutrigla gurnardus", "Leucoraja naevus", "Lepidopus caudatus", "Dipturus oxyrinchus",
              "Trigla lyra", "Raja montagui","Raja miraletus", "Leucoraja fullonica", "Beryx decadactylus","Beryx splendens", "Todaropsis eblanae", "Todarodes sagittatus",
              "Illex coindetii","Chelidonichthys cuculus", "Chelidonichthys obscurus","Eledone cirrhosa", "Trisopterus minutus", "Trisopterus spp",
              "Sepia spp")

head (Datos)
head (muestreos1)
Datos2<-filter(Datos1, COD_TIPO_MUE %in% c("MT4","MT6") & FUENTE %in% c("RIM", "DP", "NV", "CONSUM"))
head (Datos2)
lbls_desc  <- paste(c("", "\n"), levels(factor(Datos2$ESPECIE)))
ggplot(Datos2, aes(y=CPUE, x=factor(ESPECIE, labels = lbls_desc),  fill=factor(FUENTE)))+
        geom_bar(position="dodge",stat="identity" ,size=1, color="black")  +
        labs(subtitle=Datos2$PUERTO, title= Datos2$ESTRATO_RIM)  +
        facet_wrap(PUERTO~COD_TIPO_MUE, ncol = 1)   +
        theme(axis.text.x=element_text(angle=0,size=14, face="bold.italic"))  +
        #scale_fill_manual(values = c("lightsteelblue","cornflowerblue", "lightsteelblue", "darkslateblue"))+
        labs(x="",y="CPUE")   + theme(plot.title = element_text(lineheight=.8, face="bold"))+
        theme(strip.text.x = element_text(size=14, angle=0,face="bold.italic"),
              strip.text.y = element_text(size=9, face="bold"),
              strip.background = element_blank())   +
        scale_fill_manual(values = c("lightsteelblue","dodgerblue4", "darkorchid3","orange"))   +
        #strip.background = element_rect(colour="darkblue", fill="#CCCCFF")     +
        #ylim(0,3500)  +
        theme(legend.position=c(0.1,0.9), legend.box = "horizontal")+
        theme(legend.title=element_text( size = 12, face = "bold", colour="black"),
              legend.text = element_text( size = 10, face = "italic"))+
        guides(fill=guide_legend("Año", override.aes = list(size=6)))+
        geom_text(data=subset(Datos2,subset=(FUENTE=="RIM")),aes(x=5, y=5000, label=paste(" Mareas Muestreadas=",MAREAS)),
                  fontface="bold",    size=4, col="darkblue")   + #ylim(0,10000)+
        
        geom_text(aes(label = round(CPUE,0)),
                  position=position_dodge(width=1), hjust=.6,vjust=-0.5,parse = TRUE,size=2.5) 



########BACA_GC
########BACA_GC

#str(muestreoscat)
#Datos_con_respon<-left_join(Datos, muestreoscat[,c(1,7)])%>%distinct()
#head(Datos_con_respon)
Datos1<-Datos %>%
        group_by(  ESTRATO_RIM,FUENTE, COD_TIPO_MUE) %>%
        dplyr::mutate(MAREAS=length(unique(COD_ID)))   %>%
        group_by( ESTRATO_RIM, FUENTE,ESPECIE, MAREAS, COD_TIPO_MUE ) %>%
        dplyr::summarise (PESO= sum(PESO)) %>%
        group_by(  ESTRATO_RIM, FUENTE,ESPECIE,COD_TIPO_MUE ) %>%
        dplyr::mutate(CPUE = round(PESO/MAREAS,2)) %>%  
        subset (ESTRATO_RIM %in% c("BACA_GC")) %>%	
        #subset (PUERTO %in% c( "Santa Eugenia de Ribeira")) %>%
        subset (COD_TIPO_MUE %in% c("MT4")) %>%
        #?subset (ESPECIE %in% c(ESPECIES$ESPECIE))  %>%
        subset(ESPECIE %in% c("Micromesistius poutassou",  "Eledone spp","Merluccius merluccius",
                              "Squilla mantis", "Parapenaeus longirostris", "Sepia spp", "Octopus vulgaris", "Loligo spp",
                              "Nephrops norvegicus",  "Galeorrhinus galeus", "Penaeus kerathurus")) %>%
        as.data.frame() 

Datos2<-filter(Datos1,  FUENTE %in% c("CONSUM" ,  "RIM", "DP", "NV") & !(COD_TIPO_MUE  %in% ("MT2")))
head (Datos2)
lbls_baca_gc<-paste(c("", "\n"), levels(reorder(Datos2$ESPECIE, -Datos2$CPUE)))
lbls_baca_gc<-c("Parapenaeus\n longirostris",  "Squilla\n mantis", "Sepia\n spp" ,"Merluccius\n merluccius",
                "Loligo\n spp","Eledone\n spp", "Penaeus\n kerathurus","Octopus\n vulgaris", 
                "Microm.\n poutassou", "Nephrops\n norvegicus")

CPUE_BACA_GC<- ggplot(Datos2, aes(y=CPUE, x=factor(reorder(ESPECIE, -CPUE), labels = lbls_baca_gc),  fill=factor(FUENTE)))+
        geom_bar(position="dodge",stat="identity" ,size=1, color="black")  +
        labs(subtitle="Muestreos a bordo 2018", title= Datos2$ESTRATO_RIM)  +
        facet_wrap(.~COD_TIPO_MUE, ncol=1)   +
        theme(axis.text.x=element_text(angle=0,size=11, face="bold.italic"))  +
        #scale_fill_manual(values = c("cornflowerblue", "lightsteelblue", "darkslateblue"))+
        labs(x="",y="CPUE")   + theme(plot.title = element_text(lineheight=.8, face="bold"))+
        
        theme(strip.text.x = element_text(size=10, angle=0,face="bold"),
              strip.text.y = element_text(size=9, face="bold"),
              strip.background = element_blank())   +
        scale_fill_manual(values = c( "lightsteelblue","dodgerblue4", "darkorchid3","orange"))   +
        #strip.background = element_rect(colour="darkblue", fill="#CCCCFF")     +
        #ylim(0,3500)  +
        theme(legend.position="top", legend.box = "horizontal")+
        theme(legend.title=element_text( size = 11, face = "bold", colour="black"),
              legend.text = element_text( size = 10, face = "italic"))+
        guides(fill=guide_legend("FUENTE", override.aes = list(size=6)))+
        geom_text(data=subset(Datos2,subset=(FUENTE=="RIM")),aes(x=3, y=max(0.8*CPUE), label=paste(" Mareas Muestreadas=",MAREAS)),
                  fontface="bold",  size=4, col="darkblue")   + #ylim(0,10000)+
        
        geom_text(aes(label = round(CPUE,0)),
                  position=position_dodge(width=1), hjust=.6,vjust=-0.5,parse = TRUE,size=3) 


CPUE_BACA_GC
gc()
mareas1<-subset(Datos, FUENTE %in% c("NV", "DP", "RIM") & ESTRATO_RIM==c("BACA_GC") & COD_TIPO_MUE %in% c("MT4") &
                        ESPECIE %in% c("Micromesistius poutassou",  "Eledone spp","Merluccius merluccius",
                                       "Squilla mantis", "Parapenaeus longirostris", "Sepia spp", "Octopus vulgaris", "Loligo spp",
                                       "Nephrops norvegicus",  "Galeorrhinus galeus", "Penaeus kerathurus"))
mareas1 = subset(mareas1, !(ESPECIE %in% c("Scomber colias")))
head (mareas1)

n_fun <- function(x){
        return(data.frame(y =-10,
                          label = length(x)))
}

MAREAS_bacadiz<-ggplot(subset(mareas1, PESO>1 ),
                aes(x=reorder(factor(ESPECIE), -PESO),y=PESO), fill=FUENTE)   +
        geom_point(aes(fill=ESPECIE,size=PESO), shape=21,
                   position=position_jitter(width=0.2, height=0.2)) +
        scale_size(range = c(2,8)) +
        stat_boxplot(geom ='errorbar', width = 0.26)    +
        geom_boxplot(width = 0.5, aes(fill = ESPECIE),alpha=0.4, show.legend=FALSE) +
        
        guides(size=FALSE,fill=guide_legend(override.aes=list(size=1)))  +
        labs(title=paste(mareas1$ESTRATO_RIM,"Todos los puertos"), subtitle="Pesos por Marea",
             x=NULL)  +
        theme_bw()+
        facet_grid(COD_TIPO_MUE~FUENTE)  +
        stat_summary(fun.data = n_fun, geom = "text", hjust = 0.5, fontface="bold", size=3) +
        #expand_limits(y = -310) +
        theme(plot.title = element_text(lineheight=.8, face="bold"))+
        theme(axis.text.x=element_text(angle=45,size=10, hjust=1,face= "bold.italic"))   +
        theme(strip.text.x = element_text(size=11, angle=0,face="bold", colour="white"),
              strip.text.y = element_text(size=10, face="bold",colour="white"),
              strip.background = element_rect(colour="white", fill=c( "steelblue")))+
        theme(legend.position="none")+
        theme(panel.background = element_rect(fill = "lightblue", colour = 'red'))  +
        theme(legend.title=element_text( size = 12, face = "bold", colour="blue"),
              legend.text = element_text( size = 10, face = "italic"))+
        guides(fill=guide_legend("Especie", override.aes = list(size=6))) +
        scale_fill_manual(values=col)# +scale_y_sqrt()

MAREAS_bacadiz




mareas1$facet_fill_color <- c("dodgerblue4", "darkorchid3","orange1")[factor(mareas1$FUENTE)] 
head (mareas1)
g <- ggplot_gtable(ggplot_build(MAREAS_bacadiz))
strip_t <- which(grepl('strip-t', g$layout$name))
fills <- c("dodgerblue4", "darkorchid3","orange1") 
k <- 1
for (i in strip_t) {
        j <- which(grepl('rect', g$grobs[[i]]$grobs[[1]]$childrenOrder))
        g$grobs[[i]]$grobs[[1]]$children[[j]]$gp$fill <- fills[k]
        k <- k+1
}
dev.off()
MAREAS_bacadizg2<-grid.draw(g)   


        ################CERCO_GC###########3
        
        ########CERCO_GC
Datos1<-Datos %>%
        group_by(  ESTRATO_RIM,FUENTE, COD_TIPO_MUE) %>%
        dplyr::mutate(MAREAS=length(unique(COD_ID)))   %>%
        group_by( ESTRATO_RIM, FUENTE,ESPECIE, MAREAS, COD_TIPO_MUE ) %>%
        dplyr::summarise (PESO= sum(PESO)) %>%
        group_by(  ESTRATO_RIM, FUENTE,ESPECIE,COD_TIPO_MUE ) %>%
        dplyr::mutate(CPUE = round(PESO/MAREAS,2)) %>%  
        subset (ESTRATO_RIM %in% c("CERCO_GC")) %>%	
        #subset (PUERTO %in% c( "Santa Eugenia de Ribeira")) %>%
        subset (COD_TIPO_MUE %in% c("MT4")) %>%
        #?subset (ESPECIE %in% c(ESPECIES$ESPECIE))  %>%
        subset(ESPECIE %in% c("Scomber colias", "Scomber scombrus", "Sardina pilchardus","Sarda sarda",
                              "Trachurus spp", "Engraulis encrasicolus", "Pomatomus saltatrix")) %>%
        as.data.frame() 

head (Datos)
table(Datos1$ESPECIE)

Datos2<-filter(Datos1,  FUENTE %in% c("CONSUM" ,  "RIM", "DP", "NV") & !(COD_TIPO_MUE  %in% ("MT2")))
head (Datos2)
Datos<-arrange(Datos, -PESO)
Datos$PESO<-as.character (Datos$PESO)
Datos$PESO[Datos$PESO== 110250.00 ] <- 4410
Datos$PESO<-as.numeric (Datos$PESO)
head (Datos)
lbls_cerco_gc <- paste(c("", "\n"), levels(factor(Datos2$ESPECIE)))
lbls_cerco_gc<-paste(c("", "\n"), levels(reorder(Datos2$ESPECIE, -Datos2$CPUE)))
CPUE_CERCO_GC<- ggplot(Datos2, aes(y=CPUE, x=factor(reorder(ESPECIE, -CPUE), labels = lbls_cerco_gc),  fill=factor(FUENTE)))+
        geom_bar(position="dodge",stat="identity" ,size=1, color="black")  +
        labs(subtitle="Muestreos a bordo 2018", title= Datos2$ESTRATO_RIM)  +
        facet_wrap(.~COD_TIPO_MUE, ncol=1)   +
        theme(axis.text.x=element_text(angle=0,size=12, face="bold.italic"))  +
        #scale_fill_manual(values = c("cornflowerblue", "lightsteelblue", "darkslateblue"))+
        labs(x="",y="CPUE")   + theme(plot.title = element_text(lineheight=.8, face="bold"))+
        
        theme(strip.text.x = element_text(size=11, angle=0,face="bold"),
              strip.text.y = element_text(size=9, face="bold"),
              strip.background = element_blank())   +
        scale_fill_manual(values = c( "lightsteelblue","dodgerblue4", "darkorchid3","orange"))   +
        #strip.background = element_rect(colour="darkblue", fill="#CCCCFF")     +
        #ylim(0,3500)  +
        theme(legend.position="top", legend.box = "horizontal")+
        theme(legend.title=element_text( size = 11, face = "bold", colour="black"),
              legend.text = element_text( size = 10, face = "italic"))+
        #guides(fill=guide_legend("A?o", override.aes = list(size=6)))+
        geom_text(data=subset(Datos2,subset=(FUENTE=="RIM")),aes(x=3, y=max(0.8*CPUE), label=paste(" Mareas Muestreadas=",MAREAS)),
                  fontface="bold",  size=4, col="darkblue")   + #ylim(0,10000)+
        
        geom_text(aes(label = round(CPUE,0)),
                  position=position_dodge(width=1), hjust=.6,vjust=-0.5,parse = TRUE,size=3) 


CPUE_CERCO_GC
gc()
mareas1<-subset(Datos, FUENTE %in% c("NV", "DP", "RIM") & ESTRATO_RIM==c("CERCO_GC") & COD_TIPO_MUE %in% c("MT4") &
                        ESPECIE %in% c("Scomber colias", "Scomber scombrus", "Sardina pilchardus",
                                       "Trachurus spp", "Engraulis encrasicolus", "Pomatomus saltatrix", "Sarda sarda"))
mareas1 = subset(mareas1, !(ESPECIE %in% c("Scomber colias")))
head (mareas1)

n_fun <- function(x){
        return(data.frame(y =-10,
                          label = length(x)))
}

MAREAS_CERCO_GC1<-ggplot(subset(mareas1, PESO>5 ),
                       aes(x=reorder(factor(ESPECIE), -PESO),y=PESO), fill=FUENTE)   +
        geom_point(aes(fill=ESPECIE,size=PESO), shape=21,
                   position=position_jitter(width=0.2, height=0.2)) +
        scale_size(range = c(2,8)) +
        stat_boxplot(geom ='errorbar', width = 0.26)    +
        geom_boxplot(width = 0.5, aes(fill = ESPECIE),alpha=0.4, show.legend=FALSE) +
        
        guides(size=FALSE,fill=guide_legend(override.aes=list(size=1)))  +
        labs(title=paste(mareas1$ESTRATO_RIM,"Muestreos a Bordo 2018"), subtitle="Pesos por Marea",
             x=NULL)  +
        #theme_bw()+
        facet_grid(COD_TIPO_MUE~FUENTE)  +
        stat_summary(fun.data = n_fun, geom = "text", hjust = 0.5, fontface="bold", size=3) +
        #expand_limits(y = -310) +
        theme(plot.title = element_text(lineheight=.8, face="bold"))+
        theme(axis.text.x=element_text(angle=45,size=10, hjust=1,face= "bold.italic"))   +
        theme(strip.text.x = element_text(size=11, angle=0,face="bold", colour="white"),
              strip.text.y = element_text(size=10, face="bold",colour="white"),
              strip.background = element_rect(colour="white", fill=c( "steelblue")))+
        theme(legend.position="none")+
        theme(panel.background = element_rect(fill = "lightblue", colour = 'red'))  +
        theme(legend.title=element_text( size = 12, face = "bold", colour="blue"),
              legend.text = element_text( size = 10, face = "italic"))+
        guides(fill=guide_legend("Especie", override.aes = list(size=6))) +
        scale_fill_manual(values=col) +scale_y_sqrt()

MAREAS_CERCO_GC1
library(grid)
mareas1$facet_fill_color <- c("dodgerblue4", "darkorchid3","orange1")[factor(mareas1$FUENTE)] 
head (mareas1)
MAREAS_CERCO_GC1g <- ggplot_gtable(ggplot_build(MAREAS_CERCO_GC1))
strip_t <- which(grepl('strip-t', MAREAS_CERCO_GC1g$layout$name))
fills <- c("dodgerblue4", "darkorchid3","orange1") 
k <- 1
for (i in strip_t) {
        j <- which(grepl('rect', MAREAS_CERCO_GC1g$grobs[[i]]$grobs[[1]]$childrenOrder))
        MAREAS_CERCO_GC1g$grobs[[i]]$grobs[[1]]$children[[j]]$gp$fill <- fills[k]
        k <- k+1
}
dev.off()
MAREAS_CERCO_GC12<- grid.draw(MAREAS_CERCO_GC1g)  

#################################################3
        



####################
################3palangre cn
ls()
Datos1<-Datos %>%
        group_by(  ESTRATO_RIM,FUENTE, PUERTO,COD_TIPO_MUE) %>%
        dplyr::mutate(MAREAS=length(unique(COD_ID)))   %>%
        group_by( ESTRATO_RIM,PUERTO, FUENTE,ESPECIE, MAREAS, COD_TIPO_MUE ) %>%
        dplyr::summarise (PESO= sum(PESO)) %>%
        group_by(  ESTRATO_RIM,PUERTO, FUENTE,ESPECIE,COD_TIPO_MUE ) %>%
        dplyr::mutate(CPUE = round(PESO/MAREAS,2)) %>%  
        subset (ESTRATO_RIM %in% c("PALANGRE_CN")) %>%	
        #subset (PUERTO %in% c( "Santa Eugenia de Ribeira")) %>%
        #subset (COD_TIPO_MUE %in% c("MT4")) %>%
        #?subset (ESPECIE %in% c(ESPECIES$ESPECIE))  %>%
        subset(ESPECIE %in% c("Conger conger", "Merluccius merluccius","Phycis blennoides",
                               "Pagellus bogaraveo", "Dicentrarchus labrax","Pollachius pollachius",
                              "Helicolenus dac.", "Helicolenus dactylopterus")) %>%
        as.data.frame() 

Datos2<-filter(Datos1,  FUENTE %in% c("CONSUM" ,  "RIM", "DP", "NV") &
                       PUERTO %in% c("Avilés", "A Coruña", "Ribeira") &
                       !(COD_TIPO_MUE  %in% ("MT2") ))
head (Datos2)
table(Datos2$ESPECIE)

lbls_PALN <- c("\n Merluccius\n merluccius","Conger\n conger", "Pollachius\n pollachius","Phycis\n blennoides",
               "Helicolenus\n dactylopterus",
               "Pagellus\n bogaraveo","Dicentrarchus\n labrax")
#lbls_PALN<- paste(c("", "\n"), levels(factor(Datos2$ESPECIE)))
#lbls_PALN<-paste(c("", "\n"), levels(reorder(Datos2$ESPECIE, -Datos2$CPUE)))
CPUE_PALN2<- ggplot(Datos2, aes(y=CPUE, x=factor(reorder(ESPECIE, -CPUE), labels = lbls_PALN),  fill=factor(FUENTE)))+
        geom_bar(position="dodge",stat="identity" ,size=1, color="black")  +
        labs(subtitle=Datos2$COD_TIPO_MUE, title= Datos2$ESTRATO_RIM)  +
        facet_wrap(.~PUERTO, ncol=1, scales="free_y")   +
        theme(axis.text.x=element_text(angle=0,size=11, face="bold.italic"))  +
        #scale_fill_manual(values = c("cornflowerblue", "lightsteelblue", "darkslateblue"))+
        labs(x="",y="CPUE")   + theme(plot.title = element_text(lineheight=.8, face="bold"))+
        
        theme(strip.text.x = element_text(size=11, angle=0,face="bold"),
              strip.text.y = element_text(size=9, face="bold"),
              strip.background = element_blank())   +
        scale_fill_manual(values = c( "lightsteelblue","dodgerblue4", "darkorchid3","orange"))   +
        #strip.background = element_rect(colour="darkblue", fill="#CCCCFF")     +
        #ylim(0,3500)  +
        theme(legend.position= "top")+
        theme(legend.title=element_text( size = 12, face = "bold", colour="black"),
              legend.text = element_text( size = 10, face = "italic"))+
        guides(fill=guide_legend("FUENTE", override.aes = list(size=5)))+
        geom_text(data=subset(Datos2,subset=(FUENTE=="RIM")),aes(x=3, y=max(0.8*CPUE), label=paste(" Mareas Muestreadas=",MAREAS)),
                  fontface="bold",  size=4, col="darkblue")   + #ylim(0,10000)+
        
        geom_text(aes(label = round(CPUE,2)),
                  position=position_dodge(width=1), hjust=.6,vjust=-0.5,parse = TRUE,size=3) #+
       # scale_y_sqrt(breaks=c(50,100,250,500,750,1000))


CPUE_PALN
CPUE_PALN2



################BETA
Datos1<-Datos %>%
        group_by(  ESTRATO_RIM,FUENTE, PUERTO,COD_TIPO_MUE) %>%
        dplyr::mutate(MAREAS=length(unique(COD_ID)))   %>%
        group_by( ESTRATO_RIM,PUERTO, FUENTE,ESPECIE, MAREAS, COD_TIPO_MUE ) %>%
        dplyr::summarise (PESO= sum(PESO)) %>%
        group_by(  ESTRATO_RIM,PUERTO, FUENTE,ESPECIE,COD_TIPO_MUE ) %>%
        dplyr::mutate(CPUE = round(PESO/MAREAS,2)) %>%  
        subset (ESTRATO_RIM %in% c("BETA_CN")) %>%	
        #subset (PUERTO %in% c( "Santa Eugenia de Ribeira")) %>%
        #subset (COD_TIPO_MUE %in% c("MT4")) %>%
        #?subset (ESPECIE %in% c(ESPECIES$ESPECIE))  %>%
        subset(ESPECIE %in% c("Trisopterus spp","Mullus spp","Merluccius merluccius",
                              "Pagellus acarne","Trachurus spp", "Scomber colias", "Scomber scombrus")) %>%
        as.data.frame() 


Datos2<-filter(Datos1,  FUENTE %in% c("CONSUM" ,  "RIM", "DP", "NV") &
                       PUERTO %in% c("Avilés", "A Coruña", "Muros", "Finisterre") &
                       !(COD_TIPO_MUE  %in% ("MT1") ))
head (Datos2)
table(Datos2$ESPECIE)

lbls_beta<- c("Trisopterus spp","Mullus spp","Merluccius\n merluccius",
               "Pagellus\n acarne","Trachurus spp", "Scomber\n colias", "Scomber\n scombrus")
lbls_beta<- paste(c("", "\n"), levels(reorder(Datos2$ESPECIE, -Datos2$CPUE)))
CPUE_BETA2<- ggplot(Datos2, aes(y=CPUE, x=factor(reorder(ESPECIE, -CPUE), labels = lbls_beta),  fill=factor(FUENTE)))+
        geom_bar(position="dodge",stat="identity" ,size=1, color="black")  +
        labs(subtitle=Datos2$COD_TIPO_MUE, title= Datos2$ESTRATO_RIM)  +
        facet_wrap(.~PUERTO, ncol=1, scales="free_y")   +
        theme(axis.text.x=element_text(angle=0,size=11, face="bold.italic"))  +
        #scale_fill_manual(values = c("cornflowerblue", "lightsteelblue", "darkslateblue"))+
        labs(x="",y="CPUE")   + theme(plot.title = element_text(lineheight=.8, face="bold"))+
        
        theme(strip.text.x = element_text(size=11, angle=0,face="bold"),
              strip.text.y = element_text(size=9, face="bold"),
              strip.background = element_blank())   +
        scale_fill_manual(values = c( "lightsteelblue","dodgerblue4", "darkorchid3","orange"))   +
        #strip.background = element_rect(colour="darkblue", fill="#CCCCFF")     +
        #ylim(0,3500)  +
        theme(legend.position= "top")+
        theme(legend.title=element_text( size = 12, face = "bold", colour="black"),
              legend.text = element_text( size = 10, face = "italic"))+
        guides(fill=guide_legend("FUENTE", override.aes = list(size=6)))+
        geom_text(data=subset(Datos2,subset=(FUENTE=="RIM")),aes(x=5, y=max(0.9*CPUE), label=paste(" Mareas Muestreadas=",MAREAS)),
                  fontface="bold",  size=4, col="darkblue")   + #ylim(0,10000)+
        
        geom_text(aes(label = round(CPUE,2)),
                  position=position_dodge(width=1), hjust=.6,vjust=-0.5,parse = TRUE,size=3) #+
       # scale_y_sqrt(breaks=c(10,25,50,75))

CPUE_BETA2
CPUE_BETA1

#palangre ac
Datos1<-Datos %>%
group_by(  ESTRATO_RIM,FUENTE, PUERTO,COD_TIPO_MUE) %>%
        dplyr::mutate(MAREAS=length(unique(COD_ID)))   %>%
        group_by( ESTRATO_RIM,PUERTO, FUENTE,ESPECIE, MAREAS, COD_TIPO_MUE ) %>%
        dplyr::summarise (PESO= sum(PESO)) %>%
        group_by(  ESTRATO_RIM,PUERTO, FUENTE,ESPECIE,COD_TIPO_MUE ) %>%
        dplyr::mutate(CPUE = round(PESO/MAREAS,2)) %>%  
        subset (ESTRATO_RIM %in% c("PALANGRE_AC")) %>%	
        #subset (PUERTO %in% c( "Santa Eugenia de Ribeira")) %>%
        #subset (COD_TIPO_MUE %in% c("MT4")) %>%
        #?subset (ESPECIE %in% c(ESPECIES$ESPECIE))  %>%
        subset(ESPECIE %in% c("Polyprion americanus","Pagellus bogaraveo","Merluccius merluccius",
                             "Helicolenus dac.", "Molva molva", "Phycis blennoides")) %>%
        as.data.frame() 

head (Datos1)
table(Datos1$PUERTO)

Datos2<-filter(Datos1,  FUENTE %in% c("CONSUM" ,  "RIM", "DP", "NV") )
                       #PUERTO %in% c("Avilés", "A Coruña", "Muros", "Finisterre") &
                       !(COD_TIPO_MUE  %in% ("MT1") ))
head (Datos2)
table(Datos2$ESPECIE)

lbls_PALANGRE_AC<- c(" Merluccius\n merluccius" ,"Helicolenus\n dactylopterus" ,
                     " Phycis blennoides" , "Pagellus\n bogaraveo",  "Molva molva" ,         
                      "\n Conger conger" )
lbls_beta<- paste(c("", "\n"), levels(factor(Datos2$ESPECIE)))
CPUE_PALANGRE_AC<- ggplot(Datos2, aes(y=CPUE, x=factor(reorder(ESPECIE, -CPUE), labels = lbls_PALANGRE_AC),  fill=factor(FUENTE)))+
        geom_bar(position="dodge",stat="identity" ,size=1, color="black")  +
        labs(subtitle=Datos2$COD_TIPO_MUE, title= Datos2$ESTRATO_RIM)  +
        facet_grid(TIPO_MUESTREO~PUERTO)   +
        theme(axis.text.x=element_text(angle=0,size=11, face="bold.italic"))  +
        #scale_fill_manual(values = c("cornflowerblue", "lightsteelblue", "darkslateblue"))+
        labs(x="",y="CPUE")   + theme(plot.title = element_text(lineheight=.8, face="bold"))+
        
        theme(strip.text.x = element_text(size=11, angle=0,face="bold"),
              strip.text.y = element_text(size=9, face="bold"),
              strip.background = element_blank())   +
        scale_fill_manual(values = c( "lightsteelblue","dodgerblue4", "darkorchid3","orange"))   +
        #strip.background = element_rect(colour="darkblue", fill="#CCCCFF")     +
        #ylim(0,3500)  +
        theme(legend.position= "top")+
        theme(legend.title=element_text( size = 12, face = "bold", colour="black"),
              legend.text = element_text( size = 10, face = "italic"))+
        guides(fill=guide_legend("FUENTE", override.aes = list(size=6)))+
        geom_text(data=subset(Datos2,subset=(FUENTE=="RIM")),aes(x=5, y=max(1.5*CPUE), label=paste(" Mareas Muestreadas=",MAREAS)),
                  fontface="bold",  size=4, col="darkblue")   + #ylim(0,10000)+
        
        geom_text(aes(label = round(CPUE,2)),
                  position=position_dodge(width=1), hjust=.6,vjust=-0.5,parse = TRUE,size=3) +
        scale_y_sqrt()

CPUE_PALANGRE_AC

#############CERCO_CN
Datos1<-Datos %>%
        group_by(  ESTRATO_RIM,FUENTE, PUERTO,COD_TIPO_MUE) %>%
        dplyr::mutate(MAREAS=length(unique(COD_ID)))   %>%
        group_by( ESTRATO_RIM,PUERTO, FUENTE,ESPECIE, MAREAS, COD_TIPO_MUE ) %>%
        dplyr::summarise (PESO= sum(PESO)) %>%
        group_by(  ESTRATO_RIM,PUERTO, FUENTE,ESPECIE,COD_TIPO_MUE ) %>%
        dplyr::mutate(CPUE = round(PESO/MAREAS,2)) %>%  
        subset (ESTRATO_RIM %in% c("CERCO_CN")) %>%	
        #subset (PUERTO %in% c( "Santa Eugenia de Ribeira")) %>%
        #subset (COD_TIPO_MUE %in% c("MT4")) %>%
        #?subset (ESPECIE %in% c(ESPECIES$ESPECIE))  %>%
        subset(ESPECIE %in% c("Trisopterus spp","Engraulis encrasicolus", "Boops boops",
                              "Sardina pilchardus","Trachurus spp", "Scomber colias", "Scomber scombrus")) %>%
        as.data.frame() 

Datos2<-filter(Datos1,  FUENTE %in% c("CONSUM" ,  "RIM", "DP", "NV") &
                       PUERTO %in% c("Avilés", "A Coruña", "Santoña", "Ribeira", "Vigo", "Gijón") &
                       COD_TIPO_MUE  %in% ("MT2") )


#lbls_cerco<- paste(c("", "\n"), levels(factor(Datos2$ESPECIE)))
lbls_cerco<- c("Boops\n boops","\n Engraulis\n encrasicolus", 
"Sardina\n  pilchardus", "Scomber\n colias", "Scomber\n scombrus","Trachurus\n spp")
CPUE_CERCO1<- ggplot(Datos2, aes(y=CPUE, x=factor(ESPECIE, labels = lbls_cerco),  fill=factor(FUENTE)))+
        geom_bar(position="dodge",stat="identity" ,size=1, color="black")  +
        labs(subtitle=Datos2$COD_TIPO_MUE, title= Datos2$ESTRATO_RIM)  +
        facet_wrap(.~PUERTO, ncol=2, scales="free_y")   +
        theme(axis.text.x=element_text(angle=0,size=10, face="bold.italic"))  +
        #scale_fill_manual(values = c("cornflowerblue", "lightsteelblue", "darkslateblue"))+
        labs(x="",y="CPUE")   + theme(plot.title = element_text(lineheight=.8, face="bold"))+
        
        theme(strip.text.x = element_text(size=11, angle=0,face="bold"),
              strip.text.y = element_text(size=9, face="bold"),
              strip.background = element_blank())   +
        scale_fill_manual(values = c( "lightsteelblue","dodgerblue4", "darkorchid3","orange"))   +
        #strip.background = element_rect(colour="darkblue", fill="#CCCCFF")     +
        #ylim(0,3500)  +
        theme(legend.position= "top")+
        theme(legend.title=element_text( size = 11, face = "bold", colour="black"),
              legend.text = element_text( size = 10, face = "italic"))+
        guides(fill=guide_legend("FUENTE", override.aes = list(size=5)))+
        geom_text(data=subset(Datos2,subset=(FUENTE=="RIM")),aes(x=3.5, y=max(0.8*CPUE), label=paste(" Mareas Muestreadas=",MAREAS)),
                  fontface="bold",  size=4, col="darkblue")   + #ylim(0,10000)+
        
        geom_text(aes(label = round(CPUE,0)),
                  position=position_dodge(width=1), hjust=.6,vjust=-0.5,parse = TRUE,size=3)# +
       # scale_y_sqrt(breaks=c(100,500,1000,2000,3000,4000))



CPUE_CERCO1
CPUE_CERCO2


###########RASCO_CN

Datos1<-Datos %>%
        group_by(  ESTRATO_RIM,FUENTE, PUERTO,COD_TIPO_MUE) %>%
        dplyr::mutate(MAREAS=length(unique(COD_ID)))   %>%
        group_by( ESTRATO_RIM,PUERTO, FUENTE,ESPECIE, MAREAS, COD_TIPO_MUE ) %>%
        dplyr::summarise (PESO= sum(PESO)) %>%
        group_by(  ESTRATO_RIM,PUERTO, FUENTE,ESPECIE,COD_TIPO_MUE ) %>%
        dplyr::mutate(CPUE = round(PESO/MAREAS,2)) %>%  
        subset (ESTRATO_RIM %in% c("RASCO_CN")) %>%	
        #subset (PUERTO %in% c( "Santa Eugenia de Ribeira")) %>%
        #subset (COD_TIPO_MUE %in% c("MT4")) %>%
        #?subset (ESPECIE %in% c(ESPECIES$ESPECIE))  %>%
        subset(ESPECIE %in% c("Molva macrophthalma", "Lophius spp", "Scophthalmus maximus",
                            "Scomber scombrus" )) %>%
        as.data.frame() 

Datos2<-subset(Datos1,  FUENTE %in% c("CONSUM" ,  "RIM", "DP", "NV")    &
                       PUERTO %in% c( "Cedeira", "Santoña", "San Vicente de la Barquera") &
                       COD_TIPO_MUE  %in% c("MT2", "MT1") )

#lbls_RASCO<- paste(c("", "\n"), levels(factor(Datos2$ESPECIE)))
lbls_RASCO<- c("Lophius\ spp", 
               "\n Molva\n macrophthalma", "Scomber scombrus","\n Scophthalmus\n maximus")
CPUE_RASCO<- ggplot(Datos2, aes(y=CPUE, x=factor(ESPECIE, labels = lbls_RASCO),  fill=factor(FUENTE)))+
        geom_bar(position="dodge",stat="identity" ,size=1, color="black")  +
        labs(subtitle="Todos los puertos", title= Datos2$ESTRATO_RIM)  +
        facet_grid(COD_TIPO_MUE~PUERTO,  scales="free_y")   +
        theme(axis.text.x=element_text(angle=0,size=11, face="bold.italic"))  +
        #scale_fill_manual(values = c("cornflowerblue", "lightsteelblue", "darkslateblue"))+
        labs(x="",y="CPUE")   + theme(plot.title = element_text(lineheight=.8, face="bold"))+
        
        theme(strip.text.x = element_text(size=11, angle=0,face="bold"),
              strip.text.y = element_text(size=9, face="bold"),
              strip.background = element_blank())   +
        scale_fill_manual(values = c( "lightsteelblue","dodgerblue4", "darkorchid3","orange"))   +
        #strip.background = element_rect(colour="darkblue", fill="#CCCCFF")     +
        #ylim(0,3500)  +
        theme(legend.position= "top")+
        theme(legend.title=element_text( size = 12, face = "bold", colour="black"),
              legend.text = element_text( size = 10, face = "italic"))+
        guides(fill=guide_legend("FUENTE", override.aes = list(size=5)))+
        geom_text(data=subset(Datos2,subset=(FUENTE=="RIM")),aes(x=3, y=max(0.8*CPUE), label=paste(" Mareas Muestreadas=",MAREAS)),
                  fontface="bold",  size=4, col="darkblue")   + #ylim(0,10000)+
        
        geom_text(aes(label = round(CPUE,0)),
                  position=position_dodge(width=1), hjust=.6,vjust=-0.5,parse = TRUE,size=3) +
scale_y_sqrt()




CPUE_RASCO


#volanta_cn
Datos1<-Datos %>%
        group_by(  ESTRATO_RIM,FUENTE, PUERTO,COD_TIPO_MUE) %>%
        dplyr::mutate(MAREAS=length(unique(COD_ID)))   %>%
        group_by( ESTRATO_RIM,PUERTO, FUENTE,ESPECIE, MAREAS, COD_TIPO_MUE ) %>%
        dplyr::summarise (PESO= sum(PESO)) %>%
        group_by(  ESTRATO_RIM,PUERTO, FUENTE,ESPECIE,COD_TIPO_MUE ) %>%
        dplyr::mutate(CPUE = round(PESO/MAREAS,2)) %>%  
        subset (ESTRATO_RIM %in% c("VOLANTA_CN")) %>%	
        #subset (PUERTO %in% c( "Santa Eugenia de Ribeira")) %>%
        #subset (COD_TIPO_MUE %in% c("MT4")) %>%
        #?subset (ESPECIE %in% c(ESPECIES$ESPECIE))  %>%
        subset(ESPECIE %in% c("Conger conger", "Merluccius merluccius", "Pagellus acarne","Scomber colias",
                              "Trachurus spp", "Trisopterus spp" )) %>%
        as.data.frame() 

Datos2<-subset(Datos1,  FUENTE %in% c("CONSUM" ,  "RIM", "DP", "NV")    &
                      PUERTO %in% c( "Cedeira", "Gijón", "Cedeira", "Finisterre","Luarca") &
                       COD_TIPO_MUE  %in% c("MT2", "MT1") )


lbls_VOL<- paste(c("", "\n"), levels(factor(Datos2$ESPECIE)))
lbls_VOL<- c("Merluccius\n merluccius","\n Trachurus\n spp",
             "Pagellus\n acarne" , "\n Trisopterus\n spp","Conger\n conger")
#lbls_VOL<-paste(c("", "\n"), levels(reorder(Datos2$ESPECIE, -Datos2$CPUE)))
CPUE_VOL<- ggplot(Datos2, aes(y=CPUE, x=factor(reorder(ESPECIE, -CPUE), labels = lbls_VOL),  fill=factor(FUENTE)))+
        geom_bar(position="dodge",stat="identity" ,size=1, color="black")  +
        labs(subtitle="Todos los puertos", title= Datos2$ESTRATO_RIM)  +
        facet_grid(PUERTO~COD_TIPO_MUE,  scales="free_y")   +
        theme(axis.text.x=element_text(angle=0,size=11, face="bold.italic"))  +
        #scale_fill_manual(values = c("cornflowerblue", "lightsteelblue", "darkslateblue"))+
        labs(x="",y="CPUE")   + theme(plot.title = element_text(lineheight=.8, face="bold"))+
        
        theme(strip.text.x = element_text(size=10, angle=0,face="bold"),
              strip.text.y = element_text(size=9, face="bold"),
              strip.background = element_blank())   +
        scale_fill_manual(values = c( "lightsteelblue","dodgerblue4", "darkorchid3","orange"))   +
        #strip.background = element_rect(colour="darkblue", fill="#CCCCFF")     +
        #ylim(0,3500)  +
        theme(legend.position= "top")+
        theme(legend.title=element_text( size = 11, face = "bold", colour="black"),
              legend.text = element_text( size = 10, face = "italic"))+
        guides(fill=guide_legend("FUENTE", override.aes = list(size=5)))+
        geom_text(data=subset(Datos2,subset=(FUENTE=="RIM")),aes(x=4, y=max(0.8*CPUE), label=paste(" Mareas Muestreadas=",MAREAS)),
                  fontface="bold",  size=4, col="darkblue")   + #ylim(0,10000)+
        
        geom_text(aes(label = round(CPUE,0)),
                  position=position_dodge(width=1), hjust=.6,vjust=-0.5,parse = TRUE,size=3) +
        scale_y_sqrt(breaks=c(50,100,250,500,750))




CPUE_VOL

########nasa
Datos1<-Datos %>%
        group_by(  ESTRATO_RIM,FUENTE, PUERTO,COD_TIPO_MUE) %>%
        dplyr::mutate(MAREAS=length(unique(COD_ID)))   %>%
        group_by( ESTRATO_RIM,PUERTO, FUENTE,ESPECIE, MAREAS, COD_TIPO_MUE ) %>%
        dplyr::summarise (PESO= sum(PESO)) %>%
        group_by(  ESTRATO_RIM,PUERTO, FUENTE,ESPECIE,COD_TIPO_MUE ) %>%
        dplyr::mutate(CPUE = round(PESO/MAREAS,2)) %>%  
        subset (ESTRATO_RIM %in% c("NASAPULP_CN")) %>%	
        #subset (PUERTO %in% c( "Santa Eugenia de Ribeira")) %>%
        #subset (COD_TIPO_MUE %in% c("MT4")) %>%
        #?subset (ESPECIE %in% c(ESPECIES$ESPECIE))  %>%
        subset(ESPECIE %in% c("Conger conger", "Merluccius merluccius", "Octopus vulgaris")) %>%
        as.data.frame() 

Datos2<-subset(Datos1,  FUENTE %in% c("CONSUM" ,  "RIM", "DP", "NV")    &
                       #PUERTO %in% c( "Cedeira", "Gijón", "Cedeira", "Finisterre","Luarca") &
                       COD_TIPO_MUE  %in% c("MT2", "MT1") )
table(Datos2$COD_TIPO_MUE)

lbls_VOL<- paste(c("", "\n"), levels(factor(Datos2$ESPECIE)))
lbls_VOL<- c("Boops boops","Engraulis\n encrasicolus", 
             "Sardina\n pilchardus", "Scomber\n colias", "Scomber\n scombrus","Trachurus\n spp")
lbls_nasa<-paste(c("", "\n"), levels(reorder(Datos2$ESPECIE, -Datos2$CPUE)))
CPUE_NASA<- ggplot(Datos2, aes(y=CPUE, x=factor(reorder(ESPECIE, -CPUE),labels = lbls_nasa),  fill=factor(FUENTE)))+
        geom_bar(position="dodge",stat="identity" ,size=1, color="black")  +
        labs(subtitle="Todos los puertos", title= Datos2$ESTRATO_RIM)  +
        facet_grid(COD_TIPO_MUE~PUERTO,  scales="free_y")   +
        theme(axis.text.x=element_text(angle=0,size=11, face="bold.italic"))  +
        #scale_fill_manual(values = c("cornflowerblue", "lightsteelblue", "darkslateblue"))+
        labs(x="",y="CPUE")   + 
        theme(plot.title = element_text(lineheight=.8, face="bold"))+
        
        theme(strip.text.x = element_text(size=10, angle=0,face="bold"),
              strip.text.y = element_text(size=9, face="bold"),
              strip.background = element_blank())   +
        scale_fill_manual(values = c( "lightsteelblue","dodgerblue4", "darkorchid3","orange"))   +
        #strip.background = element_rect(colour="darkblue", fill="#CCCCFF")     +
        #ylim(0,3500)  +
        theme(legend.position= "top")+
        theme(legend.title=element_text( size = 11, face = "bold", colour="black"),
              legend.text = element_text( size = 10, face = "italic"))+
        guides(fill=guide_legend("FUENTE", override.aes = list(size=5)))+
        geom_text(data=subset(Datos2,subset=(FUENTE=="RIM")),aes(x=2, y=max(0.8*CPUE), label=paste(" Mareas\n Muestreadas=",MAREAS)),
                  fontface="bold",  size=4, col="darkblue")   + 
        geom_text(aes(label = round(CPUE,1)),
                  position=position_dodge(width=1), hjust=.6,vjust=-0.5,parse = TRUE,size=3) 


CPUE_NASA

mareas<-subset(Datos,COD_TIPO_MUE %in% c("MT2", "MT1") & ESTRATO_RIM %in% c("NASAPULP_CN")  
               & FUENTE %in% c("NV", "DP", "RIM")&
                       PESO>5 &
                       PUERTO %in%  c("A Coruña", "Avilés", "Muros") &
                       ESPECIE %in% c("Merluccius merluccius", "Octopus vulgaris", "Conger conger"))
head (mareas)


n_fun <- function(x){
        return(data.frame(y =-10,
                          label = length(x)))
}

#  lbls  <- paste(c("", "\n"), levels(factor(mareas1$ESPECIE)))

MAREAS_NASA<-ggplot(mareas,
                    aes(x=reorder(factor(ESPECIE), -PESO),y=PESO), fill=FUENTE)   +
        geom_point(aes(fill=ESPECIE,size=PESO), shape=21,
                   position=position_jitter(width=0.2, height=0.2)) +
        scale_size(range = c(2,6)) +
        stat_boxplot(geom ='errorbar', width = 0.26)    +
        geom_boxplot(width = 0.5, aes(fill = ESPECIE),alpha=0.4, show.legend=FALSE) +
        
        guides(size=FALSE,fill=guide_legend(override.aes=list(size=1)))  +
        labs(title=paste("NASAPULP_CN", subtitle="Pesos por Marea",caption = "MT1 +MT2"),
             x=NULL)  +
        theme_bw()+
        facet_grid(PUERTO~FUENTE, scales="free_x")  +
        #xpand_limits(y = -550) +
        stat_summary(fun.data = n_fun, geom = "text", hjust = 0.5, fontface="bold", size=3) +
        
        theme(plot.title = element_text(lineheight=.8, face="bold"))+
        theme(axis.text.x=element_text(angle=45,size=10, hjust=1,face= "bold.italic"))   +
        theme(strip.text.x = element_text(size=11, angle=0,face="bold", colour="white"),
              strip.text.y = element_text(size=10, face="bold",colour="white"),
              strip.background = element_rect(colour="white", fill=c( "steelblue")))  +
        theme(panel.background = element_rect(fill = "lightblue", colour = 'red'))  +
        theme(legend.position="none")+
        scale_fill_manual(values=col) #+scale_y_sqrt()
MAREAS_NASA

mareas$facet_fill_color <- c("dodgerblue4", "darkorchid3","orange1")[mareas$FUENTE] 
nasag <- ggplot_gtable(ggplot_build(MAREAS_NASA))
strip_t <- which(grepl('strip-t', nasag$layout$name))
#fills <- MAREAS$facet_fill_color
k <- 1
for (i in strip_t) {
        j <- which(grepl('rect', nasag$grobs[[i]]$grobs[[1]]$childrenOrder))
        nasag$grobs[[i]]$grobs[[1]]$children[[j]]$gp$fill <- fills[k]
        k <- k+1
}
nasag2<-grid.draw(nasag)  

####GRAFICAS DE BARRAS


MT<-muestreos_lonja2%>%
        group_by(ESTRATO_RIM,PUERTO, TIPO_MUE) %>%
        summarise(num_mue=length(unique((as.factor(COD_ID)))))   %>%
        filter(TIPO_MUE %in% c("MT1A (Encuestas IEO)","MT2A (Biometrico puerto)"))%>%
        ungroup()
head (MT,10)
tail (MT,10)
subset(MT, ESTRATO_RIM %in% c("PAREJA_CN"))

 MT<- MT%>%group_by(ESTRATO_RIM,  TIPO_MUE)%>%
                mutate(pct=100*num_mue/sum(num_mue))%>%arrange(ESTRATO_RIM, TIPO_MUE, PUERTO)
       head (MT,20)
       
       names(MT)[c(2,1,3,5,4)] <- c("PUERTO","ESTRATO_RIM","TIPO_MUESTREO", "Porcentaje","Frecuencia")
       ggplot(data=subset(MT, ESTRATO_RIM %in% c("ARRASTRE_CN", "BACA_APN", "BACA_AC", "PAREJA_CN")),
              aes(x=reorder(factor(PUERTO), Frecuencia), y=Frecuencia, fill=factor(PUERTO))) +
                geom_bar(stat="identity", position= "stack") + 
                
                
                coord_flip()  + 
                geom_label(aes(label=ifelse(Porcentaje >0.01, paste(Frecuencia,"","(",round(Porcentaje,1),"%",")",sep= " ") ,NA)),
                                           hjust=0,vjust=0.5, position= "stack", fontface="bold",
                                           colour="blue")  +
                ylim(0,150)   +
                labs(x="",y="Frecuencia /(Porcentaje %)", face= "bold", size=4) +
                
                 
                ggtitle(paste(MT$ESTRATO_RIM, " \n Frecuencia de Muestreos ")) + 
                theme_classic()  +
                theme(plot.title = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=24, hjust=0.1))     +
                theme(legend.position="none") +
                theme(axis.text.y=element_text(angle=0, size=10)) +
                facet_grid(ESTRATO_RIM~TIPO_MUESTREO,scales="free_y")   +
                scale_fill_manual(values=(cols))
       # theme(panel.grid.major = element_line(colour = "black"))
        
    install.packages("ggthemes")
        library(ggthemes)
    cols
        num_arr<-  ggplot(data=subset(MT, ESTRATO_RIM %in% c("ARRASTRE_CN")& Frecuencia>1 ),# "BACA_APN", "BACA_AC", "PAREJA_CN")), 
               aes(x=reorder(PUERTO, Frecuencia), y=Frecuencia, fill=PUERTO)) + 
                geom_bar(stat="identity", position= "stack") + coord_flip() + #ylim(c(0,200)) + 
              theme_economist() + scale_fill_manual(values=cols) +
                facet_grid( .~ TIPO_MUESTREO) + theme(legend.position="none") + 
                labs(title="N?mero de Muestreos por Puerto y Tipo de Muestreo",x="PUERTO DE MUESTREO",
                     subtitle = "ARRASTRE_CN") +
                ylab(c("N?mero de Muestreos y %"))  + #theme_classic(base_size = 10)+
                geom_label_repel(aes(label=ifelse(Porcentaje >0.01, paste(Frecuencia,"","(",round(Porcentaje,1),"%",")",sep= " ") ,NA)),
                                hjust=0.1,vjust=0.5, position= "identity", fontface="bold",size=4.5,
                           colour="white") 
num_arr
MT2<-MT
substring2(MT2$Frecuencia,"23") <- "22"
MT2$Frecuencia<-as.numeric(MT2$Frecuencia)
num_par<-  ggplot(data=subset(MT2, ESTRATO_RIM %in% c("PAREJA_CN")& Frecuencia>1 ),# "BACA_APN", "BACA_AC", "PAREJA_CN")), 
                  aes(x=reorder(PUERTO, Frecuencia), y=Frecuencia, fill=PUERTO)) + 
        geom_bar(stat="identity", position= "stack") + coord_flip() + #ylim(c(0,200)) + 
        theme_economist() + scale_fill_manual(values=cols) +
        facet_grid( ESTRATO_RIM~ TIPO_MUESTREO) + theme(legend.position="none") + 
        labs(title="N?mero de Muestreos por Puerto y Tipo de Muestreo",x="PUERTO DE MUESTREO",
             subtitle = "PAREJA_CN") +
        ylab(c("N?mero de Muestreos y %"))  + #theme_classic(base_size = 10)+
        geom_label_repel(aes(label=ifelse(Porcentaje >0.01, paste(Frecuencia,"","(",round(Porcentaje,1),"%",")",sep= " ") ,NA)),
                        hjust=0.1,vjust=0.5, position= "identity", fontface="bold",size=4.5,
                        colour="white") 
num_par
substring2(MT2$Frecuencia,"177") <- "176"
MT2$Frecuencia<-as.numeric(MT2$Frecuencia)            
num_pal<-  ggplot(data=subset(MT2, ESTRATO_RIM %in% c("PALANGRE_CN")& Frecuencia>0 ),# "BACA_APN", "BACA_AC", "PAREJA_CN")), 
                  aes(x=reorder(PUERTO, Frecuencia), y=Frecuencia, fill=PUERTO)) + 
        geom_bar(stat="identity", position= "stack") + coord_flip() + #ylim(c(0,200)) + 
        theme_economist() + scale_fill_manual(values=cols) +
        facet_grid( ESTRATO_RIM~ TIPO_MUESTREO) + theme(legend.position="none") + 
        labs(title="N?mero de Muestreos por Puerto y Tipo de Muestreo",x="PUERTO DE MUESTREO",
             subtitle = "PALANGRE_CN") +
        ylab(c("N?mero de Muestreos y %"))  + #theme_classic(base_size = 10)+
        geom_label_repel(aes(label=ifelse(Porcentaje >0.01, paste(Frecuencia,"","(",round(Porcentaje,1),"%",")",sep= " ") ,NA)),
                        hjust=-0.1,vjust=0.5, position= "identity", fontface="bold",size=4.5,
                        colour="white") 
num_pal
head (muestreos_lonja)
subset(muestreos_lonja, ESTRATO_RIM %in% c("BETA_CN")& PUERTO=="A Coru?a" & COD_TIPO_MUE=="1" ) %>%
        select(COD_ID,IDMAREA, ESTRATO_RIM, FECHA,TIPO_MUE)%>%distinct()
num_bet<-  ggplot(data=subset(MT, ESTRATO_RIM %in% c("BETA_CN")& Frecuencia>2 ),# "BACA_APN", "BACA_AC", "PAREJA_CN")), 
                  aes(x=reorder(PUERTO, Frecuencia), y=Frecuencia, fill=PUERTO)) + 
        geom_bar(stat="identity", position= "stack") + coord_flip() + #ylim(c(0,200)) + 
        theme_economist() + scale_fill_manual(values=cols) +
        facet_grid( ESTRATO_RIM~ TIPO_MUESTREO) + theme(legend.position="none") + 
        labs(title="N?mero de Muestreos por Puerto y Tipo de Muestreo",x="PUERTO DE MUESTREO",
             subtitle = "BETA_CN") +
        ylab(c("N?mero de Muestreos y %"))  + #theme_classic(base_size = 10)+
        geom_label_repel(aes(label=ifelse(Porcentaje >0.01, paste(Frecuencia,"","(",round(Porcentaje,1),"%",")",sep= " ") ,NA)),
                        hjust=0.1,vjust=0.5, position= "identity", fontface="bold",size= 4.5,
                        colour="White") 
num_bet


num_ras<-  ggplot(data=subset(MT, ESTRATO_RIM %in% c("RASCO_CN")& PUERTO %in% c("Cedeira", "San Vicente de la Barquera",
                                                                           "Santo?a"     ) ),# "BACA_APN", "BACA_AC", "PAREJA_CN")), 
                  aes(x=reorder(PUERTO, Frecuencia), y=Frecuencia, fill=PUERTO)) + 
        geom_bar(stat="identity", position= "stack") + coord_flip() + #ylim(c(0,200)) + 
        theme_economist() + scale_fill_manual(values=cols) +
        facet_grid( ESTRATO_RIM~ TIPO_MUESTREO) + theme(legend.position="none") + 
        labs(title="N?mero de Muestreos por Puerto y Tipo de Muestreo",x="Puerto de Muestreo",
             subtitle = "RASCO_CN") +
        ylab(c("N?mero de Muestreos y %"))  + #theme_classic(base_size = 10)+
        geom_label_repel(aes(label=ifelse(Porcentaje >0.01, paste(Frecuencia,"","(",round(Porcentaje,1),"%",")",sep= " ") ,NA)),
                        hjust=0.1,vjust=0.5, position= "identity", fontface="bold",size=4.5,
                        colour="White") 
num_ras
ls()
rm(getPalette,nvdp_ventas,muestreos1)
cernum<-subset(MT, ESTRATO_RIM %in% c("CERCO_CN"))
$ESTRATO_RIM<-as.character (Datos$ESTRATO_RIM)
library(Hmisc)
substring2(MT$Frecuencia,"145") <- "142"
MT$Frecuencia<-as.numeric(MT$Frecuencia)
str(cernum)
cernum
num_cer<-  ggplot(data=subset(MT, ESTRATO_RIM %in% c("CERCO_CN") ),# "BACA_APN", "BACA_AC", "PAREJA_CN")), 
                  aes(x=reorder(PUERTO, Frecuencia), y=Frecuencia, fill=PUERTO)) + 
        geom_bar(stat="identity", position= "stack") + coord_flip() + #ylim(c(0,200)) + 
        theme_economist() + scale_fill_manual(values=cols) +
        facet_grid( ESTRATO_RIM~ TIPO_MUESTREO) + theme(legend.position="none") + 
        labs(title="N?mero de Muestreos por Puerto y Tipo de Muestreo",x="Puerto de Muestreo",
             subtitle = "CERCO_CN") +
        ylab(c("N?mero de Muestreos y %"))  + #theme_classic(base_size = 10)+
        geom_label_repel(aes(label=ifelse(Porcentaje >0.01, paste(Frecuencia,"","(",round(Porcentaje,1),"%",")",sep= " ") ,NA)),
                        hjust=0.1,vjust=0.5, position= "identity", fontface="bold",size=4.5,
                        colour="white") 
num_cer


num_vol<-  ggplot(data=subset(MT, ESTRATO_RIM %in% c("VOLANTA_CN")& Frecuencia>1 ),# "BACA_APN", "BACA_AC", "PAREJA_CN")), 
                  aes(x=reorder(PUERTO, Frecuencia), y=Frecuencia, fill=PUERTO)) + 
        geom_bar(stat="identity", position= "stack") + coord_flip() + #ylim(c(0,200)) + 
        theme_economist() + scale_fill_manual(values=cols) +
        facet_grid( ESTRATO_RIM~ TIPO_MUESTREO) + theme(legend.position="none") + 
        labs(title="N?mero de Muestreos por Puerto y Tipo de Muestreo",x="Puerto de Muestreo",
             subtitle = "VOLANTA_CN") +
        ylab(c("N?mero de Muestreos y %"))  + #theme_classic(base_size = 10)+
        geom_label_repel(aes(label=ifelse(Porcentaje >0.01, paste(Frecuencia,"","(",round(Porcentaje,1),"%",")",sep= " ") ,NA)),
                        hjust=0.1,vjust=0.5, position= "identity", fontface="bold",size=4.5,
                        colour="white") 
num_vol

num_nasa<-  ggplot(data=subset(MT, ESTRATO_RIM %in% c("NASAPULP_CN")& Frecuencia>2 ),# "BACA_APN", "BACA_AC", "PAREJA_CN")), 
                  aes(x=reorder(PUERTO, Frecuencia), y=Frecuencia, fill=PUERTO)) + 
        geom_bar(stat="identity", position= "stack") + coord_flip() + #ylim(c(0,200)) + 
        theme_economist() + scale_fill_manual(values=cols) +
        facet_grid( ESTRATO_RIM~ TIPO_MUESTREO) + theme(legend.position="none") + 
        labs(title="N?mero de Muestreos por Puerto y Tipo de Muestreo",
             subtitle = "NASAPULP_CN") +
        ylab(c("N?mero de Muestreos y %"))  + #theme_classic(base_size = 10)+
        geom_label_repel(aes(label=ifelse(Porcentaje >0.01, paste(Frecuencia,"","(",round(Porcentaje,1),"%",")",sep= " ") ,NA)),
                        hjust=0.1,vjust=0.5, position= "identity", fontface="bold",
                        colour="white") 
num_nasa
library(grid)
MAREAS_LIN2<- grid.draw(MAREAS_LINg)

cols<-c("#00BA38", "#006748", "#9100B1", "deeppink", "deepskyblue", "cornflowerblue", "#F8766D", "#FF6C91",
        "yellowgreen" ,"aquamarine2", "orange", "chartreuse4" , "goldenrod", "darkmagenta", "navy", "#FF61CC",
        "navy")
names (cols)<-c("Avil?s", "Burela", "Gij?n", "San Vicente de la Barquera", "Santander", "Santa Eugenia de Ribeira" ,
                "A Coru?a", "Santo?a", "Mar?n", "Muros", "Vigo", "Cedeira" ,"Finisterre", "Cillero", "Tarifa","Barbate",
                "Luarca")

cols   
devtools::install_github("tidyverse/dtplyr")
3
library(data.table)
library(dtplyr)
library(dplyr, warn.conflicts = FALSE)
NVDT2 <- lazy_dt(NVDT)
as.data.frame(head (NVDT2))
NVDT2%>% 
        filter(PesoConsumo >10000) %>%group_by(ESTRATO_RIM,ESPECIE) %>%  
        dplyr::mutate(MAREAS2=length(unique(COD_ID)))   %>%
        
        summarise(l100k = mean(l100k)) %>% 
        as_tibble()
save.image("~/2019/COMPARATIVA_RIM/COMPARATIVA_RIM.RData")
