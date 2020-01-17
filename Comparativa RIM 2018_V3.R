
    @  Una vez hemos hecho la matrix vamos con los graficos comparativos

col <- c("darkorange1","#FF61CC", "darkslateblue", "violet", "mediumorchid",  "firebrick1","chartreuse3",  "pink","brown","orange", "goldenrod", "violet",
         "tomato","chocolate", "magenta", "black", "seagreen3", "#F8766D", "mediumslateblue", "aliceblue", "navy", "red",
         "cadetblue", "#00BA38", "darkorchid3", "slategray3","#619CFF", "darkgreen", "darkmagenta", "#F8766D", "purple", "royalblue",
         "hotpink", "violetred", "gray96", "lemonchiffon2", "indianred2", "lightsalmon1", "blue3" , "darkseagreen", "cyan3", "lightblue","darkgoldenrod2",
         "tan1", "cornsilk2", "deeppink2", "darkblue", "darkorchid3", "mediumaquamarine", "Chartreuse", "blue", "LavenderBlush", "cornsilk","forestgreen",
         "darkmagenta","#D39200",  "mediumslateblue","#FF61CC", "darkslateblue", "darkseagreen", "mediumorchid","#619CFF","Chartreuse" ,"#FF61CC",
         "goldenrod","deeppink2","firebrick1","darkorange3","greenyellow","mediumturquoise","thistle1","skyblue4","springgreen3","violetred2","sandybrown",
         "lightslateblue","lightsalmon2","lightsalmon3", "lightpink", "ivory4","hotpink4", "green1", "khaki1","gold","floralwhite", "darkseagreen")

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
              "Illex coindetii","Chelidonichthys cuculus", "Chelidonichthys obscurus","Eledone cirrhosa", "Trisopterus minutus", "Trisopterus spp" )
              
              
              
              
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
library(Hmisc)
 table (Datos$ESTRATO_RIM, Datos$FUENTE)
Datos$COD_TIPO_MUE[Datos$COD_TIPO_MUE==  "1"] <- "MT1"
Datos$COD_TIPO_MUE[Datos$COD_TIPO_MUE==  "2"] <- "MT2"
Datos$COD_TIPO_MUE[Datos$COD_TIPO_MUE==  "4"] <- "MT4"
Datos$COD_TIPO_MUE[Datos$COD_TIPO_MUE==  "6"] <- "MT6"
Datos$ESTRATO_RIM<-as.character (Datos$ESTRATO_RIM)
substring2(Datos$ESTRATO_RIM,"BACA_CN") <- "ARRASTRE_CN"
substring2(Datos$ESTRATO_RIM,"JURELERA_CN") <- "ARRASTRE_CN"
Datos$PUERTO<-as.character (Datos$PUERTO)
Datos$PUERTO[Datos$PUERTO==  "Avilés"] <- "Aviles-Gijon"
Datos$PUERTO[Datos$PUERTO==  "Gijón"] <- "Aviles-Gijon"
head (Datos)


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


    Datos2<-filter(Datos1, COD_TIPO_MUE %in% c("MT1","MT2") & FUENTE %in% c("CONSUM","RIM", "DP", "NV") & (PUERTO  %in% ("A Coruña")))
Datos2 = subset(Datos2, !(ESPECIE %in% c('Nephrops norvegicus', "Scomber colias")))
head (Datos2)
lbls1  <- paste(c("", "\n"), levels(factor(Datos2$ESPECIE)))
ARCOR<-ggplot(Datos2, aes(y=CPUE, x=factor(ESPECIE, labels = lbls1),  fill=factor(FUENTE)))+
 geom_bar(position="dodge",stat="identity" ,size=1, color="black")  +
        labs(subtitle=Datos2$PUERTO, title= Datos2$ESTRATO_RIM)  +
        facet_wrap(.~COD_TIPO_MUE, ncol = 1)   +
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
            geom_text(data=subset(Datos2,subset=(FUENTE=="RIM")),aes(x=5, y=500, label=paste(" Mareas Muestreadas=",MAREAS)),
                      fontface="bold",    size=4, col="darkblue")   + #ylim(0,10000)+

            geom_text(aes(label = round(CPUE,0)),
                      position=position_dodge(width=1), hjust=.6,vjust=-0.5,parse = TRUE,size=2.5)
        ARCOR
        
        
        
        #PESOS POR MAREA
        
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
  ARCOR3<-ggplot(mareas1,
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
          #stat_summary(fun.data = n_fun, geom = "text", hjust = 0.5, fontface="bold", size=3) +

          theme(plot.title = element_text(lineheight=.8, face="bold"))+
          theme(axis.text.x=element_text(angle=45,size=10, hjust=1,face= "bold.italic"))   +
          theme(strip.text.x = element_text(family = "sans",size=18, angle=0,face=c("italic")),
                strip.text.y = element_text(size=12, face="bold"),
                strip.background = element_blank())+
          theme(legend.position="top")+
          theme(legend.title=element_text( size = 12, face = "bold", colour="blue"),
                legend.text = element_text( size = 10, face = "italic"))+
          guides(fill=guide_legend("Especie", override.aes = list(size=6))) +
          scale_fill_manual(values=col) +scale_y_sqrt(breaks=c(50,500,1000,2500,5000,7500,10000))+
          geom_abline(aes(slope = 0, intercept = 1), lty = "dotted", lwd = 1.5)
  ARCOR3

