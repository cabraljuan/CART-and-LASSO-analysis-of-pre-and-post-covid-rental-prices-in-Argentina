

###########################################################################
###########################################################################
###                                                                     ###
###                     CLEANING AND PREPARING DATA                     ###
###                                                                     ###
###########################################################################
###########################################################################



# Path:
rm(list=ls())
fuente <-"G:/MELI/input/"
path<-"G:/Facultad/San andres/big data/trabajo final/"
setwd(path)

# Libraries

library(beepr)
library(readxl)
library(stargazer)
library(plyr)
library(dplyr)

# Load and merge data
INMUEBLES <- c("Departamento","Casa","Local","Oficina")

data <- read.csv(paste0(fuente,"REALESTATE_MLA_AMBA_201901.csv"), encoding = "UTF-8", stringsAsFactors = FALSE)
data <- data[data$TIPOPROPIEDADNORM %in% INMUEBLES,]

data1 <- read.csv(paste0(fuente,"REALESTATE_MLA_AMBA_201902.csv"), encoding = "UTF-8", stringsAsFactors = FALSE)
data1 <- data1[data1$TIPOPROPIEDADNORM %in% INMUEBLES,]
data <- rbind(data,data1)

data1 <- read.csv(paste0(fuente,"REALESTATE_MLA_AMBA_201903.csv"), encoding = "UTF-8", stringsAsFactors = FALSE)
data1 <- data1[data1$TIPOPROPIEDADNORM %in% INMUEBLES,]
data <- rbind(data,data1)

data1 <- read.csv(paste0(fuente,"REALESTATE_MLA_AMBA_201904.csv"), encoding = "UTF-8", stringsAsFactors = FALSE)
data1 <- data1[data1$TIPOPROPIEDADNORM %in% INMUEBLES,]
data <- rbind(data,data1)

data1 <- read.csv(paste0(fuente,"REALESTATE_MLA_AMBA_201905.csv"), encoding = "UTF-8", stringsAsFactors = FALSE)
data1 <- data1[data1$TIPOPROPIEDADNORM %in% INMUEBLES,]
data <- rbind(data,data1)

data1 <- read.csv(paste0(fuente,"REALESTATE_MLA_AMBA_201906.csv"), encoding = "UTF-8", stringsAsFactors = FALSE)
data1 <- data1[data1$TIPOPROPIEDADNORM %in% INMUEBLES,]
data <- rbind(data,data1)

data1 <- read.csv(paste0(fuente,"REALESTATE_MLA_AMBA_201907.csv"), encoding = "UTF-8", stringsAsFactors = FALSE)
data1 <- data1[data1$TIPOPROPIEDADNORM %in% INMUEBLES,]
data <- rbind(data,data1)

data1 <- read.csv(paste0(fuente,"REALESTATE_MLA_AMBA_201908.csv"), encoding = "UTF-8", stringsAsFactors = FALSE)
data1 <- data1[data1$TIPOPROPIEDADNORM %in% INMUEBLES,]
data <- rbind(data,data1)

data1 <- read.csv(paste0(fuente,"REALESTATE_MLA_AMBA_201909.csv"), encoding = "UTF-8", stringsAsFactors = FALSE)
data1 <- data1[data1$TIPOPROPIEDADNORM %in% INMUEBLES,]
data <- rbind(data,data1)

data1 <- read.csv(paste0(fuente,"REALESTATE_MLA_AMBA_201910.csv"), encoding = "UTF-8", stringsAsFactors = FALSE)
data1 <- data1[data1$TIPOPROPIEDADNORM %in% INMUEBLES,]
data <- rbind(data,data1)

data1 <- read.csv(paste0(fuente,"REALESTATE_MLA_AMBA_201911.csv"), encoding = "UTF-8", stringsAsFactors = FALSE)
data1 <- data1[data1$TIPOPROPIEDADNORM %in% INMUEBLES,]
data <- rbind(data,data1)

data1 <- read.csv(paste0(fuente,"REALESTATE_MLA_AMBA_201912.csv"), encoding = "UTF-8", stringsAsFactors = FALSE)
data1 <- data1[data1$TIPOPROPIEDADNORM %in% INMUEBLES,]
data <- rbind(data,data1)

write.csv(data,"G:/Facultad/San andres/big data/trabajo final/REALESTATE_MLA_AMBA_2019_v1.csv")

# Load database

data2 <- read.csv("G:/Facultad/San andres/big data/trabajo final/REALESTATE_MLA_AMBA_2019_v1.csv")
data3 <- read.csv("G:/actualización noviembre 2020/REALESTATE_MLA_AMBA_2020_11.csv")


DT<-rbind(data2,data3)

############################################################################
############################################################################
###                                                                      ###
###                               CLEANING                               ###
###                                                                      ###
############################################################################
############################################################################




# Metros variable
DT$METROS<- DT$STotalM2 

# Drop observations for which 0 squared meters:
DT <- DT[DT$METROS!=0,]

# Metros variable
DT$METROSdescub <- DT$METROS-DT$SConstrM2


# Price/Squared meters:
DT$PM2 <- DT$ITE_BASE_CURRENT_PRICE/DT$METROS

#PROPERTIES:
DT$INMUEBLE <- ""
DT$INMUEBLE<-ifelse(DT$TIPOPROPIEDADNORM=="Casa","Casa",
                    ifelse(DT$TIPOPROPIEDADNORM=="Departamento","Departamento",
                           ifelse(DT$TIPOPROPIEDADNORM=="Oficina"|DT$TIPOPROPIEDADNORM=="Local","Oficina",NA)))
# Remove NA in INMUEBLE
DT <- DT[!is.na(DT$INMUEBLE),]

# Ambientes variable
ambientes <- data.frame(table(DT[DT$INMUEBLE=="Departamento",]$Ambientes,DT[DT$INMUEBLE=="Departamento",]$Dormitorios))
ambientes <- ambientes[ambientes$Freq>0,]

# Another inmueble variable
DT$INMUEBLE1 <- ifelse(DT$INMUEBLE=="Departamento" & DT$Ambientes==1,"Monoambiente",
                       ifelse(DT$INMUEBLE=="Departamento"&(DT$Ambientes==2|DT$Ambientes==3),"Dos o tres ambientes",
                              ifelse(DT$INMUEBLE=="Departamento"&DT$Ambientes>=4,"Cuatro o mas ambientes",0)))



# TIME:
DT$TIM_DAY <- as.Date(DT$TIM_DAY, "%Y-%m-%d")
DT$ITE_AUCTION_START <- as.Date(DT$ITE_AUCTION_START, "%Y-%m-%d")
DT$ITE_AUCTION_STOP <- as.Date(DT$ITE_AUCTION_STOP, "%Y-%m-%d")
DT$MONTH <- format(as.Date(DT$TIM_DAY), "%Y-%m")
DT$MONTH_START <- format(as.Date(DT$ITE_AUCTION_START), "%Y-%m")
DT$MONTH_END <- format(as.Date(DT$ITE_AUCTION_STOP), "%Y-%m")


DT$VISITASTOT <- DT$VISITASANDROID+DT$VISITASIOS+DT$VISITASMOBILE+DT$VISITASSTD



# Quarantine 
quarantine<-as.Date("2020-03-20")


DT$quarantine<-ifelse(DT$TIM_DAY >=quarantine,1,0)

# unique(DT$Gimnasio)
# unique(DT$Pileta)
# unique(DT$AreaParrillas)

# Dummies
DT$Gimnasio<-ifelse(DT$Gimnasio=="Sí",1,ifelse(DT$Gimnasio=="No",0,NA))
DT$Pileta<-ifelse(DT$Pileta=="Sí",1,ifelse(DT$Pileta=="No",0,NA))
DT$AreaParrillas<-ifelse(DT$AreaParrillas=="Sí"|
                           DT$AreaParrillas=="Si",1,ifelse(DT$AreaParrillas=="No",0,NA))
DT$SalonFiestas<-ifelse(DT$SalonFiestas=="Sí"|
                          DT$SalonFiestas=="Si",1,ifelse(DT$SalonFiestas=="No",0,NA))
DT$SalonDeUsosMul<-ifelse(DT$SalonDeUsosMul=="Sí"|
                            DT$SalonDeUsosMul=="Si",1,ifelse(DT$SalonDeUsosMul=="No",0,NA))
DT$Ascensor<-ifelse(DT$Ascensor=="Sí"|
                      DT$Ascensor=="Si",1,ifelse(DT$Ascensor=="No",0,NA))
DT$Estacionamiento<-ifelse(DT$Estacionamiento=="Sí"|
                             DT$Estacionamiento=="Si",1,ifelse(DT$Estacionamiento=="No",0,NA))
DT$Seguridad<-ifelse(DT$Seguridad=="Sí"|
                       DT$Seguridad=="Si"|
                       DT$Seguridad=="Si¿¿",1,ifelse(DT$Seguridad=="No",0,NA))

# Antiguedad
# Remove words like "Años"
DT$Antiguedad<-gsub("[a-zA-Z ]", "", DT$Antiguedad)
DT$Antiguedad<-gsub("[ñ ]", "", DT$Antiguedad)

# More than 4 numbers doesn't make sense
DT<-DT[nchar(DT$Antiguedad)<=4,]

# Replace years
DT$Antiguedad<-ifelse(nchar(DT$Antiguedad)>3,as.numeric(substr(DT$TIM_DAY,1,4))
                      - as.numeric(DT$Antiguedad),DT$Antiguedad)



# Constant prices
bcra <- read_excel("Inflación BCRA.xlsx")
bcra$Fecha <-format(as.Date(bcra$Fecha),"%Y-%m")
DT <- merge(DT, bcra, by.x="MONTH",by.y="Fecha",all=TRUE)
DT$precioreal <- (DT$ITE_CURRENT_PRICE/DT$Pondera)*100
DT$PM2real<-DT$precioreal/DT$METROS
DT$PM2descubreal<-ifelse(DT$METROSdescub==0,NA,DT$precioreal/DT$METROSdescub)



write.csv(DT,"DT_clean.csv")

# Only interested in "Alquiler" and "departamento
DT<-DT[DT$OPERACION=="Alquiler"&
         DT$INMUEBLE=="Departamento",]

write.csv(DT,"DT_cleanalquilerdep.csv")

############################################################################
############################################################################
###                                                                      ###
###                        Working with datasets                         ###
###                                                                      ###
############################################################################
############################################################################


#DT<-read.csv("DT_clean.csv",stringsAsFactors = FALSE)

DT<-read.csv("DT_cleanalquilerdep.csv",stringsAsFactors = FALSE)


DT2<-DT





# TIME:
DT2$TIM_DAY <- as.Date(DT2$TIM_DAY, "%Y-%m-%d")
DT2$ITE_AUCTION_START <- as.Date(DT2$ITE_AUCTION_START, "%Y-%m-%d")
DT2$ITE_AUCTION_STOP <- as.Date(DT2$ITE_AUCTION_STOP, "%Y-%m-%d")
DT2$MONTH <- format(as.Date(DT2$TIM_DAY), "%Y-%m")
DT2$MONTH_START <- format(as.Date(DT2$ITE_AUCTION_START), "%Y-%m")
DT2$MONTH_END <- format(as.Date(DT2$ITE_AUCTION_STOP), "%Y-%m")


DT2<-DT2 %>% group_by(ITE_ITEM_ID) %>%
  slice(which.max(TIM_DAY))

# quarantine 
quarantine<-as.Date("2020-03-20")
DT2$quarantine<-ifelse(DT2$TIM_DAY >=quarantine,1,0)


# Pre and post quarantine
DT2Q<-DT2[DT2$quarantine==1,]
DT2NQ<-DT2[DT2$quarantine==0,]



length(unique(DT2$ITE_ITEM_ID))
# 173508



##################################################################
##                      Eliminate outliers                      ##
##################################################################


DT2NQ<-DT2[DT2$quarantine==0,]
DT2NQ<-as.data.frame(DT2NQ)
#DT2NQ<-slice_sample(DT2NQ,prop=0.3)

ceiling<-0.95
floor<-0.05
# Dropping outliers
DT2NQ <- DT2NQ %>% 
  mutate(p05=quantile(precioreal,probs=floor,na.rm=TRUE))%>%
  mutate(p95=quantile(precioreal,probs=ceiling,na.rm=TRUE))%>%
  mutate(q05=quantile(METROS,probs=floor,na.rm=TRUE))%>%
  mutate(q95=quantile(METROS,probs=ceiling,na.rm=TRUE))%>%
  mutate(qd05=quantile(METROSdescub,probs=floor,na.rm=TRUE))%>%
  mutate(qd95=quantile(METROSdescub,probs=ceiling,na.rm=TRUE))
DT2NQ <- DT2NQ[DT2NQ$precioreal<=DT2NQ$p95& 
                 DT2NQ$precioreal>=DT2NQ$p05&
                 DT2NQ$METROS<=DT2NQ$q95&DT2NQ$METROS>=DT2NQ$q05&
                 DT2NQ$METROSdescub<=DT2NQ$qd95&
                 DT2NQ$METROSdescub>=DT2NQ$qd05,] 

length(unique(DT2Q$ITE_ITEM_ID))
# 59124

DT2Q<-DT2[DT2$quarantine==1,]
DT2Q<-as.data.frame(DT2Q)



# Dropping outliers
DT2Q <- DT2Q %>% 
  mutate(p05=quantile(precioreal,probs=floor,na.rm=TRUE))%>%
  mutate(p95=quantile(precioreal,probs=ceiling,na.rm=TRUE))%>%
  mutate(q05=quantile(METROS,probs=floor,na.rm=TRUE))%>%
  mutate(q95=quantile(METROS,probs=ceiling,na.rm=TRUE))%>%
  mutate(qd05=quantile(METROSdescub,probs=floor,na.rm=TRUE))%>%
  mutate(qd95=quantile(METROSdescub,probs=ceiling,na.rm=TRUE))
DT2Q <- DT2Q[DT2Q$precioreal<=DT2Q$p95& 
               DT2Q$precioreal>=DT2Q$p05&
               DT2Q$METROS<=DT2Q$q95&DT2Q$METROS>=DT2Q$q05&
               DT2Q$METROSdescub<=DT2Q$qd95&
               DT2Q$METROSdescub>=DT2Q$qd05,] 




DT2NQ<-as.data.frame(DT2NQ)

length(unique(DT2NQ$ITE_ITEM_ID))
# 92043



#################################################################
##           Descriptive statistics for two datasets           ##
#################################################################



DT2NQ<-subset(DT2NQ, select=c("METROS", "PM2real","PM2descubreal","precioreal","METROSdescub"))
DT2NQ<-as.data.frame(DT2NQ)


# OMIT NA
DT2NQ<-na.omit(DT2NQ)


stargazer(DT2NQ,summary=TRUE,median=TRUE,omit.summary.stat = c("n","max","min","p25","p75","mean","sd"),
          label="descriptivapost",title="Estadísticas descriptivas 2019-01-01 hasta 2020-03-20" )



# Descriptive statistics Post-quarantine
DT2Q<-DT2[DT2$quarantine==1,]

# Only some columns
DT2Q<-subset(DT2Q, select=c("METROS", "PM2real","PM2descubreal" ,"precioreal","METROSdescub"))
DT2Q<-as.data.frame(DT2Q)

# OMIT NA
DT2Q<-na.omit(DT2Q)

stargazer(DT2Q,summary=TRUE,median=TRUE,omit.summary.stat = c("n","max","min","p25","p75","mean","sd"),
          label="descriptiva",title="Estadísticas descriptivas 2020-03-20 hasta 2020-11-30")






##################################################################
##                    Descriptive statistics                    ##
##################################################################



# Only some columns
DT2<-subset(DT2, select=c("METROS", "PM2real","PM2descubreal" ,"precioreal","METROSdescub"))
DT2<-as.data.frame(DT2)


ceiling<-0.95
floor<-0.05
# Dropping outliers
DT2 <- DT2 %>% 
  mutate(p05=quantile(precioreal,probs=floor,na.rm=TRUE))%>%
  mutate(p95=quantile(precioreal,probs=ceiling,na.rm=TRUE))%>%
  mutate(q05=quantile(METROS,probs=floor,na.rm=TRUE))%>%
  mutate(q95=quantile(METROS,probs=ceiling,na.rm=TRUE))%>%
  mutate(qd05=quantile(METROSdescub,probs=floor,na.rm=TRUE))%>%
  mutate(qd95=quantile(METROSdescub,probs=ceiling,na.rm=TRUE))
DT2 <- DT2[DT2$precioreal<=DT2$p95& 
             DT2$precioreal>=DT2$p05&
             DT2$METROS<=DT2$q95&DT2$METROS>=DT2$q05&
             DT2$METROSdescub<=DT2$qd95&
             DT2$METROSdescub>=DT2$qd05,] 





# OMIT NA
DT2<-na.omit(DT2)
DT2<-subset(DT2, select=c("METROS", "PM2real","PM2descubreal" ,"precioreal","METROSdescub"))
DT2<-as.data.frame(DT2)


stargazer(DT2,summary=TRUE,median=TRUE,omit.summary.stat =  c("p25","p75","sd"),
          label="descriptivatotal",title="Estadísticas descriptivas")







##################################################################
##                         CORRELATIONS                         ##
##################################################################





round( cor(DT2NQ$precioreal,DT2NQ$Gimnasio,use="complete.obs") ,2)
round( cor(DT2Q$precioreal,DT2Q$Gimnasio,use="complete.obs") ,2)

round( cor(DT2NQ$precioreal,DT2NQ$Pileta,use="complete.obs") ,2)
round( cor(DT2Q$precioreal,DT2Q$Pileta,use="complete.obs") ,2)

# round( cor(DT2NQ$precioreal,DT2NQ$AreaParrillas,use="complete.obs") ,2)
# round( cor(DT2Q$precioreal,DT2Q$AreaParrillas,use="complete.obs") ,2)


round( cor(DT2NQ$precioreal,DT2NQ$SalonFiestas,use="complete.obs") ,2)
round( cor(DT2Q$precioreal,DT2Q$SalonFiestas,use="complete.obs") ,2)


round( cor(DT2NQ$precioreal,DT2NQ$SalonDeUsosMul,use="complete.obs") ,2)
round( cor(DT2Q$precioreal,DT2Q$SalonDeUsosMul,use="complete.obs") ,2)




##################################################################
##                            Graphs                            ##
##################################################################


DTtotal<-rbind(DT2NQ,DT2Q)

DTtotal$Ambientes<-ifelse(DTtotal$Ambientes>=6,"6 o más",DTtotal$Ambientes)
DTtotal$Ambientes<-ifelse(DTtotal$Ambientes==0,NA,DTtotal$Ambientes)
DTtotal<-DTtotal[!is.na(DTtotal$Ambientes),]
unique(DTtotal$Ambientes)
unique(DTtotal$quarantine)


DTtotal<-ddply(DTtotal,
               .(Ambientes,quarantine), summarize, 
               PM2descubreal=median(PM2descubreal, na.rm=TRUE),
               PM2real=mean(PM2real,na.rm=TRUE))


# Evolution of PM2descubreal
ggplot(DTtotal, aes(y=PM2descubreal,x=as.factor(Ambientes),fill=as.factor(quarantine) ) )  +
  geom_bar(stat="identity",color="blue",
           position = "dodge2" ,width=0.5)+
  
  labs(fill="Pre cuarentena = 0, 
       Cuarentena = 1")+
  ylab("Mediana del precio real (2018) por metro cuadrado descubierto")+
  xlab("Cantidad de ambientes")+
  theme_bw(base_size = 13) 



# Save graph

ggsave("G:/Facultad/San andres/big data/trabajo final/medianaspmd.png",
       dpi=320,units="in",width=10,height=5)

# Evolution of PM2real
ggplot(DTtotal, aes(y=PM2real,x=as.factor(Ambientes),fill=as.factor(quarantine) ) )  +
  geom_bar(stat="identity",color="blue",
           position = "dodge2" ,width=0.5)+
  
  labs(fill="Pre cuarentena = 0, 
       Cuarentena = 1")+
  ylab("Mediana del precio real (2018) por metro cuadrado")+
  xlab("Cantidad de ambientes")+
  theme_bw(base_size = 13)



#################################################################
##                  Differences in percentage                  ##
#################################################################



diff<-as.data.frame( ((DTtotal[DTtotal$quarantine==1,]$PM2descubreal-
                         DTtotal[DTtotal$quarantine==0,]$PM2descubreal)/
                        DTtotal[DTtotal$quarantine==0,]$PM2descubreal  )*100)
diff$metros<-"descubierto"
diff$ambientes<-c(1,2,3,4,5,"6 o más")  

diff2<- as.data.frame(  ( (DTtotal[DTtotal$quarantine==1,]$PM2real- 
                             DTtotal[DTtotal$quarantine==0,]$PM2real )/
                            DTtotal[DTtotal$quarantine==0,]$PM2real )*100)
diff2$metros<-"cubierto"
diff2$ambientes<-c("1 post","2 post","3 post","4 post","5 post","6 o más post")  
diff2$ambientes<-c(1,2,3,4,5,"6 o más")  


names(diff)[1] <- "diff"
names(diff2)[1] <- "diff"

diff<-rbind(diff,diff2)




ggplot(diff, aes(x=as.factor(ambientes), y=diff)) + 
  geom_bar(stat='identity', aes(fill=as.factor(metros) ) , position = "dodge2", width=0.5,
           color="blue")  +
  coord_flip()+
  theme_bw(base_size = 15)+
  xlab("Cantidad de ambientes")+
  ylab("Diferencias en porcentaje")+
  labs(fill = "Tipos de metros cuadrados")



ggsave("G:/Facultad/San andres/big data/trabajo final/difmedianasporcentaje.png",
       dpi=320,units="in",width=10,height=5)









##################################################################
##                          Cuarentena                          ##
##################################################################



DT2Q<-DT2[DT2$quarantine==1,]
DT2Q<-as.data.frame(DT2Q)
#DT2Q<-slice_sample(DT2Q,prop=0.3)

# Dropping outliers
DT2Q <- DT2Q %>% 
  mutate(p05=quantile(precioreal,probs=0.05,na.rm=TRUE))%>%
  mutate(p95=quantile(precioreal,probs=0.95,na.rm=TRUE))%>%
  mutate(q05=quantile(METROS,probs=0.05,na.rm=TRUE))%>%
  mutate(q95=quantile(METROS,probs=0.95,na.rm=TRUE))%>%
  mutate(qd05=quantile(METROSdescub,probs=0.05,na.rm=TRUE))%>%
  mutate(qd95=quantile(METROSdescub,probs=0.95,na.rm=TRUE))
DT2Q <- DT2Q[DT2Q$precioreal<=DT2Q$p95& 
               DT2Q$precioreal>=DT2Q$p05&
               DT2Q$METROS<=DT2Q$q95&DT2Q$METROS>=DT2Q$q05&
               DT2Q$METROSdescub<=DT2Q$qd95&
               DT2Q$METROSdescub>=DT2Q$qd05,] 


DT2Q<-subset(DT2Q,ambientes==1)
p <- ggplot(DT2Q, aes(x = METROSdescub)) + 
  geom_point(data=DT2Q,aes(y = precioreal, colour = "Cuarentena"))+
  labs(y = "Precio real",
       x = "Metros descubiertos")  +
  theme(legend.position = c(0.8, 0.9)) +
  theme_bw(base_size = 13) +
  theme(legend.title=element_blank())+
  xlim(0,50)


p




ggsave("G:/Facultad/San andres/big data/trabajo final/scattermetrosdescubq.png",
       dpi=320,units="in",width=10,height=5)






DT2NQ<-DT2[DT2$quarantine==0,]
DT2NQ<-as.data.frame(DT2NQ)
#DT2NQ<-slice_sample(DT2NQ,prop=0.3)

# Dropping outliers
DT2NQ <- DT2NQ %>% 
  mutate(p05=quantile(precioreal,probs=0.05,na.rm=TRUE))%>%
  mutate(p95=quantile(precioreal,probs=0.95,na.rm=TRUE))%>%
  mutate(q05=quantile(METROS,probs=0.05,na.rm=TRUE))%>%
  mutate(q95=quantile(METROS,probs=0.95,na.rm=TRUE))%>%
  mutate(qd05=quantile(METROSdescub,probs=0.05,na.rm=TRUE))%>%
  mutate(qd95=quantile(METROSdescub,probs=0.95,na.rm=TRUE))
DT2NQ <- DT2NQ[DT2NQ$precioreal<=DT2NQ$p95& 
                 DT2NQ$precioreal>=DT2NQ$p05&
                 DT2NQ$METROS<=DT2NQ$q95&DT2NQ$METROS>=DT2NQ$q05&
                 DT2NQ$METROSdescub<=DT2NQ$qd95&
                 DT2NQ$METROSdescub>=DT2NQ$qd05,] 


DT2NQ<-subset(DT2NQ,ambientes==1)
p <- ggplot(DT2NQ, aes(x = METROSdescub)) + 
  geom_point(data=DT2NQ,aes(y = precioreal, colour = "Antes de cuarentena"))+
  labs(y = "Precio real",
       x = "Metros descubiertos")  +
  theme(legend.position = c(0.8, 0.9)) +
  theme_bw(base_size = 13) +
  theme(legend.title=element_blank())

p


##################################################################
##                        METROS TOTALES                        ##
##################################################################




DT2Q<-DT2[DT2$quarantine==1,]
DT2Q<-as.data.frame(DT2Q)
#DT2Q<-slice_sample(DT2Q,prop=0.3)

# Dropping outliers
DT2Q <- DT2Q %>% 
  mutate(p05=quantile(precioreal,probs=0.05,na.rm=TRUE))%>%
  mutate(p95=quantile(precioreal,probs=0.95,na.rm=TRUE))%>%
  mutate(q05=quantile(METROS,probs=0.05,na.rm=TRUE))%>%
  mutate(q95=quantile(METROS,probs=0.95,na.rm=TRUE))%>%
  mutate(qd05=quantile(METROSdescub,probs=0.05,na.rm=TRUE))%>%
  mutate(qd95=quantile(METROSdescub,probs=0.95,na.rm=TRUE))
DT2Q <- DT2Q[DT2Q$precioreal<=DT2Q$p95& 
               DT2Q$precioreal>=DT2Q$p05&
               DT2Q$METROS<=DT2Q$q95&DT2Q$METROS>=DT2Q$q05&
               DT2Q$METROSdescub<=DT2Q$qd95&
               DT2Q$METROSdescub>=DT2Q$qd05,] 


DT2Q<-subset(DT2Q,ambientes==2)
p <- ggplot(DT2Q, aes(x = METROS)) + 
  geom_point(data=DT2Q,aes(y = precioreal, colour = "Cuarentena"))+
  labs(y = "Precio real",
       x = "Metros descubiertos")  +
  theme(legend.position = c(0.8, 0.9)) +
  theme_bw(base_size = 13) +
  theme(legend.title=element_blank())


p








DT2NQ<-DT2[DT2$quarantine==0,]
DT2NQ<-as.data.frame(DT2NQ)
#DT2NQ<-slice_sample(DT2NQ,prop=0.3)

# Dropping outliers
DT2NQ <- DT2NQ %>% 
  mutate(p05=quantile(precioreal,probs=0.05,na.rm=TRUE))%>%
  mutate(p95=quantile(precioreal,probs=0.95,na.rm=TRUE))%>%
  mutate(q05=quantile(METROS,probs=0.05,na.rm=TRUE))%>%
  mutate(q95=quantile(METROS,probs=0.95,na.rm=TRUE))%>%
  mutate(qd05=quantile(METROSdescub,probs=0.05,na.rm=TRUE))%>%
  mutate(qd95=quantile(METROSdescub,probs=0.95,na.rm=TRUE))
DT2NQ <- DT2NQ[DT2NQ$precioreal<=DT2NQ$p95& 
                 DT2NQ$precioreal>=DT2NQ$p05&
                 DT2NQ$METROS<=DT2NQ$q95&DT2NQ$METROS>=DT2NQ$q05&
                 DT2NQ$METROSdescub<=DT2NQ$qd95&
                 DT2NQ$METROSdescub>=DT2NQ$qd05,] 


DT2NQ<-subset(DT2NQ,ambientes==2)
p <- ggplot(DT2NQ, aes(x = METROS)) + 
  geom_point(data=DT2NQ,aes(y = precioreal, colour = "Antes de cuarentena"))+
  labs(y = "Precio real",
       x = "Metros descubiertos")  +
  theme(legend.position = c(0.8, 0.9)) +
  theme_bw(base_size = 13) +
  theme(legend.title=element_blank())

p





DT2Q<-DT2[DT2$quarantine==1,]
DT2Q<-as.data.frame(DT2Q)
#DT2Q<-slice_sample(DT2Q,prop=0.3)

# Dropping outliers
DT2Q <- DT2Q %>% 
  mutate(p05=quantile(precioreal,probs=0.05,na.rm=TRUE))%>%
  mutate(p95=quantile(precioreal,probs=0.95,na.rm=TRUE))%>%
  mutate(q05=quantile(METROS,probs=0.05,na.rm=TRUE))%>%
  mutate(q95=quantile(METROS,probs=0.95,na.rm=TRUE))%>%
  mutate(qd05=quantile(METROSdescub,probs=0.05,na.rm=TRUE))%>%
  mutate(qd95=quantile(METROSdescub,probs=0.95,na.rm=TRUE))
DT2Q <- DT2Q[DT2Q$precioreal<=DT2Q$p95& 
               DT2Q$precioreal>=DT2Q$p05&
               DT2Q$METROS<=DT2Q$q95&DT2Q$METROS>=DT2Q$q05&
               DT2Q$METROSdescub<=DT2Q$qd95&
               DT2Q$METROSdescub>=DT2Q$qd05,] 





DT2NQ<-DT2[DT2$quarantine==0,]
DT2NQ<-as.data.frame(DT2NQ)
#DT2NQ<-slice_sample(DT2NQ,prop=0.3)

# Dropping outliers
DT2NQ <- DT2NQ %>% 
  mutate(p05=quantile(precioreal,probs=0.05,na.rm=TRUE))%>%
  mutate(p95=quantile(precioreal,probs=0.95,na.rm=TRUE))%>%
  mutate(q05=quantile(METROS,probs=0.05,na.rm=TRUE))%>%
  mutate(q95=quantile(METROS,probs=0.95,na.rm=TRUE))%>%
  mutate(qd05=quantile(METROSdescub,probs=0.05,na.rm=TRUE))%>%
  mutate(qd95=quantile(METROSdescub,probs=0.95,na.rm=TRUE))
DT2NQ <- DT2NQ[DT2NQ$precioreal<=DT2NQ$p95& 
                 DT2NQ$precioreal>=DT2NQ$p05&
                 DT2NQ$METROS<=DT2NQ$q95&DT2NQ$METROS>=DT2NQ$q05&
                 DT2NQ$METROSdescub<=DT2NQ$qd95&
                 DT2NQ$METROSdescub>=DT2NQ$qd05,] 



preQ<-subset(DT2NQ,ambientes==2)
postQ<-subset(DT2Q,ambientes==2)



# Gráficos juntos 
p <- ggplot(DT2Q, aes(x = METROSdescub)) + 
  geom_point(data=postQ,aes(y = precioreal, colour = "Cuarentena"),alpha=0.5 )+
  geom_point(data=preQ,aes(y = precioreal, colour = "Antes de cuarentena"),alpha=0.5)+
  geom_smooth(data=preQ,aes(y=precioreal,colour = "Antes de cuarentena"),se=FALSE,method="lm")+
  geom_smooth(data=postQ,aes(y=precioreal,colour = "Cuarentena"),se=FALSE,method="lm")+
  labs(y = "Precio real",
       x = "Metros cuadrados descubiertos")  +
  theme(legend.position = c(0.8, 0.9)) +
  theme_bw(base_size = 13) +
  theme(legend.title=element_blank())

p





preQ<-subset(DT2NQ)
postQ<-subset(DT2Q)

# Gráficos juntos 
p <- ggplot(DT2Q, aes(x = Gimnasio)) + 
  geom_point(data=postQ,aes(y = precioreal, colour = "Cuarentena"),alpha=0.5 )+
  geom_point(data=preQ,aes(y = precioreal, colour = "Antes de cuarentena"),alpha=0.5)+
  geom_smooth(data=preQ,aes(y=precioreal,colour = "Antes de cuarentena"),se=FALSE,method="lm")+
  geom_smooth(data=postQ,aes(y=precioreal,colour = "Cuarentena"),se=FALSE,method="lm")+
  labs(y = "Precio real",
       x = "Metros totales")  +
  theme(legend.position = c(0.8, 0.9)) +
  theme_bw(base_size = 13) +
  theme(legend.title=element_blank())

p





#################################################################
##                             OLD                             ##
#################################################################

p <- ggplot(DT2Q, aes(x = MONTH)) + 
  geom_line(data=mardel,aes(y = mediana_realp, colour = "Mar del Plata"))+
  geom_line(data=carilo,aes(y = mediana_realp, colour = "Cariló"))+
  labs(y = "Mediana real como proporción de 2017",
       x = "Fecha")  +
  # scale_colour_manual(values = c("blue", "red","green","orange","cyan","yellow","black")) +
  scale_shape_discrete(name  ="Región")+
  theme(legend.position = c(0.8, 0.9)) +
  theme_bw(base_size = 13) +
  theme(legend.title=element_blank())+
  #ylim(NA,1.5)+
  scale_x_date(date_labels = "%y-%m-%d" )+ 
  
  geom_vline(xintercept = as.Date("2017-01-01"),linetype=1,color="black",size=1)+
  geom_vline(xintercept = as.Date("2018-01-01"),linetype=1,color="black",size=1)+
  geom_vline(xintercept = as.Date("2019-01-01"),linetype=1,color="black",size=1)+
  geom_vline(xintercept = as.Date("2020-01-01"),linetype=1,color="black",size=1)+
  # geom_vline(xintercept = as.Date("2020-08-01"),linetype=1,color="gray",size=1)+
  # geom_vline(xintercept = as.Date("2020-09-01"),linetype=1,color="gray",size=1)+
  # geom_vline(xintercept = as.Date("2020-10-01"),linetype=1,color="gray",size=1)+
  
  geom_vline(xintercept = as.Date("2020-03-01"),linetype=1,color="brown",size=1,
             show.legend=TRUE)+
  geom_text(aes(x=as.Date("2020-03-01"), label="Cuarentena", y=1.35),
            colour="brown", angle=90, vjust = -0.5)

p



