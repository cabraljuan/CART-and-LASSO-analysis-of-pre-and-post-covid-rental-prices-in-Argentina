############################################################################
############################################################################
###                                                                      ###
###                                 CART                                 ###
###                                                                      ###
############################################################################
############################################################################




# Path:
rm(list=ls())
path<-"G:/Facultad/San andres/big data/trabajo final/"
setwd(path)

# Libraries

library(beepr)
library(readxl)
library(stargazer)
library(plyr)
library(dplyr)


# Load data
DT<-read.csv("DT_cleanalquilerdep.csv",stringsAsFactors = FALSE)

DT2<-DT

# Only CABA
DT2<-DT2[DT2$ITE_ADD_STATE_NAME=="Capital Federal",]





# TIME Format:
DT2$TIM_DAY <- as.Date(DT2$TIM_DAY, "%Y-%m-%d")

# Keep last update
DT2<-DT2 %>% group_by(ITE_ITEM_ID) %>%
  slice(which.max(TIM_DAY))

# Quarantine 
quarantine<-as.Date("2020-03-20")
DT2$quarantine<-ifelse(DT2$TIM_DAY >=quarantine,1,0)


# Only keep some variables
DT2<-subset(DT2, select=c("precioreal","METROS","METROSdescub","Ambientes","Gimnasio",
                          "SalonDeUsosMul","Pileta","SalonFiestas","Antiguedad","Estacionamiento",
                          "Ascensor","Seguridad","quarantine"))





##################################################################
##                      Eliminate outliers                      ##
##################################################################


DT2NQ<-DT2[DT2$quarantine==0,]
DT2NQ<-as.data.frame(DT2NQ)



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


##################################################################
##                         Full dataset                         ##
##################################################################

DTtotal<-rbind(DT2NQ,DT2Q)

DTtotal$Ambientes<-as.factor(DTtotal$Ambientes)
DTtotal$SalonDeUsosMul<-as.factor(DTtotal$SalonDeUsosMul)
DTtotal$Pileta<-as.factor(DTtotal$Pileta)
DTtotal$Estacionamiento<-as.factor(DTtotal$Estacionamiento)
DTtotal$Ascensor<-as.factor(DTtotal$Ascensor)
DTtotal$Seguridad<-as.factor(DTtotal$Seguridad)
DTtotal$Gimnasio<-as.factor(DTtotal$Gimnasio)
DTtotal$SalonFiestas<-as.factor(DTtotal$SalonFiestas)
DTtotal$quarantine<-as.factor(DTtotal$quarantine)

DTtotal<-subset(DTtotal, select=c("precioreal","METROS","METROSdescub","Ambientes","Gimnasio",
                              "SalonDeUsosMul","Pileta","SalonFiestas","Antiguedad","Estacionamiento",
                              "Ascensor","Seguridad","quarantine"))

# Remove NAs
DTtotal<-na.omit(DTtotal)


method<-"poisson"
# Tree pre quarantine
tree<-rpart(precioreal~.,data=DTtotal,method=method)
summary(tree)

# pdf("tree_preQ.pdf",height=10,width=10,paper="special")
# fancyRpartPlot(preQ,sub="")
# dev.off()
# 

plot(tree)
text(tree,pretty=0)

##################################################################
##                         Full dataset                         ##
##################################################################







# Predictors
DT2NQ<-subset(DT2NQ, select=c("precioreal","METROS","METROSdescub","Ambientes","Gimnasio",
                              "SalonDeUsosMul","Pileta","SalonFiestas","Antiguedad","Estacionamiento",
                              "Ascensor","Seguridad"))


DT2Q<-subset(DT2Q, select=c("precioreal","METROS","METROSdescub","Ambientes","Gimnasio",
                            "SalonDeUsosMul","Pileta","SalonFiestas","Antiguedad","Estacionamiento",
                            "Ascensor","Seguridad"))

# # Save dataset ready to use 
# write.csv(DT2Q,"DT2Qready.csv")
# write.csv(DT2NQ,"DT2NQready.csv")
# 

# Format
DT2Q$Ambientes<-as.factor(DT2Q$Ambientes)
DT2Q$SalonDeUsosMul<-as.factor(DT2Q$SalonDeUsosMul)
DT2Q$Pileta<-as.factor(DT2Q$Pileta)
DT2Q$Estacionamiento<-as.factor(DT2Q$Estacionamiento)
DT2Q$Ascensor<-as.factor(DT2Q$Ascensor)
DT2Q$Seguridad<-as.factor(DT2Q$Seguridad)
DT2Q$Gimnasio<-as.factor(DT2Q$Gimnasio)
DT2Q$SalonFiestas<-as.factor(DT2Q$SalonFiestas)


DT2NQ$Ambientes<-as.factor(DT2NQ$Ambientes)
DT2NQ$SalonDeUsosMul<-as.factor(DT2NQ$SalonDeUsosMul)
DT2NQ$Pileta<-as.factor(DT2NQ$Pileta)
DT2NQ$Estacionamiento<-as.factor(DT2NQ$Estacionamiento)
DT2NQ$Ascensor<-as.factor(DT2NQ$Ascensor)
DT2NQ$Seguridad<-as.factor(DT2NQ$Seguridad)
DT2NQ$Gimnasio<-as.factor(DT2NQ$Gimnasio)
DT2NQ$SalonFiestas<-as.factor(DT2NQ$SalonFiestas)



# Trees pre and post quarantine

# Remove NAs
DT2NQ<-na.omit(DT2NQ)
DT2Q<-na.omit(DT2Q)


# Libraries for CART
library(rpart)
library(class) 
library(rattle)


method<-"poisson"
# Tree pre quarantine
preQ<-rpart(precioreal~.,data=DT2NQ,method=method)
summary(preQ)


plot(preQ)
text(preQ,pretty=0)


pdf("tree_preQ.pdf",height=10,width=10,paper="special")
fancyRpartPlot(preQ,sub="")
dev.off()


# Tree post quarantine
postQ<-rpart(precioreal~.,data=DT2Q,method=method)
summary(postQ)



plot(postQ)
text(postQ,pretty=0)


pdf("tree_postQ.pdf",height=10,width=10,paper="special")
fancyRpartPlot(postQ,sub="")
dev.off()







###########################################################################
###########################################################################
###                                                                     ###
###                                LASSO                                ###
###                                                                     ###
###########################################################################
###########################################################################



# Path:
rm(list=ls())
path<-"G:/Facultad/San andres/big data/trabajo final/"
setwd(path)

# Libraries

library(beepr)
library(readxl)
library(stargazer)
library(plyr)
library(dplyr)


# Load data
DT<-read.csv("DT_cleanalquilerdep.csv",stringsAsFactors = FALSE)

DT2<-DT

# Only CABA
DT2<-DT2[DT2$ITE_ADD_STATE_NAME=="Capital Federal",]


# TIME Format:
DT2$TIM_DAY <- as.Date(DT2$TIM_DAY, "%Y-%m-%d")

# Keep last update
DT2<-DT2 %>% group_by(ITE_ITEM_ID) %>%
  slice(which.max(TIM_DAY))

# Quarantine 
quarantine<-as.Date("2020-03-20")
DT2$quarantine<-ifelse(DT2$TIM_DAY >=quarantine,1,0)


# Only keep some variables
DT2<-subset(DT2, select=c("precioreal","METROS","METROSdescub","Ambientes","Gimnasio",
                          "SalonDeUsosMul","Pileta","SalonFiestas","Antiguedad","Estacionamiento",
                          "Ascensor","Seguridad","quarantine"))





##################################################################
##                      Eliminate outliers                      ##
##################################################################


DT2NQ<-DT2[DT2$quarantine==0,]
DT2NQ<-as.data.frame(DT2NQ)



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



# Predictors
DT2NQ<-subset(DT2NQ, select=c("precioreal","METROS","METROSdescub","Ambientes","Gimnasio",
                              "SalonDeUsosMul","Pileta","SalonFiestas","Antiguedad","Estacionamiento",
                              "Ascensor","Seguridad"))


DT2Q<-subset(DT2Q, select=c("precioreal","METROS","METROSdescub","Ambientes","Gimnasio",
                            "SalonDeUsosMul","Pileta","SalonFiestas","Antiguedad","Estacionamiento",
                            "Ascensor","Seguridad"))




# Remove NAs
DT2NQ<-na.omit(DT2NQ)
DT2Q<-na.omit(DT2Q)


# Libraries for lasso
library(glmnet)       # CV



# Guardamos los predictores y la variable de respuesta como matrices y vectores:
x <- subset(DT2NQ, select = -c(precioreal))
y <- subset(DT2NQ, select = c(precioreal))

# Convierto en matriz
x <- as.matrix(x)
y <- as.matrix(y)




# 10-fold CROSS-VALIDATION para elegir lambda

# El paquete glmnet viene con su propia funcion de CV para 10 grupos de 
# datos aleatoriamente seleccionados
cv.outL <- cv.glmnet(x,y,alpha=1) # Lasso #alpha=1


# Grfáico del ECM y ln de lambda
plot(cv.outL)

# Lambda óptimo
bestlamL <- cv.outL$lambda.min
bestlamL



# Ejercicio 3: 
# En el caso de LASSO, ¿qué variables fueron descartadas? ¿Tiene sentido? ¿Se confirma lo que respondió en el inciso 4 de la Parte I?


# Lasso
lasso <- glmnet(x,y,lambda=bestlamL,alpha=1) # lasso

# Variables
coef_lassopre<-coef(lasso)






# Guardamos los predictores y la variable de respuesta como matrices y vectores:
x <- subset(DT2Q, select = -c(precioreal))
y <- subset(DT2Q, select = c(precioreal))

# Convierto en matriz
x <- as.matrix(x)
y <- as.matrix(y)




# 10-fold CROSS-VALIDATION para elegir lambda

# El paquete glmnet viene con su propia funcion de CV para 10 grupos de 
# datos aleatoriamente seleccionados
cv.outL <- cv.glmnet(x,y,alpha=1) # Lasso #alpha=1


# Grfáico del ECM y ln de lambda
plot(cv.outL)

# Lambda óptimo
bestlamL <- cv.outL$lambda.min
bestlamL



# Ejercicio 3: 
# En el caso de LASSO, ¿qué variables fueron descartadas? ¿Tiene sentido? ¿Se confirma lo que respondió en el inciso 4 de la Parte I?


# Lasso
lasso <- glmnet(x,y,lambda=bestlamL,alpha=1) # lasso

# Variables
coef_lassopost<-coef(lasso)


coefslasso<-cbind(coef_lassopre, coef_lassopost)


colnames(coefslasso)<-c("Pre cuarentena","Cuarentena")

coefslasso<-as.matrix(coefslasso)
coefslasso<-as.data.frame(coefslasso)
coefslasso$var<-c(unlist(coef_lassopost@Dimnames[1]))


diff<-as.data.frame(( (coefslasso$Cuarentena-coefslasso$`Pre cuarentena` )/ coefslasso$`Pre cuarentena` )*100)
diff$var<-coefslasso$var


colnames(diff)<-c("diff","var")

# Remove variables that before were 0
diff<-diff[!diff$diff=="-Inf",]


library(ggplot2)

# Plot
ggplot(diff, aes(x=var, y=diff)) + 
  theme_bw(base_size = 14)+
  geom_point(size=3) + 
  geom_segment(aes(x=var, 
                   xend=var, 
                   y=0, 
                   yend=diff)) + 
  theme(axis.text.x = element_text(angle=65, vjust=0.6))+
  xlab("Variable")+
  ylab("Diferencia en porcentajes")


ggsave("Difflasso.png",
       dpi=320,units="in",width=10,height=5)




