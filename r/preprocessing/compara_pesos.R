setwd("/home/yuliaxis/GUTBRAIN")
#######################################################################
#######################################################################
library(RMySQL)
library(lme4)
library(doBy)
m<-dbDriver("MySQL")
con<-dbConnect(m,user='juanpablo',password='123',host='172.17.30.50',dbname='DB_CAP')
#####################
animales<-dbReadTable(con, "Historico_CONTROL_ANIMALES")
#camada         <- paste(animales$M,animales$FN,sep="")

camada         <- paste(animales$M,animales$FN,sep="@")
camada2        <- paste(animales$FN,animales$M,sep="@")

animales       <- cbind(animales,camada,camada2)

animales       <- animales[ ( animales$E == "GUTBRAIN"
 )
                             ,]
animales       <- animales[     is.na(animales$FB) ,]
animales       <- animales[     animales$FN != "0000-00-00" , ]

###################################
summaryBy(T ~ E+L,data=animales,FUN=length)
#############

# T-> Tatuaje
# FN-> Fecha Nacimiento
# R-> Raza
# P-> Padre
# M-> Madre
# O-> Granja
# S-> Sexo
# FE-> Fecha de Entrada al control
# Tr-> Tratamiento
# Ob-> Observaciones (generalmente vacio)
# E-> Experiento
# L-> Lote
# Ch-> Chip electronico de la oreja de los cerdos
# N-> Nave (sala en la que estan)
# C-> Corral
# FB-> Fecha Baja (antes de tiempo por alguna razon)
# CB-> Causa de la baja (generalmente vacio)
# camada-> camada de la que proviene

animales$lote_box    <-paste(animales$L,animales$C ,            sep="-")
animales$exp_trt     <-paste(animales$E,animales$Tr,            sep="-")
animales$exp_lote    <-paste(animales$E,animales$L ,            sep="-")
animales$exp_lote_trt<-paste(animales$E,animales$L ,animales$Tr,sep="-")
animales$exp_lote_box<-paste(animales$E,animales$L ,animales$C, sep="-")

tb<-table(animales$exp_lote_box)
tb
selected_exp_lote_box<-names(tb)[tb>=7]
animales<-animales[!is.na(match(animales$exp_lote_box,selected_exp_lote_box)),]
animales1<-animales

####
####


#####
consumo        <-dbReadTable(con, "Historico_CONTROL_CONSUMO")
consumo<-consumo[consumo$ERR == 0,]
consumo<-consumo[consumo$Co > 0,]

#  T-> Tatuaje  #  F-> Fecha de consumo  #  Vi-> Visitas  #  Ti-> Tiempo (segundos)  #  Co->Consumo diario (gr)  #  ERR-> Comentarios de errores en la medida (vacio)


##########
peso           <-dbReadTable(con, "Historico_CONTROL_PESO")
peso           <-peso[peso$Pe>0,]
peso$Toc       <-ifelse(peso$Toc == 0,NA,peso$Toc)
peso$Lo        <-ifelse(peso$Lo  == 0,NA,peso$Lo )

#  T-> Tatuaje  #  FP-> Fecha de Pesaje  #  Pe-> Peso(kg)  #  Toc->Tocino Dorsal  #  Lo->Espesor del Lomo  #  I->Incidencias (vacio)

#####
###############
min_FP<-tapply(as.Date(peso$FP),peso$T,min)
max_FP<-tapply(as.Date(peso$FP),peso$T,max)
ID_FPs<-cbind(min_FP,max_FP)
T<-rownames(ID_FPs)
ID_FPs<-data.frame(T,ID_FPs)
ID_FPs$min_FP<-as.Date(ID_FPs$min_FP,origin="1970-01-01")
ID_FPs$max_FP<-as.Date(ID_FPs$max_FP,origin="1970-01-01")
#####
consumo<-merge(consumo,ID_FPs)
consumo<-consumo[(as.Date(consumo$F)>=as.Date(consumo$min_FP)) & (as.Date(consumo$F)<=as.Date(consumo$max_FP)) ,]

peso<-merge(peso,ID_FPs)
peso<-peso[(as.Date(peso$FP)>=as.Date(peso$min_FP)) & (as.Date(peso$FP)<=as.Date(peso$max_FP)) ,]
##############################

#######################################################################
peso    <-merge(peso,animales,by.x="T",by.y="T")
EDAD    <-round(as.numeric(as.Date(peso$FP) - as.Date(peso$FN)),0)
peso    <-cbind(peso,EDAD)
consumo <-merge(consumo,animales,by.x="T",by.y="T")
EDAD    <-round(as.numeric(as.Date(consumo$F) - as.Date(consumo$FN)),0)
consumo <-cbind(consumo,EDAD)
#######################################################################



raw_visits           <-dbReadTable(con, "Historico_CONTROL_raw_VISITS")
raw_visits    <-merge(raw_visits,animales,by.x="T",by.y="T")

raw_visits$P_animal<-raw_visits$P_animal/1000

peso_NEDAP<-summaryBy( P_animal ~ T +  F,data=raw_visits,FUN=c(length,mean,median,sd))

peso1<-merge(peso,peso_NEDAP,by.x=c("T","FP"),by.y=c("T","F"))

plot(peso1$Pe,peso1$P_animal.mean,xlab="PESO MANUAL (Kg)",ylab="PESO NEDAP (Kg) - media del día")
fit<-lm(P_animal.mean ~ Pe,data=peso1)
summary(fit)
with(peso1,cor(P_animal.mean, Pe))
abline(coef(fit),col="red"  )
abline(   c(0,1),col="green")


plot(peso1$Pe,peso1$P_animal.median,xlab="PESO MANUAL (Kg)",ylab="PESO NEDAP (Kg) - mediana del día")
fit<-lm(P_animal.median ~ Pe,data=peso1)
summary(fit)
with(peso1,cor(P_animal.median, Pe))
abline(coef(fit),col="red"  )
abline(   c(0,1),col="green")











































