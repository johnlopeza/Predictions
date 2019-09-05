#predecir posibles Fraudes 

#paquetes
install.packages("arm")

#paquetes
library(data.table)
library(ggplot2)
library(pROC)
library(arm)
library(mice)


#importar los datos train
train <- fread("TrainFraude.csv")

#importar los datos test
test <- fread("TestFraude.csv")

#asignar Columnas para poder separar
train[,datos := "train"]
test[,FRAUDE := NA]
test[,datos := "test"]

# juntar los datos
fraude <- rbind(test, train)
fraude[, TRAVELED_TIME_MIN := round(TRAVELED_TIME/60) ]

#summry str
summary(fraude)
str(fraude)


#limpieza de Data 
fraude[is.na(CANTIDAD_SERVICIOS), CANTIDAD_SERVICIOS := median(fraude$CANTIDAD_SERVICIOS, na.rm = T)]
fraude[is.na(CALIFICACION_CONDUCTOR), CALIFICACION_CONDUCTOR := median(fraude$CALIFICACION_CONDUCTOR, na.rm = T)]

#cambiar a factor 
fraude$COSTO_FINAL <- as.factor(fraude$COSTO_FINAL)
fraude$TRAVELED_TIME <-  as.factor(fraude$TRAVELED_TIME)
fraude$TELEFONO_CONDUCTOR <-  as.factor(fraude$TELEFONO_CONDUCTOR)
fraude$CANTIDAD_SERVICIOS <-  as.factor(fraude$CANTIDAD_SERVICIOS)
fraude$FRAUDE <- as.factor(fraude$FRAUDE)
fraude$FECHA <-  as.Date.factor(fraude$FECHA) 
fraude$BILLETERA_CONDUCTOR <- as.factor(fraude$BILLETERA_CONDUCTOR)
fraude$LOCALIDAD <- as.factor(fraude$LOCALIDAD)



#analisis Exploratorio Fraudes
ggplot(data = fraude[datos == "train"], aes(x = FRAUDE, fill = FRAUDE)) + geom_bar(aes(y = (..count..)/sum(..count..)))+ ggtitle(" Fraudes %")

#analisis FECHA 
ggplot(data = fraude[datos == "train"], aes(x = FECHA, fill = FRAUDE)) + geom_bar(aes(y = (..count..)/sum(..count..)))+ theme(axis.text.x = element_text(angle = 45, size = 5, hjust = 1, vjust = 1))  + ggtitle(" Fraudes %")

#analisis LOCALIDAD *
ggplot( data = fraude[datos == "train"], aes(x = LOCALIDAD, fill = FRAUDE)) + geom_bar(aes(y = (..count..)/sum(..count..))) + theme(axis.text.x = element_text(angle = 45, size = 5, hjust = 1, vjust = 1)) + ggtitle("Localidades %")

#analisis CALIFICACION **
ggplot( data = fraude[datos == "train"], aes(x = CALIFICACION_CONDUCTOR, fill = FRAUDE)) + geom_bar(aes(y = (..count..)/sum(..count..))) + facet_wrap(~ FRAUDE)+ theme(axis.text.x = element_text(angle = 45, size = 5, hjust = 1, vjust = 1)) + ggtitle("Calificaiones del Servicio %")

#analisis telefono Conductor
ggplot( data = fraude[datos == "train"], aes(x = TELEFONO_CONDUCTOR, fill = FRAUDE)) + geom_bar(aes(y = (..count..)/sum(..count..))) + theme(axis.text.x = element_text(angle = 45, size = 5, hjust = 1, vjust = 1)) + ggtitle("Calificaiones del Servicios %")


# Analisis Fecha vs localidad
ggplot(data = fraude[datos == "train"],
       aes(x = FECHA, y = LOCALIDAD, colour = FRAUDE)) +
  geom_point() + theme(axis.text.x = element_text(angle = 45, size = 5, hjust = 1, vjust = 1)) 


# Analisis Fecha vs MINUTOS
ggplot(data = fraude[datos == "train"],
       aes(x = FECHA, y = TRAVELED_TIME_MIN, colour = FRAUDE)) +
  geom_point() + theme(axis.text.x = element_text(angle = 45, size = 5, hjust = 1, vjust = 1)) 


# Analisis MINUTOS vs COSTO_FINAL
ggplot(data = fraude[datos == "train"],
       aes(x = TRAVELED_TIME_MIN, y = COSTO_FINAL, colour = FRAUDE)) +
  geom_point() + theme(axis.text.x = element_text(angle = 45, size = 5, hjust = 1, vjust = 1)) 


# Analisis CALIFICACION_CONDUCTOR vs BILLETERA_CONDUCTOR
ggplot(data = fraude[datos == "train"],
       aes(x = CALIFICACION_CONDUCTOR, y = BILLETERA_CONDUCTOR, colour = FRAUDE)) +
  geom_point() + theme(axis.text.x = element_text(angle = 45, size = 5, hjust = 1, vjust = 1)) 


ggplot(data = fraude[datos == "train"],
       aes(x = CALIFICACION_CONDUCTOR, y = CANTIDAD_SERVICIOS, colour = FRAUDE)) +
  geom_point() + theme(axis.text.x = element_text(angle = 45, size = 5, hjust = 1, vjust = 1)) 


ggplot(data = fraude[datos == "train"],
       aes(x = CANTIDAD_SERVICIOS, y = LOCALIDAD, colour = FRAUDE)) +
  geom_point() + theme(axis.text.x = element_text(angle = 45, size = 5, hjust = 1, vjust = 1)) 


#set Seet
set.seed(123)

# parametros del modelo 
# TRAVELED_TIME_MIN + CALIFICACION_CONDUCTOR + CANTIDAD_SERVICIOS + LOCALIDAD 

# Descartados
# BILLETERA_CONDUCTOR + FECHA

# bayesglm modelo 
model.glm <- bayesglm(FRAUDE ~ TRAVELED_TIME_MIN + CALIFICACION_CONDUCTOR + CANTIDAD_SERVICIOS + LOCALIDAD , data = fraude[datos == "train"], family = binomial)
summary(model.glm)

#Predicciones
predicciones <- predict(model.glm, fraude)

#Asignar Prodicciones
fraude[,pred := predicciones]

# Calcular ROC
roc <- roc(fraude$FRAUDE, fraude$pred)
roc
plot(roc)

#Asignar Probabilidades 0, 1
fraude[pred >= 0.5, pred_grupo := 1]
fraude[pred < 0.5, pred_grupo := 0]

# hacer una tabla 
tbl <- table(fraude$FRAUDE, fraude$pred_grupo)

# Precision 87 %
precisio <- (tbl[1,1] + tbl[2,2])/sum(tbl)

############ Datos Test

#repetir esto para datos no vistos 
tam <- floor(0.75*nrow(fraude[datos =="train"]))
set.seed(123)

posicion <- sample(seq_len(nrow(fraude[datos =="train"])), size = tam) 
train.train <- fraude[datos =="train"][posicion]
train.test <- fraude[datos =="train"][-posicion]


# bayesglm modelo 
model.glm <- bayesglm(FRAUDE ~ TRAVELED_TIME_MIN + CALIFICACION_CONDUCTOR  + LOCALIDAD , data = train.train, family = binomial)
summary(model.glm)

#Predicciones
predicciones <- predict(model.glm, train.test)

#Asignar Prodicciones
train.test[,pred := predicciones]

# Calcular ROC
roc <- roc(train.test$FRAUDE, train.test$pred)
roc
plot(roc)

#Asignar Probabilidades 0, 1
train.test[pred >= 0.5, pred_grupo := 1]
train.test[pred < 0.5, pred_grupo := 0]

# hacer una tabla 
tbl <- table(train.test$FRAUDE, train.test$pred_grupo)

# Precision 80 %
precisio <- (tbl[1,1] + tbl[2,2])/sum(tbl)

#exportar usando test
#prediciones
predicciones <- predict(model.glm, fraude[datos == "test"])


#Asignar Prodicciones
fraude[datos == "test",pred := predicciones]


#Asignar Probabilidades 0, 1
fraude[datos == "test" & pred >= 0.5, pred_grupo := 1]
fraude[datos == "test" & pred < 0.5, pred_grupo := 0]

#para exportar
exportar <- fraude[datos == "test"]


fwrite(exportar,"PrediccionFraude.csv", row.names = F)

pred <- fread("PrediccionFraude.csv")











