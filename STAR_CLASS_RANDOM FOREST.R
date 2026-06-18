#RANDOM FOREST
library(rpart)
library(DMwR2)
library(randomForest)
library(ROCR)
library(caret)

sdss.og <- read.csv("C:/Users/javie/OneDrive/Master Business Intelligence + Big Data/TRABAJO FINAL MASTER/DRSS17/star_class2.csv")
sdss.og$Class <- factor(sdss.og$Class)

variables_validas <- c("u","g","r","i","z","Redshift","Class")
datos_modelo <- sdss.og[,variables_validas]

colSums(is.na(datos_modelo))
summary(datos_modelo)

#Metodo de tipificacion
#Variable u
z.u <- (datos_modelo$u - mean(datos_modelo$u))/sd(datos_modelo$u)
head(z.u)
k <- 4
filas_anomalas_u <- which(z.u > k)
z.u[which(z.u > k)]
#Se estandariza los valores anómalos
datos_modelo$u[filas_anomalas_u] <- mean(datos_modelo$u)
datos_modelo$u[filas_anomalas_u]

z.g <- (datos_modelo$g - mean(datos_modelo$g))/sd(datos_modelo$g)
head(z.g)
filas_anomalas_g <- which(z.g > k)
z.g[which(z.g > k)]
#Se estandariza los valores anómalos
datos_modelo$g[filas_anomalas_g] <- mean(datos_modelo$g)
datos_modelo$g[filas_anomalas_g]

z.r <- (datos_modelo$r - mean(datos_modelo$r))/sd(datos_modelo$r)
head(z.r)
filas_anomalas_r <- which(z.r > k)
z.r[which(z.r > k)]
#Se estandariza los valores anómalos
datos_modelo$r[filas_anomalas_r] <- mean(datos_modelo$r)
datos_modelo$r[filas_anomalas_r]

z.i <- (datos_modelo$i - mean(datos_modelo$i))/sd(datos_modelo$i)
head(z.i)
filas_anomalas_i <- which(z.i > k)
z.i[which(z.i > k)]
#Se estandariza los valores anómalos
datos_modelo$i[filas_anomalas_i] <- mean(datos_modelo$i)
datos_modelo$i[filas_anomalas_i]

z.z <- (datos_modelo$z - mean(datos_modelo$z))/sd(datos_modelo$z)
head(z.z)
filas_anomalas_z <- which(z.z > k)
z.z[which(z.z > k)]
#Se estandariza los valores anómalos
datos_modelo$z[filas_anomalas_z] <- mean(datos_modelo$z)
datos_modelo$z[filas_anomalas_z]

boxplot(datos_modelo$Redshift)
boxplot.stats(datos_modelo$Redshift)$out


z.Redshift <- (datos_modelo$Redshift - mean(datos_modelo$Redshift))/sd(datos_modelo$Redshift)
head(z.Redshift)
filas_anomalas_Red <- which(z.Redshift > k)
z.Redshift[which(z.Redshift > k)]
#Se valida los valores anómalos
table(na.omit(sdss.og[,c(variables_validas,"Class")])$Class[filas_anomalas_Red])

set.seed(123)
obj_stelar1 <- datos_modelo[datos_modelo$Class=="STAR",]
#2.Obtenemos el tamaño de las muestras de entrenamiento y test
ntrain <- ceiling(0.75 * nrow(obj_stelar1))
ntest <- nrow(obj_stelar1) - ntrain
#3. Hacemos una muestra aletaoria entre el tamaño de las datos
sampletrain <- sample.int(n=nrow(obj_stelar1), size = ntrain,replace = F)
sampletest <- sample.int(n=nrow(obj_stelar1), size = ntest,replace = F)
#4. Finalmente obtenemos la muestra de entrenamiento y reemplazo
train1 <- obj_stelar1[sampletrain,]
test1 <- obj_stelar1[sampletest,]

#Realizamos los pasos 1-4 para las siguientes clases de Galaxy y QUASAR
obj_stelar2  <- datos_modelo[datos_modelo$Class=="GALAXY",]
ntrain <- ceiling(0.75 * nrow(obj_stelar2))
ntest <- nrow(obj_stelar2) - ntrain
sampletrain <- sample.int(n=nrow(obj_stelar2), size = ntrain,replace = F)
sampletest <- sample.int(n=nrow(obj_stelar2),size = ntest,replace=F)
train2 <- obj_stelar2[sampletrain,]
test2 <- obj_stelar2[sampletest,]

obj_stelar3 <- datos_modelo[datos_modelo$Class=="CUASAR",]
ntrain <- ceiling(0.75 * nrow(obj_stelar3))
ntest <- nrow(obj_stelar3) - ntrain
sampletrain <- sample.int(n=nrow(obj_stelar3), size = ntrain,replace = F)
sampletest <- sample.int(n=nrow(obj_stelar3),size = ntest,replace=F)
train3 <- obj_stelar3[sampletrain,]
test3 <- obj_stelar3[sampletest,]

#Concatenamos los datasets de entrenamiento y test respectivamente
train <- rbind(train1,train2,train3)
test <- rbind(test1,test2,test3)

modelo.RF <- randomForest(Class~., data=train, ntree=500, mtry=5, importance=T)
modelo.RF

errores_finales <- modelo.RF$err.rate[nrow(modelo.RF$err.rate),]
print(errores_finales)

head(modelo.RF$votes)
modelo.RF$importance[,c(4,5)]
varImpPlot(x = modelo.RF, sort = T, n.var = 6)

modelo.RF.pred <- predict(object = modelo.RF, newdata = test, type = "response")
matriz_caret <- confusionMatrix(data = modelo.RF.pred, reference = test$Class, mode = "everything")
print(matriz_caret)
metricas_por_clase <- matriz_caret$byClass
metricas_resumen <- metricas_por_clase[,c("Precision", "Recall", "F1")]
print(metricas_resumen)

print(modelo.RF.pred)
plot(modelo.RF.pred, main ="Evolución del Error OOB vs Número de árboles")
legend("topright", colnames(modelo.RF.pred$err.rate), col = 1:4, cex = 0.8, fill = 1:4)

error_ob_final <- modelo.RF$err.rate[nrow(modelo.RF$err.rate), "OOB"]
print(error_ob_final)

varImpPlot(modelo.RF, main = "Importancia de Variables", col="darkblue", pch=16)


library(pROC)
roc_multi <- multiclass.roc(response = test$Class, predictor = modelo.RF.pred)
print(roc_multi)
