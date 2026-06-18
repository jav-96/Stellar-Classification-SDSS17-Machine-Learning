#Carga de datos
#ANALISIS 2
library(e1071)
library(ROCR)
library(rpart)
library(rpart.plot)

library(caret)
library(nnet)
# NOTA: Asegúrese de descargar el dataset desde Kaggle (enlace en el README)
star_class1 <- read.csv("star_class2.csv")
summary(star_class1)
star_class1$Class <- factor(star_class1$Class)

variables_validas <- c("u","g","r","i","z","Redshift","Class")
datos_modelo <- star_class1[,variables_validas]

#LIMPIEZA DE DATOS
#1)Variable u
z.u <- (datos_modelo$u - mean(datos_modelo$u))/sd(datos_modelo$u)
head(z.u)
k <- 4
filas_anomalas_u <- which(z.u > k)
z.u[which(z.u > k)]
  #Se estandariza los valores anómalos
datos_modelo$u[filas_anomalas_u] <- mean(datos_modelo$u)
datos_modelo$u[filas_anomalas_u]

#2)Variable g
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

#Se valida los valores anómalos
z.Redshift <- (datos_modelo$Redshift - mean(datos_modelo$Redshift))/sd(datos_modelo$Redshift)
head(z.Redshift)
filas_anomalas_Red <- which(z.Redshift > k)
table(datos_modelo$Class[filas_anomalas_Red])

prop.table(table(datos_modelo$Class))

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

#Realizamos los pasos anteriores para las siguientes clases de Galaxy y QUASAR
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

preProcesamiento <- preProcess(train, method = c("center","scale"))

train_norm <- predict(preProcesamiento, train)
test_norm <- predict(preProcesamiento, test)

modelo <- multinom(formula = Class~., data = train_norm, maxit=1000)
modelo

modelo.baseline <- multinom(formula = Class~1, data=train_norm, maxit=1000)
modelo.baseline

#Resta de deviance entre modelo base y modelo ajustado
chi <- modelo.baseline$deviance - modelo$deviance
#Grados de libertad
gl <- modelo$edf - modelo.baseline$edf
gl
#Test de verosimilitud a través de p-valor
pvalor <- 1 - pchisq(chi, df = gl)
pvalor

modelo.pred.class <- predict(object = modelo, newdata = test_norm, type = "class")

test_norm[36,]
modelo.pred.class[36]

test_norm[1500,]
modelo.pred.class[1500]

confusionMatrix(data = modelo.pred.class, reference = test_norm$Class)
matriz_evaluacion <- confusionMatrix(data = modelo.pred.class, 
                                     reference = test_norm$Class, 
                                     mode = "everything")

print(matriz_evaluacion)
metricas_por_clase <- matriz_evaluacion$byClass
metricas_resumen <- metricas_por_clase[,c("Precision", "Recall", "F1")]
print(metricas_resumen)


test_norm[,7]
sum(modelo.pred.class==test_norm[,7])
sum(modelo.pred.class==test_norm[,7])/nrow(test_norm)

table(test_norm[,7],modelo.pred.class)
