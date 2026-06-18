#Paso 1 - Carga de datos
# NOTA: Asegúrese de descargar el dataset desde Kaggle (enlace en el README)
sdss.og <- read.csv("star_class2.csv")
#Paso 2 - Variable categórica
sdss.og$Class <- factor(sdss.og$Class)
#Paso 3 - Se seleccionan las variables validas para el modelo y se crea un nuevo dataset
variables_validas <- c("u","g","r","i","z","Redshift")
sdss <- sdss.og[,variables_validas]

summary(sdss)

#Metodo de tipificacion
#Variable u
z.u <- (sdss$u - mean(sdss$u))/sd(sdss$u)
head(z.u)
k <- 4
filas_anomalas_u <- which(z.u > k)
z.u[which(z.u > k)]
#Se estandariza los valores anómalos
sdss$u[filas_anomalas_u] <- mean(sdss$u)
sdss$u[filas_anomalas_u]

#Variable g
z.g <- (sdss$g - mean(sdss$g))/sd(sdss$g)
head(z.g)
filas_anomalas_g <- which(z.g > k)
z.g[which(z.g > k)]
#Se estandariza los valores anómalos
sdss$g[filas_anomalas_g] <- mean(sdss$g)
sdss$g[filas_anomalas_g]

z.r <- (sdss$r - mean(sdss$r))/sd(sdss$r)
head(z.r)
filas_anomalas_r <- which(z.r > k)
z.r[which(z.r > k)]
#Se estandariza los valores anómalos
sdss$r[filas_anomalas_r] <- mean(sdss$r)
sdss$r[filas_anomalas_r]

z.i <- (sdss$i - mean(sdss$i))/sd(sdss$i)
head(z.i)
filas_anomalas_i <- which(z.i > k)
z.i[which(z.i > k)]
#Se estandariza los valores anómalos
sdss$i[filas_anomalas_i] <- mean(sdss$i)
sdss$i[filas_anomalas_i]

z.z <- (sdss$z - mean(sdss$z))/sd(sdss$z)
head(z.z)
filas_anomalas_z <- which(z.z > k)
z.z[which(z.z > k)]
#Se estandariza los valores anómalos
sdss$z[filas_anomalas_z] <- mean(sdss$z)
sdss$z[filas_anomalas_z]

boxplot(sdss$Redshift)
boxplot.stats(sdss$Redshift)$out
boxplot.stats(sdss$Redshift)
anomalo_Red <- which(sdss$Redshift>1.68)

z.Redshift <- (sdss$Redshift - mean(sdss$Redshift))/sd(sdss$Redshift)
head(z.Redshift)
filas_anomalas_Red <- which(z.Redshift > k) 
z.Redshift[which(z.Redshift > k)]
#Se valida los valores anómalos
table(na.omit(sdss.og[,c(variables_validas,"Class")])$Class[filas_anomalas_Red])
table(na.omit(sdss.og[,c(variables_validas,"Class")])$Class[anomalo_Red])

#Logaritmo del análisis de componentes principales 
pca <- prcomp(sdss, center = T, scale. = TRUE)

resumen_pca <- summary(pca)
print(resumen_pca)

v <- pca$sdev^2

sum(v[1:2])/sum(v)*100

z1 <- abs(pca$rotation[,1])
z2 <- abs(pca$rotation[,2])

sort(round(z1,4), decreasing = TRUE)

plot(seq(1,6),v,type = "b")
library(paran)
paran(x=sdss, iterations = 1000, graph = T, color = T)

round(sort(z1),2)
round(sort(z2),2)

round(sort(pca$rotation[,1]*(-1)),2)
round(sort(pca$rotation[,2]*(-1)),2)

pca$x <- pca$x * (-1)
biplot(x=pca, scale = 0, cex = 0.6, col=c("darkblue","brown"),xlim=c(-2.5,2.5),ylim=c(-3.5,2.5))

abline(h=0, lty=2)
abline(v=0, lty=2)

library(ggplot2)

df_final <- na.omit(sdss.og[,c(variables_validas,"Class")])
pca_definitivo <- prcomp(df_final[,variables_validas], center = TRUE, scale. = TRUE)

datos_grafico <- data.frame(
  Eje_X = as.numeric(pca_definitivo$x[, 1]),
  Eje_Y = as.numeric(pca_definitivo$x[, 2]),
  Clase_obj = as.factor(unlist(df_final$Class))
)

#GRÁFICO

ggplot(data = datos_grafico, aes(x = Eje_X, y=Eje_Y, color=Clase_obj)) +
  geom_point(alpha=0.3, size=1) +
  scale_color_manual(values = c("GALAXY"="#E41A1C",
                                "STAR"="#377EB8",
                                "CUASAR"="#4DAF4A")) +
  
  theme_minimal() +
  labs(
    title = "Componentes Principales: Clasificación Estelar SDSS",
    x="Componente Principal 1 (PC1)",
    y="Componente Principal 2 (PC2)"
  )

library(factoextra)
fviz_pca_var(pca_definitivo,
             col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE)
