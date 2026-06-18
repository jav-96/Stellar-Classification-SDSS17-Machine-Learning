# Stellar-Classification-SDSS17-Machine-Learning
Repositorio sobre modelos de clasificación de aprendizaje supervisado y no supervisado con la data SDSS17
Descripción del proyecto: Clasificación de estrellas, galaxias y cuásares utilizando el dataset SDSS17.
Tecnologías utilizadas: R, RStudio, paquetes (caret, randomForest, etc.)
Estructura del código: Script 1: PCA_STAR_CLASS - Construcción del modelo PCA. Script 2: MODEL_STAR_CLASS2 - Construcción del modelo de regresión logística. Script 3: STAR_CLASS_RANDOM FOREST - Construcción del modelo random forest.

El dataset base se encuentra en Kaggle en el siguiente enlace:
https://www.kaggle.com/fedesoriano/stellar-classification-dataset-sdss17

El dataset con ligeras pasos de limpieza y transformación se encuentra en el siguiente enlace:
https://drive.google.com/file/d/1KKxhz1kP3EeLcUEepkyScJSv0GUvtFbt/view?usp=sharing

A continuación se detallan un poco más sobre algunos pasos de limpieza y transformación que se realizaron:
Cambio de QSO a CUASAR - CAMBIO NECESARIO PARA QUE NO DE ERROR EL MODELO
Eliminación de un valor anómalo de 99999 en las variables u-g-z
Renombres de diferentes columnas que finalmente no se ocupan en el modelo. 
