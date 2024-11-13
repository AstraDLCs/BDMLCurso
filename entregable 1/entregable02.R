# librerias
install.packages("dplyr")
install.packages("ggplot2")
install.packages("tidyr")
install.packages("randomForest")
install.packages("rpart")
install.packages("rpart.plot")

library(dplyr)
library(ggplot2)
library(tidyr)
library(randomForest)
library(rpart)
library(rpart.plot)

# cargamos el csv
datos <- read.csv("listing_data.csv")

# limpieza
datos_limpios <- datos %>%
  drop_na() %>%
  distinct()

datos_limpios$Median_Family_Income <- as.numeric(gsub("[\\$,]", "", datos_limpios$Median_Family_Income))
datos_limpios$Price <- as.numeric(gsub("[\\$,]", "", datos_limpios$Price))

# categorias para price
datos_limpios$Price_Cat <- cut(datos_limpios$Price, breaks = c(-Inf, 100000, 300000, Inf), labels = c("bajo", "medio", "alto"))

# dividir datos
set.seed(123)
indices <- sample(1:nrow(datos_limpios), 0.8 * nrow(datos_limpios))
datos_train <- datos_limpios[indices, ]
datos_test <- datos_limpios[-indices, ]

# random forest
modelo_rf_cat <- randomForest(Price_Cat ~ Number_Beds + Number_Baths + Province + Population + Median_Family_Income, data = datos_train, ntree = 100)
predicciones_rf_cat <- predict(modelo_rf_cat, datos_test)
confusion_rf <- table(datos_test$Price_Cat, predicciones_rf_cat)
print("matriz de confusion del modelo random forest:")
print(confusion_rf)

# arbol de decision
modelo_arbol_cat <- rpart(Price_Cat ~ Number_Beds + Number_Baths + Province + Population + Median_Family_Income, data = datos_train, method = "class")
predicciones_arbol_cat <- predict(modelo_arbol_cat, datos_test, type = "class")
confusion_arbol <- table(datos_test$Price_Cat, predicciones_arbol_cat)
print("matriz de confusion del modelo arbol de decision:")
print(confusion_arbol)
