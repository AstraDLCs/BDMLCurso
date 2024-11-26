# importamos las librerias necesarias
install.packages("dplyr")
install.packages("ggplot2")
install.packages("tidyr")

library(dplyr)
library(ggplot2)
library(tidyr)

# cargamos el csv de hugging face
datos <- read.csv("listing_data.csv")

# limpia los datos, elimina filas vacias o duplicadas
datos_limpios <- datos %>%
  drop_na() %>%
  distinct()

# convierte las columnas de ingreso y precio a numeros, eliminando los simbolos de moneda
datos_limpios$Median_Family_Income <- as.numeric(gsub("[\\$,]", "", datos_limpios$Median_Family_Income))
datos_limpios$Price <- as.numeric(gsub("[\\$,]", "", datos_limpios$Price))



# analisis exploratorio con graficos

# histograma para ver como se distribuyen los precios de las propiedades
ggplot(datos_limpios, aes(x = Price)) +
  geom_histogram(binwidth = 50000, fill = "blue", color = "black") +
  labs(title = "Distribucion de precios de las propiedades", x = "Precio", y = "Frecuencia")


# ver como se relaciona el precio con el numero de habitaciones
ggplot(datos_limpios, aes(x = as.numeric(Number_Beds), y = Price)) +
  geom_point(color = "darkgreen") +
  labs(title = "Precio vs. Numero de habitaciones", x = "Numero de habitaciones", y = "Precio")



# preparar los datos para los modelos

datos_limpios$Province <- as.factor(datos_limpios$Province)
datos_limpios$Province <- as.numeric(datos_limpios$Province)

# divide los datos en 80% para entrenar y 20% para probar
set.seed(123)
indices <- sample(1:nrow(datos_limpios), 0.8 * nrow(datos_limpios))
datos_train <- datos_limpios[indices, ]
datos_test <- datos_limpios[-indices, ]

# ver unas filas de los datos de entrenamiento y prueba para chequear que todo este bien
head(datos_train)
head(datos_test)
