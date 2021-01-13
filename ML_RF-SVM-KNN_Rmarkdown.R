---
  title: "Machine Learning R"
author: "Victor Carrasco"
date: "23/10/2020"
output:
  prettydoc::html_pretty:
  theme: cayman
---
  
  ```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```


***Usando la base de datos wine (disponible en el paquete rattle.data) contiene información de 178 vinos y se clasifican en 3 tipos de vino (Type=1,2,3) a partir de 13 variables disponibles.***
  
#### primero hay que  cargar datos y convertir en factor la variable "Type" ya que es la variable target:
  
  ```{r }

library(rattle.data)
data(wine)

wine$Type <- as.factor(wine$Type)

wine_original <-wine 

```

Explorando la base de datos

```{r }

head(wine)

table(wine$Type)

```


#### antes de proceder a utilziar un mecanismo de ML, es conveniente estandarizar variables. Pueden hacerse de 2 maneras, normalizando o desviacion estandar. Utilizo el primero

```{r }

normalizar_variables <- function(x)
{
  return((x- min(x)) /(max(x)-min(x)))
}


wine[,-1] <- as.data.frame(lapply(wine[,-1],normalizar_variables))

```

#### separamos la base de datos en 70% para entrenamiento y 30% para prueba.

Para hacer esta separación siempre replicable, lo mejor es usar una semilla.


```{r }

set.seed(2020)



position <- sample(1:nrow(wine), 0.7*nrow(wine))

# tomo una muestra aleatoria de la base de datos y uso el 70% (sample y 0.7)

training <- wine[position, ]

# determinada la muestra anterior la uso de base de entrenamiento

test <- wine[-position, ]

# y el resto de la base 30%  para prueba

```

#### utilizamos 3 tipos de algortimos Random Forest (20 árboles y 4 variables) y Support Vector Machine (kernel radial) y  KNN(k=7).

$Random Forest$
  
  ```{r message=FALSE}
require(randomForest)


# n. tree = 20, reemplazo  si, importancia de cada variable 
# mtry=4
model.rf <- randomForest(Type ~ ., data=wine, 
                         ntree=20, mtry=4, replace=TRUE, importance=T)

varImpPlot(model.rf)
```

Con esto es posible determinar la importancia según 2 tipos de criterios, de precisión y Gini, por convención utilizaremos este ultimo.

Según el criterio de Gini, la variable Proline es la que se repite más veces en los arboles, esto viene seguido de Color y Flavanoids.

$Support Vector Machine$
  
  ```{r message=FALSE}
require(e1071)
model.svm <- svm(Type ~ ., data=training,
                 kernel="radial") 

```

$k-Nearest Neighbors$
  
  ```{r message=FALSE}

require(class)
modelo_KNN <- knn(train=training[,-1], test=training[,-1], cl=training[,1], k=7)

```

#### el porcentaje de clasificaciones correctas utilizando la base de entrenamiento podemos calcularlo como:

$Random Forest$
  
  tenemos para Random Forest que la calidad de ajuste viene dada por:
  
  ```{r }
# Valores predictivos
predval.rf1 <- predict(model.rf, training)


# Matriz de confusión
table(training$Type, predval.rf1)
```

--------------------------------------------------------
  
  - Se clasifico correctamente 45 vinos de tipo 1

- Se clasifico correctamente 44 vinos de tipo 2

- Se clasifico correctamente 35 vinos de tipo 3

------------------------------------------------------
  
  Porcentaje de clasificación correcta

```{r }
mean(training$Type == predval.rf1)
```

Para random forest se ha obtenido un 100% de ajuste en la base de entrenamiento, lo cual podria ser indicio de un sobre ajuste, para determinar de mejor manera esto, es conveniente realizar una validación cruzada.

$Support Vector Machine$
  
  tenemos para Support Vector Machine la calidad de ajuste viene dada por:
  
  ```{r }
# Valores predictivos
predval.svm <- predict(model.svm, training)

# Matriz de confusión
table(training$Type, predval.svm)

```

--------------------------------------------------------
  
  - Se clasifico correctamente 45 vinos de tipo 1

- Se clasifico correctamente 44 vinos de tipo 2

- Se clasifico correctamente 35 vinos de tipo 3

------------------------------------------------------
  
  Porcentaje de clasificación correcta

```{r }
mean(training$Type == predval.svm)

```

$k-Nearest Neighbors$
  
  tenemos para KNN la calidad de ajuste viene dada por:
  
  ```{r }

# Matriz de confusión
table(training$Type, modelo_KNN)

```

--------------------------------------------------------
  
  - Se clasifico correctamente 45 vinos de tipo 1

- Se clasifico correctamente 41 vinos de tipo 2

- Se clasifico correctamente 35 vinos de tipo 3

------------------------------------------------------
  
  Porcentaje de clasificación correcta

```{r }
mean(training$Type == modelo_KNN)

```


#### Aplicando una validación cruzada 

Metodo cross validation, 30 iteraciones, 

70% para la base de entrenamiento 30% prueba

$Random Forest$
  
  la validación cruzada para Random Forest viene dada por:
  
  ```{r message=FALSE}

library(caret)



( cv_rf <- train(Type ~., data=training, method="rf", 
                 trControl=trainControl(method="cv", number=30, p=0.7), 
                 tuneGrid=expand.grid(.mtry=4)) )

```

Donde el porcentaje de ajuste para random forest es del 99,1%

$Support Vector Machine$
  
  la validación cruzada para Support Vector Machine viene dada por:
  
```{r message=FALSE}
(cv.svm <- train(Type ~., data=training, method="svmRadial", 
                 trControl=trainControl(method="cv", number=30, p=0.7))) 
```

Donde el porcentaje de ajuste para Support Vector Machine es del 98,3% 

$k-Nearest Neighbors$
  
  la validación cruzada para KNN viene dada por:

```{r message=FALSE}
( cv_KNN <- train(Type ~., data=training, method="knn", 
                  trControl=trainControl(method="cv", number=30, p=0.7)))
```

Donde el porcentaje de ajuste para KNN es del 96,6% 

#### (vi) [0.5 pt] Aplicando estos modelos ajustados a la base de prueba 

$Random Forest$
  
  Validacion para Random Forest utilizando la base de prueba.

```{r message=FALSE}

# Valores predictivos
predval.rf2 <- predict(model.rf, test)

# Matriz de confusión
table(test$Type, predval.rf2)

```

--------------------------------------------------------
  
  - Se clasifico correctamente 14 vinos de tipo 1

- Se clasifico correctamente 27 vinos de tipo 2

- Se clasifico correctamente 13 vinos de tipo 3

------------------------------------------------------
  
  Porcentaje de clasificación correcta:
  
  ```{r message=FALSE}

mean(test$Type == predval.rf2)

```

Donde existe una clasificación correcta para Random Forest del 100% de los vinos según su tipo.


$Support Vector Machine$
  
  Validacion para Support Vector Machine utilizando la base de prueba.

```{r message=FALSE}

# Valores predictivos
predval.svm <- predict(model.svm, test)

# Matriz de confusión
table(test$Type, predval.svm)

```

--------------------------------------------------------
  
  - Se clasifico correctamente 14 vinos de tipo 1

- Se clasifico correctamente 27 vinos de tipo 2

- Se clasifico correctamente 13 vinos de tipo 3

------------------------------------------------------
  
  Porcentaje de clasificación correcta:
  
  ```{r message=FALSE}

mean(test$Type == predval.svm)

```

Donde existe una clasificación correcta para Support Vector Machine del 98.1% de los vinos según su tipo.


$k-Nearest Neighbors$


  Validacion para KNN utilizando la base de prueba.

```{r message=FALSE}

modelo_KNN_test <- knn(train=test[,-1], test=test[,-1], cl=test[,1], k=7)

# Matriz de confusión
table(test$Type, modelo_KNN_test)

```

--------------------------------------------------------
  
  - Se clasifico correctamente 14 vinos de tipo 1

- Se clasifico correctamente 25 vinos de tipo 2

- Se clasifico correctamente 12 vinos de tipo 3

------------------------------------------------------
  
  Porcentaje de clasificación correcta:
  
  ```{r message=FALSE}

mean(test$Type == modelo_KNN_test)

```

Donde existe una clasificación correcta para KNN del 94.4% de los vinos según su tipo.

