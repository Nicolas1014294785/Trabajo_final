rm(list=ls())

###### Cargamos las librerías necesarias
#Arboles
library(pacman)
p_load(tidyverse, ggplot2, doParallel, rattle, MLmetrics,
      janitor, fastDummies, tidymodels, caret, modelsummary,
      gamlr, class, lmtest, AER)
#Datos
install.packages("faraway")
library(faraway)
#Funciones de limpieza
library(tidyverse)
install.packages("skimr")
library(skimr)
install.packages("DataExplorer")
library(DataExplorer)
install.packages("scales")
library(scales)
install.packages("corrr")
library(corrr)
require("stargazer")
library("psych")
require("gtsummary")
require("caret")
#Modelos
install.packages("glmnet")
library(glmnet)
install.packages("pls")
library(pls)

###### Definimos directorio
getwd()
setwd("C:/Users/Asus/OneDrive - Universidad de los Andes/Documentos/MAESTRIA - MEcA/BIG DATA/Problem_Set2/dataPS2RDS")
dir()


###### Cargamos los datos
train_hogares <- readRDS("train_hogares.Rds")
train_personas <- readRDS("train_personas.Rds")
test_hogares <- readRDS("test_hogares.Rds")
test_personas <- readRDS("test_personas.Rds")


###### Verificamos las variables que tiene cada una de las bases
colnames(train_hogares)
colnames(train_personas)


###### Creo una variable que sea la suma de los ingresos de los individuos 
###### en el hogar a partir de la base de personas.
sum_ingresos<-train_personas %>%
  group_by(id) %>% 
  summarize(Ingtot_hogar=sum(Ingtot,na.rm = TRUE)) 

summary(sum_ingresos)

colSums(is.na(sum_ingresos))

###### Se une la base de datos train_hogares y sum_ingresos
train_hogares <- left_join(train_hogares,sum_ingresos)
colnames(train_hogares)

head(train_hogares[c("id","Ingtotug","Ingtot_hogar")])

###### Creo una variable que sea el porcentaje de ocupados en edad de trabajar 
###### en el hogar a partir de la base de personas.
sum_ocupacion<-train_personas %>%
  group_by(id) %>% 
  summarize(por_ocu=sum(Oc,na.rm = TRUE)/sum(Pet,na.rm = TRUE)) 

summary(sum_ocupacion)

colSums(is.na(sum_ocupacion))

###### Se une la base de datos train_hogares y sum_ocupacion
train_hogares <- left_join(train_hogares,sum_ocupacion)
colnames(train_hogares)


###### Seleccion de variables para el jefe del hogar
datos_jefe <-train_personas %>%  filter(P6050==1) %>%  select (c(P6210,P6100,P6020,id))
train_hogares<-train_hogares %>% left_join(datos_jefe,by="id")



### cambiamos los nombres de las variables para facil identificación
train_hogares <- train_hogares %>% rename(esc_jh = P6210,
                                          seguridad_social_jh = P6100,
                                          hombre_jh = P6020,
                                          num_hab=P5000,
                                          valor_arriendo=P5140,
                                          propiedad=P5090)

### Se cre crea la variable hacin que hace referencia al indice de hacinamiento del hogar
train_hogares <- train_hogares %>% 
  mutate(hacin = Nper/num_hab)
colnames(train_hogares)
summary(train_hogares$hacin)

### Se borra la variable P5010 debido a que se creó una varible similiar llamada hacin,
### que hace referencia al indice de hacinamiento del hogar.

train_hogares <- train_hogares %>%
  select(-"P5010")


##Según la base del DANE un hogar es clasificado pobre si el “Ingreso percápita 
##de la unidad de gasto con imputación de arriendo a propietarios y usufructuarios” 
##es menor a la Linea de pobreza que le corresponde al hogar.
table(train_hogares$Pobre)


##Para testear si esto es cierto comparemos la variable Pobre incluida en la 
##base con una creada por nosotros siguiendo el enfoque del DANE.
train_hogares<- train_hogares %>% mutate(Pobre_hand=ifelse(Ingpcug<Lp,1,0))
table(train_hogares$Pobre,train_hogares$Pobre_hand)

#Corregir las clases de las variables
str(train_hogares)
glimpse(train_hogares)

# Utilizando el diccionario, identificamos variables categóricas
variables_categoricas <- c(
  "id", "Clase", "Dominio", "propiedad", "Pobre", "Indigente", "Depto",
  "esc_jh", "seguridad_social_jh", "hombre_jh", "Pobre_hand")

# Volvemos las variables categoricas a tipo factor
train_hogares <- train_hogares %>%
  mutate_at(.vars = variables_categoricas,
            .funs = factor)


##################################Limpieza de Base Train############################
####################################################################################

cant_na <- colSums(is.na(train_hogares)) #Se guarda la cantidad de missing values por variables
class(cant_na) # se verifica la clase de "cant_na", lo queiro volver data frame para poderlo analizar

## cant_na se vuelve un data frame, enumeramos las variables y ponemos el titulo "variable"
cant_na <- data.frame(cant_na) %>%
  rownames_to_column("variable")

## Se organizan la variables, en orden descendente (desde la que mas tiene valores missing)
cant_na <- cant_na %>%
  arrange(desc(cant_na))

## Creo una columna que se llame porcentaje_na que me indica qel porcentaje de missing que hay en esa variable
cant_na$porcentaje_na <- cant_na$cant_na/nrow(train_hogares)

######## Graficamos el procentaje de missing
p_load("ggplot2")

# Eliminamos del data frame cant_na las variables que no tienen missing
filtro <- cant_na$cant_na != 0
cant_na <- cant_na[filtro,]

#Graficamos las variables
cant_na <- cant_na[1:nrow(cant_na),]
cant_na$variable <- factor(cant_na$variable,
                                levels = cant_na$variable) ## para mostrar el diagrama de barras de menor a mayor

# Graficamos
ggplot(data = cant_na, aes(x = porcentaje_na, y = variable)) +
  geom_bar(stat = "identity", fill = "darkblue") +
  labs(x = "Porcentaje de NAs", y = "Variable",
       title = "Porcentaje de NAs")


###### Eliminaremos las variables que tienen un porcentaje de missing mayor al 10%
filtro2 <- cant_na$porcentaje_na > 0.1
borrar_variables <- cant_na$variable[filtro2]

###### Guardamos una nueva base de datos solo con las variables que tienen un porcentaje de missing
###### menor al 10%
train_hogares_limp <- train_hogares %>%
  select(-borrar_variables)

## Analizando las variables con menos del 10% de valores missing 
tail(cant_na)
train_hogares_limp$seguridad_social_jh
table(train_hogares_limp["seguridad_social_jh"])

## teniendo en cuenta que seguridad_social_jh es una variable categorica, imputaremos por la moda
moda_seguridad_social_jh <- which(table(train_hogares_limp$seguridad_social_jh) ==
                             max(table(train_hogares_limp$seguridad_social_jh)))

## Ponemos el valor de la moda "1" en los NA
filtro3 <- is.na(train_hogares_limp$seguridad_social_jh)
train_hogares_limp$seguridad_social_jh[filtro3] <- moda_seguridad_social_jh
table(train_hogares_limp$seguridad_social_jh)


## Analizando las variables con menos del 10% de valores missing 
tail(cant_na)
train_hogares_limp$por_ocu
table(train_hogares_limp["por_ocu"])

## teniendo en cuenta que por_ocu es una variable numerica, imputaremos por la media
mean_por_ocu <- mean(train_hogares_limp$por_ocu, na.rm = T)

## Ponemos el valor de la media 0.582818
filtro3 <- is.na(train_hogares_limp$por_ocu)
train_hogares_limp$por_ocu[filtro3] <- mean_por_ocu
table(train_hogares_limp$por_ocu)

train_hogares_limp %>%
  is.na() %>%
  sum()

### Se eliminan variables que no son relevantes para determinar la pobreza
str(train_hogares_limp)
glimpse(train_hogares_limp)

train_hogares_limp <- train_hogares_limp %>%
  select(-"Dominio", -"Fex_c", -"Fex_dpto", -"Depto")


############# Análisis descriptivo de train_hogares_limp########################
################################################################################

######## Se observan estadísticas
describe(train_hogares_limp[,c('Pobre', 'Clase', 'num_hab', 'propiedad',
                               'Npersug', 'por_ocu', 'hacin', 'esc_jh',
                               'seguridad_social_jh', 'hombre_jh')])

summary(train_hogares_limp[,c('Pobre', 'Clase', 'num_hab', 'propiedad',
                                'Npersug', 'por_ocu', 'hacin', 'esc_jh',
                                'seguridad_social_jh', 'hombre_jh')])

####### Se construyen tablas con estadisticas que mas nos interesan
tbl1<- train_hogares_limp %>%
  select(c(Pobre, Clase, num_hab, propiedad, Npersug, por_ocu, hacin)) %>%
  tbl_summary(by = Pobre,
              statistic = list(all_continuous() ~ "{mean} ({sd})"))

tbl2<- train_hogares_limp %>%
  select(c(Pobre, esc_jh, seguridad_social_jh, hombre_jh)) %>%
  tbl_summary(by = Pobre,
              statistic = list(all_continuous() ~ "{mean} ({sd})"))

### Graficamos
ggplot(train_hogares_limp) +
  geom_point(aes(x=hacin,y=Ingtot_hogar))

ggplot(train_hogares_limp) +
  geom_point(aes(x=esc_jh,y=Ingtot_hogar))





####################### AHORA LIMPIAMOS LA BASE TEST###############################
####################################################################################
####################################################################################


###### Verificamos las variables que tiene cada una de las bases
colnames(test_hogares)
colnames(test_personas)

###### Creo una variable que sea el porcentaje de ocupados en edad de trabajar 
###### en el hogar a partir de la base de personas.
sum_ocupacion<-test_personas %>%
  group_by(id) %>% 
  summarize(por_ocu=sum(Oc,na.rm = TRUE)/sum(Pet,na.rm = TRUE)) 

summary(sum_ocupacion)

colSums(is.na(sum_ocupacion))

###### Se une la base de datos train_hogares y sum_ocupacion
test_hogares <- left_join(test_hogares,sum_ocupacion)
colnames(test_hogares)


###### Seleccion de variables para el jefe del hogar
datos_jefe <-test_personas %>%  filter(P6050==1) %>%  select (c(P6210,P6100,P6020,id))
test_hogares<-test_hogares %>% left_join(datos_jefe,by="id")



### cambiamos los nombres de las variables para facil identificación
test_hogares <- test_hogares %>% rename(esc_jh = P6210,
                                        seguridad_social_jh = P6100,
                                        hombre_jh = P6020,
                                        num_hab=P5000,
                                        valor_arriendo=P5140,
                                        propiedad=P5090)

### Se crea la variable hacin que hace referencia al indice de hacinamiento del hogar
test_hogares <- test_hogares %>% 
  mutate(hacin = Nper/num_hab)

colnames(test_hogares)
summary(test_hogares$hacin)



### Se borra la variable P5010 debido a que se creó una varible similiar llamada hacin,
### que hace referencia al indice de hacinamiento del hogar.

test_hogares <- test_hogares %>%
  select(-"P5010")


#Corregir las clases de las variables

str(test_hogares)
glimpse(test_hogares)

# Utilizando el diccionario, identificamos variables categóricas
variables_categoricas <- c(
  "id", "Clase", "Dominio", "propiedad", "Depto",
  "esc_jh", "seguridad_social_jh", "hombre_jh")

# Volvemos las variables categoricas a tipo factor
test_hogares <- test_hogares %>%
  mutate_at(.vars = variables_categoricas,
            .funs = factor)





##################################Limpieza de Base Test############################
####################################################################################

cant_na <- colSums(is.na(test_hogares)) #Se guarda la cantidad de missing values por variables
class(cant_na) # se verifica la clase de "cant_na", lo queiro volver data frame para poderlo analizar

## cant_na se vuelve un data frame, enumeramos las variables y ponemos el titulo "variable"
cant_na <- data.frame(cant_na) %>%
  rownames_to_column("variable")

## Se organizan la variables, en orden descendente (desde la que mas tiene valores missing)
cant_na <- cant_na %>%
  arrange(desc(cant_na))

## Creo una columna que se llame porcentaje_na que me indica qel porcentaje de missing que hay en esa variable
cant_na$porcentaje_na <- cant_na$cant_na/nrow(test_hogares)

######## Graficamos el procentaje de missing
p_load("ggplot2")

# Eliminamos del data frame cant_na las variables que no tienen missing
filtro <- cant_na$cant_na != 0
cant_na <- cant_na[filtro,]

#Graficamos las variables
cant_na <- cant_na[1:nrow(cant_na),]
cant_na$variable <- factor(cant_na$variable,
                           levels = cant_na$variable) ## para mostrar el diagrama de barras de menor a mayor

# Graficamos
ggplot(data = cant_na, aes(x = porcentaje_na, y = variable)) +
  geom_bar(stat = "identity", fill = "darkblue") +
  labs(x = "Porcentaje de NAs", y = "Variable",
       title = "Porcentaje de NAs")


###### Eliminaremos las variables que tienen un porcentaje de missing mayor al 10%
filtro2 <- cant_na$porcentaje_na > 0.1
borrar_variables <- cant_na$variable[filtro2]

###### Guardamos una nueva base de datos solo con las variables que tienen un porcentaje de missing
###### menor al 10%
test_hogares_limp <- test_hogares %>%
  select(-borrar_variables)

## Analizando las variables con menos del 10% de valores missing 
tail(cant_na)
test_hogares_limp$seguridad_social_jh
table(test_hogares_limp["seguridad_social_jh"])

## teniendo en cuenta que seguridad_social_jh es una variable categorica, imputaremos por la moda
moda_seguridad_social_jh <- which(table(test_hogares_limp$seguridad_social_jh) ==
                                    max(table(test_hogares_limp$seguridad_social_jh)))

## Ponemos el valor de la moda "1" en los NA
filtro3 <- is.na(test_hogares_limp$seguridad_social_jh)
test_hogares_limp$seguridad_social_jh[filtro3] <- moda_seguridad_social_jh
table(test_hogares_limp$seguridad_social_jh)


## Analizando las variables con menos del 10% de valores missing 
tail(cant_na)
test_hogares_limp$por_ocu
table(test_hogares_limp["por_ocu"])

## teniendo en cuenta que por_ocu es una variable numerica, imputaremos por la media
mean_por_ocu <- mean(test_hogares_limp$por_ocu, na.rm = T)

## Ponemos el valor de la media 0.579636
filtro3 <- is.na(test_hogares_limp$por_ocu)
test_hogares_limp$por_ocu[filtro3] <- mean_por_ocu
table(test_hogares_limp$por_ocu)

test_hogares_limp %>%
  is.na() %>%
  sum()

### Se eliminan variables que no son relevantes para determinar la pobreza
str(test_hogares_limp)

test_hogares_limp <- test_hogares_limp %>%
  select(-"Dominio", -"Fex_c", -"Fex_dpto", -"Depto")


############# Análisis descriptivo de test_hogares_limp########################
################################################################################

######## Se observan estadísticas
describe(test_hogares_limp[,c('Clase', 'num_hab', 'propiedad',
                               'Npersug', 'por_ocu', 'hacin', 'esc_jh',
                               'seguridad_social_jh', 'hombre_jh')])

summary(test_hogares_limp[,c('Clase', 'num_hab', 'propiedad',
                              'Npersug', 'por_ocu', 'hacin', 'esc_jh',
                              'seguridad_social_jh', 'hombre_jh')])

####### Se construyen tablas con estadisticas que mas nos interesan
tbl1<- test_hogares_limp %>%
  select(c(hombre_jh, Clase, num_hab, propiedad, Npersug, por_ocu, hacin)) %>%
  tbl_summary(by = hombre_jh,
              statistic = list(all_continuous() ~ "{mean} ({sd})"))

tbl2<- test_hogares_limp %>%
  select(c(esc_jh, seguridad_social_jh, hombre_jh)) %>%
  tbl_summary(by = hombre_jh,
              statistic = list(all_continuous() ~ "{mean} ({sd})"))




####################### Modelos de Clasificación ###############################
################################################################################
################################################################################
##################################################################################
##################################################################################
##################################################################################


## Partimos la muestra train en tres partes
train_hogares_limp$Pobre<- factor((train_hogares_limp$Pobre), 
                                  levels = c(0, 1),
                                  labels = c("No", "si"))

set.seed(156)
split1 <- createDataPartition(train_hogares_limp$Pobre, p = .7)[[1]]
length(split1)

head(split1, n=20)

other <- train_hogares_limp[-split1,]
training <- train_hogares_limp[ split1,]

set.seed(934)
split2 <- createDataPartition(other$Pobre, p = 1/3)[[1]]
length(split2)
evaluation <- other[ split2,]
testing <- other[-split2,]

dim(training)
dim(testing)
dim(evaluation)

####### Usamos Caret para poder comparar modelos (defaultSummary)
ctrl_def <- trainControl(method = "cv",
                         number = 5,
                         summaryFunction = defaultSummary,
                         classProbs = TRUE,
                         verbose=FALSE,
                         savePredictions = T)

#### Fijamos la semilla para correr varios modelos con la misma particion
#logit
set.seed(1410)
mylogit_caret_def <- train(Pobre~Clase+num_hab+propiedad+Npersug+por_ocu+esc_jh+seguridad_social_jh+hombre_jh+hacin,
                           data = training,
                           method = "glm", #Para logit
                           trControl = ctrl_def,
                           family = "binomial",
                           preProcess = c("center", "scale")
)


####### Usamos Caret para poder comparar modelos (fiveStats)

fiveStats <- function(...) c(twoClassSummary(...), defaultSummary(...))
ctrl<- trainControl(method = "cv",
                    number = 5,
                    summaryFunction = fiveStats,
                    classProbs = TRUE,
                    verbose=FALSE,
                    savePredictions = T)
####logit
set.seed(1410)
mylogit_caret <- train(
  Pobre~Clase+num_hab+propiedad+Npersug+por_ocu+esc_jh+seguridad_social_jh+hombre_jh+hacin,
  data = training,
  method = "glm", #for logit
  trControl = ctrl,
  family = "binomial",
  preProcess = c("center", "scale")
)

##### Con el fin de maximizar la capacidad predictiva del modelo (lasso para mejorar la predicción)
##### SENSIBILIDAD (quiero maximizar la sensibilidad porque quiero evitar lo falsos negativos)
lambda_grid <- 10^seq(-4, 0.01, length = 100)
lambda_grid

set.seed(1410)
mylogit_lasso_sens <- train(
  Pobre~Clase+num_hab+propiedad+Npersug+por_ocu+esc_jh+seguridad_social_jh+hombre_jh+hacin,
  data = training,
  method = "glmnet",
  trControl = ctrl,
  family = "binomial",
  metric = "Sens",
  tuneGrid = expand.grid(alpha = 0,lambda=lambda_grid),
  preProcess = c("center", "scale")
)


##### ROC para el Cutoff
set.seed(1410)
mylogit_lasso_roc <- train(
  Pobre~Clase+num_hab+propiedad+Npersug+por_ocu+esc_jh+seguridad_social_jh+hombre_jh+hacin,
  data = training,
  method = "glmnet",
  trControl = ctrl,
  family = "binomial",
  metric = "ROC",
  tuneGrid = expand.grid(alpha = 0,lambda=lambda_grid),
  preProcess = c("center", "scale")
)

##### Cutoffs
evalResults <- data.frame(Pobre = evaluation$Pobre)
evalResults$Roc <- predict(mylogit_lasso_roc,
                           newdata = evaluation,
                           type = "prob")[,1]

library(pROC)
rfROC <- roc(evalResults$Pobre, evalResults$Roc, levels = rev(levels(evalResults$Pobre)))

rfThresh <- coords(rfROC, x = "best", best.method = "closest.topleft")
rfThresh

evalResults<-evalResults %>% mutate(hat_def_05=ifelse(evalResults$Roc>0.5,"Si","No"),
                                    hat_def_rfThresh=ifelse(evalResults$Roc>rfThresh$threshold,"Si","No"))

with(evalResults,table(Pobre,hat_def_05))
with(evalResults,table(Pobre,hat_def_rfThresh))



### Con el fin de equilibrar la muestra
### realizamos un upsampled
set.seed(1103)
upSampledTrain <- upSample(x = training,
                           y = training$Pobre,
                           ## keep the class variable name the same:
                           yname = "Pobre")
dim(training)
dim(upSampledTrain)
table(upSampledTrain$Pobre)# queda equilibrada la muestra

##### Modelo con upsample
##### SENSIBILIDAD (quiero maximizar la sensibilidad porque quiero evitar lo falsos negativos)
set.seed(1410)
mylogit_lasso_upsample <- train(
  Pobre~Clase+num_hab+propiedad+Npersug+por_ocu+esc_jh+seguridad_social_jh+hombre_jh+hacin,
  data = upSampledTrain,
  method = "glmnet",
  trControl = ctrl,
  family = "binomial",
  metric = "Sens",
  tuneGrid = expand.grid(alpha = 0,lambda=lambda_grid),
  preProcess = c("center", "scale")
)


### Con el fin de equilibrar la muestra
### realizamos un downsampled
set.seed(1103)
downSampledTrain <- downSample(x = training,
                           y = training$Pobre,
                           ## keep the class variable name the same:
                           yname = "Pobre")
dim(training)
dim(downSampledTrain)
table(downSampledTrain$Pobre)# queda equilibrada la muestra

##### Modelo con downsample
##### SENSIBILIDAD (quiero maximizar la sensibilidad porque quiero evitar lo falsos negativos)
set.seed(1410)
mylogit_lasso_downsample <- train(
  Pobre~Clase+num_hab+propiedad+Npersug+por_ocu+esc_jh+seguridad_social_jh+hombre_jh+hacin,
  data = downSampledTrain,
  method = "glmnet",
  trControl = ctrl,
  family = "binomial",
  metric = "Sens",
  tuneGrid = expand.grid(alpha = 0,lambda=lambda_grid),
  preProcess = c("center", "scale")
)




##################################comparación###################################
################################################################################
################################################################################

testResults <- data.frame(Pobre = testing$Pobre)
testResults$logit<- predict(mylogit_caret,
                            newdata = testing,
                            type = "prob")[,1]
testResults$lasso<- predict(mylogit_lasso_roc,
                            newdata = testing,
                            type = "prob")[,1]
testResults$lasso_Sens<- predict(mylogit_lasso_sens,
                            newdata = testing,
                            type = "prob")[,1]
testResults$lasso_thresh<- predict(mylogit_lasso_roc,
                                   newdata = testing,
                                   type = "prob")[,1]
testResults$lasso_upsample<- predict(mylogit_lasso_upsample,
                                     newdata = testing,
                                     type = "prob")[,1]
testResults$lasso_downsample<- predict(mylogit_lasso_downsample,
                                               newdata = testing,
                                               type = "prob")[,1]
testResults<-testResults %>%
  mutate(logit=ifelse(logit>0.5,"Si","No"),
         lasso=ifelse(lasso>0.5,"Si","No"),
         lasso_Sens=ifelse(lasso_Sens>0.5,"Si","No"),
         lasso_thresh=ifelse(lasso_thresh>rfThresh$threshold,"Si","No"),
         lasso_upsample=ifelse(lasso_upsample>0.5,"Si","No"),
         lasso_downsample=ifelse(lasso_downsample>0.5,"Si","No")
  )

with(testResults,table(Pobre,logit))
with(testResults,table(Pobre,lasso))
with(testResults,table(Pobre,lasso_Sens))
with(testResults,table(Pobre,lasso_thresh))
with(testResults,table(Pobre,lasso_upsample))
with(testResults,table(Pobre,lasso_downsample))

#### se realiza el cálculo de la sensibilidad
sen_logit <- 3877/(3877+24939)
sen_lasso <- 4166/(4166+25233)
sen_lasso_Sens <- 6588/(6588+26385)
sen_lasso_thresh <- 1386/(1386+20011)
sen_lasso_upsample <- 1469/(1469+20292)
sen_lasso_downsample <- 1477/(1477+20318)

#### De acuerdo con la compración procedemos a testear en la muestra test_hogares_limp
#### Seleccionamos el modelo lasso_sens
Results <- data.frame(id = test_hogares_limp$id)
Results$lasso_Sens<- predict(mylogit_lasso_sens,
                             newdata = test_hogares_limp,
                             type = "prob")[,1]
Results <- Results %>%
  mutate(Pobre=ifelse(lasso_Sens>0.5,"Si","No"))

with(Results,table(Pobre))
prop.table(table(Results$Pobre))

### cambiamos los nombres de las variables para facil identificación
Results$Pobre <- factor((Results$Pobre),
                        levels = c("No", "Si"),
                        labels = c(0, 1))

Results <- Results %>% rename(Pobre_classification = Pobre)

Results <- Results %>%
  select(-"lasso_Sens")

write.csv(Results, "predictions_dueñas_garcia_c9_r9.csv")









####################### Modelos de Regresión para predicción de ingreso ###############################
#######################################################################################################
#######################################################################################################

###########################################################################################
##################################################MCO######################################
x_train<-select(train_hogares_limp, Clase, num_hab, propiedad, Npersug, por_ocu, esc_jh,
                seguridad_social_jh, hombre_jh, hacin)

y_train<-train_hogares_limp$Ingtot_hogar

x_test<-select(test_hogares_limp, Clase, num_hab, propiedad, Npersug, por_ocu, esc_jh,
               seguridad_social_jh, hombre_jh, hacin)


modelo_mco<-lm(Ingtot_hogar ~ Clase + num_hab + propiedad + Npersug + por_ocu+ esc_jh+
                 seguridad_social_jh+ hombre_jh+ hacin, data=train_hogares_limp)
summary(modelo_mco)

betas_base<-modelo_mco$coefficients %>% enframe(name="variable", value="coeficiente")

ggplot(betas_base) + aes(x=variable, y=coeficiente)+
  geom_col()

#Calcular el MSE dentro de muestra
y_hat_in<- predict(modelo_mco, data.matrix(x_train))
y_real_in <-train_hogares_limp$Ingtot_hogar
mse_in_mco<-mean((y_hat_in-y_real_in)^2)
print(mse_in_mco) ### MSE=4.546605e+12

##################################################################################
############################################Ridge##################################


glmnet(x= x_train, 
       y=y_train, 
       alpha=0, 
       nlambda=100, 
       standardize=T)

modelo_ridge<-glmnet(x=x_train, 
                     y=y_train,
                     alpha=0,
                     nlambda=100, 
                     standardize=TRUE)

regularizacion_l2<-modelo_ridge$beta %>% 
  as.matrix() %>%
  t() %>%
  as_tibble() %>%
  mutate(lambda=modelo_ridge$lambda)


regularizacion_l2_long<-regularizacion_l2 %>%
  pivot_longer(
    cols=!lambda, 
    names_to="variable", 
    values_to= "coeficientes"
  )

ggplot(regularizacion_l2_long, aes(x=lambda, y=coeficientes, 
                                   color=variable))+
  geom_line() +
  scale_x_log10() +
  theme_classic( )+
  theme(legend.position="none")

#Cross-validation
set.seed(123)
cv_error <- cv.glmnet(
  x = data.matrix(x_train),
  y = y_train, 
  alpha = 0,
  nfolds = 10, 
  type.measure = "mse", 
  standardize = TRUE, 
  nlambda=100
)

plot(cv_error)
cv_error$lambda.min
cv_error$lambda.1se

modelo_ridge_opt<-glmnet(x=x_train, 
                         y=y_train,
                         alpha=0,
                         lambda=cv_error$lamda.1se, 
                         standardize=TRUE)

betas_base <- modelo_ridge_opt$coefficients %>% enframe(name="variables", value="coeficiente")

coef_ridge<-coef(modelo_ridge_opt) %>%
  as.matrix() %>%
  as.tibble(rownames="variables") %>%
  rename (coeficiente=s0)%>%
  filter(variables !="(Intercept)")

ggplot(coef_ridge, aes(x=variables, y=coeficiente))+
  geom_col()+
  theme_classic()+
  theme(axis.text.x=element_text(size=6, angle=45))

y_hat_in<-predict(modelo_ridge_opt, data.matrix(x_train))
y_hat_out<-predict(modelo_ridge_opt, data.matrix(x_test))
mse_in_ridge<-mean((y_hat_in-y_real_in)^2)
print(mse_in_ridge)#MSE 5.561188e+12
        
########################################################
#######################Lasso############################
modelo_lasso<-glmnet(x=x_train, 
                     y=y_train,
                     alpha=1,#Lasso
                     nlambda=100,
                     standardize=TRUE)
        
regularizacion_l1<-modelo_lasso$beta %>%
  as.matrix() %>%
  t() %>%
  as_tibble() %>%
  mutate(lambda=modelo_lasso$lambda)
        
regularizacion_l1_long<-regularizacion_l1 %>%
  pivot_longer(cols=!lambda, 
               names_to="variable", 
               values_to= "coeficientes")
        
ggplot(regularizacion_l1_long, aes(x=lambda, y=coeficientes, color=variable))+
          geom_line()+
          scale_x_log10()+
          theme_classic()+
          theme(legend.position="none")
        
########Cross validation
set.seed(123)
cv_error<-cv.glmnet(
  x=data.matrix(x_train),
  y=y_train, 
  alpha=1,
  nfolds=10, 
  type.measure="mse", 
  standardize=TRUE, 
  nlambda=100)
        
plot(cv_error)
cv_error$lambda.1se
        
modelo_lasso_opt<-glmnet(x=x_train, 
                         y=y_train,
                         alpha=1,#Lasso
                         lambda=cv_error$lambda.1se, 
                         standardize=TRUE)
        
betas_base <- modelo_lasso_opt$coefficients %>% enframe(name="variables", value="coeficiente")

coef_lasso<-coef(modelo_lasso_opt) %>%
  as.matrix() %>%
  as.tibble(rownames="variables") %>%
  rename (coeficiente=s0) %>% 
  filter(variables!="(Intercept)")

ggplot(coef_lasso, aes(x=variables, y=coeficiente))+
  geom_col()+
  theme_classic()+
  theme(axis.text.x=element_text(size=6, angle=45))
        
y_hat_in<-predict(modelo_lasso_opt, data.matrix(x_train))
y_hat_out<-predict(modelo_lasso_opt, data.matrix(x_test))
mse_in_lasso<-mean((y_hat_in-y_real_in)^2)
print(mse_in_lasso)## MSE=4.89816e+12

###########################################################################################
#####################MCO SIN LAS TRES VARIABLES DE LASSO###################################
###### los valores de cero de ingtot los cambio a 1 para poder calcular el log
filtro4 <- train_hogares_limp$Ingtot_hogar == 0
train_hogares_limp$Ingtot_hogar[filtro4] <- 1
table(train_hogares_limp$Ingtot_hogar)

modelo_mco_6<-lm(Ingtot_hogar ~ num_hab + propiedad + Npersug + por_ocu+ esc_jh+
                 seguridad_social_jh, data=train_hogares_limp)
summary(modelo_mco_6)

betas_base<-modelo_mco_6$coefficients %>% enframe(name="variable", value="coeficiente")

ggplot(betas_base) + aes(x=variable, y=coeficiente)+
  geom_col()

#Calcular el MSE dentro de muestra
y_hat_in<-predict(modelo_mco_6, x_train)
y_hat_out<-predict(modelo_mco_6, data.matrix(x_test))
mse_in_mco_6<-mean((y_hat_in-y_real_in)^2)
print(mse_in_mco_6)## MSE=4.585282e+12

###########################################################################################
#####################MCO SIN LAS TRES VARIABLES DE LASSO###################################

x_train2<-select(train_hogares_limp, num_hab, propiedad, Npersug, por_ocu, esc_jh,
                seguridad_social_jh)

y_train<-train_hogares_limp$Ingtot_hogar

x_test<-select(test_hogares_limp, Clase, num_hab, propiedad, Npersug, por_ocu, esc_jh,
               seguridad_social_jh, hombre_jh, hacin)
modelo_lasso2<-glmnet(x=x_train2, 
                     y=y_train,
                     alpha=1,#Lasso
                     nlambda=100,
                     standardize=TRUE)

regularizacion_l1<-modelo_lasso2$beta %>%
  as.matrix() %>%
  t() %>%
  as_tibble() %>%
  mutate(lambda=modelo_lasso2$lambda)

regularizacion_l1_long<-regularizacion_l1 %>%
  pivot_longer(cols=!lambda, 
               names_to="variable", 
               values_to= "coeficientes")

ggplot(regularizacion_l1_long, aes(x=lambda, y=coeficientes, color=variable))+
  geom_line()+
  scale_x_log10()+
  theme_classic()+
  theme(legend.position="none")

########Cross validation
set.seed(123)
cv_error<-cv.glmnet(
  x=data.matrix(x_train2),
  y=y_train, 
  alpha=1,
  nfolds=10, 
  type.measure="mse", 
  standardize=TRUE, 
  nlambda=100)

plot(cv_error)
cv_error$lambda.1se

modelo_lasso_opt2<-glmnet(x=x_train2, 
                         y=y_train,
                         alpha=1,#Lasso
                         lambda=cv_error$lambda.1se, 
                         standardize=TRUE)

betas_base <- modelo_lasso_opt2$coefficients %>% enframe(name="variables", value="coeficiente")

coef_lasso<-coef(modelo_lasso_opt2) %>%
  as.matrix() %>%
  as.tibble(rownames="variables") %>%
  rename (coeficiente=s0) %>% 
  filter(variables!="(Intercept)")

ggplot(coef_lasso, aes(x=variables, y=coeficiente))+
  geom_col()+
  theme_classic()+
  theme(axis.text.x=element_text(size=6, angle=45))

y_hat_in<-predict(modelo_lasso_opt2, data.matrix(x_train2))
y_hat_out<-predict(modelo_lasso_opt2, data.matrix(x_test))
mse_in_lasso2<-mean((y_hat_in-y_real_in)^2)
print(mse_in_lasso2)## MSE=4.982394e+12


print(mse_in_mco)
print(mse_in_ridge)
print(mse_in_lasso)
print(mse_in_mco_6)
print(mse_in_lasso2)


#### De acuerdo con la compración procedemos a testear en la muestra test_hogares_limp
#### Seleccionamos el modelo mco
### cambiamos los nombres de las variables para facil identificación
Results$modelo_mco6<- predict(modelo_mco_6,
                             newdata = test_hogares_limp,
                             type = "terms")[,1]

test_hogares_limp_prueba <- left_join(test_hogares_limp,Results)
test_hogares_limp_prueba <- test_hogares_limp_prueba %>%
  mutate(Ingtot=abs(modelo_mco6))

test_hogares_limp_prueba<-test_hogares_limp_prueba %>%
  mutate(Pobre_inncome=ifelse(Ingtot>Lp, 0, 1))

Results <- test_hogares_limp_prueba %>%
  select(id, Pobre_classification,Pobre_inncome)

write.csv(Results, "predictions_dueñas_garcia_c9_r6.csv")


