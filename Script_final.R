rm(list=ls())

###### Cargamos las librerías necesarias
#Arboles
library(pacman)
p_load(tidyverse, ggplot2, doParallel, rattle, MLmetrics,
      janitor, fastDummies, tidymodels, caret, modelsummary,
      gamlr, class, lmtest, AER, faraway, skimr, DataExplorer,
      scales, corrr, stargazer, psych, gtsummary, glmnet, pls, 
      readxl, fastAdaboost, xgboost)

###### Definimos directorio
getwd()
setwd("C:/Users/Asus/OneDrive - Universidad de los Andes/Documentos/MAESTRIA - MEcA/BIG DATA/Trabajo_final/Stores")
dir()


###### Cargamos los datos
ha_coca <- read_excel("ha_coca.xlsx")
lideres <- read_excel("PrensaFIP_lideres.xlsx") 
mdm2020 <- read_excel("MDM_2020.xlsx") 
map<-read_excel("map.xlsx") 
deforestacion<-read_excel("deforestacion.xls") 

#Pegamos las bases por código de municipio y año
# merge two data frames by ID and Country
base <- left_join(lideres, ha_coca, by=c("anio","cod_mpio"))#Merge coca y líderes
base_1 <- left_join(base, mdm2020, by=c("cod_mpio"))#Merge líderes y mdm
base_final<-left_join(base_1, map, by=c("anio","cod_mpio"))#Merge líderes y víctimas minas
base_2<-left_join(base_final, deforestacion, by=c("anio","cod_mpio"))#Merge líderes y víctimas minas

df <- base_2[ -c(2:8, 10, 11, 13, 16, 17, 19, 20, 22:27, 29) ]#Eliminar variables irrelevantes

###### Verificamos las variables que tiene cada una de las bases
colnames(df)

#####Eliminamos observaciones sin municipio 

df<-subset(df,cod_mpio != 0) 

################################################################################
#############Creación de variables por medio del texto##########################
##### Transformamos todas las letras de la descripción en minuscula 
#Crear varibale dummy de homicidio 
df = df %>% 
  mutate(tipo_agresion=str_to_lower(df$tipo_agresion),
         homicidio = ifelse(df$tipo_agresion =="homicidio", 1, 0))

#Crear varibale dummy de género
df = df %>% 
  mutate(genero = ifelse(df$genero =="Mujer", 1, 0))

##############################Missing values######################################

cant_na <- colSums(is.na(df)) #Se guarda la cantidad de missing values por variables
class(cant_na) # se verifica la clase de "cant_na", lo queiro volver data frame para poderlo analizar

## cant_na se vuelve un data frame, enumeramos las variables y ponemos el titulo "variable"
cant_na <- data.frame(cant_na) %>%
  rownames_to_column("variable")

## Se organizan la variables, en orden descendente (desde la que mas tiene valores missing)
cant_na <- cant_na %>%
  arrange(desc(cant_na))


###### Cambiamos los nombres de las variables

df <- df %>% rename(movilizacion_recursos = "Movilización de recursos",
                                  ejecucion_recursos = "Ejecución de recursos",
                                  gobierno_abierto = "Gobierno abierto",
                                  ordenamiento_territorial = "Ordenamiento erritorial",
                                  gestion_2020 = "Gestión 2020",
                                  educacion = "Educación",
                                  salud = "Salud",
                                  servicios_publicos = "Servicios públicos",
                                  seguridad_convivencia = "Seguridad y convivencia",
                                  resultados_2020 = "Resultados 2020",
                                  ajuste_resultados = "Ajuste por resultados",
                                  MDM_2020 = "MDM 2020")


##################################Limpieza de Base #################################
####################################################################################

###### Imputamos valores de variable "map_muse"
filtro <- is.na(df$map_muse)
df$map_muse[filtro] <- 0
table(is.na(df$map_muse))


###### Imputamos valores de variable "ha_coca"
filtro <- is.na(df$ha_coca)
df$ha_coca[filtro] <- 0
table(is.na(df$ha_coca))


###Impuetamos los NA de tc_loss_ha por la mediana 
median_tc_loss_ha <- median(df$tc_loss_ha,na.rm=T)

## Ponemos el valor de la mediana
filtro <- is.na(df$tc_loss_ha)
df$tc_loss_ha[filtro] <- median_tc_loss_ha
table(is.na(df$tc_loss_ha))


###### Imputamos valores de variable "PNIS"
filtro <- is.na(df$PNIS)
df$PNIS[filtro] <- 0
table(is.na(df$PNIS))

###### Imputamos valores de variable "PDET"
filtro <- is.na(df$PDET)
df$PDET[filtro] <- 0
table(is.na(df$PDET))



###Impuetamos los NA de MDM_2020 por la mean 
mean_MDM_2020 <- mean(df$MDM_2020,na.rm=T)

## Ponemos el valor de la mediana
filtro <- is.na(df$MDM_2020)
df$MDM_2020[filtro] <- mean_MDM_2020
table(is.na(df$MDM_2020))


###Impuetamos los NA de seguridad_convivencia por la mean 
mean_seguridad_convivencia <- mean(df$seguridad_convivencia,na.rm=T)

## Ponemos el valor de la mediana
filtro <- is.na(df$seguridad_convivencia)
df$seguridad_convivencia[filtro] <- mean_seguridad_convivencia
table(is.na(df$seguridad_convivencia))




#Corregir las clases de las variables
str(df)
glimpse(df)

# Utilizando el diccionario, identificamos variables categóricas
variables_categoricas <- c(
  "actor", "genero", "PDET", "PNIS", "homicidio")

# Volvemos las variables categoricas a tipo factor
df <- df %>%
  mutate_at(.vars = variables_categoricas,
            .funs = factor)


############# Análisis descriptivo de train_hogares_limp########################
################################################################################

######## Se observan estadísticas
describe(df[,c('homicidio', 'actor', 'genero', 'PDET',
               'PNIS', 'ha_coca', 'seguridad_convivencia', 'MDM_2020',
               'map_muse', 'tc_loss_ha')])

summary(df[,c('homicidio', 'actor', 'genero', 'PDET',
              'PNIS', 'ha_coca', 'seguridad_convivencia', 'MDM_2020',
              'map_muse', 'tc_loss_ha')])

####### Se construyen tablas con estadisticas que mas nos interesan
tbl1<- df %>%
  select(c(homicidio, genero, PDET, PNIS, ha_coca, seguridad_convivencia, MDM_2020,
           map_muse, tc_loss_ha)) %>%
  tbl_summary(by = homicidio,
              statistic = list(all_continuous() ~ "{mean} ({sd})"))


### Graficamos
ggplot(df) +
  geom_point(aes(x=tc_loss_ha,y=ha_coca))


####################### Modelos de Clasificación ###############################
################################################################################
################################################################################
##################################################################################
##################################################################################
##################################################################################


## Partimos la muestra train en tres partes
df$homicidio<- factor((df$homicidio), 
                      levels = c(0, 1),
                      labels = c("No", "si"))

set.seed(156)
split1 <- createDataPartition(df$homicidio, p = .7)[[1]]
length(split1)

head(split1, n=20)

other <- df[-split1,]
training <- df[ split1,]

set.seed(934)
split2 <- createDataPartition(other$homicidio, p = 1/3)[[1]]
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
mylogit_caret_def <- train(homicidio~genero+PDET+PNIS+ha_coca+seguridad_convivencia+MDM_2020+map_muse+tc_loss_ha,
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
  homicidio~genero+PDET+PNIS+ha_coca+seguridad_convivencia+MDM_2020+map_muse+tc_loss_ha,
  data = training,
  method = "glm", #for logit
  trControl = ctrl,
  family = "binomial",
  preProcess = c("center", "scale")
)

##### Con el fin de maximizar la capacidad predictiva del modelo (lasso para mejorar la predicción)
##### SENSIBILIDAD (quiero maximizar la sensibilidad para evitar lo falsos negativos)
lambda_grid <- 10^seq(-4, 0.01, length = 100)
lambda_grid

set.seed(1410)
mylogit_lasso_sens <- train(
  homicidio~genero+PDET+PNIS+ha_coca+seguridad_convivencia+MDM_2020+map_muse+tc_loss_ha,
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
  homicidio~genero+PDET+PNIS+ha_coca+seguridad_convivencia+MDM_2020+map_muse+tc_loss_ha,
  data = training,
  method = "glmnet",
  trControl = ctrl,
  family = "binomial",
  metric = "ROC",
  tuneGrid = expand.grid(alpha = 0,lambda=lambda_grid),
  preProcess = c("center", "scale")
)

##### Cutoffs
evalResults <- data.frame(homicidio = evaluation$homicidio)
evalResults$Roc <- predict(mylogit_lasso_roc,
                           newdata = evaluation,
                           type = "prob")[,1]

library(pROC)
rfROC <- roc(evalResults$homicidio, evalResults$Roc, levels = rev(levels(evalResults$homicidio)))

rfThresh <- coords(rfROC, x = "best", best.method = "closest.topleft")
rfThresh

evalResults<-evalResults %>% mutate(hat_def_05=ifelse(evalResults$Roc>0.5,"Si","No"),
                                    hat_def_rfThresh=ifelse(evalResults$Roc>rfThresh$threshold,"Si","No"))

with(evalResults,table(homicidio,hat_def_05))
with(evalResults,table(homicidio,hat_def_rfThresh))



### Con el fin de equilibrar la muestra
### realizamos un upsampled
set.seed(1103)
upSampledTrain <- upSample(x = training,
                           y = training$homicidio,
                           ## keep the class variable name the same:
                           yname = "homicidio")
dim(training)
dim(upSampledTrain)
table(upSampledTrain$homicidio)# queda equilibrada la muestra

##### Modelo con upsample
##### SENSIBILIDAD (quiero maximizar la sensibilidad porque quiero evitar lo falsos negativos)
set.seed(1410)
mylogit_lasso_upsample <- train(
  homicidio~genero+PDET+PNIS+ha_coca+seguridad_convivencia+MDM_2020+map_muse+tc_loss_ha,
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
                           y = training$homicidio,
                           ## keep the class variable name the same:
                           yname = "homicidio")
dim(training)
dim(downSampledTrain)
table(downSampledTrain$homicidio)# queda equilibrada la muestra

##### Modelo con downsample
##### SENSIBILIDAD (quiero maximizar la sensibilidad porque quiero evitar lo falsos negativos)
set.seed(1410)
mylogit_lasso_downsample <- train(
  homicidio~genero+PDET+PNIS+ha_coca+seguridad_convivencia+MDM_2020+map_muse+tc_loss_ha,
  data = downSampledTrain,
  method = "glmnet",
  trControl = ctrl,
  family = "binomial",
  metric = "Sens",
  tuneGrid = expand.grid(alpha = 0,lambda=lambda_grid),
  preProcess = c("center", "scale")
)

###############################################################
#######################Random Forest############################
set.seed(1410)
forest<-train(
  homicidio~genero+PDET+PNIS+ha_coca+seguridad_convivencia+MDM_2020+map_muse+tc_loss_ha,
  data=training,
  method="rf",
  trControl=ctrl, 
  family="binomial",
  metric="Sens" 
)


################################################################################
#########################Adaboost###############################################
set.seed(1410)
adaboost <- train(
  homicidio~genero+PDET+PNIS+ha_coca+seguridad_convivencia+MDM_2020+map_muse+tc_loss_ha,
  data = training,
  method = "adaboost",
  trControl = ctrl,
  family = "binomial",
  metric = "Sens"
)

###############################################################################
########################XGboost#################################################
grid_default <- expand.grid(nrounds = c(100,1000),
                            max_depth = c(4,6,8),
                            eta = c(0.01,0.3,0.5),
                            gamma = c(0,1),
                            min_child_weight = c(10, 25,50),
                            colsample_bytree = c(0.7),
                            subsample = c(0.6))
set.seed(1410)
xgboost <- train(
  homicidio~genero+PDET+PNIS+ha_coca+seguridad_convivencia+MDM_2020+map_muse+tc_loss_ha,
  data = training,
  method = "xgbTree",
  trControl = ctrl,
  metric = "Sens",
  tuneGrid = grid_default,
  preProcess = c("center", "scale")
)



##################################comparación###################################
################################################################################
################################################################################

testResults <- data.frame(id=testing$ID,
                          homicidio = testing$homicidio)
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
testResults$forest<- predict(forest,
                             newdata = testing,
                             type = "prob")[,1]
testResults$adaboost<- predict(adaboost,
                             newdata = testing,
                             type = "prob")[,1]
testResults$xgboost<- predict(xgboost,
                               newdata = testing,
                               type = "prob")[,1]

testResults<-testResults %>%
  mutate(logit=ifelse(logit>0.5,"Si","No"),
         lasso=ifelse(lasso>0.5,"Si","No"),
         lasso_Sens=ifelse(lasso_Sens>0.5,"Si","No"),
         lasso_thresh=ifelse(lasso_thresh>rfThresh$threshold,"Si","No"),
         lasso_upsample=ifelse(lasso_upsample>0.5,"Si","No"),
         lasso_downsample=ifelse(lasso_downsample>0.5,"Si","No"),
         forest=ifelse(forest>0.5,"Si","No"),
         adaboost=ifelse(adaboost>0.5,"Si","No"),
         xgboost=ifelse(xgboost>0.5,"Si","No")
  )

with(testResults,table(homicidio,logit))
with(testResults,table(homicidio,lasso))
with(testResults,table(homicidio,lasso_Sens))
with(testResults,table(homicidio,lasso_thresh))
with(testResults,table(homicidio,lasso_upsample))
with(testResults,table(homicidio,lasso_downsample))
with(testResults,table(homicidio,forest))
with(testResults,table(homicidio,adaboost))
with(testResults,table(homicidio,xgboost))

#### se realiza el cálculo de la sensibilidad
sen_logit <- 191/(191+403)
sen_lasso <- 201/(201+411)
sen_lasso_Sens <- 230/(230+435)
sen_lasso_thresh <- 83/(83+236)
sen_lasso_upsample <- 104/(104+271)
sen_lasso_downsample <- 107/(107+270)
sen_forest <- 124/(124+396)
sen_adaboost <- 118/(118+316)
sen_xgboost <- 213/(213+422)

write.csv(testResults, "predictions_dueñas_garcia_trabajo_final.csv")



