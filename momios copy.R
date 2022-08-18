
################################Librerías#######################################
library(fbRanks)
library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)
library(igraph)

#######################Directorio de trabajo####################################

setwd("~/Documents/BEDU/FASE 2/Proyecto")   #Difiere dependiendo del usuario

########################Descarga de archivos####################################

u1011 <- "https://www.football-data.co.uk/mmz4281/1011/SP1.csv"   #Se guarda cada URL en una variable diferente
u1112 <- "https://www.football-data.co.uk/mmz4281/1112/SP1.csv"
u1213 <- "https://www.football-data.co.uk/mmz4281/1213/SP1.csv"
u1314 <- "https://www.football-data.co.uk/mmz4281/1314/SP1.csv"
u1415 <- "https://www.football-data.co.uk/mmz4281/1415/SP1.csv"
u1516 <- "https://www.football-data.co.uk/mmz4281/1516/SP1.csv"
u1617 <- "https://www.football-data.co.uk/mmz4281/1617/SP1.csv"
u1718 <- "https://www.football-data.co.uk/mmz4281/1718/SP1.csv"
u1819 <- "https://www.football-data.co.uk/mmz4281/1819/SP1.csv"
u1920 <- "https://www.football-data.co.uk/mmz4281/1920/SP1.csv"

########################Lectura de datos########################################

lista <- list(u1011,u1112,u1213,u1314,u1415,u1516,u1617,u1718,u1819)  #Se crea una lista que incluye las direcciones
lista<-lapply(lista, read.csv)                                        #Usando la función lapply, se leen todas las URLs sin necesidad de guardar los datos en un archivo
e1920 <- read.csv(u1920)                                              #Dado que la última URL tiene un número de columnas diferente, se guarda por separado


#########################Procesamiento de datos#################################

lista<-lapply(lista,select,Date:FTAG, BbMx.2.5:BbAv.2.5.1)            #Selecciona las columnas a usar para la lista que contiene los datos del 2011-2019
e1920<-select(e1920,Date:FTAG, Max.2.5:Avg.2.5.1)                     #Selecciona las columnas a usar para los datos del 2019-2020
e1920 <- select(e1920, -Time)                                         #Se elimina la columna de Tiempo en los datos del 2019-2020 dado que no se encuentra en los demás sets de datos

#############################Formato de datos###################################

lista<- lapply(lista,mutate, Date=as.Date(Date,format = "%d/%m/%y"))  #Aplica un formato homogéneo de tipo fecha al set de listas
e1920<- mutate(e1920,Date=as.Date(Date,format = "%d/%m/%y"))          #Aplica un formato homogéneo de tipo fecha al data set del 2019-2020

################################Unificación#####################################

lista[c(1,2,3,4)]=NULL                             #Dado que se usarán los datos a partir del 2014, se eliminan los datos del 2011-2013
dt1419<-do.call(rbind,lista)                       #Usando do.call, combina las listas en un data frame

###############################Formato de Columnas##############################

dt1419 <- dplyr::rename(dt1419, Max.2.5.O = BbMx.2.5,    #Usando rename del paquete dplyr, se cambian los nombres de las columnas para el data frame 2014-2019
                 Avg.2.5.O = BbAv.2.5, 
                 Max.2.5.U = BbMx.2.5.1,
                 Avg.2.5.U = BbAv.2.5.1)
e1920 <- dplyr::rename(e1920,  Max.2.5.O = Max.2.5,      #Usando rename del paquete dplyr, se cambian los nombres de las columnas para los datos del 2019-2020
                 Avg.2.5.O = Avg.2.5, 
                 Max.2.5.U = Max.2.5.1,
                 Avg.2.5.U = Avg.2.5.1)

dt1419 <- select(dt1419, colnames(e1920))                #Se ponen las columnas en el mismo orden para los dos data sets

################################Unificación#####################################

dt1419 <- rbind(dt1419, e1920)                     #Usando rbind, se combinan por filas los dataframes para tener los datos del 2014-2020

###############################Formato de Columnas##############################

dt1419 <- dplyr::rename(dt1419, date = Date, home.team = HomeTeam,   #Renombra las columnas del dataframe
                        home.score = FTHG, away.team = AwayTeam, 
                        away.score = FTAG)

data <- select(dt1419, date, home.team, home.score,                  #Selecciona las columnas que se usarán en el órden que se desean
               away.team, away.score:Avg.2.5.U)

head(data, n = 2L); tail(data, n = 2L)                               #Comprueba el formato del dataframe al ver los primeros y últimos elementos

####################Data frames de partidos y equipos###########################

md <- data %>% select(date:away.score)                              #Genera un nuevo dataframe (2014-2020) con los datos de la fecha, los nombres de los equipos, y los goles de casa y visitante
write.csv(md, "match.data.csv", row.names = FALSE)                  #Guarda los el dataframe en un nuevo archivo.csv
df <- create.fbRanks.dataframes(scores.file = "match.data.csv")     #Crea un dataframe a partir del archivo creado
teams <- df$teams; scores <- df$scores                              #Guarda en una variable el nombre de los equipos (teams) y guarda el registro de los goles en otra (scores)

head(teams, n = 2L); dim(teams); head(scores, n = 2L); dim(scores)  #Paso de verificación

##############Conjuntos iniciales de entrenamiento y de prueba##################

f <- scores$date                                         #Guarda las fechas de los partidos en una variable
fu <- unique(f)                                          #Toma las fechas que no están repetidas
Ym <- format(fu, "%Y-%m")                                #Especifica el formato de las fechas
Ym <- unique(Ym)                                         #Toma los años-meses que no están repetidos
places <- which(Ym[15]==format(scores$date, "%Y-%m"))    #Considera partidos de 15 meses para comenzar a ajustar el modelo
ffe <- scores$date[max(places)]                          #Fecha final (máxima) del conjunto de entrenamiento

###################Preparación para el ajuste del modelo########################

train <- scores %>% filter(date <= ffe)         #Para entrenar al modelo se usan los datos desde el inicio hasta la fecha final (ffe)
test <- scores %>% filter(date > ffe)           #Los datos de prueba son los que van después de los 15 meses que toma en cuenta el dataframe train

head(train, n = 1); tail(train, n = 1)          #Paso de verificación
head(test, n = 1); tail(test, n = 1)            #Paso de verificación

########################Primer ajuste del modelo################################

traindate <- unique(train$date)              #Toma las fechas que no están repetidas
testdate <- unique(test$date)                #Toma las fechas que no están repetidas

ranks <- rank.teams(scores = scores, teams = teams,            #Hace un ranking de los equipos a partir del dataframe train
                    min.date = traindate[1], 
                    max.date = traindate[length(traindate)])

#########################Primera predicción#####################################

pred <- predict(ranks, date = testdate[1])        #Hace la predicción con la primera fecha de prueba
phs <- pred$scores$pred.home.score                #Predicción del resultado de casa
pas <- pred$scores$pred.away.score                #Predicción del resultado de visitante
pht <- pred$scores$home.team                      #Predicción del equipo de casa
pat <- pred$scores$away.team                      #Predicción del equipo de visitante

#######################Segundo ajusto y predicción##############################

phs <- NULL; pas <- NULL; pht <- NULL; pat <- NULL                 #Se eliminan las predicciones anteriores
for(i in 1:(length(unique(scores$date))-170)){                     #Loop for que cubre las fechas en el dataframe scores
  ranks <- rank.teams(scores = scores, teams = teams,              #El ciclo hace un ranking de los equipos y los resultados por cada fecha del dataframe scores
                      min.date = unique(scores$date)[i], 
                      max.date = unique(scores$date)[i+170-1], 
                      silent = TRUE,
                      time.weight.eta = 0.0005)
  pred <- predict(ranks, date = unique(scores$date)[i+170],       #El ciclo hace una predicción por fecha
                  silent = TRUE)
  
  phs <- c(phs, pred$scores$pred.home.score)                      #Predicción de los resultados de casa
  pas <- c(pas, pred$scores$pred.away.score)                      #Predicción de los resultados de visitante
  pht <- c(pht, pred$scores$home.team)                            #Predicción de los equipos de casa
  pat <- c(pat, pred$scores$away.team)                            #Predicción de los equipos de visitante
}

############################Limpieza de datos###################################

buenos <- !(is.na(phs) | is.na(pas))                                 #Muestra los NA's de las predicciones como FALSE
phs <- phs[buenos]                                                   #Incluye las predicciones sin datos NA
pas <- pas[buenos] 
pht <- pht[buenos] 
pat <- pat[buenos] 
momio <- data %>% filter(date >= unique(scores$date)[171])           #Momio de conjunto de prueba. Dataframe con los datos que son mayores a las fechas únicas después de la posición 171
momio <- momio[buenos,]                                              #Quita los NA's del dataframe
mean(pht == momio$home.team); mean(pat == momio$away.team)           #Calcula el promedio de las igualdades en las que aparece TRUE al comparar los datos predecidos vs. el del momio
mean(phs + pas > 2.5 & momio$home.score + momio$away.score > 2.5)    #Calcula el promedio en el que las predicciones y los datos originales son mayores a 2.5
mean(phs + pas < 2.5 & momio$home.score + momio$away.score < 2.5)    #Se puede ver que los números de los promedios son similares entre sí, tanto de los equipos como el de las anotaciones
hs <- momio$home.score                                               #Guarda las anotaciones del juego de casa
as <- momio$away.score                                               #Guarda las anotaciones del juego de visitante

#######################Probabilidades condicionales#############################

mean(phs + pas > 3)                                           #Proporción de partidos con más de tres goles según el modelo
mean(phs + pas > 3 & hs + as > 2.5)/mean(phs + pas > 3)       #Probabilidad condicional estimada de ganar en over 2.5
mean(phs + pas < 2.1)                                         #Proporción de partidos con menos de 2.1 goles según el modelo
mean(phs + pas < 2.1 & hs + as < 2.5)/mean(phs + pas < 2.1)   #Probabilidad condicional estimada de ganar en under 2.5

######################Juegos con momios máximos#################################

cap <- 50000; g <- NULL                                                #Condiciones iniciales

for(j in 1:length(phs)){                                               #Loop for desde el inicio hasta el final de la predicción de la anotación de casa
  if(((phs[j] + pas[j]) > 3) & (0.64/(momio$Max.2.5.O[j]^-1) > 1)){    #Condicional if en donde se requiere que el marcador de la predicción sea mayor a 3 y que el momio máximo sea mayor a 1
    if((hs[j] + as[j]) > 2.5) cap <- cap + 1000*(momio$Max.2.5.O[j]-1) #Condicional if en donde se requiere que el marcador original sea mayor a 2.5
    else cap <- cap - 1000
    g <- c(g, cap)
  }
  
  if(((phs[j] + pas[j]) < 2.1) & (0.58/(momio$Max.2.5.U[j]^-1) > 1)){  #Condicional if en donde se requiere que el marcador de la predicción sea menor a 2.1 y que el momio máximo sea mayor a 1
    if((hs[j] + as[j]) < 2.5) cap <- cap + 1000*(momio$Max.2.5.U[j]-1) #Condicional if en donde se requiere que el marcador original sea menor a 2.5
    else cap <- cap - 1000
    g <- c(g, cap)
  }
}


##########################Escenario con momios máximos##########################

g <- data.frame(Num_Ap = 1:length(g), Capital = g)                                                  #Crea un dataframe con la lista generada en el loop for                                           
p <- ggplot(g, aes(x=Num_Ap, y=Capital)) + geom_line( color="purple") + geom_point() +              #Crea un gráfico
  labs(x = "Número de juego", 
       y = "Capital",
       title = "Secuencia de juegos",
       subtitle= "Escenario con momios máximos") +
  theme(plot.title = element_text(size=12))  +
  theme(axis.text.x = element_text(face = "bold", color="blue" , size = 10, angle = 25, hjust = 1),
        axis.text.y = element_text(face = "bold", color="blue" , size = 10, angle = 25, hjust = 1))  #Color, ángulo y estilo de las abcisas y ordenadas 
p

#######################Juegos con momios promedio############################

cap <- 50000; g <- NULL                                                  #Condiciones iniciales

for(j in 1:length(phs)){                                                 #Loop for desde el inicio hasta el final de la predicción de la anotación de casa
  if(((phs[j] + pas[j]) > 3) & (0.64/(momio$Avg.2.5.O[j]^-1) > 1)){      #Condicional if en donde se requiere que el marcador de la predicción sea mayor a 3 y que el momio promedio sea mayor a 1
    if((hs[j] + as[j]) > 2.5) cap <- cap + 1000*(momio$Avg.2.5.O[j]-1)   #Condicional if en donde se requiere que el marcador original sea mayor a 2.5
    else cap <- cap - 1000
    g <- c(g, cap)
  }
  
  if(((phs[j] + pas[j]) < 2.1) & (0.58/(momio$Avg.2.5.U[j]^-1) > 1)){    #Condicional if en donde se requiere que el marcador de la predicción sea menor a 2.1 y que el momio promedio sea mayor a 1
    if((hs[j] + as[j]) < 2.5) cap <- cap + 1000*(momio$Avg.2.5.U[j]-1)   #Condicional if en donde se requiere que el marcador original sea menor a 2.5
    else cap <- cap - 1000
    g <- c(g, cap)
  }
}

#######################Escenario con momios promedio############################

g <- data.frame(Num_Ap = 1:length(g), Capital = g)                                                   #Crea un dataframe con la lista generada en el loop for 
p <- ggplot(g, aes(x=Num_Ap, y=Capital)) + geom_line( color="red") + geom_point() +               #Crea un gráfico
  labs(x = "Número de juego", 
       y = "Capital",
       title = "Secuencia de juegos",
       subtitle= "Escenario con momios promedio") +
  theme(plot.title = element_text(size=12))  +
  theme(axis.text.x = element_text(face = "bold", color="blue" , size = 10, angle = 25, hjust = 1),
        axis.text.y = element_text(face = "bold", color="blue" , size = 10, angle = 25, hjust = 1))  # color, ángulo y estilo de las abcisas y ordenadas 
p

############################Gráfico de barras####################################

ggplot(md,aes(home.score))+
  geom_bar(col="black",fill="purple")+ 
  facet_wrap("away.team") +
  labs(x ="Goles de local", y = "Frecuencia") + 
  ggtitle("Liga Española Primera División")+
  ylim(0,50)

ggplot(md,aes(away.score))+
  geom_bar(col="black",fill="light blue")+ 
  facet_wrap("away.team") +
  labs(x ="Goles de visitante", y = "Frecuencia") + 
  ggtitle("Liga Española Primera División")+
  ylim(0,50)

##############################Serie de tiempo###################################

rm<-data.frame(date=md$date,home.team=md$home.team,home.score=md$home.score,away.score=md$away.score)
rm<- subset(rm, home.score>away.score)
rm<- subset(rm, home.team == "Real Madrid")
rm.ts <- ts(rm$home.score, start = 0, freq = 12)
plot(rm.ts, xlab = "Time", ylab = "Goles del RM")
title(main = "Serie de Tiempo")

plot(diff(rm.ts), xlab = "Time", ylab = "Cambio en los Goles del RM")
title(main = "Primera diferencia")

acf(diff(as.numeric(rm.ts)), main = "Detectar modelo AR(p)")
pacf(diff(as.numeric(rm.ts)), main = "Detectar modelo MA(q)")
#AR,I,MA
arima_model <- arima(rm.ts, order = c(2, 1, 2), 
                     seas = list(order = c(2, 1, 2), 12))
arima_model$coef


pred <- predict(arima_model, 10)$pred
ts.plot(cbind(rm.ts, pred), col = c("blue", "red"), xlab = "")
title(main = "Time Series Prediction ARIMA(2,1,3)",
      xlab = "Time",
      ylab = "Goles del RM")
