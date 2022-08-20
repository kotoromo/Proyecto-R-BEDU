############################### Librerías ######################################
library(fbRanks)
library(dplyr)
library(ggplot2)
library(ggdark)

###################### Directorio de trabajo ###################################

setwd("~/Documents/BEDU/FASE_2/Proyecto")                                       #Difiere dependiendo del usuario

####################### Descarga de archivos ###################################
# https://www.football-data.co.uk/spainm.php

dir.create("./RawData")                                                         #Creación de directorio para guardar los archivos .csv

for (i in 10:19) {                                                              #Mediante un ciclo for descargamos los archivos de cada URL indicada
  current.url <- paste(
    "https://www.football-data.co.uk/mmz4281/",i,i+1,"/SP1.csv", sep = "")      #Incrementamos el valor de la penúltima ruta en cada URL, vamos de 1011 a 1921
  download.file(url = current.url, destfile = paste(
    "./RawData/SP1-",i,i+1,".csv", sep = ""), mode = "wb")                      #De igual forma incrementamos el valor en el nombre de los archivos de salida
}

####################### Lectura de datos #######################################

files.path <- paste("./RawData/", list.files(path = "./RawData"), sep = "")     #Creamos una lista con las rutas de los archivos .csv

d.list <- lapply(files.path, read.csv)                                          #Leemos cada archivo con read.csv usando lapply y guardamos el contenido de cada archivo en una lista

######################## Procesamiento de datos ################################
                                                                                # Realizamos las selecciones de columnas para cada dataframe
d.list[1:9] <- lapply(d.list[1:9], select, Date:FTAG, BbMx.2.5:BbAv.2.5.1)      #Equivale a la selección de d1011S a d1819S
d.list[10] <- lapply(d.list[10], select, Date, HomeTeam:FTAG, Max.2.5:Avg.2.5.1)#Equivale a la selección en d1920S 

#############################Formato de datos###################################

d.list <- lapply(d.list, mutate, Date = as.Date(Date, format = "%d/%m/%y"))     #Aplica un formato homogéneo de tipo fecha al set de listas

################################Unificación#####################################

d1019S <- bind_rows(d.list[1:9], .id = NULL)                                    # Unimos de 1011 a 1819 usando la función bind_rows() de dplyr

############################### Renombrar ######################################

d1019S <- rename(d1019S,  Max.2.5.O = BbMx.2.5,                                 #Usando rename del paquete dplyr, se cambian los nombres de las columnas para el data frame 2010-2019
                 Avg.2.5.O = BbAv.2.5, 
                 Max.2.5.U = BbMx.2.5.1,
                 Avg.2.5.U = BbAv.2.5.1)

d1920S <- d.list[[10]]                                                          #guardamos el dataframe 1920 en d1920S          

d1920S <- rename(d1920S,  Max.2.5.O = Max.2.5,                                  #Usando rename del paquete dplyr, se cambian los nombres de las columnas para los datos del 2019-2020
                 Avg.2.5.O = Avg.2.5, 
                 Max.2.5.U = Max.2.5.1,
                 Avg.2.5.U = Avg.2.5.1)

d1019S <- select(d1019S, colnames(d1920S))                                      #Se ponen las columnas en el mismo orden para los dos data sets

############################### Unificación ####################################

d1020S <- rbind(d1019S, d1920S)                                                 #Usando rbind, se combinan por filas los dataframes para tener los datos del 2010-2020

############################## Formato de Columnas #############################

d1020S <- rename(d1020S, date = Date, home.team = HomeTeam, home.score = FTHG,  #Renombra las columnas del dataframe
                 away.team = AwayTeam, away.score = FTAG)

data <- select(d1020S, date, home.team, home.score, away.team,                   #Selecciona las columnas que se usarán en el órden que se desean                 
               away.score:Avg.2.5.U) 

head(data, n = 2L); tail(data, n = 2L)                                          # Este data frame contiene todos los datos necesarios

################### Data frames de partidos y equipos ##########################

md <- data %>% select(date:away.score)                                          #Genera un nuevo dataframe (2010-2020) con los datos de la fecha, los nombres de los equipos, y los goles de casa y visitante
write.csv(md, "match.data.csv", row.names = FALSE)                              #Guarda los el dataframe en un nuevo archivo.csv
df <- create.fbRanks.dataframes(scores.file = "match.data.csv")                 #Crea un dataframe a partir del archivo creado
teams <- df$teams; scores <- df$scores                                          #Guarda en una variable el nombre de los equipos (teams) y guarda el registro de los goles en otra (scores)

head(teams, n = 2L); dim(teams); head(scores, n = 2L); dim(scores)              #Paso de verificación

############# Conjuntos iniciales de entrenamiento y de prueba #################

f <- scores$date                                                                #Guarda las fechas de los partidos en una variable
fu <- unique(f)                                                                 #Toma las fechas que no están repetidas
Ym <- format(fu, "%Y-%m")                                                       #Especifica el formato de las fechas
Ym <- unique(Ym)                                                                #Toma los años-meses que no están repetidos
places <- which(Ym[15]==format(scores$date, "%Y-%m"))                           #Considera partidos de 15 meses para comenzar a ajustar el modelo
ffe <- scores$date[max(places)]                                                 #Fecha final (máxima) del conjunto de entrenamiento

################## Preparación para el ajuste del modelo #######################

train <- scores %>% filter(date <= ffe)                                         #Para entrenar al modelo se usan los datos desde el inicio hasta la fecha final (ffe)
test <- scores %>% filter(date > ffe)                                           #Los datos de prueba son los que van después de los 15 meses que toma en cuenta el dataframe train

head(train, n = 1); tail(train, n = 1)                                          #Paso de verificación
head(test, n = 1); tail(test, n = 1)                                            #Paso de verificación

####################### Primer ajuste del modelo ###############################

traindate <- unique(train$date)                                                 #Toma las fechas que no están repetidas
testdate <- unique(test$date)                                                   #Toma las fechas que no están repetidas

ranks <- rank.teams(scores = scores, teams = teams,                             #Hace un ranking de los equipos a partir del dataframe train
                    min.date = traindate[1], 
                    max.date = traindate[length(traindate)])

########################### Primera predicción #################################

pred <- predict(ranks, date = testdate[1])                                      #Hace la predicción con la primera fecha de prueba
phs <- pred$scores$pred.home.score                                              #Predicción del resultado de casa
pas <- pred$scores$pred.away.score                                              #Predicción del resultado de visitante
pht <- pred$scores$home.team                                                    #Predicción del equipo de casa
pat <- pred$scores$away.team                                                    #Predicción del equipo de visitante

###################### Segundo ajusto y predicción #############################

phs <- NULL; pas <- NULL; pht <- NULL; pat <- NULL                              #Se eliminan las predicciones anteriores
for(i in 1:(length(unique(scores$date))-170)){                                  #Loop for que cubre las fechas en el dataframe scores
  ranks <- rank.teams(scores = scores, teams = teams,                           #El ciclo hace un ranking de los equipos y los resultados por cada fecha del dataframe scores
                      min.date = unique(scores$date)[i], 
                      max.date = unique(scores$date)[i+170-1], 
                      silent = TRUE,
                      time.weight.eta = 0.0005)
  pred <- predict(ranks, date = unique(scores$date)[i+170],                     #El ciclo hace una predicción por fecha
                  silent = TRUE)
  
  phs <- c(phs, pred$scores$pred.home.score)                                    #Predicción de los resultados de casa
  pas <- c(pas, pred$scores$pred.away.score)                                    #Predicción de los resultados de visitante
  pht <- c(pht, pred$scores$home.team)                                          #Predicción de los equipos de casa
  pat <- c(pat, pred$scores$away.team)                                          #Predicción de los equipos de visitante
}

########################### Limpieza de datos ##################################

buenos <- !(is.na(phs) | is.na(pas))                                            #Muestra los NA's de las predicciones como FALSE
phs <- phs[buenos]                                                              #Incluye las predicciones sin datos NA
pas <- pas[buenos] 
pht <- pht[buenos] 
pat <- pat[buenos] 
momio <- data %>% filter(date >= unique(scores$date)[171])                      #Momio de conjunto de prueba. Dataframe con los datos que son mayores a las fechas únicas después de la posición 171
momio <- momio[buenos,]                                                         #Quita los NA's del dataframe
mean(pht == momio$home.team); mean(pat == momio$away.team)                      #Calcula el promedio de las igualdades en las que aparece TRUE al comparar los datos predecidos vs. el del momio
mean(phs + pas > 2.5 & momio$home.score + momio$away.score > 2.5)               #Calcula el promedio en el que las predicciones y los datos originales son mayores a 2.5
mean(phs + pas < 2.5 & momio$home.score + momio$away.score < 2.5)               #Se puede ver que los números de los promedios son similares entre sí, tanto de los equipos como el de las anotaciones
hs <- momio$home.score                                                          #Guarda las anotaciones del juego de casa
as <- momio$away.score                                                          #Guarda las anotaciones del juego de visitante

###################### Probabilidades condicionales ############################

mean(phs + pas > 3)                                                             #Proporción de partidos con más de tres goles según el modelo
mean(phs + pas > 3 & hs + as > 2.5)/mean(phs + pas > 3)                         #Probabilidad condicional estimada de ganar en over 2.5
mean(phs + pas < 2.1)                                                           #Proporción de partidos con menos de 2.1 goles según el modelo
mean(phs + pas < 2.1 & hs + as < 2.5)/mean(phs + pas < 2.1)                     #Probabilidad condicional estimada de ganar en under 2.5

##################### Juegos con momios máximos ################################

cap <- 50000; g <- NULL                                                         #Condiciones iniciales

for(j in 1:length(phs)){                                                        #Loop for desde el inicio hasta el final de la predicción de la anotación de casa
  if(((phs[j] + pas[j]) > 3) & (0.64/(momio$Max.2.5.O[j]^-1) > 1)){             #Condicional if en donde se requiere que el marcador de la predicción sea mayor a 3 y que el momio máximo sea mayor a 1
    if((hs[j] + as[j]) > 2.5) cap <- cap + 1000*(momio$Max.2.5.O[j]-1)          #Condicional if en donde se requiere que el marcador original sea mayor a 2.5
    else cap <- cap - 1000
    g <- c(g, cap)
  }
  
  if(((phs[j] + pas[j]) < 2.1) & (0.58/(momio$Max.2.5.U[j]^-1) > 1)){           #Condicional if en donde se requiere que el marcador de la predicción sea menor a 2.1 y que el momio máximo sea mayor a 1
    if((hs[j] + as[j]) < 2.5) cap <- cap + 1000*(momio$Max.2.5.U[j]-1)          #Condicional if en donde se requiere que el marcador original sea menor a 2.5
    else cap <- cap - 1000
    g <- c(g, cap)
  }
}


######################### Escenario con momios máximos #########################

g <- data.frame(Num_Ap = 1:length(g), Capital = g)                              #Crea un dataframe con la lista generada en el loop for                                           
p <- ggplot(g, aes(x=Num_Ap, y=Capital)) + geom_line( color="deepskyblue4") +   #Crea un gráfico
  geom_point(size=.6) +              
  labs(x = "Número de juego", 
       y = "Capital",
       title = "Secuencia de juegos",
       subtitle= "Escenario con momios máximos") +
  theme(plot.title = element_text(size=12))  +
  theme(axis.text.x = element_text(face = "bold", color="blue" ,                #Color, ángulo y estilo de las abcisas y ordenadas 
                                   size = 10, angle = 25, hjust = 1),
        axis.text.y = element_text(face = "bold", 
                                   color="blue" , size = 10, angle = 25, hjust = 1))+
  dark_theme_gray()

p

png(filename="Dashboard/www/momios_max.png", width = 900, height = 498)         #Guardamos la gráfica resultante para usarla en nuestro dashboard
p
dev.off()

######################## Juegos con momios promedio ############################

cap <- 50000; g <- NULL                                                         #Condiciones iniciales

for(j in 1:length(phs)){                                                        #Loop for desde el inicio hasta el final de la predicción de la anotación de casa
  if(((phs[j] + pas[j]) > 3) & (0.64/(momio$Avg.2.5.O[j]^-1) > 1)){             #Condicional if en donde se requiere que el marcador de la predicción sea mayor a 3 y que el momio promedio sea mayor a 1
    if((hs[j] + as[j]) > 2.5) cap <- cap + 1000*(momio$Avg.2.5.O[j]-1)          #Condicional if en donde se requiere que el marcador original sea mayor a 2.5
    else cap <- cap - 1000
    g <- c(g, cap)
  }
  
  if(((phs[j] + pas[j]) < 2.1) & (0.58/(momio$Avg.2.5.U[j]^-1) > 1)){           #Condicional if en donde se requiere que el marcador de la predicción sea menor a 2.1 y que el momio promedio sea mayor a 1
    if((hs[j] + as[j]) < 2.5) cap <- cap + 1000*(momio$Avg.2.5.U[j]-1)          #Condicional if en donde se requiere que el marcador original sea menor a 2.5
    else cap <- cap - 1000
    g <- c(g, cap)
  }
}

###################### Escenario con momios promedio ###########################

g <- data.frame(Num_Ap = 1:length(g), Capital = g)                              #Crea un dataframe con la lista generada en el loop for 
p <- ggplot(g, aes(x=Num_Ap, y=Capital)) + geom_line( color="brown4") +            #Crea un gráfico
  geom_point(size=.4) +               
  labs(x = "Número de juego", 
       y = "Capital",
       title = "Secuencia de juegos",
       subtitle= "Escenario con momios promedio") +
  theme(plot.title = element_text(size=12))  +
  theme(axis.text.x = element_text(face = "bold", color="blue" ,                # color, ángulo y estilo de las abcisas y ordenadas 
                                   size = 10, angle = 25, hjust = 1),
        axis.text.y = element_text(face = "bold", color="blue" , 
                                   size = 10, angle = 25, hjust = 1))+
  dark_theme_gray()

p

png(filename="Dashboard/www/momios_prom.png", width = 900, height = 498)        #Guardamos la gráfica resultante para usarla en nuestro dashboard
p
dev.off()


######################## Teorema central de limite #############################

scores_rm = filter(scores, home.team == "Real Madrid")                          #Escogemos un equipo de nuestro conjunto

(frecuencias <-  as.data.frame(table(scores_rm$home.score)))                    #Calculamos la frecuencia de número de goles como local

ggplot(frecuencias, aes(x=Var1, y=Freq))+                                       # Se gráfica esa frecuencia
  geom_col(color='white',fill='deepskyblue4')+
  labs(x = "Goles", 
       y = "Frecuencia",
       title = "Goles anotados por Real Madrid",
       subtitle= "Jugando como local") +
  theme(plot.title = element_text(size=22)) +
  dark_theme_gray()

# Se observa una distribución asimétrica por la derecha
# Entonces aplicaremos el teorema del límite central

library(data.table)

muestras <- c()
for(i in 1:100){                                                                # Generar 100 muestras de 40 registros
  tmp <- sample(scores_rm$home.score, size = 40)
  tmp <- as.data.table(tmp)
  tmp[, grupo := i] 
  l <- list(muestras, tmp)
  muestras <- rbindlist(l) 
}

medias <- muestras[, mean(tmp), by = grupo]                                     # Objeto tipo tabla con la media de cada muestra
setnames(medias, "V1", "promedio"); medias

(promedio_muestras <- mean(medias$promedio))                                    # Promedio de las medias de las muestras


d <- density(medias$promedio, adjust = 1.8)

ggplot(data.frame(x = d$x, y = d$y),aes(x, y)) +                                # Gráfica de la distribución de las medias de las muestras
  geom_line(colour = "white", size = 2) + 
  geom_area(fill = "cadetblue") +
  labs(y = "Densidad",
       title = "Densidad de los promedios de goles como local")+
  theme(plot.title = element_text(size=22)) +
  dark_theme_gray()
  
#Con esto ya se aproxima a una distribución normal y podemos hacer una prueba de hipótesis


############################## Prueba de hipótesis #############################

m_scores_rm <- sample(scores_rm$home.score, 40)                                 #Extraemos un subconjunto de nuestra población

# Nos interesa saber si en promedio el numero de goles que anota el Real Madrid
# cuando juega como local es mayor a 2

# Prueba de cola superior
# H0 goles <= 2    Hipótesis Nula el equipo anota 2 o menos goles
# H1 goles > 2    Hipótesis Alterna el equipo anota más de 2 goles

prom <- mean(m_scores_rm)                                                       # Promedio
desv <- sd(m_scores_rm)                                                         # Desviación estándar
n <- length(m_scores_rm)                                                        # Tamaño de muestra

#(z0 <- (mean(muestra)-media_de_prueba)/(sd(muestra)/sqrt(tamaño_de_muestra)))  # Estadístico de prueba
(z0 <- (prom-2)/(desv/sqrt(n)))

(z.05 <- qnorm(p = 0.05,  lower.tail = FALSE))                                  # Valor critico para identificar zona de rechazo a 95% de confianza

(pvalue <- pnorm(z0, lower.tail = FALSE))                                       # Calculo de P value

t.test(x=m_scores_rm, alternative = 'greater', mu=2)                            # Comprobamos con la prueba t test de forma directa

x <- seq(-4, 4, 0.1)
y <- dnorm(x)

ggplot(data.frame(x,y),aes(x, y)) +                                             #Graficamos nuestra zona de rechazo
  stat_function(fun=dnorm, geom="line", col = "white")+
  geom_ribbon(data=subset(data.frame(x,y) ,x>=z.05 & x<4),aes(ymax=y),ymin=0,
              fill="brown3", colour=NA)+
  geom_text(label=round(z.05,5), x=z.05, y=0, color = "white")+
  geom_label(label="Zona de rechazo", x=3.5, y=0.2, 
            label.padding = unit(0.55, "lines"), fill = "brown3", alpha = 0.2)+
  labs(y = "Densidad",
       title = "Densidad normal estándar")+
  theme(plot.title = element_text(size=22)) +
  dark_theme_gray()

# Como nuestro estadístico de prueba se encuentra en zona de rechazo,
# existe evidencia estadística necesaria para rechazar la hipótesis nula.
# Entonces se puede afirmar con una confianza del 95% que el Real Madrid anotará en promedio MÁS DE DOS GOLES jugando como local.


############################## Prueba de hipótesis visitante #############################

m_scores_rm <- sample(scores_rm$away.score, length(scores_rm$away.score), size = 40)                             #Tomamos la muestra aleatoria

# Nos interesa saber si en promedio el numero de goles que anota el equipo 
# visitante vs Real Madrid cuando juega como local es mayor a 2
# 

# Prueba de cola inferior
# H0 goles >= 2    Hipótesis Nula el equipo visitante mete 2 o más goles
# H1 goles < 2    Hipótesis Alterna el equipo anota más de 1 gol

prom <- mean(m_scores_rm)                                                       # Promedio de la muestra
desv <- sd(m_scores_rm)                                                         # Desviación estándar de la muestra
n <- length(m_scores_rm)                                                        # Tamaño de la muestra

# hist(scores_rm$away.score, breaks = 4)                                        # La población tiene un sesgo hacia la derecha. Sin embargo, usando el TCL sabemos que las medias de las muestras distribuyen normalmente
# as.data.frame(table(scores_rm$away.score))                                    # La tabla de frecuencias confirma esto

#(z0 <- (mean(muestra)-media_de_prueba)/(sd(muestra)/sqrt(tamaño_de_muestra)))  # Estadístico de prueba
(z0 <- (prom-2)/(desv/sqrt(n)))                                                 # Prueba Z para Ho

(z.05 <- qnorm(p = 0.05,  lower.tail = TRUE))                                   # Valor critico para identificar zona de rechazo a 95% de confianza

(pvalue <- pnorm(z0, lower.tail = TRUE))                                        # Calculo de P value



t.test(x=m_scores_rm, alternative = 'less', mu=2)                               # Comprobamos con la prueba t test de forma directa

x <- seq(-4, 4, 0.1)
y <- dnorm(x)

ggplot(data.frame(x,y),aes(x, y)) +                                             #Graficamos nuestra zona de rechazo
  stat_function(fun=dnorm, geom="line", col = "white")+
  geom_ribbon(data=subset(data.frame(x,y) ,x<=z.05 & x<4),aes(ymax=y),ymin=0,
              fill="brown3", colour=NA)+
  geom_text(label=round(z.05,5), x=z.05, y=0, color = "white")+
  geom_vline(aes(xintercept = z.05)) +
  geom_label(label="Zona de rechazo", x=3.5, y=0.2, 
             label.padding = unit(0.55, "lines"), fill = "brown3", alpha = 0.2)+
  labs(y = "Densidad",
       title = "Densidad normal estándar")+
  theme(plot.title = element_text(size=22)) +
  dark_theme_gray()

# Como nuestro estadístico de prueba se encuentra en zona de rechazo, pues z0 = -11.54 < z.05 = -1.64
# existe evidencia estadística necesaria para rechazar la hipótesis nula.
# Entonces se puede afirmar con una confianza del 95% que el equipo visitante anotará MENOS DE DOS GOLES jugando contra el Real Madrid como local.



##############################Probabilidades####################################

visitante<-data.frame(date=md$date,away.team=md$away.team,away.score=md$away.score)

set.seed(23)
bar_40<- sample(visitante$away.score,40)                            #Selecciona una muestra aleatoria de tamaño 40
bar_40<- as.data.frame(bar_40)                                      #Guarda como dataframe
bar_40 <- dplyr::rename(bar_40, away.score=bar_40)                  #Renombra la columna que contiene las anotaciones de visitante

mean_bar<- mean(bar_40$away.score)                                  #Calcula el promedio de la muestra
sd_bar<- sd(bar_40$away.score)                                      #Calcula la desviación estándar de la muestra

barcelona<- NULL                                                    #Crea una variable vacía para poder guardar una lista

for(k in 0:10){                                                     #Loop for de 0 a 10 para calcular la probabilidad de anotar de x> goles como equipo visitante
  #Guarda cada probabilidad porcentual en una lista 
  prob <- round(pnorm(k,mean_bar,sd_bar,lower.tail=FALSE),3)        #Esta estructura se repite para cada equipo que tiene más de 40 observaciones
  prob<- prob*100
  barcelona<- rbind(barcelona,prob)
  
}

ala<-subset(visitante, away.team =="Alaves")
ala_40<- sample(ala$away.score,40)                            
ala_40<- as.data.frame(ala_40)
ala_40 <- dplyr::rename(ala_40, away.score=ala_40)

mean_ala<- mean(ala_40$away.score)
sd_ala<- sd(ala_40$away.score)

alaves<- NULL

for(k in 0:10){
  
  prob <- round(pnorm(k,mean_ala,sd_ala,lower.tail=FALSE),3)
  prob<- prob*100
  alaves<- rbind(alaves,prob)
  
}

alm<-subset(visitante, away.team =="Almeria")
alm_40<- sample(alm$away.score,40)                            
alm_40<- as.data.frame(alm_40)
alm_40 <- dplyr::rename(alm_40, away.score=alm_40)

mean_alm<- mean(alm_40$away.score)
sd_alm<- sd(alm_40$away.score)

almeria<- NULL

for(k in 0:10){
  
  prob <- round(pnorm(k,mean_alm,sd_alm,lower.tail=FALSE),3)
  prob<- prob*100
  almeria<- rbind(almeria,prob)
  
}

bil<-subset(visitante, away.team =="Ath Bilbao")
bil_40<- sample(bil$away.score,40)                            
bil_40<- as.data.frame(bil_40)
bil_40 <- dplyr::rename(bil_40, away.score=bil_40)

mean_bil<- mean(bil_40$away.score)
sd_bil<- sd(bil_40$away.score)

bilbao<- NULL

for(k in 0:10){
  
  prob <- round(pnorm(k,mean_bil,sd_bil,lower.tail=FALSE),3)
  prob<- prob*100
  bilbao<- rbind(bilbao,prob)
  
}

mad<-subset(visitante, away.team =="Ath Madrid")
mad_40<- sample(mad$away.score,40)                            
mad_40<- as.data.frame(mad_40)
mad_40 <- dplyr::rename(mad_40, away.score=mad_40)

mean_mad<- mean(mad_40$away.score)
sd_mad<- sd(mad_40$away.score)

madrid<- NULL

for(k in 0:10){
  
  prob <- round(pnorm(k,mean_mad,sd_mad,lower.tail=FALSE),3)
  prob<- prob*100
  madrid<- rbind(madrid,prob)
  
}

bet<-subset(visitante, away.team =="Betis")
bet_40<- sample(bet$away.score,40)                            
bet_40<- as.data.frame(bet_40)
bet_40 <- dplyr::rename(bet_40, away.score=bet_40)

mean_bet<- mean(bet_40$away.score)
sd_bet<- sd(bet_40$away.score)

betis<- NULL

for(k in 0:10){
  
  prob <- round(pnorm(k,mean_bet,sd_bet,lower.tail=FALSE),3)
  prob<- prob*100
  betis<- rbind(betis,prob)
  
}

cel<-subset(visitante, away.team =="Celta")
cel_40<- sample(cel$away.score,40)                            
cel_40<- as.data.frame(cel_40)
cel_40 <- dplyr::rename(cel_40, away.score=cel_40)

mean_cel<- mean(cel_40$away.score)
sd_cel<- sd(cel_40$away.score)

celta<- NULL

for(k in 0:10){
  
  prob <- round(pnorm(k,mean_cel,sd_cel,lower.tail=FALSE),3)
  prob<- prob*100
  celta<- rbind(celta,prob)
  
}

eib<-subset(visitante, away.team =="Eibar")
eib_40<- sample(eib$away.score,40)                            
eib_40<- as.data.frame(eib_40)
eib_40 <- dplyr::rename(eib_40, away.score=eib_40)

mean_eib<- mean(eib_40$away.score)
sd_eib<- sd(eib_40$away.score)

eibar<- NULL

for(k in 0:10){
  
  prob <- round(pnorm(k,mean_eib,sd_eib,lower.tail=FALSE),3)
  prob<- prob*100
  eibar<- rbind(eibar,prob)
  
}

esp<-subset(visitante, away.team =="Espanol")
esp_40<- sample(esp$away.score,40)                            
esp_40<- as.data.frame(esp_40)
esp_40 <- dplyr::rename(esp_40, away.score=esp_40)

mean_esp<- mean(esp_40$away.score)
sd_esp<- sd(esp_40$away.score)

espanol<- NULL

for(k in 0:10){
  
  prob <- round(pnorm(k,mean_esp,sd_esp,lower.tail=FALSE),3)
  prob<- prob*100
  espanol<- rbind(espanol,prob)
  
}

get<-subset(visitante, away.team =="Getafe")
get_40<- sample(get$away.score,40)                            
get_40<- as.data.frame(get_40)
get_40 <- dplyr::rename(get_40, away.score=get_40)

mean_get<- mean(get_40$away.score)
sd_get<- sd(get_40$away.score)

getafe<- NULL

for(k in 0:10){
  
  prob <- round(pnorm(k,mean_get,sd_get,lower.tail=FALSE),3)
  prob<- prob*100
  getafe<- rbind(getafe,prob)
  
}

gra<-subset(visitante, away.team =="Granada")
gra_40<- sample(gra$away.score,40)                            
gra_40<- as.data.frame(gra_40)
gra_40 <- dplyr::rename(gra_40, away.score=gra_40)

mean_gra<- mean(gra_40$away.score)
sd_gra<- sd(gra_40$away.score)

granada<- NULL

for(k in 0:10){
  
  prob <- round(pnorm(k,mean_gra,sd_gra,lower.tail=FALSE),3)
  prob<- prob*100
  granada<- rbind(granada,prob)
  
}

con<-subset(visitante, away.team =="La Coruna")
con_40<- sample(con$away.score,40)                            
con_40<- as.data.frame(con_40)
con_40 <- dplyr::rename(con_40, away.score=con_40)

mean_con<- mean(con_40$away.score)
sd_con<- sd(con_40$away.score)

coruna<- NULL

for(k in 0:10){
  
  prob <- round(pnorm(k,mean_con,sd_con,lower.tail=FALSE),3)
  prob<- prob*100
  coruna<- rbind(coruna,prob)
  
}

pal<-subset(visitante, away.team =="Las Palmas")
pal_40<- sample(pal$away.score,40)                            
pal_40<- as.data.frame(pal_40)
pal_40 <- dplyr::rename(pal_40, away.score=pal_40)

mean_pal<- mean(pal_40$away.score)
sd_pal<- sd(pal_40$away.score)

palmas<- NULL

for(k in 0:10){
  
  prob <- round(pnorm(k,mean_pal,sd_pal,lower.tail=FALSE),3)
  prob<- prob*100
  palmas<- rbind(palmas,prob)
  
}

leg<-subset(visitante, away.team =="Leganes")
leg_40<- sample(leg$away.score,40)                            
leg_40<- as.data.frame(leg_40)
leg_40 <- dplyr::rename(leg_40, away.score=leg_40)

mean_leg<- mean(leg_40$away.score)
sd_leg<- sd(leg_40$away.score)

leganes<- NULL

for(k in 0:10){
  
  prob <- round(pnorm(k,mean_leg,sd_leg,lower.tail=FALSE),3)
  prob<- prob*100
  leganes<- rbind(leganes,prob)
  
}

lev<-subset(visitante, away.team =="Levante")
lev_40<- sample(lev$away.score,40)                            
lev_40<- as.data.frame(lev_40)
lev_40 <- dplyr::rename(lev_40, away.score=lev_40)

mean_lev<- mean(lev_40$away.score)
sd_lev<- sd(lev_40$away.score)

levante<- NULL

for(k in 0:10){
  
  prob <- round(pnorm(k,mean_lev,sd_lev,lower.tail=FALSE),3)
  prob<- prob*100
  levante<- rbind(levante,prob)
  
}

mal<-subset(visitante, away.team =="Malaga")
mal_40<- sample(mal$away.score,40)                            
mal_40<- as.data.frame(mal_40)
mal_40 <- dplyr::rename(mal_40, away.score=mal_40)

mean_mal<- mean(mal_40$away.score)
sd_mal<- sd(mal_40$away.score)

malaga<- NULL

for(k in 0:10){
  
  prob <- round(pnorm(k,mean_mal,sd_mal,lower.tail=FALSE),3)
  prob<- prob*100
  malaga<- rbind(malaga,prob)
  
}

mall<-subset(visitante, away.team =="Mallorca")
mall_40<- sample(mall$away.score,40)                            
mall_40<- as.data.frame(mall_40)
mall_40 <- dplyr::rename(mall_40, away.score=mall_40)

mean_mall<- mean(mall_40$away.score)
sd_mall<- sd(mall_40$away.score)

mallorca<- NULL

for(k in 0:10){
  
  prob <- round(pnorm(k,mean_mall,sd_mall,lower.tail=FALSE),3)
  prob<- prob*100
  mallorca<- rbind(mallorca,prob)
  
}

osa<-subset(visitante, away.team =="Osasuna")
osa_40<- sample(osa$away.score,40)                            
osa_40<- as.data.frame(osa_40)
osa_40 <- dplyr::rename(osa_40, away.score=osa_40)

mean_osa<- mean(osa_40$away.score)
sd_osa<- sd(osa_40$away.score)

osasuna<- NULL

for(k in 0:10){
  
  prob <- round(pnorm(k,mean_osa,sd_osa,lower.tail=FALSE),3)
  prob<- prob*100
  osasuna<- rbind(osasuna,prob)
  
}

rma<-subset(visitante, away.team =="Real Madrid")
rma_40<- sample(rma$away.score,40)                            
rma_40<- as.data.frame(rma_40)
rma_40 <- dplyr::rename(rma_40, away.score=rma_40)

mean_rma<- mean(rma_40$away.score)
sd_rma<- sd(rma_40$away.score)

real<- NULL

for(k in 0:10){
  
  prob <- round(pnorm(k,mean_rma,sd_rma,lower.tail=FALSE),3)
  prob<- prob*100
  real<- rbind(real,prob)
  
}

sev<-subset(visitante, away.team =="Sevilla")
sev_40<- sample(sev$away.score,40)                            
sev_40<- as.data.frame(sev_40)
sev_40 <- dplyr::rename(sev_40, away.score=sev_40)

mean_sev<- mean(sev_40$away.score)
sd_sev<- sd(sev_40$away.score)

sevilla<- NULL

for(k in 0:10){
  
  prob <- round(pnorm(k,mean_sev,sd_sev,lower.tail=FALSE),3)
  prob<- prob*100
  sevilla<- rbind(sevilla,prob)
  
}

soc<-subset(visitante, away.team =="Sociedad")
soc_40<- sample(soc$away.score,40)                            
soc_40<- as.data.frame(soc_40)
soc_40 <- dplyr::rename(soc_40, away.score=soc_40)

mean_soc<- mean(soc_40$away.score)
sd_soc<- sd(soc_40$away.score)

sociedad<- NULL

for(k in 0:10){
  
  prob <- round(pnorm(k,mean_soc,sd_soc,lower.tail=FALSE),3)
  prob<- prob*100
  sociedad<- rbind(sociedad,prob)
  
}

gij<-subset(visitante, away.team =="Sp Gijon")
gij_40<- sample(gij$away.score,40)                            
gij_40<- as.data.frame(gij_40)
gij_40 <- dplyr::rename(gij_40, away.score=gij_40)

mean_gij<- mean(gij_40$away.score)
sd_gij<- sd(gij_40$away.score)

gijon<- NULL

for(k in 0:10){
  
  prob <- round(pnorm(k,mean_gij,sd_gij,lower.tail=FALSE),3)
  prob<- prob*100
  gijon<- rbind(gijon,prob)
  
}

val<-subset(visitante, away.team =="Valencia")
val_40<- sample(val$away.score,40)                            
val_40<- as.data.frame(val_40)
val_40 <- dplyr::rename(val_40, away.score=val_40)

mean_val<- mean(val_40$away.score)
sd_val<- sd(val_40$away.score)

valencia<- NULL

for(k in 0:10){
  
  prob <- round(pnorm(k,mean_val,sd_val,lower.tail=FALSE),3)
  prob<- prob*100
  valencia<- rbind(valencia,prob)
  
}

valla<-subset(visitante, away.team =="Valladolid")
valla_40<- sample(valla$away.score,40)                            
valla_40<- as.data.frame(valla_40)
valla_40 <- dplyr::rename(valla_40, away.score=valla_40)

mean_valla<- mean(valla_40$away.score)
sd_valla<- sd(valla_40$away.score)

valladolid<- NULL

for(k in 0:10){
  
  prob <- round(pnorm(k,mean_valla,sd_valla,lower.tail=FALSE),3)
  prob<- prob*100
  valladolid<- rbind(valladolid,prob)
  
}

valle<-subset(visitante, away.team =="Vallecano")
valle_40<- sample(valle$away.score,40)                            
valle_40<- as.data.frame(valle_40)
valle_40 <- dplyr::rename(valle_40, away.score=valle_40)

mean_valle<- mean(valle_40$away.score)
sd_valle<- sd(valle_40$away.score)

vallecano<- NULL

for(k in 0:10){
  
  prob <- round(pnorm(k,mean_valle,sd_valle,lower.tail=FALSE),3)
  prob<- prob*100
  vallecano<- rbind(vallecano,prob)
  
}

villa<-subset(visitante, away.team =="Villarreal")
villa_40<- sample(villa$away.score,40)                            
villa_40<- as.data.frame(villa_40)
villa_40 <- dplyr::rename(villa_40, away.score=villa_40)

mean_villa<- mean(villa_40$away.score)
sd_villa<- sd(villa_40$away.score)

villarreal<- NULL

for(k in 0:10){
  
  prob <- round(pnorm(k,mean_villa,sd_villa,lower.tail=FALSE),3)
  prob<- prob*100
  villarreal<- rbind(villarreal,prob)
  
}


prob_total <- as.data.frame(cbind(alaves,almeria,bilbao,madrid,barcelona,betis,celta,eibar,              #Crea un dataframe con las listas de probailidades por equipo
                                  espanol,getafe,granada,coruna,palmas,leganes,
                                  levante,malaga,mallorca,osasuna,real,sevilla,
                                  sociedad,gijon,valencia,valladolid,vallecano,
                                  villarreal))
prob_total <- dplyr::rename(prob_total, Alaves=V1, Almeria=V2, Ath_Bilbao=V3, Ath_Madrid=V4,             #Renombra las columnas con los nombres de los equipos
                            Barcelona=V5, Betis=V6, Celta=V7, Eibar=V8, Espanol=V9, Getafe=V10,
                            Granada=V11, Coruna=V12, Palmas=V13, Leganes=V14, Levante=V15,
                            Malaga=V16, Mallorca=V17, Osasuna=V18, Real_Madrid=V19,
                            Sevilla=V20, Sociedad=V21, Sp_Gijon=V22, Valencia=V23,
                            Valladolid=V24, Vallecano=V25, Villarreal=V26)
prob_total <- t(prob_total)                                                                             #Transpone el dataframe
prob_total <- as.data.frame(prob_total)                                                                 #Crea nuevamente el dataframe
prob_total <- dplyr::rename(prob_total, Mas_de_0_goles = prob, Mas_de_1_gol = prob.1,                   #Renombra las columnas 
                            Mas_de_2_goles= prob.2, Mas_de_3_goles= prob.3, Mas_de_4_goles =prob.4,
                            Mas_de_5_goles= prob.5, Mas_de_6_goles= prob.6, Mas_de_7_goles = prob.7,
                            Mas_de_8_goles= prob.8, Mas_de_9_goles= prob.9, Mas_de_10_goles= prob.10)

write.csv(prob_total, "Dashboard/www/Probabilidades.csv", row.names = TRUE) 

##############################Serie de tiempo###################################

rm<-data.frame(date=md$date,home.team=md$home.team,home.score=md$home.score,away.score=md$away.score)
rm<- subset(rm, home.score>away.score)
rm<- subset(rm, home.team == "Real Madrid")
rm.ts <- ts(rm$home.score, start = 0, freq = 12)
plot(rm.ts, xlab = "Time", ylab = "Goles del RM", main="Serie de Tiempo")

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

