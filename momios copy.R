############################### Librerías ######################################
library(fbRanks)
library(dplyr)
library(ggplot2)
library(ggdark)

###################### Directorio de trabajo ###################################

setwd("~/WorkingDirectory/Proyecto-R-BEDU/")                                       #Difiere dependiendo del usuario

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

png(filename="Dashboard/www/momios_prom.png", width = 900, height = 498)         #Guardamos la gráfica resultante para usarla en nuestro dashboard
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
# H0 goles <= 2    Hipótesis Nula el equipo 2 o menos goles
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
# Entonces se puede afirmar con una confianza del 95% que el Real Madrid anotará MÁS DE DOS GOLES jugando como local.

############################## Prueba de hipótesis visitante #############################

m_scores_rm <- sample(scores_rm$away.score, length(scores_rm$away.score), size = 64)                             #Tomamos la muestra aleatoria

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
# Entonces se puede afirmar con una confianza del 95% que el equipo local anotará MENOS DE DOS GOLES jugando como local.

