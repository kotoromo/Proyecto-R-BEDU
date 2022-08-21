library(shiny)
library(shinythemes)

ui <- navbarPage("Equipo 8",
                 collapsible = TRUE,
                 fluid = TRUE,
                 theme = shinytheme("darkly"),
                 #Página 1
                 tabPanel("Gráficas de barras", icon = icon("chart-simple"),
                          h1("Goles de quipos local y visitante"),
                          selectInput("x", "Seleccione los equipos locales o visitantes: ",
                                      choices = c("Local", "Visitante")),
                          plotOutput("plot", height = 800, width = 900),
                          br(),
                          p("Mostramos los histogramas de los equipos cuando juegan como locales y visitantes en sus respectivas vistas. En general, podemos observar de los histogramas un fuerte sesgo hacia la derecha. La distribución de los goles no es simétrica excepto en contadas ocasiones de acuerdo con los histogramas.")
                 ),
                 #Página 2
                 tabPanel("Probabilidades marginales", icon = icon("chart-area"),
                          h1("Gráficas de probabilidades marginales de anotar goles"),
                          selectInput("y", "Seleccione el equipo que desea ver la probabilidad de anorta gol(es): ",
                                      choices = c("Marginal Local", "Marginal Visitante", "Conjuntas Casa-Visitante")),
                          imageOutput("image1"),
                          br(), br(), br(), br(), br(),
                          p("En las gráficas 1 y 2 se muestra el comportamiento de las probabilidades marginales de que el equipo jugando en modalidad de local o visitante anota un número de goles. La tercer gráfica muestra la probabilidad conjunta que se den los resultados de los encuentros comparando los goles de casa contra los de visitante.")
                 ),
                 #Página 3
                 tabPanel("Resultados de partidos", icon = icon("table"),
                          h1("Resultados de partidos por fecha"),
                          dataTableOutput ("data_table"),
                          br(),
                          p("Tabla interactiva con controles de usuario sencillos de utilizar para mostrar los datos obtenidos después de descargar, depurar y organizar los resultados de los marcadores de partidos de la liga premier española entre 2010 y 2020.")
                 ),
                 #Página 4
                 tabPanel("Factores de ganancia", icon = icon("chart-line"),
                          h1("Factores de ganancia promedio y máximo"),
                          selectInput("z", "Seleccione los factores de ganancia: ",
                                      choices = c("Factores de ganancia promedio", "Factores de ganancia máximo")),
                          imageOutput("image2"),
                          br(), br(), br(), br(), br(), br(), 
                          p("Los momios son la probabilidad de que ocurra determinado evento. En el caso específico de este proyecto, es la probabilidad de que ocurra un determinado resultado propuesto por un corredor de apuestas. Corriendo el código de predicción que se muestra en el repositorio, se arma el escenario en donde se tiene el capital de 50,000. En el caso de los momios máximos, se puede ver que el comportamiento es muy drástico, con ascensos y descensos bruscos. Al final de los casi 700 juegos, el capital decreció a 35,000. Comparando con los momios promedio, se puede ver que los descensos en el capital son mucho más pequeños juego con juego pero al final de los 700 juegos, termina perdiendo casi el 90% del capital, restando solo alrededor de 5000. Con esto se puede concluir que aunque apostarle a los momios máximos puede perder más capital a corto plazo, a largo plazo es más rentable."),
                          br()
                 ),
                 #Página 5
                 tabPanel("Contraste Hipótesis", icon = icon("heading"),
                          h1("Contrastes de Hipótesis"),
                          selectInput("t", "Seleccione un equipo: ",
                                      choices = c("Alaves", "Almeria", "Ath Bilbao", "Ath Madrid",
                                                  "Barcelona", "Betis", "Celta",
                                                  "Eibar", "Espanol", "Getafe",      
                                                  "Granada", "La Coruna", 
                                                  "Las Palmas", "Leganes", "Levante", "Malaga",
                                                  "Mallorca", "Osasuna", "Real Madrid",
                                                  "Sevilla", "Sociedad", "Sp Gijon", 
                                                  "Valencia", "Valladolid",  "Vallecano",
                                                  "Villarreal", "Zaragoza")),
                          textOutput("output_1"), br(),
                          p("Ho: goles <= 2    Hipótesis Nula el equipo anota 2 o menos goles"), 
                          p("Ha: goles > 2    Hipótesis Alterna el equipo anota más de 2 goles"), br(),
                          textOutput("output_z0"),
                          textOutput("output_pvalue"), br(),
                          plotOutput("plot2", height = 700, width = 850),
                          br(), textOutput("output_resultado"), br()
                 ),
                 #Página 6
                 tabPanel("Probabilidades goles", icon = icon("table"),
                          h1("Probabilidades de goles como visitante"),
                          dataTableOutput ("data_table2"),
                          br(),
                          p("Tabla de probabilidades que incluye a los equipos de la Primera División de la Liga Española cuando juegan como visitantes. La tabla contiene porcentajes de que cierto equipo meta más de cierto número de goles.")
                 )
                 
)



server <- function(input, output) { 
  
  library(dplyr)
  library(fbRanks)
  library(ggplot2)
  library(ggdark)
  
  df <- read.csv("https://raw.githubusercontent.com/kotoromo/Proyecto-R-BEDU/main/match.data.csv")
  df2 <- create.fbRanks.dataframes(scores.file = "https://raw.githubusercontent.com/kotoromo/Proyecto-R-BEDU/main/match.data.csv")
  scores <- df2$scores
  
  #Output Página 1
  output$plot <- renderPlot({
    
    if (is.null(input$x))
      return(NULL)
    
    if (input$x == "Local") {
      ggplot(df,aes(home.score))+
        geom_bar(col="white",fill="purple3")+ 
        facet_wrap("home.team") +
        labs(x ="Goles de local", y = "Frecuencia") + 
        scale_x_continuous(breaks = c(0:10))+ 
        ggtitle("Liga Española Primera División")+
        ylim(0,50)+ dark_theme_gray()
    } else if (input$x == "Visitante") {
      ggplot(df,aes(away.score))+
        geom_bar(col="white",fill="deepskyblue4")+ 
        facet_wrap("away.team") +
        labs(x ="Goles de visitante", y = "Frecuencia") + 
        scale_x_continuous(breaks = c(0:10))+ 
        ggtitle("Liga Española Primera División")+
        ylim(0,50) + dark_theme_gray()
    }
    
  })

  #Output Página 2
  output$image1 <- renderImage({
    
    if (is.null(input$y))
      return(NULL)
    
    if (input$y == "Marginal Local") {
      return(list(
        src = "www/prob_marg_loc.png",
        contentType = "image/png",
        alt = "Grafica Resultado 1"
      ))
    } else if (input$y == "Marginal Visitante") {
      return(list(
        src = "www/prob_marg_vis.png",
        filetype = "image/png",
        alt = "Grafica Resultado 2"
      ))
    }
    
    else if (input$y == "Conjuntas Casa-Visitante") {
      return(list(
        src = "www/prob_conj.png",
        filetype = "image/png",
        alt = "Grafica Resultado 3"
      ))
    }
  
  })

  #Output Página 3
  output$data_table <- renderDataTable( {
    datos<-read.csv("https://raw.githubusercontent.com/kotoromo/Proyecto-R-BEDU/main/match.data.csv")
    }, 
    options = list(aLengthMenu = c(10,15,20), iDisplayLength = 10)
  )

  #Output Página 4
  output$image2 <- renderImage({
    
    if (is.null(input$z))
      return(NULL)
    
    if (input$z == "Factores de ganancia promedio") {
      return(list(
        src = "www/momios_prom.png",
        contentType = "image/png",
        alt = "Grafica Resultado 1"
      ))
    } else if (input$z == "Factores de ganancia máximo") {
      return(list(
        src = "www/momios_max.png",
        filetype = "image/png",
        alt = "Grafica Resultado 2"
      ))
    }
    
  })
  
  #Output Página 5
  output$plot2 <- renderPlot({
    
    output$output_1 <- renderText(paste("Nos interesa saber si en promedio el número de goles que anota " 
                                        , input$t, " cuando juega como local es mayor a 2."))
    scores_team = filter(scores, home.team == input$t)
    m_scores_team <- sample(scores_team$home.score, 40)
    
    prom <- mean(m_scores_team)                                                     
    desv <- sd(m_scores_team)                                                        
    n <- length(m_scores_team) 
    
    z0 <- (prom-2)/(desv/sqrt(n))
    output$output_z0 <- renderText(paste("Estadistico de prueba: ", z0))
    
    z.05 <- qnorm(p = 0.05,  lower.tail = FALSE)
    
    pvalue <- pnorm(z0, lower.tail = FALSE)
    output$output_pvalue <- renderText(paste("P-value: ", pvalue))
    
    
    if(z0 > z.05){
      output$output_resultado <- renderText(paste(
        "Como nuestro estadístico de prueba se encuentra en zona de rechazo (estadístico de prueba > valor critico), 
         existe evidencia estadística necesaria para rechazar la hipótesis nula.
         Entonces se puede afirmar con una confianza del 95% que", input$t, 
         "anotará en promedio MÁS DE DOS GOLES jugando como local."
      ))
    }else{
      output$output_resultado <- renderText(paste(
        "Como nuestro estadístico de prueba se encuentra en zona de NO rechazo (estadistico de prueba < valor critico), 
         no existe evidencia estadística necesaria para rechazar la hipótesis nula.
         Entonces se puede afirmar con una confianza del 95% que", input$t, 
        "anotará en promedio MENOS DE DOS GOLES jugando como local."
      ))
    }
    
    x <- seq(-4, 4, 0.1)
    y <- dnorm(x)
    
    ggplot(data.frame(x,y),aes(x, y)) +                                           
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
    
  })
  
  #Output Página 6
  output$data_table2 <- renderDataTable( {
    datos<-read.csv("www/Probabilidades.csv")
  }, 
  options = list(aLengthMenu = c(5,10,20), iDisplayLength = 10)
  )
  
}
 
shinyApp(ui, server)
