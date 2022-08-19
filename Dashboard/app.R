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
                          plotOutput("plot", height = 800, width = 900)
                 ),
                 #Página 2
                 tabPanel("Probabilidades marginales", icon = icon("chart-area"),
                          h1("Gráficas de probabilidades marginales de anotar goles"),
                          selectInput("y", "Seleccione el equipo que desea ver la probabilidad de anorta gol(es): ",
                                      choices = c("Marginal Local", "Marginal Visitante", "Conjuntas Casa-Visitante")),
                          imageOutput("image1")
                 ),
                 #Página 3
                 tabPanel("Resultados de partidos", icon = icon("table"),
                          h1("Resultados de partidos por fecha"),
                          dataTableOutput ("data_table")
                 ),
                 #Página 4
                 tabPanel("Factores de ganancia", icon = icon("chart-line"),
                          h1("Factores de ganancia promedio y máximo"),
                          selectInput("z", "Seleccione los factores de ganancia: ",
                                      choices = c("Factores de ganancia promedio", "Factores de ganancia máximo")),
                          imageOutput("image2")
                 )
                 
)



server <- function(input, output) { 
  
  library(ggplot2)
  library(ggdark)
  
  df <- read.csv("https://raw.githubusercontent.com/kotoromo/Proyecto-R-BEDU/main/match.data.csv")
  
  output$plot <- renderPlot({
    
    if (is.null(input$x))
      return(NULL)
    
    if (input$x == "Local") {
      ggplot(df,aes(home.score))+
        geom_bar(col="black",fill="purple3")+ 
        facet_wrap("away.team") +
        labs(x ="Goles de local", y = "Frecuencia") + 
        ggtitle("Liga Española Primera División")+
        ylim(0,50)+ dark_theme_gray()
    } else if (input$x == "Visitante") {
      ggplot(df,aes(away.score))+
        geom_bar(col="black",fill="deepskyblue4")+ 
        facet_wrap("away.team") +
        labs(x ="Goles de visitante", y = "Frecuencia") + 
        ggtitle("Liga Española Primera División")+
        ylim(0,50) + dark_theme_gray()
    }
    
    
    
  })
  
  
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
  
  
  output$data_table <- renderDataTable( {
    datos<-read.csv("https://raw.githubusercontent.com/kotoromo/Proyecto-R-BEDU/main/match.data.csv")
  }, 
  options = list(aLengthMenu = c(5,10,15), iDisplayLength = 5)
  )
  
  
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
  
}

shinyApp(ui, server)