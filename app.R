#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(skimr)
library(dplyr)
library(tidyverse)

#tests
x <- c(1,2,3,4,5)
y <- c(2,5,9,7,8)

graph1 = ggplot(data= data.frame(x,y), mapping = aes(x,y)) + geom_point()
graph2 = ggplot(data= data.frame(c(1,2,3,4,5),c(9,8,5,7,2)), mapping = aes(x,y)) + geom_point()
graph3 = ggplot(data= data.frame(x = c(1,2,3,4,5),y=c(4,3,5,1,6)), mapping = aes(x,y)) + geom_point()
plot(graph3)

# fonction pour vérifier que les variables du jeu de données sont toutes numériques

tdc <- function(x){
  list <- data.frame("test"="")
  for (i in 2:length(x)) {
    test <- is.numeric(x[,i])
    list <- rbind(list, test)
  }
  if(count(list %>% 
           filter(test == F))>0){
    tap <- FALSE
  }else{tap <- TRUE}
  return(tap)
}
  # Define UI for application that draws a histogram
  ui <- fluidPage(
    tabsetPanel(tabPanel("Importation et visualisation des données",
                         sidebarLayout(
                           sidebarPanel(
                             # jeu de données
                             fileInput("file1", "Choose csv File", accept = ".csv"),
                             checkboxInput("header", "Header", TRUE),
                             # Nom d'une première colonne ?
                             # Si le jeu de donnée est mal orienté
                             checkboxInput("oriante", "Y'a t-il un nom de première colonne ? Si oui le jeu de données
                    sera ré-orianté automatiquement",value = FALSE),
                             # variables présentent dans le jeu de données et à transformer
                             selectInput("variable",label = "Variables à transformer en facteur",
                                         choices = NULL,multiple = T),
                           ),
                           mainPanel(
                             tabsetPanel(
                               tabPanel("Data validation",
                                        # visualisation du jeu de données
                                        dataTableOutput("dataframe")
                               ),
                               tabPanel("Data visualisation",
                                        #test pour savoir où je dois mettre quoi
                                        fluidRow(
                                          column(12,
                                                 p("Voici les données visualisées:")
                                          )
                                        ),
                                        flowLayout(
                                          numericInput("rows", "How many rows?", 5),
                                          selectInput("letter", "Which letter?", LETTERS),
                                          sliderInput("value", "What value?", 0, 100, 50)),
                                        tableOutput("test")
                               ),
                               tabPanel("Data type",
                               )
                             )
                           )
                         )
    ),
    tabPanel("ACPF",
             #test pour savoir où je mets quoi
             fluidRow(
               column(12,
                      p("Voici les données visualisées:")
               )
             ),
             flowLayout(
               numericInput("rows", "How many rows?", 5),
               selectInput("letter", "Which letter?", LETTERS),
               sliderInput("value", "What value?", 0, 100, 50)
               ),
             fluidPage(
               plotOutput("graphique_affiche") #essai graphe
             ),
             tableOutput("test")
    )
    )
  )
  # Define server logic required to draw a histogram
  server <- function(input, output, session) {
    # créer un jeu de données réactive
    data <- reactive({
      file <- input$file1
      ext <- tools::file_ext(file$datapath)
      req(file)
      validate(need(ext == "csv", "Please upload a csv file"))
      donnees <- read.csv(file$datapath, header = input$header)
      if(input$oriante==T & tdc(donnees)==T){
        donnees <- read.csv(file$datapath, header = input$header)
        donnees <- donnees %>% 
          pivot_longer(!colnames(donnees[1]), names_to = "ID", values_to = "Value")
        }
      return(donnees)
    })
    output$dataframe <- renderDataTable({
      data()
    })
    # modifie l'input variable
    observe({
      data()
      var_names <- colnames(data())
      updateSelectInput(session, "variable", choices = var_names, selected = var_names[1])
    })
    # visualisation des données
    output$test <- renderTable({
      data2 <- data() %>% 
        mutate_at(input$variable, as.factor)
      skim(data2)
      # Donner automatiquement le type de donnée à l'utilisateur (sparse ou dense)
      # regarder le nombres d'occurences de l'ID Si pas le même alors données éparses
      # Regarder le nombre de NA 
      # demander à l'utilisateur la variable ID, la variable numérique et la variable temps
      
    })
    output$graphique_affiche <- renderPlot({
      plot(x, y, type = "o", col = "blue", xlab = "X", ylab = "Y", main = "Graphique de X par rapport à Y") #essai
    })
    
  }
  
  # Run the application 
  shinyApp(ui = ui, server = server)






