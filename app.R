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

# fonction pour calculer l'acp

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
                             checkboxInput("oriente", "Les noms de colonnes sont t-il en première ligne ? Si oui le jeu de données
                             sera automatiquement ré-orienté",value = FALSE),
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
                                        tableOutput("test")
                               ),
                               tabPanel("Data type",
                               )
                             )
                           )
                         )
      ),
                tabPanel("ACPF",
                         sidebarLayout(
                           sidebarPanel(
                             selectInput("variable_acpf", label = "Variable à observer",
                                         choices = NULL,multiple = F),
                             selectInput("id", label = "Variable identifiant",
                                         choices = NULL,multiple = F),
                             selectInput("time", label = "Variable temps",
                                         choices = NULL,multiple = F),
                             selectInput("id_select", label = "Individu sélectionnés",
                                         choices = NULL,multiple = T)
                           ),
                           mainPanel(fluidRow(column(6, plotOutput("plot_spag")))
                           )
                         )
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
      if(input$oriente==T & tdc(donnees)==T){
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
      current_data <- data()
      var_names <- colnames(data())
      var_names_num <- var_names[sapply(current_data[var_names], is.numeric)]
      updateSelectInput(session, "variable", choices = var_names, selected = var_names[1])
      updateSelectInput(session, "variable_acpf", choices = var_names_num, selected = var_names_num[1])
      updateSelectInput(session, "id", choices = var_names, selected = if (is.null(input$id)) var_names[1] else input$id)
      updateSelectInput(session, "time", choices = var_names, selected = var_names[1])
      
      #print(input$id)
      if (!is.null(input$id) && input$id != "") {
        id_select <- unique(current_data[, input$id])
        updateSelectInput(session, "id_select", choices = id_select, selected = id_select[1])
      } else {
        # Faites quelque chose si input$id est vide ou n'est pas une colonne valide
        print("Invalid or empty column name selected.")
      }
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
    output$plot_spag<- renderPlot({
      data_spag <- data()
      #print(data_spag)
      idSelect <- list(as.numeric(input$id_select))
      # print(idSelect)
      # data_spag <- filter(data_spag, !!sym(input$id) %in% id_select)
      ggplot(data_spag, aes(x=!!sym(input$time) ,y=!!sym(input$variable_acpf), group=!!sym(input$id), color = !!sym(input$id))) +
        geom_line() + 
        labs(title = paste("SpaghettiPlot représentant la", input$variable_acpf, "chez différents individus"),
             x = "Temps",
             y = input$variable_acpf,
             color="Individus") +
        theme_bw()
    })
  }
  
  # Run the application 
  shinyApp(ui = ui, server = server)
