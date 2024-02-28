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
                           checkboxInput("pivoter", label ="Voulez vous faire pivoter le tableau ?",value = FALSE),
                           selectInput("var", 
                                       label = "Choix de la variable à observer : ",
                                       choices = NULL),
                           selectInput("temps", 
                                       label = "Choix de la variable temps : ",
                                       choices = NULL),
                           selectInput("id", 
                                       label = "Choix de la variable individu: ",
                                       choices = NULL),
                           
                           selectInput("ind",label = "Choix des individus : ",
                                       choices = NULL, multiple = T),
                           
                           radioButtons("choix", label="Affichage des Composantes Principales en fonction : ",choices = list("Nombre de CP :" = 1, "PVE" = 2),selected = 2),
                           conditionalPanel(
                             condition = "input.choix == 1",
                             numericInput("nbCP",label="Nombre de CP :", value = 3, min = 1, max = 6)
                           ),
                           conditionalPanel(
                             condition = "input.choix == 2",
                             numericInput("PVE",label=" % de variances expliqué :", value = 99, min =50 , max = 100, step=0.01)
                           ),
                           checkboxInput("valider", "valider",FALSE)

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
                             tabPanel("plot",plotOutput("spag_plot"),plotOutput("plot_mu"), plotOutput("phi")
                             )
                           )
                         )
                       )
  ),
  tabPanel("ACPF")
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
    if(input$pivoter==T & tdc(donnees)==T){
      donnees <- read.csv(file$datapath, header = input$header)
      donnees <- donnees %>% 
        pivot_longer(colnames(donnees)[3:ncol(donnees)], names_to = "ID", values_to = "Value")
    }
    return(donnees)
  })
  # modifie l'input variable
  
    observe({
      data()
      var_names <- colnames(data())
      var_ind <- unique(data()[[input$id]])
      var <- input$var
      id <- input$id
      temps <- input$temps
      var_test <- updateSelectInput(session, "var", choices = var_names, selected = var)
      updateSelectInput(session, "id", choices = var_names, selected = id)
      updateSelectInput(session, "temps", choices = var_names, selected = temps)
      if ("tous" %in% input$ind) {
        ind<- data()[[input$id]]# Tous les individus
      } else if ("aucun" %in% input$ind){
        ind<-NULL
      }else {
        ind <- input$ind  # Individus spécifiquement sélectionnés
      }
      updateSelectInput(session, "ind", choices = c(tous="tous",aucun="aucun",var_ind), selected = ind)
      #updateSelectInput(session, "valider", selected = FALSE)
      
    })
    output$dataframe <- renderDataTable({
      data()
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
  output$plot_mu<- renderPlot({
    source("fonction_anais.R")
    donnees<-as.data.frame(data())
    acpf <- acpf(data=donnees, variable=input$var,id=input$id,time=input$temps)
    plot(acpf$mu,type="l", xlab="temps", ylab=input$var, main=" fonction mu de l'ACPF")

  })
  output$spag_plot<- renderPlot({
    tableau_ville <-filter(data(), data()[[input$id]] %in% input$ind)
    ggplot(tableau_ville, aes(x=tableau_ville[[input$temps]] ,y=tableau_ville[[input$var]], group=tableau_ville[[input$id]], color = tableau_ville[[input$id]])) +
      geom_line() + 
      labs(title = paste("Diagramme représentant", input$var, "dans différentes provinces"),
           x = "Temps",
           y = input$var,
           color="individus") +
      theme_bw()
    
  })
  output$phi<- renderPlot({
    acpf_ylim <- acpf(data=data(), variable=input$var,threshold=1,id=input$id,time=input$temps)
    min_y<-min(acpf_ylim$phi)
    max_y<-max(acpf_ylim$phi)
    if (input$choix==1){
      source("fonction_anais.R")
      donnees<-as.data.frame(data())
      acpf <- acpf(data=donnees, variable=input$var,methode=input$nbCP,id=input$id,time=input$temps)
      phi <- as.data.frame(acpf$phi)
      colnames(phi) <-1:ncol(phi)
      phi <- cbind("temps"=1:nrow(phi),phi)
      phi_pivoter<- pivot_longer(phi, cols = colnames(phi)[2:(input$nbCP+1)], names_to = "CP")
      ggplot(phi_pivoter, aes(x=phi_pivoter$temps ,y=phi_pivoter$value, group=phi_pivoter$CP, color = phi_pivoter$CP)) +
        geom_line() + 
        labs(title ="Diagramme représentant les composantes principales",
             x = "Temps",
             y = paste("Variation de ", input$var),
             color="CP",
             caption=paste("Le pourcentage de variance expliquée est de ", round(acpf$FVE*100,2), "%.")) +
        ylim(min_y,max_y)+
        theme_bw()+
        theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5), plot.caption = element_text(size = 13, hjust =0))
        
    }
    
    else if (input$choix==2){
      source("fonction_anais.R")
      donnees<-as.data.frame(data())
      acpf <- acpf(data=donnees, variable=input$var,threshold=input$PVE/100,id=input$id,time=input$temps)      
      phi <- as.data.frame(acpf$phi)
      colnames(phi) <-1:ncol(phi)
      phi <- cbind("temps"=1:nrow(phi),phi)
      phi_pivoter<- pivot_longer(phi, cols = colnames(phi)[2:ncol(phi)], names_to = "CP")
      ggplot(phi_pivoter, aes(x=phi_pivoter$temps ,y=phi_pivoter$value, group=phi_pivoter$CP, color = phi_pivoter$CP)) +
        geom_line() + 
        labs(title = paste("Diagramme représentant les composantes principales"),
             x = "Temps",
             y = paste("Variation de ", input$var),
             color="CP") +
        ylim(min_y,max_y)+
        theme_bw()+
        theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5))
    }
    
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

#choix de l'individu par l'utilisateur et du temps 
#les données temps de type %b%d devront etre pre traité donc le jeu CanadianWeather du coup sera transformée 
#on fait le pivot dans l'application shiny
#je change mon code avec les nouveaux présupposés
#1 transformer canadian weather ok
#2 changer la fonction avec la possibilité de pivoter ok
#3 rajouter choix ind / choix temps ok
