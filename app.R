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
library(fdapace)

# fonction pour calculer l'acp

# fonction pour vérifier que les variables du jeu de données sont toutes numériques

na_person <- function(x,y){
  data_na <- data.frame()
  for (i in 1:length(x)) {
    ttt <- x %>% 
      mutate(test = is.na(x[,i])==T) %>% 
      mutate(test2 = ifelse(test==T,1,0)) %>% 
      group_by(eval(parse(text = y))) %>% #eval(parse()) c'est pour enlever les guillements de l'objet y
      summarise(test = sum(test2))
    
    na_per_pers_min <- min(ttt$test)
    na_per_pers_max <- max(ttt$test)
    na_per_pers_med <- median(ttt$test)
    na_per_pers_moy <- round(mean(ttt$test),2)
    skim_variable <- colnames(x[i])
    
    new_data <- data.frame(skim_variable,na_per_pers_min,
                           na_per_pers_max,
                           na_per_pers_med,
                           na_per_pers_moy)
    data_na <- rbind(data_na,new_data)
  }
      return(data_na)
}

IsRegular = function(t){
  
  # Check the data type in terms of dense-sparse. Classification is dense (2), or  data with missing values (1) or sparse (0) data
  # t : n-by-1 list of vectors 
  
  tt = unlist(t);
  f = length(tt)/length(unique(tt))/length(t);
  if (f == 1){
    if(length(unique(tt))<8){ #In case of low number of observations per subject
      return('Sparse');
    }
    else{
      return('Dense'); # for either regular and irregular data
    }
  } else if(f > 0.80){
    return('DenseWithMV');
  } else {
    return('Sparse');
  }
}

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

acpf <- function(data, variable, id="id", time="year", obs_min = 2, threshold = 0.99){
  if (!is.data.frame(data)){
    stop("Le dataframe n'existe pas, est vide, ou n'est pas un dataframe.")
  }
  if (!(variable %in% colnames(data))){
    stop("La variable spécifiée n'existe pas dans le dataframe.")
  }
  
  data <- data %>% filter(!is.na(.data[[variable]]))
  at_least <- unlist(data %>% group_by(.data[[id]]) %>% group_map(~sum(!is.na(.x[[variable]]))>=obs_min))

  
  if (is.numeric(data[[id]])==T){
    data_var <- data %>% filter(.data[[id]] %in% sort(unique(data$id))[at_least])
    data_list <- split(data_var, f = data_var$id)
  } else{
    data_var <- data %>% filter(.data[[id]] %in% sort(unique(data[[id]])[at_least]))
    data_list <- split(data_var, f = data_var[[id]])
  }
  data_Ly <- lapply(data_list, function(.x) return(.x[[variable]]))
  data_Lt <- lapply(data_list, function(.x) return(.x[[time]]))
  
  acpf_ <- FPCA(data_Ly, data_Lt, list(FVEthreshold = threshold))
  return(list(acpf = acpf_, data_obs = data_Ly, time = data_Lt))
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
                             selectInput("id", label = "Variable identifiant",
                                         choices = NULL,multiple = F),
                             selectInput("variable_acpf", label = "Variable à observer",
                                         choices = NULL,multiple = F),
                             selectInput("time", label = "Variable temps",
                                         choices = NULL,multiple = F),
                             selectInput("id_select", label = "Individu sélectionnés",
                                         choices = NULL,multiple = T),
                             selectInput("id_select_score", label = "Individus sélectionné pour le calcul du score",
                                         choices = NULL,multiple = F),
                             actionButton("plotButton", "Plot"),
                             actionButton("selectAllButton", "Sélectionner tous les individus")
                           ),
                           mainPanel(
                             fluidRow(
                               column(6, plotOutput("plot_spag")),
                               column(6, plotOutput("plot_mu")),
                               column(6, plotOutput("plot_score")))
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
        print("Individu non selectionné ou incorrect")
      }
      
      if (!is.null(input$id) && input$id != "") {
        id_select_score <- unique(current_data[, input$id])
        updateSelectInput(session, "id_select_score", choices = id_select_score)
      } else {
        print("Individu non selectionné ou incorrect")
      }
      
    })
    # visualisation des données
    output$test <- renderTable({
      data2 <- data() %>% 
        mutate_at(input$variable, as.factor)
      
      # merge les stats les deux fonctions pour les stats g et les na par personne
      merge(skim(data2), na_person(data2,input$variable_id), by.x = "skim_variable")
    })
    observeEvent(input$plotButton, {
      data_spag <- data()
      
      data_acpf <- data()
      # print(data_acpf)
      acpfVar <- input$variable_acpf
      idVar <- input$id
      timeVar <- input$time
      acpf_obj <- acpf(data_acpf, acpfVar, id=idVar, time=timeVar, obs_min = 2, threshold = 0.99)
      
      idSelect <- input$id_select
      data_spag <- filter(data_spag, !!sym(input$id) %in% idSelect)
      output$plot_spag <- renderPlot({
        ggplot(data_spag, aes(x=!!sym(input$time) ,y=!!sym(input$variable_acpf), group=!!sym(input$id), color = !!sym(input$id))) +
          geom_line() + 
          labs(title = paste("SpaghettiPlot représentant la", input$variable_acpf, "chez différents individus"),
               x = input$time,
               y = input$variable_acpf,
               color="Individus") +
          theme_bw()
      })
      
      acpf_mu_df <- as.data.frame(acpf_obj$acpf$mu) %>% rename(mu = 'acpf_obj$acpf$mu')
      acpf_mu_df <- acpf_mu_df %>% mutate(temps = seq(1, n(), 1))
      output$plot_mu <- renderPlot({
        ggplot(acpf_mu_df, aes(x = temps, y = mu)) +
          geom_line() +
          labs(title = "Fonction mu de l'ACPF", 
               x = "Temps", y = sprintf("mu de la variable %s", input$variable_acpf)) +
          theme_minimal()
      })
      

      scores <- acpf_obj$acpf$xiEst %*% t(acpf_obj$acpf$phi) + matrix(rep(acpf_obj$acpf$mu, times = length(acpf_obj$data_obs)), nrow = length(acpf_obj$data_obs), byrow = TRUE)
      liste_id <- sort(unique(data_acpf[[idVar]]))
      liste_id_df <- as.data.frame(liste_id)
      liste_id_df <- liste_id_df %>% mutate(id_num = seq(1, n(), 1))
      position <- which(liste_id_df$liste_id %in% input$id_select_score)
      # print(selected_row)
      indivPlot <- liste_id_df[position, "id_num"]
      # print(indivPlot)acpf_obj
      
      indivScore <- scores[as.numeric(indivPlot),] # necessite de mettre en place un index pour les identifiants de type string
      indivScore_df <- as.data.frame(indivScore)
      indivScore_df <- indivScore_df %>% mutate(temps = seq(1, n(), 1))
      indivPlot_dataobs <- acpf_obj$data_obs[indivPlot]
      indivPlotdataObs <- as.data.frame(indivPlot_dataobs)
      names(indivPlotdataObs)[1] <- "Valeur"
      indivPlotdataObs <- indivPlotdataObs %>% mutate(temps = seq(1, n(), 1))
      # print(indivPlotdataObs)
      output$plot_score <- renderPlot({
        ggplot(indivScore_df, aes(x = temps, y = indivScore)) +
          geom_line() +
          geom_point(data = indivPlotdataObs, aes(x = temps, y = Valeur), color = "red") +
        theme_minimal()
      })
    })
  }
  
  # Run the application 
  shinyApp(ui = ui, server = server)

  
# Erreur lors de la transposition. Une fois le jeu de données transposé et les graph tracés,
# crash quand il y a détransposition ou quand l'on importe un autre df