# 0 - INFOS
# Dans le code le "score" sont les estimations individuelles de la fonction d'un individu --> A corriger
# Il y a un bug dans la sélection des individus pour les jeu de données sparses, en particulier ceux où des individus n'ont que des NA

# 1 - CHARGEMENT DES PACKAGES
library(shiny)
library(skimr)
library(dplyr)
library(tidyverse)
library(fdapace)
library(knitr)
library(kableExtra)
library(shinydashboard)
library(shinyjs)
library(shinyalert)


# 2 - FONCTIONS NECESSAIRES A L'APPLICATION
# fonction de calcul des NA du jeu de données
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

# fonction isRegular() --> commentaire en anglais car code importé
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

# fonction de calcul de l'acp
acpf <- function(data, variable, id="id", time="year", obs_min = 2, threshold = 0.99, methode='FVE'){
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
  
  acpf_ <- FPCA(data_Ly, data_Lt, list(FVEthreshold = threshold, methodSelectK=methode))
  return(list(acpf = acpf_, data_obs = data_Ly, time = data_Lt))
}

# 3 - APPLICATION
  # UI
  ui <- dashboardPage(title = "Acpf app",skin = "black",
    dashboardHeader(title = tags$a(href='http://www.isped.u-bordeaux.fr/',
                                           tags$img(src='isped.png', height="90%", width="90%")),
                    tags$li(
                      class = "dropdown",
                      actionButton("flag_fr", label = "", icon = icon(name = NULL, class = "custom_icon")),
                      actionButton("flag_uk", label = "", icon = icon(name = NULL, class = "custom_icon2")),
                      actionButton("flag_br", label = "", icon = icon(name = NULL, class = "custom_icon3")),
                      tags$script("$(document).ready(function() { 
                    $('#flag_fr').parent().addClass('dropdown-toggle');
                    $('#flag_fr').parent().attr('data-toggle', 'dropdown'); 
                  });")
                    )),
    dashboardSidebar(disable=TRUE),
    dashboardBody(
      uiOutput("content")
      )
  )

# Réactive pour suivre la langue sélectionnée
language <- reactiveVal("fr")  # Par défaut, la langue est le français

  # SERVER
  server <- function(input, output, session) {

    # Observer pour mettre à jour la langue lorsque les boutons sont cliqués
    observeEvent(input$flag_fr, {
      # observe({language <- reactiveVal("fr")})
      session$reload()
      language("fr")
    })
    observeEvent(input$flag_uk, {
      # observe({language <- reactiveVal("uk")})
      session$reload()
      language("uk")
    })
    observeEvent(input$flag_br, {
      # observe({language <- reactiveVal("uk")})
      session$reload()
      language("br")
    })
    
    output$content <- renderUI({
      lang <- language()
      if (lang == "fr") {
        tags$div(
          style = "height: 95vh; overflow-y: auto;",
          tags$head(tags$style(HTML('
      .content-wrapper {
        background-color: #fff;
      }
    '))),
          includeCSS(file.path("www", "custom_icon.css")),
          fluidPage(
            tabsetPanel(
              tabPanel("Importation et visualisation des données",
                       sidebarLayout(
                         sidebarPanel(
                           fileInput("file1", "Choisir un fichier csv", accept = ".csv", buttonLabel = "Parcourir...", placeholder = "Aucun fichier sélectionné"),
                           checkboxInput("header", "La première ligne contient-elle les noms de colonne ?", TRUE),
                           checkboxInput("oriente", "Pivoter le jeu de données", value = FALSE),
                           selectInput("id", label = "Variable identifiant", choices = NULL, multiple = FALSE),
                           selectInput("variable", label = "Variables à transformer en facteur", choices = NULL, multiple = TRUE)
                         ),
                         mainPanel(
                           tabsetPanel(
                             tabPanel("Validation des données",
                                      dataTableOutput("dataframe")
                             ),
                             tabPanel("Visualisation des données",
                                      tableOutput("test")
                             )
                           )
                         )
                       )
              ),
              tabPanel("ACPF",
                       sidebarLayout(
                         sidebarPanel(
                           selectInput("variable_acpf", label = "Variable d'intérêt", choices = NULL, multiple = FALSE),
                           selectInput("time", label = "Variable temps (de type numérique)", choices = NULL, multiple = FALSE),
                           numericInput("nbInput", label = "Nombre d'observations minimum pour intégrer un individu dans l'ACPF :", value = 2, min = 1 , step = 1),
                           selectInput("id_select", label = "Individu(s) sélectionné(s) pour le spaghetti plot", choices = NULL, multiple = TRUE),
                           selectInput("id_select_score", label = "Individu sélectionné pour le calcul des estimations individuelles", choices = NULL, multiple = FALSE),
                           radioButtons("choix", label = "Calcul de l'ACPF selon le : ", choices = list("Nombre de CP" = 1, "% de variance expliquée" = 2), selected = 2),
                           conditionalPanel(
                             condition = "input.choix == 1",
                             numericInput("nbCP", label = "Nombre de CP :", value = 3, min = 1, max = 20)
                           ),
                           conditionalPanel(
                             condition = "input.choix == 2",
                             numericInput("PVE", label = "% de variance expliquée :", value = 99, min = 50 , max = 100, step = 0.01)
                           ),
                           radioButtons("typeTrace", "Type de tracé pour les observations individuelles",
                                        choices = list("Ponctuel" = "etoiles", "Ligne" = "ligne"),
                                        selected = "etoiles"
                           ),
                           actionButton("plotButton", "C'est parti !")
                         ),
                         mainPanel(
                           fluidRow(
                             column(6, plotOutput("plot_spag")), # SpaghettiPlot
                             column(6, plotOutput("plot_mu")), # Plot de mu
                             column(6, plotOutput("plot_score")), # Plot des estimations individuelles
                             column(6, plotOutput("plot_phi")), # Plot des composantes principales
                             column(12, plotOutput("plot_varExpPVE")), # Plot des variances expliquées par chaque CP
                             column(12, plotOutput("plot_contrib_individu")) # Plot des contribution de chaque CP à la projection d'un individu
                           )
                         )
                       )
              )
            )
          )
        )
      } else if (lang == "uk"){
        tags$div(
          style = "height: 95vh; overflow-y: auto;",
          tags$head(tags$style(HTML('
      .content-wrapper {
        background-color: #fff;
      }
    '))),
          includeCSS(file.path("www", "custom_icon.css")),
          fluidPage(
            tabsetPanel(
              tabPanel("Import and visualize data",
                       sidebarLayout(
                         sidebarPanel(
                           fileInput("file1", "Choose a csv file", accept = ".csv", buttonLabel = "Browse...", placeholder = "No file selected"),
                           checkboxInput("header", "Does the first row contain column names?", TRUE),
                           checkboxInput("oriente", "Rotate data set", value = FALSE),
                           selectInput("id", label = "Identifier variable", choices = NULL, multiple = FALSE),
                           selectInput("variable", label = "Variables to transform into factor", choices = NULL, multiple = TRUE)
                         ),
                         mainPanel(
                           tabsetPanel(
                             tabPanel("Data validation",
                                      dataTableOutput("dataframe")
                             ),
                             tabPanel("Data visualization",
                                      tableOutput("test")
                             )
                           )
                         )
                       )
              ),
              tabPanel("FPCA",
                       sidebarLayout(
                         sidebarPanel(
                           selectInput("variable_acpf", label = "Variable of interest", choices = NULL, multiple = FALSE),
                           selectInput("time", label = "Time variable (numeric type)", choices = NULL, multiple = FALSE),
                           numericInput("nbInput", label = "Minimum number of observations to include an individual in the ACPF:", value = 2, min = 1 , step = 1),
                           selectInput("id_select", label = "Individual(s) selected for spaghetti plot", choices = NULL, multiple = TRUE),
                           selectInput("id_select_score", label = "Individuals selected for estimating calculations", choices = NULL, multiple = FALSE),
                           radioButtons("choix", label = "ACPF calculation according to:", choices = list("Number of CP :" = 1, "PVE" = 2), selected = 2),
                           conditionalPanel(
                             condition = "input.choix == 1",
                             numericInput("nbCP", label = "Number of CP :", value = 3, min = 1, max = 20)
                           ),
                           conditionalPanel(
                             condition = "input.choix == 2",
                             numericInput("PVE", label = " % variance explained :", value = 99, min = 50 , max = 100, step = 0.01)
                           ),
                           radioButtons("typeTrace", "Type of plot for individual observations",
                                        choices = list("Point" = "etoiles", "Line" = "ligne"),
                                        selected = "etoiles"
                           ),
                           actionButton("plotButton", "Let's go !")
                         ),
                         mainPanel(
                           fluidRow(
                             column(6, plotOutput("plot_spag")), # SpaghettiPlot
                             column(6, plotOutput("plot_mu")), # Plot of mu
                             column(6, plotOutput("plot_score")), # Plot of individual estimates
                             column(6, plotOutput("plot_phi")), # Plot of principal components
                             column(12, plotOutput("plot_varExpPVE")), # Plot of variances explained by each CP
                             column(12, plotOutput("plot_contrib_individu")) # Plot of contribution of each CP to the projection of an individual
                           )
                         )
                       )
              )
            )
          )
        )
      } else {
        tags$div(
          style = "height: 95vh; overflow-y: auto;",
          tags$head(tags$style(HTML('
      .content-wrapper {
        background-color: #fff;
      }
    '))),
          includeCSS(file.path("www", "custom_icon.css")),
          fluidPage(
            tabsetPanel(
              tabPanel("Enporzhiañ ha gwelet ar roadennoù",
                       sidebarLayout(
                         sidebarPanel(
                           fileInput("file1", "Dibab ur restr csv", accept = ".csv", buttonLabel = "Furchal...", placeholder = "Restr diuzet ebet"),
                           checkboxInput("header", "Hag al linenn gentañ zo enni anvioù ar bann?", TRUE),
                           checkboxInput("oriente", "Stouiñ ar strobad roadennoù", value = FALSE),
                           selectInput("id", label = "Kemmañ arouez", choices = NULL, multiple = FALSE),
                           selectInput("variable", label = "Kemmoù da dreuzfurmiñ e faktorioù", choices = NULL, multiple = TRUE)
                         ),
                         mainPanel(
                           tabsetPanel(
                             tabPanel("Kadarnaat ar roadennoù",
                                      dataTableOutput("dataframe")
                             ),
                             tabPanel("Gwelet ar roadennoù",
                                      tableOutput("test")
                             )
                           )
                         )
                       )
              ),
              tabPanel("ACPF",
                       sidebarLayout(
                         sidebarPanel(
                           selectInput("variable_acpf", label = "Gwelet ar roadennoù", choices = NULL, multiple = FALSE),
                           selectInput("time", label = "Cheñchamantoù amzer (evel niverel)", choices = NULL, multiple = FALSE),
                           numericInput("nbInput", label = "Niver izek a arselladennoù evit lakaat un den e-barzh an ACPF :", value = 2, min = 1 , step = 1),
                           selectInput("id_select", label = "Hinienn(où) diuzet evit ar spaghetti plot", choices = NULL, multiple = TRUE),
                           selectInput("id_select_score", label = "Hiniennoù diuzet evit jediñ ar priziadennoù hiniennel", choices = NULL, multiple = FALSE),
                           radioButtons("choix", label = "Jediñ an ACPF hervez :", choices = list("Niver a GP :" = 1, "PVE" = 2), selected = 2),
                           conditionalPanel(
                             condition = "input.choix == 1",
                             numericInput("nbCP", label = "Niver a GP :", value = 3, min = 1, max = 20)
                           ),
                           conditionalPanel(
                             condition = "input.choix == 2",
                             numericInput("PVE", label = "% a gemmoù displeget :", value = 99, min = 50 , max = 100, step = 0.01)
                           ),
                           radioButtons("typeTrace", "Seurt tres evit an evezhiadennoù hiniennel",
                                        choices = list("Poentel" = "etoiles", "Linenn" = "ligne"),
                                        selected = "etoiles"
                           ),
                           actionButton("plotButton", "Aet eo kuit!")
                         ),
                         mainPanel(
                           fluidRow(
                             column(6, plotOutput("plot_spag")), # SpaghettiPlot
                             column(6, plotOutput("plot_mu")), # Plot de mu
                             column(6, plotOutput("plot_score")), # Plot des estimations individuelles
                             column(6, plotOutput("plot_phi")), # Plot des composantes principales
                             column(12, plotOutput("plot_varExpPVE")), # Plot des variances expliquées par chaque CP
                             column(12, plotOutput("plot_contrib_individu")) # Plot des contribution de chaque CP à la projection d'un individu
                           )
                         )
                       )
              )
            )
          )
        )
      }
    })
    
    data <- reactive({
      lang <- language()
      file <- input$file1
      ext <- tools::file_ext(file$datapath)
      req(file)
      validate(need(ext == "csv", "Please upload a csv file"))
      donnees <- read.csv(file$datapath, header = input$header)
      if(input$oriente==T & tdc(donnees)==T){
        donnees <- read.csv(file$datapath, header = input$header)
        if (lang=="fr"){
          donnees <- donnees %>% 
            pivot_longer(!colnames(donnees[1]), names_to = "ID", values_to = "Valeurs")
        } else if (lang=="uk") {
          donnees <- donnees %>% 
            pivot_longer(!colnames(donnees[1]), names_to = "ID", values_to = "Values")
        } else {
          donnees <- donnees %>% 
            pivot_longer(!colnames(donnees[1]), names_to = "ID", values_to = "Talvoudoù")
        }
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
      var_names_num <- var_names[sapply(current_data[var_names], is.numeric)] # récupère les noms de variables du dataframe
      
      # mis à jour des champs de sélection
      updateSelectInput(session, "variable", choices = var_names, selected = var_names[1]) # nom des variables pour les transformer en facteur
      updateSelectInput(session, "id", choices = var_names, selected = if (is.null(input$id)) var_names[1] else input$id)
      updateSelectInput(session, "variable_acpf", choices = var_names_num, selected = var_names_num[1])
      updateSelectInput(session, "time", choices = var_names, selected = var_names[1])
      
      lang <- language()
      
      if (lang == "fr"){
        if (!is.null(input$id) && input$id != ""){
          observe({
            current_data_bis <- data.frame(id = current_data[[input$id]], temps = current_data[[input$time]], variable = current_data[[input$variable_acpf]])
            current_data_bis <- current_data_bis %>% group_by(id) %>% filter(all(sum(!is.na(variable)) >= input$nbInput))
            
            observe({
              id_select <- unique(current_data_bis$id)
              if ("tous" %in% input$id_select) {
                ind <- current_data_bis$id# Tous les individus
              } else if ("aucun" %in% input$id_select){
                ind<-NULL
              }else {
                ind <- input$id_select  # Individus spécifiquement sélectionnés
              }
              
              
              observe({
                updateSelectInput(session, "id_select", choices = c(tous="tous",aucun="aucun",id_select), selected = ind)
                # updateSelectInput(session, "id_select", choices = id_select) #, selected = id_select[1])
                updateSelectInput(session, "id_select_score", choices = id_select)
              })
            })
          })
        }
      } else if (lang=="uk") {
        if (!is.null(input$id) && input$id != ""){
          observe({
            current_data_bis <- data.frame(id = current_data[[input$id]], temps = current_data[[input$time]], variable = current_data[[input$variable_acpf]])
            current_data_bis <- current_data_bis %>% group_by(id) %>% filter(all(sum(!is.na(variable)) >= input$nbInput))
            
            observe({
              id_select <- unique(current_data_bis$id)
              if ("all" %in% input$id_select) {
                ind <- current_data_bis$id# Tous les individus
              } else if ("none" %in% input$id_select){
                ind<-NULL
              }else {
                ind <- input$id_select  # Individus spécifiquement sélectionnés
              }
              
              
              observe({
                updateSelectInput(session, "id_select", choices = c(all="all",none="none",id_select), selected = ind)
                # updateSelectInput(session, "id_select", choices = id_select) #, selected = id_select[1])
                updateSelectInput(session, "id_select_score", choices = id_select)
              })
            })
          })
        }
      } else {
        if (!is.null(input$id) && input$id != ""){
          observe({
            current_data_bis <- data.frame(id = current_data[[input$id]], temps = current_data[[input$time]], variable = current_data[[input$variable_acpf]])
            current_data_bis <- current_data_bis %>% group_by(id) %>% filter(all(sum(!is.na(variable)) >= input$nbInput))
            
            observe({
              id_select <- unique(current_data_bis$id)
              if ("Holl" %in% input$id_select) {
                ind <- current_data_bis$id# Tous les individus
              } else if ("Hini ebet" %in% input$id_select){
                ind<-NULL
              }else {
                ind <- input$id_select  # Individus spécifiquement sélectionnés
              }
              
              
              observe({
                updateSelectInput(session, "id_select", choices = c(Holl="Holl",`Hini ebet`="Hini ebet",id_select), selected = ind)
                # updateSelectInput(session, "id_select", choices = id_select) #, selected = id_select[1])
                updateSelectInput(session, "id_select_score", choices = id_select)
              })
            })
          })
        }
      }
      
      # Nouvelle version
      
      
      # Ancienne version
      # if (!is.null(input$id) && input$id != ""){
      #   
      #   id_select <- unique(current_data[,input$id])
      #   id_select_score <- unique(current_data[,input$id])
      #   updateSelectInput(session, "id_select", choices = id_select, selected = id_select[1])
      #   updateSelectInput(session, "id_select_score", choices = id_select_score)
      # }
      
    })
    
    output$test <- renderTable({
      lang <- language()
      data2 <- data() %>%
        mutate_at(input$variable, as.factor)
      if (lang == "fr") {
        df1 <- skim(data2)
        if (any(grepl("factor", df1$skim_type))) {
          df1 <- df1 %>%
            dplyr::select(-factor.ordered) %>%
            rename("Nombre de modalités" = factor.n_unique) %>%
            rename("Observation(s) la(les) plus fréquente(s)" = factor.top_counts)
        }
        if (any(grepl("numeric", df1$skim_type))) {
          df1 <- df1 %>%
            dplyr::select(-c(numeric.p25, numeric.p75)) %>%
            rename(Moyenne = numeric.mean) %>%
            rename("Écart-type" = numeric.sd) %>%
            rename(Minimum = numeric.p0) %>%
            rename(Médiane = numeric.p50) %>%
            rename(Maximum = numeric.p100) %>%
            rename(Histogramme = numeric.hist)
        }
        if (any(grepl("character", df1$skim_type))) {
          df1 <- df1 %>%
            dplyr::select(-c(character.min, character.max, character.empty, character.n_unique, character.whitespace))
        }
        df2 <- na_person(data2, input$id)
        
        df1 <- df1 %>%
          rename(Variable = skim_variable) %>%
          rename(Type = skim_type) %>%
          rename("Données manquantes" = n_missing) %>%
          rename("Taux de données présentes" = complete_rate)
        
        df2 <- df2 %>%
          rename(Variable = skim_variable) %>%
          rename("Nombre de données manquantes minimum par individu" = na_per_pers_min) %>%
          rename("Nombre de données manquantes maximum par individu" = na_per_pers_max) %>%
          rename("Nombre médian de données manquantes par individu" = na_per_pers_med) %>%
          rename("Nombre moyen de données manquantes par individu" = na_per_pers_moy)
        
        # Fusionne les statistiques des deux fonctions pour les statistiques générales et les NA par personne
        merge(df1, df2, by.x = "Variable")
      } else if (lang=="uk") {
        df1 <- skim(data2)
        if (any(grepl("factor", df1$skim_type))) {
          df1 <- df1 %>%
            dplyr::select(-factor.ordered) %>%
            rename("Number of levels" = factor.n_unique) %>%
            rename("Most frequent observation(s)" = factor.top_counts)
        }
        if (any(grepl("numeric", df1$skim_type))) {
          df1 <- df1 %>%
            dplyr::select(-c(numeric.p25, numeric.p75)) %>%
            rename(Mean = numeric.mean) %>%
            rename("Standard deviation" = numeric.sd) %>%
            rename(Minimum = numeric.p0) %>%
            rename(Median = numeric.p50) %>%
            rename(Maximum = numeric.p100) %>%
            rename(Histogram = numeric.hist)
        }
        if (any(grepl("character", df1$skim_type))) {
          df1 <- df1 %>%
            dplyr::select(-c(character.min, character.max, character.empty, character.n_unique, character.whitespace))
        }
        df2 <- na_person(data2, input$id)
        
        df1 <- df1 %>%
          rename(Variable = skim_variable) %>%
          rename(Type = skim_type) %>%
          rename("Missing data" = n_missing) %>%
          rename("Rate of data available" = complete_rate)
        
        df2 <- df2 %>%
          rename(Variable = skim_variable) %>%
          rename("Minimum missing data per individual" = na_per_pers_min) %>%
          rename("Maximum missing data per individual" = na_per_pers_max) %>%
          rename("Median number of missing data per individual" = na_per_pers_med) %>%
          rename("Average number of missing data per individual" = na_per_pers_moy)
        
        # Fusionne les statistiques des deux fonctions pour les statistiques générales et les NA par personne
        merge(df1, df2, by.x = "Variable")
      } else {
        df1 <- skim(data2)
        if (any(grepl("factor", df1$skim_type))) {
          df1 <- df1 %>%
            dplyr::select(-factor.ordered) %>%
            rename("Niver a zoareoù" = factor.n_unique) %>%
            rename("Arsellout ar re aliesoc’h" = factor.top_counts)
        }
        if (any(grepl("numeric", df1$skim_type))) {
          df1 <- df1 %>%
            dplyr::select(-c(numeric.p25, numeric.p75)) %>%
            rename(Keidenn = numeric.mean) %>%
            rename("Diskart standart" = numeric.sd) %>%
            rename(Minimom = numeric.p0) %>%
            rename("Talvoud kreiz" = numeric.p50) %>%
            rename(Maksimom = numeric.p100) %>%
            rename(Histogram = numeric.hist)
        }
        if (any(grepl("character", df1$skim_type))) {
          df1 <- df1 %>%
            dplyr::select(-c(character.min, character.max, character.empty, character.n_unique, character.whitespace))
        }
        df2 <- na_person(data2, input$id)
        
        df1 <- df1 %>%
          rename(Varienn = skim_variable) %>%
          rename("Tip data" = skim_type) %>%
          rename("Roadennoù a vank" = n_missing) %>%
          rename("Feur roadennoù war al bezant" = complete_rate)
        
        df2 <- df2 %>%
          rename(Varienn = skim_variable) %>%
          rename("Niver ar roadennoù a vank d'an nebeutañ dre zen" = na_per_pers_min) %>%
          rename("Niver brasañ a roadennoù a vank dre zen" = na_per_pers_max) %>%
          rename("Niver talvoud kreiz a roadennoù a vank dre zen" = na_per_pers_med) %>%
          rename("Niver keitat a roadennoù a vank dre zen" = na_per_pers_moy)
        
        # Fusionne les statistiques des deux fonctions pour les statistiques générales et les NA par personne
        merge(df1, df2, by.x = "Varienn")
      }
    })
    
    #plot
    observeEvent(input$plotButton, { # ObserveEvent qui permet de recalculer tout son contenu quand le bouton Plot est utilisé
      lang <- language()
      data_spag <- data() # jeu de données qui sert au plot du SpaghettiPlot
      
      data_acpf <- data() # jeu de données qui sert a l'ACPF
      acpfVar <- input$variable_acpf
      idVar <- input$id
      timeVar <- input$time
      obsMin <- input$nbInput
      nbcp <- input$nbCP
      nb_threshold <- input$PVE * 0.01
      
      
      if (!is.numeric(data_acpf[[timeVar]]) && !is.integer(data_acpf[[timeVar]]) && !is.factor(data_acpf[[timeVar]])){
        if (lang=="fr"){
          shinyalert("Erreur", "La variable temps n'est pas numérique", type = "error")
        } else if (lang=="uk") {
          shinyalert("Error", "The time variable is not numerical", type = "error")
        } else {
          shinyalert("Fazi", "Ar c'hemmoù amzer n'int ket niverel", type = "error")
        }
      } else {
        if (is.null(input$id_select)) {
          if (lang=="fr"){
            shinyalert("Erreur", "Aucun individu(s) sélectionné(s) pour le spaghetti plot", type = "error")
          } else if (lang=="uk") {
            shinyalert("Error", "No individual(s) selected for the spaghetti plot", type = "error")
          } else {
            shinyalert("Fazi", "Hini ebet diuzet evit ar spaghetti plot", type = "error")
          }
        } else {
          if (is.null(input$id_select_score)) {
            if (lang=="fr"){
              shinyalert("Erreur", "Aucun individu sélectionné pour le calcul des estimation individuelles", type = "error")
            } else if (lang=="uk") {
              shinyalert("Error", "No individual selected for the individual estimation", type = "error")
            } else {
              shinyalert("Fazi", "Hini ebet diuzet evit jediñ ar priziadennoù hiniennel", type = "error")
            }
          } else {
              # print(input$variable_acpf == input$temps)
              if (identical(input$variable_acpf, input$time)==T){
                if (lang=="fr"){
                  shinyalert("Erreur", "La variable d'intérêt est la même que la variable temps", type = "error")
                } else if (lang=="uk") {
                  shinyalert("Error", "The variable of interest is the same as the time variable", type = "error")
                } else {
                  shinyalert("Fazi", "Heñvel eo ar c'hemmoù talvoud hag an amzer", type = "error")
                }
              } else {
                if (input$choix==1){
                  # nbcp <- input$nbCP
                  # print(input$nbCP)
                  if (input$nbCP<1 | input$nbCP>20){
                    if (lang=="fr"){
                      shinyalert("Erreur", "Le nombre de composante principale doit être compris entre 1 et 20", type = "error")
                    } else if (lang=="uk") {
                      shinyalert("Error", "The number of principal components must be between 1 and 20", type = "error")
                    } else {
                      shinyalert("Fazi", "An niver a zarempredoù pennañ a rank bezañ etre 1 ha 20", type = "error")
                    }
                  } else {
                    acpf_obj <- acpf(data_acpf, acpfVar, id=idVar, time=timeVar, obs_min = obsMin, methode=nbcp)
                  }
                } else {
                  # nb_threshold = input$PVE * 0.01
                  # print(nb_threshold)
                  if (nb_threshold<0.50 | nb_threshold>1){
                    if (lang=="fr"){
                      shinyalert("Erreur", "Le pourcentage de variance expliquée doit être compris entre 50 et 100 %", type = "error")
                    } else if (lang=="uk") {
                      shinyalert("Error", "The percentage of variance explained must be between 50 and 100%", type = "error")
                    } else {
                      shinyalert("Fazi",  "An dregantad kemmoù displeget a rank bezañ etre 50 ha 100 %",type="error")
                    }
                  } else {
                    acpf_obj <- acpf(data_acpf, acpfVar, id=idVar, time=timeVar, obs_min = obsMin, threshold = nb_threshold)
                  }
                }
                
                
                if (!exists("acpf_obj")) {
                  return()  # Si l'objet n'existe pas, arrêter l'observation
                }
                
                
                
                if (lang == "fr"){
                  #SpaghettiPlot
                  idSelect <- input$id_select
                  data_spag <- data_spag %>% filter(.data[[idVar]] %in% idSelect)
                  output$plot_spag <- renderPlot({
                    ggplot(data_spag, aes(x=.data[[timeVar]],y=.data[[acpfVar]], group=.data[[idVar]], color = .data[[idVar]])) +
                      geom_line() +
                      labs(title = paste("SpaghettiPlot représentant", acpfVar, "chez les individus sélectionnés"),
                           x = "Temps",
                           y = acpfVar,
                           color="Individus") +
                      guides(color = F) +
                      theme_minimal()
                  })
                  
                  # MuPlot
                  acpf_mu_df <- as.data.frame(acpf_obj$acpf$mu) %>% rename(mu = 'acpf_obj$acpf$mu')
                  acpf_mu_df <- acpf_mu_df %>% mutate(temps =acpf_obj$acpf$workGrid) #mutate(temps = seq(1, n(), 1))
                  output$plot_mu <- renderPlot({
                    ggplot(acpf_mu_df, aes(x = temps, y = mu)) +
                      geom_line() +
                      labs(title = "Fonction moyenne de l'ACPF sur l'ensemble des individus", 
                           x = "Temps", y="Moyenne") +
                      theme_minimal()
                  })
                  
                  
                  # ScorePlot
                  scores <- acpf_obj$acpf$xiEst %*% t(acpf_obj$acpf$phi) + matrix(rep(acpf_obj$acpf$mu, times = length(acpf_obj$data_obs)), nrow = length(acpf_obj$data_obs), byrow = TRUE)
                  # print(scores)
                  # Récuperer la liste des identifiants selon le type de jeu de données
                  # Leur attribuer un index
                  
                  if (IsRegular(data())!="Sparse"){
                    liste_id <- sort(unique(data()[[idVar]]))
                    
                    liste_id_df <- data.frame(id = liste_id)
                    liste_id_df <- liste_id_df %>% mutate(index = seq(1, n(), 1))
                    
                    indivPlotdf <- liste_id_df %>% filter(id==input$id_select_score)
                    indivPlot <- indivPlotdf$index
                  } else {
                    # print("sparse")
                    
                    data_index <- data_frame(id = sort(unique(data()[[idVar]])))
                    data_index <- data_index %>% mutate(index = seq(1, n(), 1))
                    data_estim <- data.frame(id = data()[[input$id]], temps = data()[[input$time]]) %>% mutate(variable = data()[[input$variable_acpf]])
                    data_estim <- left_join(data_estim, data_index)
                    data_estim <- data_estim %>% group_by(id) %>% filter(all(sum(!is.na(variable)) >= input$nbInput))
                    liste_id_df <- data_estim %>% dplyr::select(id, index) %>% distinct(id, index ,.keep_all = TRUE) 
                    liste_id_df<- as.data.frame(liste_id_df) %>% mutate(index2 = row_number())
                    
                    indivPlotdf <- liste_id_df %>% filter(id==input$id_select_score)
                    indivPlot <- indivPlotdf$index
                  }
                  
                  # Prendre les t tels que Y(t) soit non manquant
                  if (is.numeric(indivPlotdf$id)==T){
                    indivPlotStr <- as.character(indivPlot)
                  } else {
                    indivPlotStr <- indivPlotdf$id
                  }
                  dfObs <- data.frame(acpf_obj$acpf$inputData$Ly[[indivPlotStr]], acpf_obj$acpf$inputData$Lt[[indivPlotStr]])
                  names(dfObs)[1] <- "Obs"
                  names(dfObs)[2] <- "Time"
                  # print(dfObs)
                  
                  
                  # Prendre les scores de l'individu selectionné
                  indivSelect <- data.frame(id = data()[[input$id]])
                  if (IsRegular(data())=="Sparse"){
                    indivPlot2 <- indivPlotdf$index2
                    print(indivPlot2)
                    indivPlotStr2 <- as.character(indivPlot2)
                    print(indivPlotStr2)
                    indivScore2 <- scores[as.numeric(indivPlotStr2),]
                    print(indivScore2)
                    # indivSelect <- indivSelect %>% filter(id %in% input$id_select)
                    # print(indivSelect)
                    # indivSelect_id <- sort(unique(indivSelect$id))
                    # print(indivSelect_id)
                    # newIndex <- data.frame(id = indivSelect_id) %>% mutate(index = seq(1, n(), 1))
                    # print(newIndex)
                    # newIndex <- newIndex %>% filter(id==input$id_select_score)
                    # print(newIndex)
                    # indivPlot2 <- newIndex$index
                    # print(indivPlot2)
                    # indivPlotStr2 <- as.character(indivPlot2)
                    # print(indivPlotStr2)
                    # indivScore2 <- scores[as.numeric(indivPlotStr2),]
                    # print(indivScore2)
                  } else {
                    indivPlot2 <- indivPlot
                    indivPlotStr2 <- as.character(indivPlot2)
                    indivScore2 <- scores[as.numeric(indivPlotStr2),]
                  }
                  
                  indivScore_df2 <- as.data.frame(indivScore2)
                  temps_score_df2 <- acpf_obj$acpf$workGrid
                  indivScore_df2 <- indivScore_df2 %>% mutate(temps = temps_score_df2)
                  # print(indivScore_df2)
                  
                  idSelectScore <- input$id_select_score
                  
                  # Plot en fonction du type de point choisi
                  if (input$typeTrace == "etoiles"){
                    output$plot_score <- renderPlot({
                      ggplot(indivScore_df2, aes(x = temps, y = indivScore2)) +
                        geom_line() +
                        geom_point(data = dfObs, aes(x = Time, y = Obs), color = "red", shape=4, size = 1) +
                        labs(title = paste("Graph des estimations individuelles de la fonction de l'individu", idSelectScore, "\net ses valeurs observées"),
                             x = "Temps", y = acpfVar) +
                        theme_minimal()
                    })
                  } else {
                    output$plot_score <- renderPlot({
                      ggplot(indivScore_df2, aes(x = temps, y = indivScore2)) +
                        geom_line() +
                        geom_line(data = dfObs, aes(x = Time, y = Obs), color = "red") +
                        labs(title = paste("Graph des estimations individuelles de la fonction de l'individu", idSelectScore, "\net valeurs observées"),
                             x = "Temps", y = acpfVar) +
                        theme_minimal()
                    })
                  }
                  
                  
                  
                  # PhiPlot
                  # Chercher les valeurs minimum de l'acpf avec paramètres par défaut
                  acpf_ylim <- acpf(data_acpf, acpfVar, id=idVar, time=timeVar)
                  min_y<-min(acpf_ylim$acpf$phi)
                  max_y<-max(acpf_ylim$acpf$phi)
                  
                  # Stocker les valeurs des composantes principales de l'acpf calculée
                  phi <- acpf_obj$acpf$phi
                  colnames(phi) <-1:ncol(phi)
                  phi <- cbind("temps"=1:nrow(phi),phi)
                  phi <- as.data.frame(phi)
                  
                  if (input$choix==1){
                    phi_pivoter<- pivot_longer(phi, cols = colnames(phi)[2:(as.numeric(nbcp)+1)], names_to = "CP", values_to = "Valeurs")
                    output$plot_phi <- renderPlot({
                      ggplot(phi_pivoter, aes(x=temps ,y=Valeurs, group=CP, color = CP)) +
                        geom_line() +
                        labs(title ="Diagramme représentant les composantes principales",
                             x = "Temps",
                             y = paste("Variation de ", input$variable_acpf),
                             color="CP",
                             caption=paste("Le pourcentage de variance expliquée est de ", round(acpf_obj$acpf$FVE*100,2), "%.")) +
                        ylim(min_y,max_y)+
                        theme_minimal()
                      # theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5), plot.caption = element_text(size = 13, hjust =0))
                    })
                  } else {
                    phi_pivoter<- pivot_longer(phi, cols = colnames(phi)[2:ncol(phi)], names_to = "CP", values_to = "Valeurs")
                    output$plot_phi <- renderPlot({
                      ggplot(phi_pivoter, aes(x=temps ,y=Valeurs, group=CP, color = CP)) +
                        geom_line() +
                        labs(title = paste("Diagramme représentant les composantes principales"),
                             x = "Temps",
                             y = paste("Variation de ", input$variable_acpf),
                             color="CP") +
                        ylim(min_y,max_y)+
                        theme_minimal()
                      # theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5))
                    })
                  }
                  
                  # PlotVarExpPVE
                  variances_expliquee <- as.data.frame(acpf_obj$acpf$lambda/sum(acpf_obj$acpf$lambda))
                  names(variances_expliquee)[1] <- "varExp"
                  pve <- as.data.frame(acpf_obj$acpf$cumFVE)
                  names(pve)[1] <- "pve"
                  composante <- as.data.frame(row.names(variances_expliquee))
                  names(composante)[1] <- "CP"
                  df_varExp_PVE <- cbind(composante, variances_expliquee, pve)
                  head(df_varExp_PVE)
                  
                  output$plot_varExpPVE <- renderPlot({
                    ggplot(df_varExp_PVE, aes(x = factor(CP, levels = unique(CP)), y = varExp)) +
                      geom_bar(stat = "identity", fill="lightblue") +
                      geom_line(aes(x = factor(CP, levels = unique(CP)), y = pve, group=1), color = "black") +
                      geom_point(aes(x = factor(CP, levels = unique(CP)), y = pve), shape=1, color = "black", size = 2) +
                      labs(title = "Graph des variances expliquées par les composantes principales et tracé de la fréquence cumulée de variation expliquée",
                           x = "Composantes principales",
                           y = "Variance expliquée") +
                      theme_minimal()
                  })
                  
                  
                  # PlotContribIndividu
                  data_contrib <- data.frame(name = paste0("CP", seq_len(ncol(acpf_obj$acpf$xiEst))),
                                             value = acpf_obj$acpf$xiEst[indivPlot2,])
                  data_contrib <- data_contrib[1:acpf_obj$acpf$selectK,]

                  output$plot_contrib_individu <- renderPlot ({
                    ggplot(data_contrib, aes(x=name, y=value)) +
                      geom_bar(stat = "identity",
                               color = "grey",
                               fill = "lightblue") +

                      coord_cartesian(ylim = c(min(acpf_obj$acpf$xiEst[,]),max(acpf_obj$acpf$xiEst[,])),#min et max en fonction du min et du max général
                                      xlim = c(1,acpf_obj$acpf$selectK)) +
                      labs(title = paste("Graph des contributions de chaque CP à la projection de l'individu :", idSelectScore),
                           x = "Composantes principales", y = "Coefficient associé") +
                      theme_minimal()
                  })
                } else if (lang=="uk"){
                  #SpaghettiPlot
                  idSelect <- input$id_select
                  data_spag <- data_spag %>% filter(.data[[idVar]] %in% idSelect)
                  output$plot_spag <- renderPlot({
                    ggplot(data_spag, aes(x=.data[[timeVar]], y=.data[[acpfVar]], group=.data[[idVar]], color = .data[[idVar]])) +
                      geom_line() +
                      labs(title = paste("SpaghettiPlot representing", acpfVar, "among selected individuals"),
                           x = "Time",
                           y = acpfVar,
                           color = "Individuals") +
                      guides(color = FALSE) +
                      theme_minimal()
                  })
                  
                  # MuPlot
                  acpf_mu_df <- as.data.frame(acpf_obj$acpf$mu) %>% rename(mu = 'acpf_obj$acpf$mu')
                  acpf_mu_df <- acpf_mu_df %>% mutate(temps =acpf_obj$acpf$workGrid) #mutate(temps = seq(1, n(), 1))
                  output$plot_mu <- renderPlot({
                    ggplot(acpf_mu_df, aes(x = temps, y = mu)) +
                      geom_line() +
                      labs(title = "Mean function of ACPF across all individuals",
                           x = "Time", y = "Mean") +
                      theme_minimal()
                  })
                  
                  
                  # ScorePlot
                  scores <- acpf_obj$acpf$xiEst %*% t(acpf_obj$acpf$phi) + matrix(rep(acpf_obj$acpf$mu, times = length(acpf_obj$data_obs)), nrow = length(acpf_obj$data_obs), byrow = TRUE)
                  
                  # Récuperer la liste des identifiants selon le type de jeu de données
                  # Leur attribuer un index
                  
                  if (IsRegular(data())!="Sparse"){
                    liste_id <- sort(unique(data()[[idVar]]))
                    # print("Non sparse")
                    
                    liste_id_df <- data.frame(id = liste_id)
                    liste_id_df <- liste_id_df %>% mutate(index = seq(1, n(), 1))
                    
                    indivPlotdf <- liste_id_df %>% filter(id==input$id_select_score)
                    indivPlot <- indivPlotdf$index
                  } else {
                    # print("sparse")
                    
                    data_index <- data_frame(id = sort(unique(data()[[idVar]])))
                    data_index <- data_index %>% mutate(index = seq(1, n(), 1))
                    data_estim <- data.frame(id = data()[[input$id]], temps = data()[[input$time]]) %>% mutate(variable = data()[[input$variable_acpf]])
                    data_estim <- left_join(data_estim, data_index)
                    data_estim <- data_estim %>% group_by(id) %>% filter(all(sum(!is.na(variable)) >= input$nbInput))
                    liste_id_df <- data_estim %>% dplyr::select(id, index) %>% distinct(id, index ,.keep_all = TRUE)
                    liste_id_df<- as.data.frame(liste_id_df) %>% mutate(index2 = row_number())
                    
                    indivPlotdf <- liste_id_df %>% filter(id==input$id_select_score)
                    indivPlot <- indivPlotdf$index
                  }
                  
                  # Prendre les t tels que Y(t) soit non manquant
                  if (is.numeric(indivPlotdf$id)==T){
                    indivPlotStr <- as.character(indivPlot)
                  } else {
                    indivPlotStr <- indivPlotdf$id
                  }
                  dfObs <- data.frame(acpf_obj$acpf$inputData$Ly[[indivPlotStr]], acpf_obj$acpf$inputData$Lt[[indivPlotStr]])
                  names(dfObs)[1] <- "Obs"
                  names(dfObs)[2] <- "Time"
                  
                  # debug <- data.frame(id = data()[[input$id]], variable = data()[[input$variable_acpf]], time = data()[[input$time]])
                  # if (IsRegular(data())!="Sparse"){
                  #   debug <- debug %>% filter(id == indivPlotdf$id)
                  # } else {
                  #   debug <- debug %>% filter(id == indivPlotdf$id)
                  # }
                  # print(debug)
                  # 
                  # print(acpf_obj$acpf$inputData$Ly[[indivPlotStr]])
                  
                  # Prendre les scores de l'individu selectionné
                  indivSelect <- data.frame(id = data()[[input$id]])
                  if (IsRegular(data())=="Sparse"){
                    indivPlot2 <- indivPlotdf$index2
                    print(indivPlot2)
                    indivPlotStr2 <- as.character(indivPlot2)
                    print(indivPlotStr2)
                    indivScore2 <- scores[as.numeric(indivPlotStr2),]
                    print(indivScore2)
                  } else {
                    indivPlot2 <- indivPlot
                    indivPlotStr2 <- as.character(indivPlot2)
                    indivScore2 <- scores[as.numeric(indivPlotStr2),]
                  }
                  
                  
                  indivScore_df2 <- as.data.frame(indivScore2)
                  temps_score_df2 <- acpf_obj$acpf$workGrid
                  indivScore_df2 <- indivScore_df2 %>% mutate(temps = temps_score_df2)
                  
                  idSelectScore <- input$id_select_score
                  
                  # Plot en fonction du type de point choisi
                  if (input$typeTrace == "etoiles") {
                    output$plot_score <- renderPlot({
                      ggplot(indivScore_df2, aes(x = temps, y = indivScore2)) +
                        geom_line() +
                        geom_point(data = dfObs, aes(x = Time, y = Obs), color = "red", shape=4, size = 1) +
                        labs(title = paste("Graph of individual estimates of the function of individual", idSelectScore, "\nand its observed values"),
                             x = "Time", y = acpfVar) +
                        theme_minimal()
                    })
                  } else {
                    output$plot_score <- renderPlot({
                      ggplot(indivScore_df2, aes(x = temps, y = indivScore2)) +
                        geom_line() +
                        geom_line(data = dfObs, aes(x = Time, y = Obs), color = "red") +
                        labs(title = paste("Graph of individual estimates of the function of individual", idSelectScore, "\nand observed values"),
                             x = "Time", y = acpfVar) +
                        theme_minimal()
                    })
                  }
                  
                  
                  
                  # PhiPlot
                  # Chercher les valeurs minimum de l'acpf avec paramètres par défaut
                  acpf_ylim <- acpf(data_acpf, acpfVar, id=idVar, time=timeVar)
                  min_y<-min(acpf_ylim$acpf$phi)
                  max_y<-max(acpf_ylim$acpf$phi)
                  
                  # Stocker les valeurs des composantes principales de l'acpf calculée
                  phi <- acpf_obj$acpf$phi
                  colnames(phi) <-1:ncol(phi)
                  phi <- cbind("temps"=1:nrow(phi),phi)
                  phi <- as.data.frame(phi)
                  
                  if (input$choix==1){
                    phi_pivoter<- pivot_longer(phi, cols = colnames(phi)[2:(as.numeric(nbcp)+1)], names_to = "PC", values_to = "Valeurs")
                    output$plot_phi <- renderPlot({
                      ggplot(phi_pivoter, aes(x = temps, y = Valeurs, group = PC, color = PC)) +
                        geom_line() +
                        labs(title = "Diagram representing the principal components",
                             x = "Time",
                             y = paste("Variation of", input$variable_acpf),
                             color = "PC",
                             caption = paste("The percentage of explained variance is", round(acpf_obj$acpf$FVE * 100, 2), "%.")) +
                        ylim(min_y, max_y) +
                        theme_minimal()
                    })
                  } else {
                    phi_pivoter<- pivot_longer(phi, cols = colnames(phi)[2:ncol(phi)], names_to = "PC", values_to = "Valeurs")
                    output$plot_phi <- renderPlot({
                      ggplot(phi_pivoter, aes(x=temps ,y=Valeurs, group=PC, color = PC)) +
                        geom_line() +
                        labs(title = paste("Diagram representing the principal components"),
                             x = "Time",
                             y = paste("Variation of ", input$variable_acpf),
                             color="PC") +
                        ylim(min_y,max_y)+
                        theme_minimal()
                      # theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5))
                    })
                  }
                  
                  # PlotVarExpPVE
                  variances_expliquee <- as.data.frame(acpf_obj$acpf$lambda/sum(acpf_obj$acpf$lambda))
                  names(variances_expliquee)[1] <- "varExp"
                  pve <- as.data.frame(acpf_obj$acpf$cumFVE)
                  names(pve)[1] <- "pve"
                  composante <- as.data.frame(row.names(variances_expliquee))
                  names(composante)[1] <- "CP"
                  df_varExp_PVE <- cbind(composante, variances_expliquee, pve)
                  head(df_varExp_PVE)
                  
                  output$plot_varExpPVE <- renderPlot({
                    ggplot(df_varExp_PVE, aes(x = factor(CP, levels = unique(CP)), y = varExp)) +
                      geom_bar(stat = "identity", fill = "lightblue") +
                      geom_line(aes(x = factor(CP, levels = unique(CP)), y = pve, group = 1), color = "black") +
                      geom_point(aes(x = factor(CP, levels = unique(CP)), y = pve), shape = 1, color = "black", size = 2) +
                      labs(title = "Graph of variances explained by the principal components and plot of the cumulative frequency of explained variation",
                           x = "Principal Components",
                           y = "Explained Variance") +
                      theme_minimal()
                  })
                  
                  # PlotContribIndividu
                  # data_contrib <- data.frame(name = c(colnames(as.data.frame(acpf_obj$acpf$xiEst))),
                  #                            value = acpf_obj$acpf$xiEst[indivPlot2,])
                  data_contrib <- data.frame(name = paste0("PC", seq_len(ncol(acpf_obj$acpf$xiEst))),
                                             value = acpf_obj$acpf$xiEst[indivPlot2,])
                  data_contrib <- data_contrib[1:acpf_obj$acpf$selectK,]
                  
                  output$plot_contrib_individu <- renderPlot({
                    ggplot(data_contrib, aes(x = name, y = value)) +
                      geom_bar(stat = "identity",
                               color = "grey",
                               fill = "lightblue") +
                      coord_cartesian(ylim = c(min(acpf_obj$acpf$xiEst[,]), max(acpf_obj$acpf$xiEst[,])), # min and max based on the general min and max
                                      xlim = c(1, acpf_obj$acpf$selectK)) +
                      labs(title = paste("Graph of contributions of each PC to the projection of the individual:", idSelectScore),
                           x = "Principal Components", y = "Associated Coefficient") +
                      theme_minimal()
                  })
                } else {
                  #SpaghettiPlot
                  idSelect <- input$id_select
                  data_spag <- data_spag %>% filter(.data[[idVar]] %in% idSelect)
                  output$plot_spag <- renderPlot({
                    ggplot(data_spag, aes(x=.data[[timeVar]],y=.data[[acpfVar]], group=.data[[idVar]], color = .data[[idVar]])) +
                      geom_line() +
                      labs(title = paste("SpaghettiPlot", acpfVar, "evit an dud diuzet"),
                           x = "Amzer",
                           y = acpfVar,
                           color="Tud prevez") +
                      guides(color = F) +
                      theme_minimal()
                  })
                  
                  # MuPlot
                  acpf_mu_df <- as.data.frame(acpf_obj$acpf$mu) %>% rename(mu = 'acpf_obj$acpf$mu')
                  acpf_mu_df <- acpf_mu_df %>% mutate(temps =acpf_obj$acpf$workGrid) #mutate(temps = seq(1, n(), 1))
                  output$plot_mu <- renderPlot({
                    ggplot(acpf_mu_df, aes(x = temps, y = mu)) +
                      geom_line() +
                      labs(title = "Keitad kefridi an ACPF war an holl hiniennoù", 
                           x = "Amzer", y="Keidenn") +
                      theme_minimal()
                  })
                  
                  
                  # ScorePlot
                  scores <- acpf_obj$acpf$xiEst %*% t(acpf_obj$acpf$phi) + matrix(rep(acpf_obj$acpf$mu, times = length(acpf_obj$data_obs)), nrow = length(acpf_obj$data_obs), byrow = TRUE)
                  # print(scores)
                  # Récuperer la liste des identifiants selon le type de jeu de données
                  # Leur attribuer un index
                  
                  if (IsRegular(data())!="Sparse"){
                    liste_id <- sort(unique(data()[[idVar]]))
                    
                    liste_id_df <- data.frame(id = liste_id)
                    liste_id_df <- liste_id_df %>% mutate(index = seq(1, n(), 1))
                    
                    indivPlotdf <- liste_id_df %>% filter(id==input$id_select_score)
                    indivPlot <- indivPlotdf$index
                  } else {
                    # print("sparse")
                    
                    data_index <- data_frame(id = sort(unique(data()[[idVar]])))
                    data_index <- data_index %>% mutate(index = seq(1, n(), 1))
                    data_estim <- data.frame(id = data()[[input$id]], temps = data()[[input$time]]) %>% mutate(variable = data()[[input$variable_acpf]])
                    data_estim <- left_join(data_estim, data_index)
                    data_estim <- data_estim %>% group_by(id) %>% filter(all(sum(!is.na(variable)) >= input$nbInput))
                    liste_id_df <- data_estim %>% dplyr::select(id, index) %>% distinct(id, index ,.keep_all = TRUE) 
                    liste_id_df<- as.data.frame(liste_id_df) %>% mutate(index2 = row_number())
                    
                    indivPlotdf <- liste_id_df %>% filter(id==input$id_select_score)
                    indivPlot <- indivPlotdf$index
                  }
                  
                  # Prendre les t tels que Y(t) soit non manquant
                  if (is.numeric(indivPlotdf$id)==T){
                    indivPlotStr <- as.character(indivPlot)
                  } else {
                    indivPlotStr <- indivPlotdf$id
                  }
                  dfObs <- data.frame(acpf_obj$acpf$inputData$Ly[[indivPlotStr]], acpf_obj$acpf$inputData$Lt[[indivPlotStr]])
                  names(dfObs)[1] <- "Obs"
                  names(dfObs)[2] <- "Time"
                  # print(dfObs)
                  
                  
                  # Prendre les scores de l'individu selectionné
                  indivSelect <- data.frame(id = data()[[input$id]])
                  if (IsRegular(data())=="Sparse"){
                    indivPlot2 <- indivPlotdf$index2
                    print(indivPlot2)
                    indivPlotStr2 <- as.character(indivPlot2)
                    print(indivPlotStr2)
                    indivScore2 <- scores[as.numeric(indivPlotStr2),]
                    print(indivScore2)
                    # indivSelect <- indivSelect %>% filter(id %in% input$id_select)
                    # print(indivSelect)
                    # indivSelect_id <- sort(unique(indivSelect$id))
                    # print(indivSelect_id)
                    # newIndex <- data.frame(id = indivSelect_id) %>% mutate(index = seq(1, n(), 1))
                    # print(newIndex)
                    # newIndex <- newIndex %>% filter(id==input$id_select_score)
                    # print(newIndex)
                    # indivPlot2 <- newIndex$index
                    # print(indivPlot2)
                    # indivPlotStr2 <- as.character(indivPlot2)
                    # print(indivPlotStr2)
                    # indivScore2 <- scores[as.numeric(indivPlotStr2),]
                    # print(indivScore2)
                  } else {
                    indivPlot2 <- indivPlot
                    indivPlotStr2 <- as.character(indivPlot2)
                    indivScore2 <- scores[as.numeric(indivPlotStr2),]
                  }
                  
                  indivScore_df2 <- as.data.frame(indivScore2)
                  temps_score_df2 <- acpf_obj$acpf$workGrid
                  indivScore_df2 <- indivScore_df2 %>% mutate(temps = temps_score_df2)
                  # print(indivScore_df2)
                  
                  idSelectScore <- input$id_select_score
                  
                  # Plot en fonction du type de point choisi
                  if (input$typeTrace == "etoiles"){
                    output$plot_score <- renderPlot({
                      ggplot(indivScore_df2, aes(x = temps, y = indivScore2)) +
                        geom_line() +
                        geom_point(data = dfObs, aes(x = Time, y = Obs), color = "red", shape=4, size = 1) +
                        labs(title = paste("Grafik priziadennoù hiniennel kefridi an hini", idSelectScore, "\nhag e dalvoudoù arsellet"),
                             x = "Amzer", y = acpfVar) +
                        theme_minimal()
                    })
                  } else {
                    output$plot_score <- renderPlot({
                      ggplot(indivScore_df2, aes(x = temps, y = indivScore2)) +
                        geom_line() +
                        geom_line(data = dfObs, aes(x = Time, y = Obs), color = "red") +
                        labs(title = paste("Grafik priziadennoù hiniennel kefridi an hini", idSelectScore, "\nhag e dalvoudoù arsellet"),
                             x = "Amzer", y = acpfVar) +
                        theme_minimal()
                    })
                  }
                  
                  
                  
                  # PhiPlot
                  # Chercher les valeurs minimum de l'acpf avec paramètres par défaut
                  acpf_ylim <- acpf(data_acpf, acpfVar, id=idVar, time=timeVar)
                  min_y<-min(acpf_ylim$acpf$phi)
                  max_y<-max(acpf_ylim$acpf$phi)
                  
                  # Stocker les valeurs des composantes principales de l'acpf calculée
                  phi <- acpf_obj$acpf$phi
                  colnames(phi) <-1:ncol(phi)
                  phi <- cbind("temps"=1:nrow(phi),phi)
                  phi <- as.data.frame(phi)
                  
                  if (input$choix==1){
                    phi_pivoter<- pivot_longer(phi, cols = colnames(phi)[2:(as.numeric(nbcp)+1)], names_to = "CP", values_to = "Valeurs")
                    output$plot_phi <- renderPlot({
                      ggplot(phi_pivoter, aes(x=temps ,y=Valeurs, group=CP, color = CP)) +
                        geom_line() +
                        labs(title ="Diagramm a daolenn ar parzhioù pennañ",
                             x = "Amzer",
                             y = paste("Kenmadur", input$variable_acpf),
                             color="CP",
                             caption=paste("Dregantad ar cheñchamantoù displeget a", round(acpf_obj$acpf$FVE*100,2), "%.")) +
                        ylim(min_y,max_y)+
                        theme_minimal()
                      # theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5), plot.caption = element_text(size = 13, hjust =0))
                    })
                  } else {
                    phi_pivoter<- pivot_longer(phi, cols = colnames(phi)[2:ncol(phi)], names_to = "CP", values_to = "Valeurs")
                    output$plot_phi <- renderPlot({
                      ggplot(phi_pivoter, aes(x=temps ,y=Valeurs, group=CP, color = CP)) +
                        geom_line() +
                        labs(title = paste("Diagramm a daolenn ar parzhioù pennañ"),
                             x = "Amzer",
                             y = paste("Kenmadur", input$variable_acpf),
                             color="CP") +
                        ylim(min_y,max_y)+
                        theme_minimal()
                      # theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5))
                    })
                  }
                  
                  # PlotVarExpPVE
                  variances_expliquee <- as.data.frame(acpf_obj$acpf$lambda/sum(acpf_obj$acpf$lambda))
                  names(variances_expliquee)[1] <- "varExp"
                  pve <- as.data.frame(acpf_obj$acpf$cumFVE)
                  names(pve)[1] <- "pve"
                  composante <- as.data.frame(row.names(variances_expliquee))
                  names(composante)[1] <- "CP"
                  df_varExp_PVE <- cbind(composante, variances_expliquee, pve)
                  head(df_varExp_PVE)
                  
                  output$plot_varExpPVE <- renderPlot({
                    ggplot(df_varExp_PVE, aes(x = factor(CP, levels = unique(CP)), y = varExp)) +
                      geom_bar(stat = "identity", fill="lightblue") +
                      geom_line(aes(x = factor(CP, levels = unique(CP)), y = pve, group=1), color = "black") +
                      geom_point(aes(x = factor(CP, levels = unique(CP)), y = pve), shape=1, color = "black", size = 2) +
                      labs(title = "Grafik an daskemmoù displeget gant ar parzhioù pennañ ha treset eus ar frekañs daskemmoù berniet displeget",
                           x = "Parzhioù pennañ",
                           y = "Variant displeget") +
                      theme_minimal()
                  })
                  
                  
                  # PlotContribIndividu
                  data_contrib <- data.frame(name = paste0("CP", seq_len(ncol(acpf_obj$acpf$xiEst))),
                                             value = acpf_obj$acpf$xiEst[indivPlot2,])
                  data_contrib <- data_contrib[1:acpf_obj$acpf$selectK,]
                  
                  output$plot_contrib_individu <- renderPlot ({
                    ggplot(data_contrib, aes(x=name, y=value)) +
                      geom_bar(stat = "identity",
                               color = "grey",
                               fill = "lightblue") +
                      
                      coord_cartesian(ylim = c(min(acpf_obj$acpf$xiEst[,]),max(acpf_obj$acpf$xiEst[,])),#min et max en fonction du min et du max général
                                      xlim = c(1,acpf_obj$acpf$selectK)) +
                      labs(title = paste("Grafik emzalc'hioù pep CP war bannañ an hinienn :", idSelectScore),
                           x = "Parzhioù pennañ", y = "Gwezhiader kevelet") +
                      theme_minimal()
                  })
                }
              }
            }
          
          
          }
        }
        
    })
  }
  
  # Run the application 
  shinyApp(ui = ui, server = server)