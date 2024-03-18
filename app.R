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
                      tags$script("$(document).ready(function() { 
                    $('#flag_fr').parent().addClass('dropdown-toggle');
                    $('#flag_fr').parent().attr('data-toggle', 'dropdown'); 
                  });")
                    )),
    dashboardSidebar(disable=TRUE),
    dashboardBody(
      style = "height: 95vh; overflow-y: auto;",
      tags$head(tags$style(HTML('
        .content-wrapper {
          background-color: #fff;
        }
      '
      ))),
      includeCSS(file.path("www", "custom_icon.css")),
      ui <- fluidPage(#title="APP ACPF",
                      # includeCSS(file.path("www", "custom_icon.css")),
                      # headerPanel(fluidRow(
                      #   actionButton("flag_fr",label = "",icon = icon(name = NULL,class = "custom_icon")),
                      #   actionButton("flag_uk",label = "",icon = icon(name = NULL,class = "custom_icon2"))
                      # )),
                      tabsetPanel(tabPanel("Importation et visualisation des données",
                                           sidebarLayout(
                                             sidebarPanel(
                                               # jeu de données
                                               fileInput("file1", "Choisir un fichier csv", accept = ".csv", buttonLabel = "Parcourir...", placeholder = "Aucun fichier sélectionné",),
                                               checkboxInput("header", "La première ligne contient-elle les noms de colonne ?", TRUE),
                                               # Nom d'une première colonne ?
                                               # Si le jeu de données est mal orienté
                                               checkboxInput("oriente", "Pivoter le jeu de données",value = FALSE),
                                               selectInput("id", label = "Variable identifiant",
                                                           choices = NULL,multiple = F),
                                               # variables présentes dans le jeu de données à transformer
                                               selectInput("variable",label = "Variables à transformer en facteur",
                                                           choices = NULL,multiple = T),
                                             ),
                                             mainPanel(
                                               tabsetPanel(
                                                 tabPanel("Validation des données",
                                                          # visualisation du jeu de données
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
                                   # selectInput("id", label = "Variable identifiant",
                                   #             choices = NULL,multiple = F),
                                   selectInput("variable_acpf", label = "Variable d'intérêt",
                                               choices = NULL,multiple = F),
                                   selectInput("time", label = "Variable temps (de type numérique)",
                                               choices = NULL,multiple = F),
                                   numericInput("nbInput",label="Nombre d'observation minimum pour intégrer un individu dans l'ACPF :", value = 2, min = 1 , step=1),
                                   selectInput("id_select", label = "Individu(s) sélectionné(s) pour le spaghetti plot",
                                               choices = NULL,multiple = T), # Ensemble des individus selectionnés
                                   selectInput("id_select_score", label = "Individus sélectionné pour le calcul des estimations individuelles",
                                               choices = NULL,multiple = F), # Ensemble des individus selectionnés pour le plot des estimation individuelles
                                   radioButtons("choix", label="Calcul de l'ACPF selon le : ",choices = list("Nombre de CP :" = 1, "PVE" = 2),selected = 2),
                                   conditionalPanel( # Choix entre le calcul par nombre de CP selectionnées ou par % de variance expliquée
                                     condition = "input.choix == 1",
                                     numericInput("nbCP",label="Nombre de CP :", value = 3, min = 1, max = 20)
                                   ),
                                   conditionalPanel(
                                     condition = "input.choix == 2",
                                     numericInput("PVE",label=" % de variances expliqué :", value = 99, min =50 , max = 100, step=0.01)
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
                                     column(12, plotOutput("plot_contrib_individu"))) # Plot des contribution de chaque CP à la projection d'un individu
                                 )
                               )
                      ) # commencer ici pour ajouter des boutons Langues
                      )
      ) #,
      # uiOutput("app_content")
      )
  )


  
  # SERVER
  server <- function(input, output, session) {
    # # Réactive pour suivre la langue sélectionnée
    # language <- reactiveVal("fr")  # Par défaut, la langue est le français
    # # Observer pour mettre à jour la langue lorsque les boutons sont cliqués
    # observeEvent(input$flag_fr, {
    #   language("fr")
    # })
    # observeEvent(input$flag_uk, {
    #   language("uk")
    # })
    # 
    # output$app_content <- renderUI({
    #   if (language() == "fr") {
    #     tags$p("Contenu en français")
    #     
    #   } else if (language() == "uk") {
    #     tags$p("Content in English")
    #   }
    # })
    
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
          pivot_longer(!colnames(donnees[1]), names_to = "ID", values_to = "Valeurs")
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
      
      # Nouvelle version
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
      
      # Ancienne version
      # if (!is.null(input$id) && input$id != ""){
      #   
      #   id_select <- unique(current_data[,input$id])
      #   id_select_score <- unique(current_data[,input$id])
      #   updateSelectInput(session, "id_select", choices = id_select, selected = id_select[1])
      #   updateSelectInput(session, "id_select_score", choices = id_select_score)
      # }
      
    })
    # visualisation des données
    output$test <- renderTable({
      data2 <- data() %>% 
        mutate_at(input$variable, as.factor) 
      
      df1 <- skim(data2)
      if (any(grepl("factor", df1$skim_type))) {
        df1 <- df1 %>% dplyr::select(-factor.ordered) %>% 
          rename("Nombre de modalités" = factor.n_unique) %>% 
          rename("Observation(s) la(les) plus fréquente(s)" = factor.top_counts)
      }
      if (any(grepl("numeric", df1$skim_type))) {
        df1 <- df1 %>% dplyr::select(-c(numeric.p25, numeric.p75)) %>% 
          rename(Moyenne = numeric.mean) %>% 
          rename("Ecart-type" = numeric.sd) %>% 
          rename(Minimum = numeric.p0) %>% 
          rename(Mediane = numeric.p50) %>% 
          rename(Maximum = numeric.p100) %>% 
          rename(Histogramme = numeric.hist)
      }
      if (any(grepl("character", df1$skim_type))) {
        df1 <- df1 %>% dplyr::select(-c(character.min, character.max, character.empty, character.n_unique, character.whitespace))
      }
      # print(df1$skim_type)
      df2 <- na_person(data2,input$id)
      
      df1 <- df1 %>% 
        rename(Variable = skim_variable) %>% 
        rename(Type = skim_type) %>% 
        rename("Donnees manquantes" = n_missing) %>% 
        rename("Taux de données présentes" = complete_rate)
      
      df2 <- df2 %>%
        rename(Variable = skim_variable) %>% 
        rename("Nombre de donnees manquantes minimum par individu" = na_per_pers_min) %>%
        rename("Nombre de donnees manquantes maximum par individu" = na_per_pers_max) %>%
        rename("Nombre médian de donnees manquantes par individu" = na_per_pers_med) %>%
        rename("Nombre moyen de donnees manquantes par individu" = na_per_pers_moy)
      
      # merge les stats les deux fonctions pour les stats g et les na par personne
      merge(df1, df2, by.x = "Variable")
    })
    
    #plot
    observeEvent(input$plotButton, { # ObserveEvent qui permet de recalculer tout son contenu quand le bouton Plot est utilisé
      data_spag <- data() # jeu de données qui sert au plot du SpaghettiPlot
      
      data_acpf <- data() # jeu de données qui sert a l'ACPF
      acpfVar <- input$variable_acpf
      idVar <- input$id
      timeVar <- input$time
      obsMax <- input$nbInput
      
      # Calcul de l'acpf avec la fonction selon le choix du nombre de CP ou du Threshold
      if (input$choix==1){
        nbcp <- input$nbCP
        acpf_obj <- acpf(data_acpf, acpfVar, id=idVar, time=timeVar, obs_min = obsMax, methode=nbcp)
      } else {
        nb_threshold = input$PVE * 0.01
        acpf_obj <- acpf(data_acpf, acpfVar, id=idVar, time=timeVar, obs_min = obsMax, threshold = nb_threshold)
      }
      
      #SpaghettiPlot
      idSelect <- input$id_select
      data_spag <- data_spag %>% filter(.data[[input$id]] %in% idSelect)
      output$plot_spag <- renderPlot({
        ggplot(data_spag, aes(x=.data[[input$time]],y=.data[[input$variable_acpf]], group=.data[[input$id]], color = .data[[input$id]])) +
          geom_line() +
          labs(title = paste("SpaghettiPlot représentant", input$variable_acpf, "chez les individus sélectionnés"),
               x = "Temps",
               y = input$variable_acpf,
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
      
      # Récuperer la liste des identifiants selon le type de jeu de données
      # Leur attribuer un index
      
      if (IsRegular(data())!="Sparse"){
        liste_id <- sort(unique(data()[[idVar]]))
        # print("Non sparse")
        
        liste_id_df <- data.frame(id = liste_id)
        liste_id_df <- liste_id_df %>% mutate(index = seq(1, n(), 1))
        
        indivPlotdf <- liste_id_df %>% filter(id==input$id_select_score)
        indivPlot <- indivPlotdf$index
        # print(indivPlot)
        # print(class(indivPlot))
      } else {
        # print("sparse")
        
        data_index <- data_frame(id = sort(unique(data()[[idVar]])))
        data_index <- data_index %>% mutate(index = seq(1, n(), 1))
        data_estim <- data.frame(id = data()[[input$id]], temps = data()[[input$time]]) %>% mutate(variable = data()[[input$variable_acpf]])
        data_estim <- left_join(data_estim, data_index)
        data_estim <- data_estim %>% group_by(id) %>% filter(all(sum(!is.na(variable)) >= input$nbInput))
        liste_id_df <- data_estim %>% dplyr::select(id, index) %>% distinct(id, index ,.keep_all = TRUE)

        indivPlotdf <- liste_id_df %>% filter(id==input$id_select_score)
        indivPlot <- indivPlotdf$index
        # print(indivPlot)
        # print(class(indivPlot))
      }
      
      # Prendre les t tels que Y(t) soit non manquant
      if (is.numeric(indivPlotdf$id)==T){
        indivPlotStr <- as.character(indivPlot)
      } else {
        indivPlotStr <- indivPlotdf$id
      }
      print(indivPlotStr)
      dfObs <- data.frame(acpf_obj$acpf$inputData$Ly[[indivPlotStr]], acpf_obj$acpf$inputData$Lt[[indivPlotStr]])
      names(dfObs)[1] <- "Obs"
      names(dfObs)[2] <- "Time"
      # print(dfObs)
      
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
      # print(scores)
      indivSelect <- data.frame(id = data()[[input$id]])
      indivSelect <- indivSelect %>% filter(id %in% input$id_select) 
      indivSelect_id <- sort(unique(indivSelect$id))
      # print(indivSelect_id)
      newIndex <- data.frame(id = indivSelect_id) %>% mutate(index = seq(1, n(), 1))
      # print(newIndex)
      newIndex <- newIndex %>% filter(id==input$id_select_score)
      indivPlot2 <- newIndex$index
      if (is.numeric(newIndex$id)==T){
        indivPlotStr2 <- as.character(indivPlot2)
      } else {
        indivPlotStr2 <- newIndex$index
      }
      # print(indivPlotStr2)
      
      indivScore2 <- scores[as.numeric(indivPlotStr2),]
      # print(indivScore2)
      indivScore_df2 <- as.data.frame(indivScore2)
      temps_score_df2 <- acpf_obj$acpf$workGrid
      indivScore_df2 <- indivScore_df2 %>% mutate(temps = temps_score_df2)
      # print(indivScore_df2)
        
      # Plot en fonction du type de point choisi
      if (input$typeTrace == "etoiles"){
        output$plot_score <- renderPlot({
          ggplot(indivScore_df2, aes(x = temps, y = indivScore2)) +
            geom_line() +
            geom_point(data = dfObs, aes(x = Time, y = Obs), color = "red", shape=4, size = 1) +
            labs(title = paste("Graph des estimations individuelles de la fonction de l'individu", input$id_select_score, "\net ses valeurs observées"),
                  x = "Temps", y = input$variable_acpf) +
            theme_minimal()
        })
      } else {
        output$plot_score <- renderPlot({
          ggplot(indivScore_df2, aes(x = temps, y = indivScore2)) +
            geom_line() +
            geom_line(data = dfObs, aes(x = Time, y = Obs), color = "red") +
            labs(title = paste("Graph des estimations individuelles de la fonction de l'individu", input$id_select_score, "\net valeurs observées"),
                 x = "Temps", y = input$variable_acpf) +
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
      
      print(acpf_obj$acpf$xiEst)
      
      # PlotContribIndividu
      data_contrib <- data.frame(name = c(colnames(as.data.frame(acpf_obj$acpf$xiEst))),
                                 value = acpf_obj$acpf$xiEst[indivPlot2,])
      print(data_contrib)
      data_contrib <- data_contrib[1:acpf_obj$acpf$selectK,]

      output$plot_contrib_individu <- renderPlot ({
        ggplot(data_contrib, aes(x=name, y=value)) +
          geom_bar(stat = "identity",
                   color = "grey",
                   fill = "lightblue") +

          coord_cartesian(ylim = c(min(acpf_obj$acpf$xiEst[,]),max(acpf_obj$acpf$xiEst[,])),#min et max en fonction du min et du max général
                          xlim = c(1,acpf_obj$acpf$selectK)) +
          labs(title = paste("Graph des contributions de chaque CP à la projection de l'individu :", input$id_select_score),
               x = "Composantes principales", y = "Coefficient associé") +
          theme_minimal()
      })
    })
  }
  
  # Run the application 
  shinyApp(ui = ui, server = server)