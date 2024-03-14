# 0 - INFOS
# Dans le code le "score" sont les estimations individuelles de la fonction d'un individu --> A corriger
# Il y a un bug dans la sélection des individus pour les jeu de données sparses, en particulier ceux où des individus n'ont que des NA

# 1 - CHARGEMENT DES PACKAGES
library(shiny)
library(skimr)
library(dplyr)
library(tidyverse)
library(fdapace)


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
  ui <- fluidPage(
    tabsetPanel(tabPanel("Importation et visualisation des données",
                         sidebarLayout(
                           sidebarPanel(
                             # jeu de données
                             fileInput("file1", "Choose csv File", accept = ".csv"),
                             checkboxInput("header", "Header", TRUE),
                             # Nom d'une première colonne ?
                             # Si le jeu de données est mal orienté
                             checkboxInput("oriente", "Les noms de colonnes sont t-il en première ligne ? Si oui le jeu de données
                             sera automatiquement ré-orienté",value = FALSE),
                             # variables présentes dans le jeu de données à transformer
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
                             selectInput("time", label = "Variable temps (de type numérique)",
                                         choices = NULL,multiple = F),
                             numericInput("nbInput",label="Nombre d'observation minimum pour intégrer un individu dans l'ACPF :", value = 2, min = 1 , step=1),
                             selectInput("id_select", label = "Individu(s) sélectionné(s) pour le spaghetti plot",
                                         choices = NULL,multiple = T), # Ensemble des individus selectionnés
                             selectInput("id_select_score", label = "Individus sélectionné pour le calcul des estimations individuelles",
                                         choices = NULL,multiple = F), # Ensemble des individus selectionnés pour le plot des estimation individuelles
                             radioButtons("choix", label="Affichage des Composantes Principales en fonction : ",choices = list("Nombre de CP :" = 1, "PVE" = 2),selected = 2),
                             conditionalPanel( # Choix entre le calcul par nombre de CP selectionnées ou par % de variance expliquée
                               condition = "input.choix == 1",
                               numericInput("nbCP",label="Nombre de CP :", value = 3, min = 1, max = 6)
                             ),
                             conditionalPanel(
                               condition = "input.choix == 2",
                               numericInput("PVE",label=" % de variances expliqué :", value = 99, min =50 , max = 100, step=0.01)
                             ),
                             radioButtons("typeTrace", "Type de tracé pour les observations individuelles",
                                          choices = list("Etoiles" = "etoiles", "Ligne" = "ligne"),
                                          selected = "etoiles"
                             ),
                             actionButton("plotButton", "Plot")
                           ),
                           mainPanel(
                             fluidRow(
                               column(6, plotOutput("plot_spag")), # SpaghettiPlot
                               column(6, plotOutput("plot_mu")), # Plot de mu
                               column(6, plotOutput("plot_score")), # Plot des estimations individuelles
                               column(6, plotOutput("plot_phi")), # Plot des composantes principales
                               column(12, plotOutput("plot_varExpPVE"))) 
                           )
                         )
      )
    )
  )
  
  # SERVER
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
      updateSelectInput(session, "variable", choices = var_names, selected = var_names[1])
      updateSelectInput(session, "variable_acpf", choices = var_names_num, selected = var_names_num[1])
      updateSelectInput(session, "id", choices = var_names, selected = if (is.null(input$id)) var_names[1] else input$id)
      updateSelectInput(session, "time", choices = var_names, selected = var_names[1])
      
      # LE BUG DES INDIVIDUS PROVIENT SUREMENT D'ICI. IL N'Y A AUCUNE VERIFICATION SI LES INDIVIDUS SELECTIONNES ONT LE NBMIN DE DONNEES DANS LA VARIABLE SELECTIONNEE
            
      # variable_select <- input$variable_acpf
      # nom_var_id <- input$id
      # variable_temps <- input$time
      
      # if (!is.null(input$id) && input$id != ""){
      if (input$id != ""){

        
        # current_data_bis <- current_data %>% select(variable_temps, variable_select, nom_var_id) %>% 
        #   filter(!is.na(variable_select))
        # print(current_data_bis)
        # 
        # id_select <- unique(current_data_bis[,nom_var_id])
        # 
        # updateSelectInput(session, "id_select", choices = id_select, selected = id_select[1])
        # updateSelectInput(session, "id_select_score", choices = id_select)
        

        # current_data_bis <- current_data %>% select()
        
        # print(current_data[which(is.null(current_data[variable_select])==F), input$id])
        
      
        id_select <- unique(current_data[,input$id])
        id_select_score <- unique(current_data[,input$id])
        updateSelectInput(session, "id_select", choices = id_select, selected = id_select[1])
        updateSelectInput(session, "id_select_score", choices = id_select_score)
      }
      
    })
    # visualisation des données
    output$test <- renderTable({
      data2 <- data() %>% 
        mutate_at(input$variable, as.factor)
      
      # merge les stats les deux fonctions pour les stats g et les na par personne
      merge(skim(data2), na_person(data2,input$id), by.x = "skim_variable")
    })
    
    #plot
    observeEvent(input$plotButton, { # ObserveEvent qui permet de recalculer tout son contenu quand le bouton Plot est utilisé
      data_spag <- data() # jeu de données qui sert au plot du SpaghettiPlot
      
      data_acpf <- data() # jeu de données qui sert a l'ACPF
      acpfVar <- input$variable_acpf
      idVar <- input$id
      timeVar <- input$time
      obsMin <- input$nbInput
      
      # Calcul de l'acpf avec la fonction selon le choix du nombre de CP ou du Threshold
      if (input$choix==1){
        nbcp <- input$nbCP
        acpf_obj <- acpf(data_acpf, acpfVar, id=idVar, time=timeVar, obs_min = obsMin, methode=nbcp)
      } else {
        nb_threshold = input$PVE * 0.01
        acpf_obj <- acpf(data_acpf, acpfVar, id=idVar, time=timeVar, obs_min = obsMin, threshold = nb_threshold)
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
      
      # /!\ ENORME BUG DES IDENTIFIANTS A CORRIGER POUR QUE LE CODE FONCTIONNE SUR LES DONNEES SPARSES OU DONNEES MANQUANTES
      # UNIQUEMENT CHEZ CERTAINS INDIVIDUS - INDEX A CORRIGER/!\
      # ScorePlot
        # Acpf calculée sur l'ensemble des individus et non sur les individus sélectionnés
      scores <- acpf_obj$acpf$xiEst %*% t(acpf_obj$acpf$phi) + matrix(rep(acpf_obj$acpf$mu, times = length(acpf_obj$data_obs)), nrow = length(acpf_obj$data_obs), byrow = TRUE)
      # Créer un index qui permet d'attribuer un numero à chaque identifiant de la variable id selectionnée. Peut importe si l'id est numeric, factor ou character
      liste_id <- sort(unique(data_acpf[[idVar]]))
      liste_id_df <- as.data.frame(liste_id)
      liste_id_df <- liste_id_df %>% mutate(id_num = seq(1, n(), 1))
      position <- which(liste_id_df$liste_id %in% input$id_select_score)
      indivPlot <- liste_id_df[position, "id_num"]
      
      # Nom des variables selon le type de la variable en entrée
      if (is.numeric(data_acpf[[idVar]])==T){
        nomV = paste0("X",indivPlot)
      } else {
        nomV = input$id_select_score
      }
      # Prendre les t tels que Y(t) soit non manquant
      # print(as.data.frame(acpf_obj$acpf$inputData$Ly[indivPlot]))
      Obs <- as.data.frame(acpf_obj$acpf$inputData$Ly[indivPlot]) %>% rename(obs = nomV)
      if (IsRegular(data_acpf)=="Sparse"){
        Time <- as.data.frame(acpf_obj$acpf$inputData$Lt[indivPlot]) %>% rename(temps = nomV)
      } else {
        Time <- as.data.frame(acpf_obj$acpf$workGrid)
        names(Time)[1] <- "temps"
      }
      dfObs <- cbind(Obs, Time) # données observées avec leur temps
      
      # Prendre les scores de l'individu selectionné
      indivScore <- scores[as.numeric(indivPlot),]
      indivScore_df <- as.data.frame(indivScore)
      temps_score_df <- acpf_obj$acpf$workGrid
      indivScore_df <- indivScore_df %>% mutate(temps = temps_score_df)
      
        # Plot en fonction du type de point choisi
      if (input$typeTrace == "etoiles"){
        output$plot_score <- renderPlot({
          ggplot(indivScore_df, aes(x = temps, y = indivScore)) +
            geom_line() +
            geom_point(data = dfObs, aes(x = temps, y = obs), color = "red") +
            labs(title = paste("Graph des estimations individuelles de la fonction de l'individu", input$id_select_score, "\net ses valeurs observées"),
                  x = "Temps", y = input$variable_acpf) +
            theme_minimal()
        })
      } else {
        output$plot_score <- renderPlot({
          ggplot(indivScore_df, aes(x = temps, y = indivScore)) +
            geom_line() +
            geom_line(data = dfObs, aes(x = temps, y = obs), color = "red") +
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
          geom_bar(stat = "identity", fill="steelblue") +
          geom_line(aes(x = factor(CP, levels = unique(CP)), y = pve, group=1), color = "black") +
          geom_point(aes(x = factor(CP, levels = unique(CP)), y = pve), shape=1, color = "black", size = 2) +
          labs(title = paste("Graph des variances expliquées par les composantes principales et tracé de la fréquence cumulée de variation expliquée"),
               x = "Composantes principales",
               y = "Variance expliquée") +
          theme_minimal()
      })
    })
  }
  
  # Run the application 
  shinyApp(ui = ui, server = server)