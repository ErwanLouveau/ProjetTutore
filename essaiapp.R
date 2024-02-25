# setup ========================================================================
Sys.setlocale("LC_TIME", "en_US.UTF-8")
library(fda)
library(fdapace)
library(tidyverse)
library(DynForest)
library(JM)

# load data ====================================================================
data("pbc2")
pbc2 <- pbc2 %>% arrange(id)
data("CanadianWeather")

# acpf function ================================================================
acpf <- function(data, variable, threshold = 0.99, type, donnees, id="id", time="time", obs_min = 2, uniteTemps = "jour", format_date = "%b %d", type_location = "ville") {
  
  acpf_sparse <- function(data, variable, id="id", time="year", obs_min = 2, threshold = 0.99){
    if (!is.data.frame(data)){
      stop("Le dataframe n'existe pas, est vide, ou n'est pas un dataframe.")
    }
    if (!(variable %in% colnames(data))){
      stop("La variable spécifiée n'existe pas dans le dataframe.")
    }
    
    data <- data %>% filter(!is.na(.data[[variable]]))
    
    at_least <- unlist(data %>% group_by(.data[[id]]) %>% group_map(~sum(!is.na(.x[[variable]]))>=obs_min))
    data_var <- data %>% filter(.data[[id]] %in% sort(unique(data$id))[at_least])
    
    data_list <- split(data_var, f = data_var$id)
    data_Ly <- lapply(data_list, function(.x) return(.x[[variable]]))
    data_Lt <- lapply(data_list, function(.x) return(.x[[time]]))
    
    acpf_ <- FPCA(data_Ly, data_Lt, list(dataType = "Sparse", FVEthreshold = threshold))
    return(list(acpf = acpf_, data_obs = data_Ly, time = data_Lt))
  }
  
  acpf_dense_list <- function(data, donnees, variable, threshold = 0.99, uniteTemps = "jour", format_date = "%b %d", type_location = "ville"){
    temp <- as.data.frame(data[[donnees]][, , variable])
    
    colname <- uniteTemps
    temp <- dplyr::mutate(temp, !!colname := rownames(temp)) %>% relocate(last_col(), .before  = 1)
    
    names_rows <- type_location
    temp_pivot <- pivot_longer(temp, cols = colnames(temp)[2:ncol(temp)], names_to = names_rows)
    
    Sys.setlocale("LC_TIME", "C")
    temp_pivot <- mutate(temp_pivot, !!uniteTemps := as.Date(temp_pivot[[uniteTemps]], format = format_date))
    temp_pivot <- mutate(temp_pivot, !!uniteTemps := as.numeric(temp_pivot[[uniteTemps]])) 
    temp_pivot <- mutate(temp_pivot, !!uniteTemps := temp_pivot[[uniteTemps]] - min(temp_pivot[[uniteTemps]]))
    
    list_temp <- split(temp_pivot, temp_pivot[[type_location]])
    Ly_temp <- lapply(list_temp, function(.x) return(.x[["value"]]))
    Lt_temp <- lapply(list_temp, function(.x) return(.x[[uniteTemps]]))
    
    acpf_ <- FPCA(Ly_temp, Lt_temp, list(dataType = "Dense", FVEthreshold = threshold))
    
    return(list(acpf = acpf_, data_obs = Ly_temp, time = Lt_temp, data_pivot = temp_pivot))
  }
  
  if (type == "sparse") {
    return(acpf_sparse(data, variable, id, time, obs_min, threshold))
  } else if (type == "denseList") {
    return(acpf_dense_list(data, donnees, variable, threshold, uniteTemps, format_date, type_location))
  } else {
    stop("Type non pris en charge.")
  }
}



# function application =========================================================
# acpf_cw_temperature <- acpf(CanadianWeather, donnees = "dailyAv", variable = "Temperature.C", type="denseList")
# acpf_cw_precipitationmm <- acpf(CanadianWeather, donnees = "dailyAv", variable = "Precipitation.mm", type="denseList")
# acpf_cw_log10prec <- acpf(CanadianWeather, donnees = "dailyAv", variable = "log10precip", type="denseList")

# Paramètre de l'application ===================================================


# User interface ----
ui <- fluidPage(
  titlePanel("Projet Tutoré"),
  sidebarLayout(
    sidebarPanel(
      helpText("Météo Canadienne par jour et par Etat"),
      
      selectInput("var", 
                  label = "Variable métérologique à observer",
                  choices = c("Précipitation", "Temperature", "log10precip"),
                  selected = "Temperature"),
      
      checkboxGroupInput("ville", 
                         label = "Villes à observer",
                         choices = sort(CanadianWeather$place),
                         selected = c("Arvida")),
      radioButtons("choix", h3("Radio buttons"),choices = list("Nombre de CP :" = 1, "PVE" = 2),selected = 2),
      
      conditionalPanel(
        condition = "input.choix == 1",
        numericInput("nbCP",label="Nombre de CP :", value = 3, min = 1, max = 6)
      ),
      conditionalPanel(
        condition = "input.choix == 2",
        numericInput("PVE",label=" % de variances expliqué :", value = 99, min =50 , max = 100)
      )
    ),
    
    mainPanel(fluidRow(
      column(6, plotOutput("plot_spag")),
      column(6, plotOutput("plot_mu"))
    ),
    fluidRow(
      column(6, plotOutput("plot_xiest")))
    )
  )
)

# Server logic ----
server <- function(input, output) {
  acpf_cw_temperature <- acpf(CanadianWeather, donnees = "dailyAv", variable = "Temperature.C", type="denseList")
  acpf_cw_precipitationmm <- acpf(CanadianWeather, donnees = "dailyAv", variable = "Precipitation.mm", type="denseList")
  acpf_cw_log10prec <- acpf(CanadianWeather, donnees = "dailyAv", variable = "log10precip", type="denseList")
  
  # Créer le diagramme spaghetti avec ggplot2
  output$plot_spag<- renderPlot({
    tableau <- switch(input$var, 
                      "Précipitation" = acpf_cw_precipitationmm$data_pivot,
                      "Temperature" = acpf_cw_temperature$data_pivot,
                      "log10precip"= acpf_cw_log10prec$data_pivot
    )
    tableau_ville <- filter(tableau, tableau$ville %in% input$ville)
    ggplot(tableau_ville, aes(x=tableau_ville$jour ,y=tableau_ville$value, group=tableau_ville$ville, color = tableau_ville$ville)) +
      geom_line() + 
      labs(title = paste("Diagramme représentant la", input$var, "dans différentes provinces"),
           x = "Jours",
           y = input$var,
           color="Villes") +
      theme_bw()
    
  })
  output$plot_mu<- renderPlot({
    data_mu <- reorder_cw(input$ville)
    acpf_cw_temperature <- acpf(data_mu, donnees = "dailyAv", variable = "Temperature.C", type="denseList")
    acpf_cw_precipitationmm <- acpf(data_mu, donnees = "dailyAv", variable = "Precipitation.mm", type="denseList")
    acpf_cw_log10prec <- acpf(data_mu, donnees = "dailyAv", variable = "log10precip", type="denseList")

    acpf <- switch(input$var,
                   "Précipitation" = acpf_cw_temperature$acpf$mu,
                   "Temperature" = acpf_cw_precipitationmm$acpf$mu,
                   "log10precip"= acpf_cw_log10prec$acpf$mu
    )

    plot(acpf,type="l", xlab="nombre de jours", ylab=input$var, main=" fonction mu de l'ACPF")
  })
  output$plot_xiest<- renderPlot({
    data_mu <- reorder_cw(input$ville)
    acpf_cw_temperature_xiest <- acpf(data_mu, donnees = "dailyAv", variable = "Temperature.C", type="denseList")
    acpf_cw_precipitationmm_xiest <- acpf(data_mu, donnees = "dailyAv", variable = "Precipitation.mm", type="denseList")
    acpf_cw_log10prec_xiest <- acpf(data_mu, donnees = "dailyAv", variable = "log10precip", type="denseList")
    # calcul des valeurs estimées des composantes + données observées
    acpf <- switch(input$var,
                   "Précipitation" = list(y_est = acpf_cw_precipitationmm$acpf$mu + (acpf_cw_precipitationmm$acpf$xiEst %*% t(acpf_cw_precipitationmm$acpf$phi)),
                                          obs = data.frame(time=1:365,acpf_cw_precipitationmm_xiest$data_obs)),
                   "Temperature" = list(y_est = acpf_cw_temperature$acpf$mu + (acpf_cw_temperature$acpf$xiEst %*% t(acpf_cw_temperature$acpf$phi)),
                                        obs = data.frame(time=1:365,acpf_cw_temperature_xiest$data_obs)),
                   "log10precip"= list(y_est = acpf_cw_log10prec$acpf$mu + (acpf_cw_log10prec$acpf$xiEst %*% t(acpf_cw_log10prec$acpf$phi)),
                                       obs = data.frame(time=1:365,acpf_cw_log10prec_xiest$data_obs))
    )
    plot(acpf$y_est[1,], type = "l", xlab = "Time", ylab = "Y_est", main="Valeurs estimées de la première composante avec toutes les villes")
    for (i in 2:ncol(acpf$obs)) {
      lines(acpf$obs$time, acpf$obs[, i], col = i - 1)
    }
  })
  # output$phi<- renderPlot({
  #   acpf_cp <- switch(input$var, 
  #                     "Précipitation" = FPCA(Ly_prec[input$ville], Lt_prec[input$ville], list(dataType = "Dense",FVEthreshold=1)),
  #                     "Temperature" = FPCA(Ly_temp[input$ville], Lt_temp[input$ville], list(dataType = "Dense",FVEthreshold=1)),
  #                     "log10precip"= FPCA(Ly_log10prec[input$ville], Lt_log10prec[input$ville], list(dataType = "Dense",FVEthreshold=1)))
  #   if (input$choix==1){
  #     phi <- as.data.frame(acpf_cp$phi)
  #     phi <- cbind("jour"=1:365,phi)
  #     phi_pivoter<- pivot_longer(phi, cols = colnames(phi)[2:(input$nbCP+1)], names_to = "CP")
  #     ggplot(phi_pivoter, aes(x=phi_pivoter$jour ,y=phi_pivoter$value, group=phi_pivoter$CP, color = phi_pivoter$CP)) +
  #       geom_line() + 
  #       labs(title = paste("Diagramme représentant les composantes principales"),
  #            x = "Jours",
  #            y = paste("Variation de la ", input$var),
  #            color="CP") +
  #       ylim(min(phi),max(phi[-1]))+
  #       theme_bw()
  #   }
  #   
  #   else if (input$choix==2){
  #     acpf_pve <- switch(input$var, 
  #                        "Précipitation" = FPCA(Ly_prec[input$ville], Lt_prec[input$ville], list(dataType = "Dense",FVEthreshold =input$PVE/100)),
  #                        "Temperature" = FPCA(Ly_temp[input$ville], Lt_temp[input$ville], list(dataType = "Dense",FVEthreshold =input$PVE/100)),
  #                        "log10precip"= FPCA(Ly_log10prec[input$ville], Lt_log10prec[input$ville], list(dataType = "Dense",FVEthreshold =input$PVE/100)))
  #     phi <- as.data.frame(acpf_pve$phi)
  #     phi <- cbind("jour"=1:365,phi)
  #     phi_pivoter<- pivot_longer(phi, cols = colnames(phi)[2:ncol(phi)], names_to = "CP")
  #     ggplot(phi_pivoter, aes(x=phi_pivoter$jour ,y=phi_pivoter$value, group=phi_pivoter$CP, color = phi_pivoter$CP)) +
  #       geom_line() + 
  #       labs(title = paste("Diagramme représentant les composantes principales"),
  #            x = "Jours",
  #            y = paste("Variation de la ", input$var),
  #            color="CP") +
  #       ylim(min(acpf_cp$phi),max(acpf_cp$phi[-1]))+
  #       theme_bw()
  #   }
    
  # })
  
  
  
  
}

# Run app ----
shinyApp(ui, server)