library(tidyverse)

#test

#1
Sys.setlocale("LC_TIME", "en_US.UTF-8")
library(fda)
library(fdapace)

#donnes temperature
data(CanadianWeather,package = "fda")
temperature <- as.data.frame(CanadianWeather$dailyAv[, , "Temperature.C"])
temperature <- cbind("jour" = rownames(temperature),temperature)
temperature_pivoter<- pivot_longer(temperature, cols = colnames(temperature)[2:36], names_to = "ville")
temperature_pivoter$jour <- as.Date(temperature_pivoter$jour, format = "%b%d")

# temperature acpf
temperature_pivoter$jour <- as.numeric(temperature_pivoter$jour) # transformer date en jour
temperature_pivoter <- temperature_pivoter %>% 
  mutate(jour=jour-19723)
list_temp <- split(temperature_pivoter, temperature_pivoter$ville) #créer une liste de data frame pour chaque ville
Ly_temp <- lapply(list_temp, function(x) return(x$value)) #le même data frame avec juste la colonne temperature
Lt_temp <- lapply(list_temp, function(x) return(x$jour)) 

#donnes precipitation
precipitation <- as.data.frame(CanadianWeather$dailyAv[, , "Precipitation.mm"])
precipitation <- cbind("jour" = rownames(precipitation), precipitation)
precipitation_pivoter<- pivot_longer(precipitation, cols = colnames(precipitation)[2:36], names_to = "ville")
precipitation_pivoter$jour <- as.Date(precipitation_pivoter$jour, format = "%b%d")

# precipitation acpf
precipitation_pivoter$jour <- as.numeric(precipitation_pivoter$jour) # transformer date en jour
precipitation_pivoter <- precipitation_pivoter %>% 
  mutate(jour=jour-19723)
list_prec <- split(precipitation_pivoter, precipitation_pivoter$ville) #créer une liste de data frame pour chaque ville
Ly_prec <- lapply(list_prec, function(x) return(x$value)) #le même data frame avec juste la colonne temperature
Lt_prec <- lapply(list_prec, function(x) return(x$jour)) 


#donnes log10prec
log10prec <- as.data.frame(CanadianWeather$dailyAv[, , "log10precip"])
log10prec <- cbind("jour" = rownames(log10prec),log10prec)
log10prec_pivoter<- pivot_longer(log10prec, cols = colnames(log10prec)[2:36], names_to = "ville")
log10prec_pivoter$jour <- as.Date(log10prec_pivoter$jour, format = "%b%d")

# precipitation acpf
log10prec_pivoter$jour <- as.numeric(log10prec_pivoter$jour) # transformer date en jour
log10prec_pivoter <- log10prec_pivoter %>% 
  mutate(jour=jour-19723)
list_log10prec <- split(log10prec_pivoter, log10prec_pivoter$ville) #créer une liste de data frame pour chaque ville
Ly_log10prec <- lapply(list_log10prec, function(x) return(x$value)) #le même data frame avec juste la colonne temperature
Lt_log10prec <- lapply(list_log10prec, function(x) return(x$jour)) 



#2
library(JM)
data(package="JM")
pbc2

#3
jd3 <- read.csv("C:/Users/Utilisateur/Downloads/test_measure.csv")
jd3
#données denses (plusieurs par minutes messures environ toute les deux secondes)
#données irrégulière les sujets prennent des messures à tes temps différents.
#erreur le fichier ne semble pas avoir de bruit
#les mesures VO2, VCO2 et VE sont manquantes pour 30 tests = données manquantes

# User interface ----
ui <- fluidPage(
  titlePanel("Projet Tutoré"),
  sidebarLayout(
    sidebarPanel(
      helpText("Météo Canadienne par jour et par Etat"),
      
      selectInput("var", 
                  label = "Choisi la variable métérologique à observer",
                  choices = c("Précipitation", "",
                              "Temperature", "log10precip"),
                  selected = "Temperature"),
      
      checkboxGroupInput("ville", 
                         label = "Choisissez les villes que vous voulez observer",
                         choices = CanadianWeather$place,
                         selected = c("St. Johns","Halifax","Sydney","Yarmouth","Charlottvl","Fredericton","Scheffervll",
                                      "Arvida","Bagottville","Quebec","Sherbrooke","Montreal","Ottawa","London","Toronto",
                                      "Thunder Bay")),
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
                column(6, plotOutput("phi")))
              )
  )
)

# Server logic ----
server <- function(input, output) {
  # Créer le diagramme spaghetti avec ggplot2
  output$plot_spag<- renderPlot({
    tableau <- switch(input$var, 
                      "Précipitation" = precipitation_pivoter,
                      "Temperature" = temperature_pivoter,
                      "log10precip"= log10prec_pivoter 
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
    acpf <- switch(input$var, 
                      "Précipitation" = FPCA(Ly_prec[input$ville], Lt_prec[input$ville], list(dataType = "Dense")),
                      "Temperature" = FPCA(Ly_temp[input$ville], Lt_temp[input$ville], list(dataType = "Dense")),
                      "log10precip"= FPCA(Ly_log10prec[input$ville], Lt_log10prec[input$ville], list(dataType = "Dense"))
    )

    plot(acpf$mu,type="l", xlab="nombre de jours", ylab=input$var, main=" fonction mu de l'ACPF")
  })
  output$phi<- renderPlot({
    acpf_cp <- switch(input$var, 
                   "Précipitation" = FPCA(Ly_prec[input$ville], Lt_prec[input$ville], list(dataType = "Dense",FVEthreshold=1)),
                   "Temperature" = FPCA(Ly_temp[input$ville], Lt_temp[input$ville], list(dataType = "Dense",FVEthreshold=1)),
                   "log10precip"= FPCA(Ly_log10prec[input$ville], Lt_log10prec[input$ville], list(dataType = "Dense",FVEthreshold=1)))
    if (input$choix==1){
      phi <- as.data.frame(acpf_cp$phi)
      phi <- cbind("jour"=1:365,phi)
      phi_pivoter<- pivot_longer(phi, cols = colnames(phi)[2:(input$nbCP+1)], names_to = "CP")
      ggplot(phi_pivoter, aes(x=phi_pivoter$jour ,y=phi_pivoter$value, group=phi_pivoter$CP, color = phi_pivoter$CP)) +
        geom_line() + 
        labs(title = paste("Diagramme représentant les composantes principales"),
             x = "Jours",
             y = paste("Variation de la ", input$var),
             color="CP") +
        ylim(min(phi),max(phi[-1]))+
        theme_bw()
    }
    
    else if (input$choix==2){
      acpf_pve <- switch(input$var, 
                     "Précipitation" = FPCA(Ly_prec[input$ville], Lt_prec[input$ville], list(dataType = "Dense",FVEthreshold =input$PVE/100)),
                     "Temperature" = FPCA(Ly_temp[input$ville], Lt_temp[input$ville], list(dataType = "Dense",FVEthreshold =input$PVE/100)),
                     "log10precip"= FPCA(Ly_log10prec[input$ville], Lt_log10prec[input$ville], list(dataType = "Dense",FVEthreshold =input$PVE/100)))
      phi <- as.data.frame(acpf_pve$phi)
      phi <- cbind("jour"=1:365,phi)
      phi_pivoter<- pivot_longer(phi, cols = colnames(phi)[2:ncol(phi)], names_to = "CP")
      ggplot(phi_pivoter, aes(x=phi_pivoter$jour ,y=phi_pivoter$value, group=phi_pivoter$CP, color = phi_pivoter$CP)) +
       geom_line() + 
        labs(title = paste("Diagramme représentant les composantes principales"),
            x = "Jours",
            y = paste("Variation de la ", input$var),
             color="CP") +
        ylim(min(acpf_cp$phi),max(acpf_cp$phi[-1]))+
           theme_bw()
    }
 
  })
 
  
  
  
}

# Run app ----
shinyApp(ui, server)


#pouvoir mettre axe y selon les variable choisi
# voir les différent thème
#changer geom_lines en geom_smooth()
#tester le graph avec les uatres jeu de données
