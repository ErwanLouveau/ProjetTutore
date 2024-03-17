# load packages ================================================================
library(tidyverse)
library(DynForest)
library(fdapace)
library(JM)
library(fda)

# load data ================================================================
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
acpf_serchol <- acpf(pbc2, "serChol", type="sparse", time="time")
acpf_albumin <- acpf(pbc2, "albumin", type="sparse", time="year")
acpf_cw_temperature <- acpf(CanadianWeather, donnees = "dailyAv", variable = "Temperature.C", type="denseList")
fpca_log10prec <- acpf(CanadianWeather, donnees = "dailyAv", variable = "log10precip", type="denseList")


# function to reorder CanadianWeather ==========================================
reorder_cw <- function(villes){
  #recreer les matrices de canadian weather avec uniquements les villes entrées en paramètres
  df_dailyAv <- as.data.frame(as.table(CanadianWeather$dailyAv))
  df_dailyAv <- df_dailyAv %>% filter(Var2 %in% villes)
  df_place <- villes
  df_province <- data.frame(ville = names(CanadianWeather$province),
                            province = CanadianWeather$province)
  rownames(df_province) <- 1:nrow(df_province)
  df_province <- df_province %>% filter(ville %in% villes)
  df_coordinates <- as.data.frame(as.table(CanadianWeather$coordinates))
  df_coordinates <- df_coordinates %>% filter(Var1 %in% villes)
  df_region <- data.frame(ville = names(CanadianWeather$region),
                          region = CanadianWeather$region)
  rownames(df_region) <- 1:nrow(df_region)
  df_region <- df_region %>% filter(ville %in% villes)
  df_monthlyTemp <- as.data.frame(as.table(CanadianWeather$monthlyTemp))
  df_monthlyTemp <- df_monthlyTemp %>% filter(Var2 %in% villes)
  df_monthlyPrecip <- as.data.frame(as.table(CanadianWeather$monthlyPrecip))
  df_monthlyPrecip <- df_monthlyPrecip %>% filter(Var2 %in% villes)
  df_geogindex <- data.frame(ville = names(CanadianWeather$geogindex),
                             geogindex = CanadianWeather$geogindex)
  rownames(df_geogindex) <- 1:nrow(df_geogindex)
  df_geogindex <- df_geogindex %>% filter(ville %in% villes)
  
  # reformer le dataframe
  cw <- list(
    dailyAv = array(df_dailyAv$Freq, dim = c(365, length(villes), 3),
                    dimnames = list(as.character(unique(df_dailyAv$Var1)), 
                                    as.character(unique(df_dailyAv$Var2)), 
                                    as.character(unique(df_dailyAv$Var3)))),
    place = df_place,
    province = setNames(df_province$province, villes),
    coordinates = array(df_coordinates$Freq, dim = c(length(villes), 2),
                        dimnames = list(as.character(unique(df_coordinates$Var1)), 
                                        as.character(unique(df_coordinates$Var2)))),
    region = setNames(df_region$region, villes),
    monthlyTemp = array(df_monthlyTemp$Freq, dim = c(12, length(villes)),
                        dimnames = list(as.character(unique(df_monthlyTemp$Var1)), 
                                        as.character(unique(df_monthlyTemp$Var2)))),
    monthlyPrecip = array(df_monthlyPrecip$Freq, dim = c(12, length(villes)),
                          dimnames = list(as.character(unique(df_monthlyPrecip$Var1)), 
                                          as.character(unique(df_monthlyPrecip$Var2)))),
    geogindex = setNames(df_geogindex$geogindex, villes)
  )
  
  return(cw)
}

# function application =========================================================
villes <- c("St. Johns", "Halifax", "Sydney", "Yarmouth")
cw <- reorder_cw(villes)
str(cw)
acpf_cw_temperature <- acpf(cw, donnees = "dailyAv", variable = "Temperature.C", type="denseList")
