# parametres : data, variable, sparse, threshold, observation_min

# load packages ================================================================
library(tidyverse)
library(DynForest)
library(fdapace)
library(JM)
library(fda)

data("pbc2")
pbc2 <- pbc2 %>% arrange(id)
data("CanadianWeather")

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
    return(acpf_)
  }
  
  acpf_dense_list <- function(data, donnees, variable, threshold = 0.99, uniteTemps = "jour", format_date = "%b %d", type_location = "ville"){
    temp <- as.data.frame(CanadianWeather[[donnees]][, , variable])
    
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
    
    return(acpf_)
  }
  
  if (type == "sparse") {
    return(acpf_sparse(data, variable, id, time, obs_min, threshold))
  } else if (type == "denseList") {
    return(acpf_dense_list(data, donnees, variable, threshold, uniteTemps, format_date, type_location))
  } else {
    stop("Type non pris en charge.")
  }
}

acpf_serchol <- acpf(pbc2, "serChol", type="sparse", time="time")
acpf_albumin <- acpf(pbc2, "albumin", type="sparse", time="time")
acpf_cw_temperature <- acpf(CanadianWeather, donnees = "dailyAv", variable = "Temperature.C", type="denseList")

fpca_log10prec <- acpf(CanadianWeather, donnees = "dailyAv", variable = "log10precip", type="denseList")
