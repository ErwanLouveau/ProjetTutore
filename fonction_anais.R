
acpf <- function(data, variable, threshold = 0.99, methode='FVE',id, time, obs_min = 2) {

    library(fdapace)
    if (!is.data.frame(data)){
      stop("Le dataframe n'existe pas, est vide, ou n'est pas un dataframe.")
    }
    if (!(variable %in% colnames(data))){
      stop("La variable spécifiée n'existe pas dans le dataframe.")
    }
    data <- data %>% filter(!is.na(.data[[variable]]))
    at_least <- unlist(data %>% group_by(.data[[id]]) %>% group_map(~sum(!is.na(.x[[variable]]))>=obs_min))
    a <- unique(data[[id]])[at_least]
    b<-data[[id]]
    data_var <- data %>% filter(b %in% a)
    data_list <- split(data_var, f = data_var[[id]] )
    data_Ly <- lapply(data_list, function(.x) return(.x[[variable]]))
    data_Lt <- lapply(data_list, function(.x) return(.x[[time]]))
    acpf <- FPCA(data_Ly, data_Lt, list(FVEthreshold = threshold, methodSelectK=methode))
    return(acpf)
}




