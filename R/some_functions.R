library(dplyr)

# binaryExtr --------------------------------------------------------------

binaryExtr <- function(feature) {

  indici <- grep(feature, data$other_features, ignore.case = TRUE)
  
  col_name <- gsub(" ", "_", tolower(feature))
  
  data[[col_name]] <<- ifelse(data$ID %in% indici, 1, 0)
  
  # Checks
  cat("Feature:", feature, "\n")
  cat("In tab:", tab[feature], "\n")
  cat("Numero di Righe:", length(data[[col_name]]), "\n")
  cat("Somma degli 1:", sum(data[[col_name]][indici]), "\n")
  cat("Somma degli 0:", sum(data[[col_name]][-indici]), "\n")
  cat("Tabella:\n")
  print(table(data[[col_name]]))
  
  data[[col_name]][indici] <<- "yes"
  data[[col_name]][-indici] <<- "no"
  
  data[[col_name]] <<- as.factor(data[[col_name]])
  
}


# Impute ------------------------------------------------------------------

impute <- function(var, var.impute) {
  if (is.ordered(var)) {
    original_levels <- levels(var)
    var_chr <- as.character(var)
    var.impute_chr <- as.character(var.impute)
    
    var_imputed <- ifelse(is.na(var_chr), var.impute_chr, var_chr)
    
    factor(var_imputed, levels = original_levels, ordered = TRUE)
    
  } else if (is.factor(var)) {
    original_levels <- levels(var)
    var_chr <- as.character(var)
    var.impute_chr <- as.character(var.impute)
    
    var_imputed <- ifelse(is.na(var_chr), var.impute_chr, var_chr)
    
    factor(var_imputed, levels = union(original_levels, var.impute_chr))
    
  } else {
    ifelse(is.na(var), var.impute, var)
  }
}



# Random Imputation -------------------------------------------------------

random.imp <- function(var){
  missing <- is.na(var)
  n.missing <- sum(missing)
  var.obs <- var[!missing]
  imputed <- var
  imputed[missing] <- sample(var.obs, n.missing, replace = T)
  return (imputed)
}


# Risultati ---------------------------------------------------------------

save(binaryExtr, random.imp, impute,  file = "funzioni_progetto.Rdata")
