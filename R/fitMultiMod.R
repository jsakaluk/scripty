#' A function that scripts and fits a CFA model
#'
#' This function takes a named list of indicator variable names;
#' if there is only one character vector, a unidimensional CFA is fit,
#' but if there is more than one character vector, a multidimensional CFA
#' is fit, whereby the latent variables are named after the elements of the
#' list. Function by Steph Gauvin
#'
#' @param df the data frame that includes the to-be analyzed indicators
#' @param vars a named list of one or more character vectors of indicator names
#' @param categ an optional logical (defaults to FALSE) for whether indicators are categorical (in which case, ordered ULSMVS estimation is used, otherwise "MLR" is used)
#'
#' @return
#' @export
#'
#' @examples
#' df <- psych::bfi
#'
#' vars <- list(agree = c("A1","A2","A3","A4","A5"),
#' open = c("O1", "O2", "O3", "O4", "O5"),
#' neuro = c("N1", "N2", "N3", "N4", "N5"))
#'
#' mod <- fitMultiMod(df, vars)
fitMultiMod <- function(df, vars, categ = FALSE){
  dir.create("./scripts")
  lambda <- list()
  for(i in 1:length(vars)) {
    eta <- sprintf("%s =~ ", names(vars)[[i]])
    loads <- paste(vars[[i]], collapse = "+")
    loadscript <- paste(eta, loads)
    lambda[[i]] <- loadscript
  }
  lambda <- paste(lambda, collapse = "\n")
  cat(lambda,"\n", file = "./scripts.txt")
  if(isTRUE(categ)){
    model.fit <- lavaan::cfa(lambda, data=df, estimator = "ULSMVS", ordered = TRUE, std.lv = TRUE)
  }else{
    model.fit <- lavaan::cfa(lambda, data=df, estimator = "MLR", missing = "ml", std.lv = TRUE)
  }
  return(model.fit)
}
