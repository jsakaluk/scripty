#' A function that scripts and fits a CFA model and returns omega reliability estimates
#'
#' This function takes a named list of indicator variable names and uses fitMultiMod
#' to either script and fit a unidimensional or multidimensional CFA model.
#' It then runs and returns output from semTools::reliability() focusing on
#' omega reliability. Function by Steph Gauvin
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
#' relout <- getOmega(df, vars)
getOmega <- function(df, vars, categ = FALSE){
  fit<-fitMultiMod(df, vars, categ)
  relout <- semTools::reliability(fit, return.total = TRUE)
  return(relout)
}
