#' A Function that Conducts a MIMIC model analysis of DIF
#'
#' This function wraps the other MIMIC-related functions to fit and reproducibly
#' report on a MIMIC model analysis of DIF
#' @param df input data frame consisting of variables in the measurement model
#' @param covname character input of name of covariate in data frame
#' @param lvname input character of latent varaible name for scripting
#' @param match.op supplies indprod (default to FALSE)
#' @param doubleMC.op supplies inprod (default to TRUE)
#' @param cat should a categorical estimator (estimator = "ULSMVS") be used? (default to FALSE)
#' @return terminates with outputting reproducible table from mimicout
#' @export
#' @examples
#' mimicdif(df, covname = "age", lvname = "openness")

mimicdif <- function(df, covname, lvname, match.op = FALSE, doubleMC.op = T, cat = FALSE){
  df2 <- prods(df, covname = covname)
  script <- mimic(proddf = df2, lvname = lvname)
  if(cat == TRUE){
    fit.mimic <- cfa(script, data = df2, meanstructure = TRUE, estimator = "ULSMVS")
  }else if(cat == FALSE){
    fit.mimic <- cfa(script, data = df2, meanstructure = TRUE)
  }

  mimic.param <- mimicparam(fit.mimic)
  mimicout(fit.mimic, mimic.param)
}
