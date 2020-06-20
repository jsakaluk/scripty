#' A Function That Fits a Congeneric Reflective Latent Variable Measurement Model to a Set of Variables
#'
#' This function fits a congeneric reflective latent variable model (described in McNeish and Wolf, 2020) using the indicators in the specified in the supplied data frame
#' and the lavaan function with fixed-factor scaling. FIML is used by default.
#' @param df input data frame consisting of variables in the measurement model
#' @return a fitted lavaan object
#' @export
#' @examples
#' cong.hs <- congeneric(df)

congeneric <- function(df, lvname){
  dir.create("./scripts")
  eta <- sprintf("%s =~ NA*", lvname)
  lambda <- gsub(" ", "",paste(eta,paste(names(df), collapse = "+")), fixed = T)
  psi <- sprintf("%s ~~ 1*%s", lvname, lvname)
  congeneric.model <- sprintf("#Loadings\n%s\n\n#Variances\n%s", lambda, psi)
  cat(congeneric.model,"\n", file = sprintf("./scripts/%s.txt", lvname))
  model.fit <- lavaan::sem(congeneric.model, data=df, missing = "ml")
  return(model.fit)
}
