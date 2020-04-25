#' A Function That Fits a Parallel Reflective Latent Variable Measurement Model to a Set of Variables
#'
#' This function fits a parallel (i.e., unit-weighting) reflective latent variable model (described in McNeish & Wolf, 2020) using the indicators  in the supplied data frame
#' and the lavaan function. Factor loadings are fixed to 1 and residual variances are fixed to equality
#' @param df input data frame consisting of variables in the measurement model
#' @return a fitted lavaan object
#' @export
#' @examples
#' par.hs <- parallel(df)

parallel <- function(df){
  eta <- sprintf("LV =~")
  lambda.list = list()
  for (i in 1:length(names(df))) {
    lambda.list[[i]]=sprintf("l%s*%s",1, names(df)[[i]])
  }
  lambda = gsub(" ", "",paste(eta,paste(lambda.list, collapse = "+")), fixed = T)

  theta.list = list()
  for (i in 1:length(names(df))) {
    theta.list[[i]]=sprintf("%s ~~ theta*%s",names(df)[i], names(df)[i])
  }
  theta = paste(theta.list, collapse = "\n")

  parallel.model <- sprintf("#Loadings\n%s\n\n#Residuals\n%s", lambda, theta)
  cat(parallel.model,"\n")
  model.fit <- lavaan::sem(parallel.model, data=df)
  return(model.fit)
}
