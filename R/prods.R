#' A Function Creates Product Indicators Needed to Script a  MIMIC Model of DIF
#'
#' This function uses semTools to create the product-terms needed to specify
#' the latent interaction needed to test non-uniform DIF in a MIMIC model
#' @param df input data frame consisting of variables in the measurement model
#' @param covname character input of name of covariate in data frame
#' @param match.op supplies indprod (default to FALSE)
#' @param doubleMC.op supplies inprod (default to TRUE)
#' @return a tibble of the original data frame, covariate, and product indicators
#' @export
#' @examples
#' df2 <- prods(df, covname = "age")


prods <- function(df, covname, match.op = FALSE, doubleMC.op = T){

  indnames <- names(df)
  indnames <- indnames[indnames != covname]

  newdf <- semTools::indProd(var1 = indnames, var2 = covname, match = match.op,
                      data = df[ , c(covname, indnames)], doubleMC = doubleMC.op)

  return(newdf)
}
