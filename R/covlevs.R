#' A Function to Extract Covariate Levels for DIF Visualization
#'
#' This function takes the outputted df from prods and generates the lavaan script necessary to fit a RFA model of uniform and non-uniform DIF
#' @param df input data frame consisting of factor indicators and DIF covariate
#' @param covname input character vector name for DIF covariate in df
#' @param covmin input numeric for minimum value of covariate to begin estimating unidimensional factor models
#' @param covmax input numeric for maximum value of covariate to begin estimating unidimensional factor models
#' @return a range of values within the covariate
#' @export
#' @examples
#' levs <- covlevs(df, covname = "pers.lad", covmin = 3, covmax = 8)

covlevs <- function(df, covname, covmin, covmax){

  levs <- seq_along(unique(df[[covname]]))
  levs <- levs[covmin:covmax]
  #levs <- levs %>%
  #dplyr::filter(<= covmax & < >=covmin)

  return(levs)
}
