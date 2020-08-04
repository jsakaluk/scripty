#' A Function to Extract Covariate Levels for DIF Visualization
#'
#' This function takes the outputted df from prods and generates the lavaan script necessary to fit a RFA model of uniform and non-uniform DIF
#' @param df input data frame consisting of factor indicators and DIF covariate
#' @param covname input character vector name for DIF covariate in df
#' @param levs range of covariate levels for DIF examination (returned by covlevs)
#' @return a list of filtered data frames consiting of the indicators of the factor (one per level of levs)
#' @export
#' @examples
#' filter.list <- covfilters(df, covname = "pers.lad", levs)

covfilters <- function(df, covname, levs){

  filterlist <- list()

  for(i in 1:length(levs)){
    filterlist[[i]] <- df %>% filter(.data[[covname]] == levs[[i]])
  }
  return(filterlist)
}
