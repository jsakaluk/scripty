#' A Function that Extracts DIF-Relevant Parameters
#'
#' This function takes the list from covfilters and runs a congeneric model on each and extracts and combines their parameter estimates
#' @param filter.dat input list from covfilters
#' @param levs range of covariate levels for DIF examination (returned by covlevs)
#' @return a data frame of parameter estimates from congeneric models fit on filtered dfs at each level of levs
#' @export
#' @examples
#' outdf <- difests(filter.dat, levs)

difests <- function(filter.dat, levs){
  mods<- list()
  output <- list()
  for(i in 1:length(filter.dat)){
    mods[[i]] <- congeneric(df = dplyr::select(filter.dat[[i]], -.data[[covname]]), lvname = "LV" )
    output[[i]] <- lavaan::parameterestimates(mods[[i]], standardized = TRUE)
    output[[i]]$covlev <- levs[[i]]
    output[[i]] <- as.data.frame(output[[i]])
  }
  outdf <- do.call(rbind, output)
  return(outdf)
}
