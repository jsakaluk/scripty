#' A Function that Creates a Filtered df for DIF plotting
#'
#' This function takes output from difests and filters it to a set of loadings or intercepts. User can specify a vector of items that show significant DIF for intercepts (uniform) or loadings (non-uniform) for later enhanced plotting
#' @param out output df of measurement model parameters from difests
#' @param type whether user wants to extract loading ("loads") or interpts "ints"
#' @param sig.uni an optional character vector of item #s that show significant uniform DIF
#' @param sig.nonuni an optional character vector of item #s that show significant non-uniform DIF
#' @return a filtered data frame ready for ggplot
#' @export
#' @examples
#' ests <- getdifests(out, type = "loads", sig.nonuni = sig.nonuni)
#'
getdifests <- function(out, type = NULL, sig.uni = NULL, sig.nonuni = NULL){
  if(type == "loads"){
    ests <- dplyr::filter(out, op == "=~")
    ests$sig <- FALSE
    for(i in 1:length(sig.nonuni)){
      ests$sig[endsWith(ests$rhs, sig.nonuni[i])==TRUE] <- TRUE
    }
  }else if(type == "ints"){
    ests <- dplyr::filter(out, op == "~1" & lhs != "LV")
    ests$sig <- FALSE
    for(i in 1:length(sig.uni)){
      ests$sig[endsWith(ests$lhs, sig.uni[i])==TRUE] <- TRUE
    }
  }
  return(ests)
}
