#' A Wrapper for DIF vizualizing functions
#'
#' This function automates visualizing uniform or nonuniform DIF across a contiuous covariate
#' @param df input data frame consisting of factor indicators and DIF covariate
#' @param covname input character vector name for DIF covariate in df
#' @param covmin input numeric for minimum value of covariate to begin estimating unidimensional factor models
#' @param covmax input numeric for maximum value of covariate to begin estimating unidimensional factor models
#' @param covtitle  character input of name for covariate (both on x-axis title, and in file name)
#' @param lvtitle character input for name of lv (for file naming)
#' @param type whether user wants to extract loading ("loads") or interpts "ints"
#' @param sig.uni an optional character vector of item #s that show significant uniform DIF
#' @param sig.nonuni an optional character vector of item #s that show significant non-uniform DIF
#' @return a ggplot of DIF (also ggsave'd to ./output)
#' @export
#' @examples
#' load.plot <- difplot(df, covname, covmin = 3, covmax = 8, covtitle = "Perceived Social Class", lvtitle = "Satisfaction", type = "loads", sig.nonuni = sig.nonuni)

difplot <- function(df, covname, covmin, covmax, covtitle, lvtitle, type = NULL, sig.uni = NULL, sig.nonuni = NULL){
  levs <- covlevs(df, covname = covname, covmin, covmax)
  filter.dat <- covfilters(df, covname = covname, levs)
  out <- difests(filter.dat, levs)
  if(type == "loads"){
    ests <- getdifests(out, type = "loads", sig.nonuni = sig.nonuni)
  }else if(type == "ints"){
    ests <- getdifests(out, type = "ints", sig.uni = sig.uni)
  }
  viz <- difviz(ests, type = type, covtitle = covtitle, lvtitle = lvtitle)
  return(viz)
}
