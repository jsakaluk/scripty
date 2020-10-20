#' Scrape standardized model statement from fitted lavaan object
#'
#' @param fit outputted \code{\link{lavaan}} object
#'
#' @return
#' @export
#'
#' @examples

scrape_stdmod <- function(fit){
  #1) Save standardized parameter estimates
  param <- lavaan::parameterestimates(fit, standardized = T)
  #2) Extract list and number of factors
  lvnames <- param %>%
    dplyr::filter(., op == "=~") %>%
    dplyr::select(., lhs) %>%
    unique(.)

  lvlength <- length(lvnames$lhs)

  #3) Save vector of indicators matching those loading onto factor in parameter estimate object
  loads <- list()

  for(i in 1:lvlength){
    loads[[i]] <-  param %>%
      dplyr::filter(., op == "=~" & lhs == lvnames$lhs[[i]]) %>%
      dplyr::select(., std.all, rhs)
  }

  sim.loads <- list()
  for(i in 1:lvlength){
    sim.loads[[i]] <-   paste(sprintf("%s*%s", loads[[i]]$std.all, loads[[i]]$rhs), collapse = "+")
  }

  sim.lambda <- list()
  for(i in 1:lvlength){
    sim.lambda[[i]] <- sprintf("%s =~ %s", lvnames$lhs[[i]], sim.loads[[i]])
  }
  sim.lambda <- paste(sim.lambda, collapse = "\n")

  if(lvlength == 1){
    #Script Creation Syntax
    sim.script = sprintf("#Loadings\n%s", sim.lambda)

  }else if(lvlength > 1){
    #5) Save pairings of lv names in correlations between factors in order of lavaan output
    #2) Extract list and number of factors
    param$corrmatch <- "no"
    param$corrmatch[param$lhs == param$rhs] = "yes"
    param$mult <- "*"

    corrs <- param %>%
      dplyr::filter(., op == "~~", lhs %in% lvnames$lhs & corrmatch == "no") %>%
      dplyr::select(., lhs, op, std.all,mult,rhs)

    sim.psi <- list()
    for(i in 1:(lvlength-1)){
      sim.psi[[i]] <- sprintf("%s%s%s%s%s", corrs$lhs[[i]], corrs$op[[i]], corrs$std.all[[i]], corrs$mult[[i]], corrs$rhs[[i]])
    }
    sim.psi <- paste(sim.psi, collapse = "\n")
    #Script Creation Syntax
    sim.script = sprintf("#Loadings\n%s\n\n#Correlations\n%s", sim.lambda, sim.psi)
  }

  #Script Creation Syntax
  cat(sim.script,"\n")

  return(sim.script)
}


