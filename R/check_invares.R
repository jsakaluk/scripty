#' Performs checks on a fitted lavaan model for invariance effect size calculation
#'
#' @param fit outputted multi-group cfa lavaan object
#' @param type character indicating which kind of check is to be performed ("lvmean" or "lvnum")

#' @return
#' @export
#'
#' @examples

check_invares <- function(fit, type = NULL){

  lvnames <- lavaan::lavNames(fit, type = "lv")

  ints <- lavaan::parameterestimates(fit) %>% #names of indicators
    dplyr::filter(., op == "~1" & lhs == lvnames)

  inds <- lavaan::parameterestimates(fit) %>% #names of indicators
    dplyr::filter(., op == "=~") %>%
    dplyr::select(., rhs) %>%
    unique(.)

  indnum <- length(inds$rhs) #number of indicators
  groupnum <- length(unique(ints$group))

  if(type == "lvmean"){
    if(ints$est[[1]] == ints$est[[2]]){
      check <- FALSE
    }else if(ints$est[[1]] != ints$est[[2]]){
      check <- TRUE
    }
  }else if(type == "lvnum"){
    if(length(lvnames) == 1){
      check <- TRUE
    }else if(length(lvnames) > 1){
      check <- FALSE
    }
  }
  return(check)
}
