#' @name es_helpers
#' @rdname es_helpers
#'
#' @title Helper functions for noninvariance effect size functions
#'
#' @param dat data frame of indicators
#' @param fit outputted multi-group cfa lavaan object
#' @param nogroupfit outputted cfa lavaan object from nogroup()
#' @param group character of grouping variable name in dat
#' @param source character for whether parameter of interest should be extracted for group "1", "2", or from the "nogroup" model
#'
#' @export

#' @rdname es_helpers
nogroup <- function(fit){
  #extract call from fit
  nogroup.call <- lavaan::lavInspect(fit, what = "call")
  #extract model syntax and data frame name from call
  nogroup.mod <- nogroup.call$model
  nogroup.dat <- nogroup.call$data

  #extract model fitting options from fit
  nogroup.ops <- lavaan::lavInspect(fit, what = "options")
  nogroups.meanstruct <- nogroup.ops$meanstructure
  nogroup.stdlv <- nogroup.ops$std.lv
  nogroup.fixfirst <- nogroup.ops$auto.fix.first
  nogroup.missing <- nogroup.ops$missing
  nogroup.est <- nogroup.ops$estimator

  #Fit same model with no group designation
  nogroupfit <- lavaan::cfa(eval(rlang::sym(nogroup.mod)),
                            data = eval(rlang::sym(nogroup.dat)),
                            meanstructure = nogroups.meanstruct,
                            std.lv = nogroup.stdlv,
                            auto.fix.first = nogroup.fixfirst,
                            missing = nogroup.missing,
                            estimator = nogroup.est)

  return(nogroupfit)
}

#' @rdname es_helpers
groupindsds <- function(dat, fit){

  fit.call <- lavInspect(fit, what = "call")
  fit.call.group <- fit.call$group

  gname <- lavaan::lavInspect(fit, what = "group.label")

  indnames <- lavaan::parameterestimates(fit) %>%
    dplyr::filter(., op == "=~") %>%
    dplyr::select(., rhs) %>%
    unique(.)

  sds <- dat %>% dplyr::group_by(.data[[fit.call.group]]) %>%
    dplyr::select(., .data[[fit.call.group]], matches(indnames$rhs)) %>%
    dplyr::summarise_all(., sd)

  #sds <- sds[match(gname, sds$group),]
  sds <- sds[-1]

  return(sds)
}

#' @rdname es_helpers
grouploads <- function(fit, source){
  if(source == "1"){
    loads <- dplyr::filter(lavaan::parameterEstimates(fit), op == "=~" & group == 1) %>%
      dplyr::select(., est)
  }else if(source == "2"){
    loads <- dplyr::filter(lavaan::parameterEstimates(fit), op == "=~" & group == 2) %>%
      dplyr::select(., est)
  }
  return(loads)
}

#' @rdname es_helpers
nogrouploads <- function(nogroupfit){
  loads <- dplyr::filter(lavaan::parameterEstimates(nogroupfit), op == "=~") %>%
    dplyr::select(., est)
  return(loads)
}

#' @rdname es_helpers
groupints <- function(fit, source, df){
  if(source == "1"){
    ints <- lavaan::parameterEstimates(fit) %>%
      dplyr::filter(., op == "~1" & group == 1 & lhs %in% names(df)) %>%
      dplyr::select(., est)
  }else if(source == "2"){
    ints <- lavaan::parameterEstimates(fit) %>%
      dplyr::filter(., op == "~1" & group == 2 & lhs %in% names(df)) %>%
      dplyr::select(., est)
  }
  return(ints)
}

#' @rdname es_helpers
nogroupints <- function(nogroupfit, df){
  ints <- dplyr::filter(lavaan::parameterEstimates(nogroupfit), op == "~1" & lhs %in% names(df)) %>%
    dplyr::select(., est)
  return(ints)
}

#' @rdname es_helpers
grouplvmean <- function(fit, source, df){
  if(source == "1"){
    lvmean <- dplyr::filter(lavaan::parameterEstimates(fit), op == "~1" & group == 1 & !lhs %in% names(df)) %>%
      dplyr::select(., est)
  }else if(source == "2"){
    lvmean <- dplyr::filter(lavaan::parameterEstimates(fit), op == "~1" & group == 2 & !lhs %in% names(df)) %>%
      dplyr::select(., est)
  }
return(lvmean)
}

#' @rdname es_helpers
grouplvsd <- function(fit, source, df){
  if(source == "1"){
    lvsd <- dplyr::filter(lavaan::parameterEstimates(fit), op == "~~" & group == 1 & !lhs %in% names(df) & !rhs %in% names(df)) %>%
      dplyr::select(., est) %>%
      sqrt(.)
  }else if(source == "2"){
    lvsd <- dplyr::filter(lavaan::parameterEstimates(fit), op == "~~" & group == 2 & !lhs %in% names(df) & !rhs %in% names(df)) %>%
      dplyr::select(., est) %>%
      sqrt(.)
  }
  return(lvsd)
}
