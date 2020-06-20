#' A function for reproducible MIMIC model output
#'
#' This function takes the outputted lavaan MIMIC model and generates a reproducible table of output
#' @param fit.mimic fitted lavaan object from MIMIC script
#' @param mimic.param list of parameters generated from mimicparam
#' @return outputs .doc to ./output subdirectory with reproducible output
#' @export
#' @examples
#' mimicout(fit.mimic, mimic.param)

mimicout <- function(fit.mimic, mimic.param){
  #Create output sub-directory
  dir.create("./output")
  #Extract parameter estimates and indicator names
  ests <- as.data.frame(lavaan::parameterestimates(fit.mimic))
  uniqnames <- unique(ests$lhs)
  namenum <- length(uniqnames)
  indnum <- (namenum-4)/2
  indnamemax <- 3 + indnum
  indname <- uniqnames[4:indnamemax]

  #Extract name of lv, covlv
  lvname <- uniqnames[1]
  covlvname <- uniqnames[2]

  any.out <- do.call(rbind, lapply(mimic.param, function(x) lavaan::lavTestScore(fit.mimic, add = x)$test))
  #Estimate
  sep.out<- lavaan::lavTestScore(fit.mimic, add = as.character(mimic.param))

  #Stitch title of table together
  tab.title <- sprintf("DIF analysis of %s measure by %s, total test DIF: X^2 (%s) = %s, p = %s",
                       lvname, covlvname, sep.out$test$df, round(sep.out$test$X2, 2), round(sep.out$test$p.value, 3))

  #Any DIF? output
  any.chi2 <- round(any.out$X2,2)
  any.df <- any.out$df
  any.p <- round(any.out$p,3)

  #Determine odd and even numbers of by-paremeter DIF output
  oddnum <- seq(1, length(sep.out$uni$lhs), 2)
  evennum <- seq(2, length(sep.out$uni$lhs), 2)

  #Extract odds (uniform DIF info)
  uni.chi2 <- round(sep.out$uni$X2[oddnum],2)
  uni.df <- sep.out$uni$df[oddnum]
  uni.p <- round(sep.out$uni$p.value[oddnum],3)

  #Extract events (non-uniform)
  non.chi2 <- round(sep.out$uni$X2[evennum],2)
  non.df <- sep.out$uni$df[evennum]
  non.p <- round(sep.out$uni$p.value[evennum],3)

  #Stitch together output table
  difout <- cbind(indname, any.chi2, any.df, any.p,
                          uni.chi2, uni.df, uni.p,
                          non.chi2, non.df, non.p)
  difout <- as.data.frame(difout)

  sjPlot::tab_df(difout, title = tab.title,
                 file = sprintf("./output/%s_%s_dif.doc", lvname, covlvname))
}
