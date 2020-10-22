#' User-facing function to calculate all noninvariance effect size measures in Gunn et al. (2020)
#'
#' @param dat data frame of indicators
#' @param fit outputted multi-group cfa lavaan object
#' @param model inputted lavaan model syntax
#' @param nodewidth space between nodes during quadrature approximation (default = .01)
#' @param lowerLV lowest latent variable value evaluated (default = -5)
#' @param upperLV greatest latent variable value evaluated (default = 5)
#' @param table logical argument to request automated outputting of reproducible table (defaults to FALSE)
#' @param lvname character to name outputted table and file.
#'
#' @return
#' @export
#'
#' @examples
#' dat <- HolzingerSwineford1939
#' dat$group[dat$school=="Pasteur"] = "Pasteur"
#' dat$group[dat$school=="Grant-White"] = "Grant-White"
#' HS.model <- '  visual =~ x1 + x2 + x3'
#' fit <- lavaan::cfa(HS.model, data = dat, group = "group")

get_invareffsize <- function(dat, fit, model, nodewidth = 0.01, lowerLV = -5, upperLV = 5, lvname = NULL){

  if(stringr::str_detect(model, pattern = "(?=\\().+?(?=\\))") == FALSE){
    cat(crayon::yellow("Warning: For maximum accuracy, we recommend manually specifying your parameter constraints instead of using the group.equal = argument"))
  }
  check_num <- check_invares(fit, type = "lvnum")
  check_mean <- check_invares(fit, type = "lvmean")

  if(check_num == FALSE & check_mean == TRUE){
    stop("Invariance effect sizes cannot currently be calculated with models consisting of more than one latent variable")
  }else if(check_mean == FALSE & check_num == TRUE){
    stop("Invariance effect sizes cannot be calculated when latent means are equivalent between groups")
  }else if(check_num == FALSE & check_mean == FALSE){
    stop("Invariance effect sizes cannot currently be calculated with models consisting of more than one latent variable, when latent means are equivalent between groups")
  }else if(check_num == TRUE & check_mean == TRUE){
    dir.create("./output")

    indnames <- lavaan::parameterestimates(fit) %>%
      dplyr::filter(., op == "=~") %>%
      dplyr::select(., rhs) %>%
      unique(.)

    colnames(indnames) = "Indicators"

    sdmacs <- get_sdmacs(dat, fit)
    dmacs <- get_dmacs(dat, fit)
    sdi2 <- get_sdi2(dat, fit)
    udi2 <- get_udi2(dat, fit)
    wsdi <- get_wsdi(dat, fit, model)
    wudi <- get_wudi(dat, fit, model)

    effsize.tib <- as.data.frame(cbind(indnames, dmacs, sdmacs, sdi2, udi2, wsdi, wudi))
    effsize.gt <- gt::gt(effsize.tib)
    gt::gtsave(effsize.gt, sprintf("./output/%s_invarEffSize.rtf", lvname))

    return(effsize.tib)
  }
}
