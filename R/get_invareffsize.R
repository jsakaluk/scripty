#' User-facing function to calculate all noninvariance effect size measures in Gunn et al. (2020)
#'
#' @param dat data frame of indicators
#' @param fit outputted multi-group cfa lavaan object
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

get_invareffsize <- function(dat, fit, nodewidth = 0.01, lowerLV = -5, upperLV = 5, lvname = NULL){
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
  wsdi <- get_wsdi(dat, fit)
  wudi <- get_wudi(dat, fit)

  effsize.tib <- as.data.frame(cbind(indnames, dmacs, sdmacs, sdi2, udi2, wsdi, wudi))

  return(effsize.tib)
}
