#' Calculates signed difference in expected indicator scores for Group 2
#'
#' @param dat data frame of indicators
#' @param fit outputted multi-group cfa lavaan object
#' @param nodewidth space between nodes during quadrature approximation (default = .01)
#' @param lowerLV lowest latent variable value evaluated (default = -5)
#' @param upperLV greatest latent variable value evaluated (default = 5)
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

get_sdi2 <- function(dat, fit, nodewidth = 0.01, lowerLV = -5, upperLV = 5){

  lcall <- lavInspect(fit, what = "call")
  group <- lcall$group
  #Get indicator names --> # of indicators
  indnames <- lavaan::parameterestimates(fit) %>% #names of indicators
    dplyr::filter(., op == "=~") %>%
    dplyr::select(., rhs) %>%
    unique(.)

  p <- length(indnames$rhs) #number of indicators

  #Use es_helpers to snatch necessary values from cfa model
  load1 <- grouploads(fit, source = "1")
  load2 <- grouploads(fit, source = "2")
  loading1 <- load1$est
  loading2 <- load2$est

  int1 <- groupints(fit, source = "1", df = dat)
  int2 <- groupints(fit, source = "2", df = dat)
  intercept1 <- int1$est
  intercept2 <- int2$est

  sds <- groupindsds(dat, fit)
  stdev2 <- as.numeric(sds[2,])

  intfac2 <- grouplvmean(fit, source = "2", df = dat)
  fmean2 <- intfac2$est

  sdfac2 <- grouplvsd(fit, source = "2", df = dat)
  fsd2 <- sdfac2$est

  # Define evaluation of latent variable
  LV = seq(lowerLV,upperLV,nodewidth)

  # Create empty matrices for future arrays, matrices, etc.
  DiffExpItem12 = matrix(NA,length(LV),p)
  pdfLV2 = matrix(NA,length(LV),1)
  SDI2numerator = matrix(NA,length(LV),p)
  SDI2 = matrix(NA,p,1)

  # Calculate SDI2
  for(j in 1:p){
    for(k in 1:length(LV)){
      # Calculate difference in expected indicator scores between groups 1 and 2
      DiffExpItem12[k,j] <- (intercept1[j]-intercept2[j]) + (loading1[j]-loading2[j])*LV[k]
      # probability density function for sample estimate of group 2 latent variable distribution
      pdfLV2[k] = dnorm(LV[k], mean=fmean2, sd=fsd2)

      # Multiply by latent variable distribution to calculate individual data point in numerator
      SDI2numerator[k,j] = DiffExpItem12[k,j]*pdfLV2[k]*nodewidth
    }
    # Sum across range of latent variable using quadrature to calculate numerator & divide by denominator
    SDI2[j] <- sum(SDI2numerator[,j])/stdev2[j]
  }
  colnames(SDI2) = "SDI2"

  return(SDI2=round(SDI2,4))
}
