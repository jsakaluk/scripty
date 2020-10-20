#' Calculates weighted signed difference in expected indicator scores
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
#' #' dat <- HolzingerSwineford1939
#' dat$group[dat$school=="Pasteur"] = "Pasteur"
#' dat$group[dat$school=="Grant-White"] = "Grant-White"
#' HS.model <- '  visual =~ x1 + x2 + x3'
#' fit <- lavaan::cfa(HS.model, data = dat, group = "group")

get_wsdi <- function(dat, fit, nodewidth = 0.01, lowerLV = -5, upperLV = 5){

  lcall <- lavInspect(fit, what = "call")
  group <- lcall$group
  #Get indicator names --> # of indicators
  indnames <- lavaan::parameterestimates(fit) %>% #names of indicators
    dplyr::filter(., op == "=~") %>%
    dplyr::select(., rhs) %>%
    unique(.)

  p <- length(indnames$rhs) #number of indicators

  #Group info
  gname <- lavaan::lavInspect(fit, what = "group.label")
  gnum <- length(gname)
  gn <- lavaan::lavInspect(fit, what = "nobs")

  nogroupfit <- nogroup(fit)

  load1 <- grouploads(fit, source = "1")
  load2 <- grouploads(fit, source = "2")
  loadnone <- nogrouploads(nogroupfit)
  loading1 = load1$est
  loading2 = load2$est
  loadingS = loadnone$est

  int1 <- groupints(fit, source = "1", df = dat)
  int2 <- groupints(fit, source = "2", df = dat)
  intnone <- nogroupints(nogroupfit, df = dat)
  intercept1 = int1$est
  intercept2 = int2$est
  interceptS = intnone$est

  sds <- groupindsds(dat, fit)
  stdev1 = as.numeric(sds[1,])
  stdev2 = as.numeric(sds[2,])

  intfac1 <- grouplvmean(fit, source = "1", df = dat)
  sdfac1 <- grouplvsd(fit, source = "1", df = dat)
  fmean1 = intfac1$est
  fsd1 = sdfac1$est

  intfac2 <- grouplvmean(fit, source = "2", df = dat)
  sdfac2 <- grouplvsd(fit, source = "2", df = dat)
  fmean2 = intfac2$est
  fsd2 = sdfac2$est

  N1 = gn[[1]]
  N2 = gn[[2]]

  # Define evaluation of latent variable
  LV = seq(lowerLV,upperLV,nodewidth)

  # Create proportions
  prop1 = N1/(N1+N2)
  prop2 = N2/(N1+N2)

  # Create empty matrices for future arrays, matrices, etc.
  DiffExpItem1S = matrix(NA,length(LV),p)
  DiffExpItemS2 = matrix(NA,length(LV),p)
  pdfLV1 = matrix(NA,length(LV),1)
  pdfLV2 = matrix(NA,length(LV),1)
  WSDInumerator1 = matrix(NA,length(LV),p)
  WSDInumerator2 = matrix(NA,length(LV),p)
  WSDI = matrix(NA,p,1)

  for(j in 1:p){
    for(k in 1:length(LV)){
      # Calculate difference in expected indicator scores
      DiffExpItem1S[k,j] <- (intercept1[j]-interceptS[j]) + (loading1[j]-loadingS[j])*LV[k]
      DiffExpItemS2[k,j] <- (interceptS[j]-intercept2[j]) + (loadingS[j]-loading2[j])*LV[k]
      # probability density functions for sample estimate of latent variable distribution
      pdfLV1[k] = dnorm(LV[k], mean=fmean1, sd=fsd1)
      pdfLV2[k] = dnorm(LV[k], mean=fmean2, sd=fsd2)

      # Multiply by latent variable distribution to calculate individual data point in numerator
      WSDInumerator1[k,j] = DiffExpItem1S[k,j]*pdfLV1[k]*nodewidth
      WSDInumerator2[k,j] = DiffExpItemS2[k,j]*pdfLV2[k]*nodewidth
    }

    # Sum across range of latent variable using quadrature to calculate numerator
    # Divide by denominator
    WSDI[j] <- prop1*sum(WSDInumerator1[,j])/stdev1[j] +
      prop2*sum(WSDInumerator2[,j])/stdev2[j]
  }
  colnames(WSDI) = "WSDI"

  return(WSDI=round(WSDI,4))
}
