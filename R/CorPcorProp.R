#' A Function That Calculates the Proportion of Correlations that are Stronger or Have a Different Sign than Partial Correlations
#'
#' This function calculates the proportion using either a raw data frame or a simulated common factor or network model. Based on procedure described by van Bork et al. (2019)
#' @param df input data frame consisting of variables in the measurement model
#' @param type input character to (arbitrarily) name LV X in lavaan syntax. Defaults to "obs" for observed proportion but can simulate "ufm" (unidimensional factor model ) or "snm" (sparse network model)
#' @param n optional input number of simulated samples to use if specifying latent or network model. If comparing ufm and snm, the same
#' n should be specified.
#' @param seed optional input number to set random seed for simulations. If comparing ufm and snm, the same
#' seed should be specified.
#' @return the observed proportion (for raw data) or a vector of simuluated proportions (based on specified model type)
#' @export
#' @examples
#' obs <- CorPcorProp(df, type = "obs")
#' ufm <- CorPcorProp(df, type = "ufm", n = 100, seed = 123)
#' snm <- CorPcorProp(df, type = "snm", n = 100, seed = 123)

CorPcorProp <- function(df, type = "obs", n = NULL, seed = NULL){
  #Import df and get names
  df <- na.omit(df)# calculating correlation matrices requires complete data
  varname <- names(df)

  #Specify and fit lavaan model for ufm with all extracted variables, and extract model implied corr matrix
  facname = paste("fac=~")
  loads = gsub(" ", "",paste(facname,paste(varname, collapse = "+")), fixed = T)

  cfa_ufm <- lavaan::cfa(loads, df)
  cov_FA <- lavaan::fitted(cfa_ufm)$cov
  cor_FA <- cov2cor(cov_FA)

  #Specify and fit a network with all variables and extract model implied correlation matrix
  EBIC <- qgraph::EBICglasso(qgraph::cor_auto(df), n=nrow(df))#fit nw model
  diag(EBIC) <- 1
  cor_NW <- corpcor::pcor2cor(EBIC)

  if( type == "obs"){
    #Extract observed matrix
    cor_data <- cor(df)
    pcor_data <- corpcor::cor2pcor(cor_data)
    U_data_cor <- cor_data[lower.tri(cor_data, diag=F)] #unique elements in correlation matrix
    U_data_pcor <- pcor_data [lower.tri(pcor_data, diag = F)]#unique elements in partial correlation matrix
    signswitch_data <- which(U_data_pcor*U_data_cor<0) #number of correlations that have different sign than pcor
    stronger_data <- which((U_data_pcor^2)>(U_data_cor^2) )
    together_data <- union(signswitch_data, stronger_data)
    total_data <- length(together_data)
    propData <- total_data/length(U_data_cor) #proportion of correlations that are stronger or have different sign than pcor in data

    return(propData)
  }else if(type == "ufm"){
    n <- n #number of i t e r a t i o n s
    nobs <- nrow(df) #number o f o b s e r v a t i o n s
    nV <- ncol (df) #number o f v a r i a b l e s
    prop<- rep(NA, n)

    set.seed(seed)
    for (i in 1:n) {
      DataFA <- data.frame(rockchalk::mvrnorm(n = nobs, mu = rep (0,nV), Sigma = cor_FA, empirical = F) )
      corFA <- cor(DataFA)
      pcorFA <- corpcor::cor2pcor(corFA)
      U_FA_cor <- corFA[lower.tri(corFA, diag = F)]
      U_FA_pcor <- pcorFA [ lower.tri( pcorFA, diag = F)]
      signswitch_FA <- which(U_FA_pcor*U_FA_cor < 0)
      stronger_FA <- which( (U_FA_pcor ^2)>(U_FA_cor ^2) )
      together_FA <- union(signswitch_FA, stronger_FA)
      total_FA <- length(together_FA)
      prop[i] <- total_FA/length(U_FA_cor)
    }

    UFM <- as_tibble(prop)
    UFM$model <- "UFM"
    return(UFM)

  }else if (type =="snm"){
    n <- n #number of i t e r a t i o n s
    nobs <- nrow(df) #number o f o b s e r v a t i o n s
    nV <- ncol (df) #number o f v a r i a b l e s
    prop<- rep(NA, n)

    set.seed(seed)

    for (i in 1:n) {
      DataNW <- data.frame(rockchalk::mvrnorm(n = nobs, mu = rep (0,nV), Sigma = cor_NW, empirical = F) )
      corNW <- cor(DataNW)
      pcorNW <- corpcor::cor2pcor(corNW)
      U_NW_cor <- corNW[lower.tri(corNW, diag = F)]
      U_NW_pcor <- pcorNW[lower.tri(pcorNW, diag = F)]
      signswitch_NW<- which(U_NW_pcor*U_NW_cor < 0)
      stronger_NW <- which( (U_NW_pcor ^2)>(U_NW_cor ^2) )
      together_NW <- union(signswitch_NW, stronger_NW)
      total_NW <- length(together_NW)
      prop[i] <- total_NW/length(U_NW_cor)
    }

    SNM <- as.tibble(prop)
    SNM$model <- "SNM"
    return(SNM)
  }
}
