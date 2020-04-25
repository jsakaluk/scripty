#' A Function that Calculates Coefficient H and Omega Total
#'
#' This function takes a fitted lavaan measurement model and calculates two indexes of latent reliability: Coefficient H and Omega Total. Based on McNeish (2018)
#' @param model a fitted lavaan object from a measurement model for which the user wishes to estimate reliability
#' @return a tibble of H and Omega Total
#' @export
#' @examples
#' cong.hs <- congeneric(df)
#' reliability(cong.hs)

reliability <- function(model){
  params <- tibble::as_tibble(lavaan::standardizedsolution(model))
  loads <- dplyr::filter(params, op == "=~")
  errors <- dplyr::filter(params, op == "~~")
  errors <- errors[-1,]

  #H
  loads <- dplyr::mutate(loads, I2 = (est.std^2)/(1-est.std^2))
  sumI2 <- sum(loads$I2)
  mind <- 1/sumI2
  majd <- 1+mind
  H <- 1/majd

  #Omega Total
  sumload <- sum(loads$est.std)
  sumerror <- sum(errors$est.std)
  omegnum <- sumload^2
  omegdenom <- omegnum+sumerror
  omega.total <- omegnum/omegdenom

  index <- c("H", "Omega Total")
  estimate <- c(H, omega.total)
  reliability <- tibble::as_tibbleas_tibble(cbind(index, estimate))
  return(reliability)
}
