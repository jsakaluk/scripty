#' A Function that Computes Factor Scores According to the Procedure Outlined By McNeish and Wolf
#'
#' This function takes the df of items and executes psyscores::congeneric().
#' It then takes the lavaan model and applies the procedure outlined by McNeish and Wolf (2020), following from Skrondal & Laake (2001)
#' @param itemdf input data frame of the items used when executing psyscores::congeneric()
#' @param datadf target data frame to which the computed vector of factor scores should be appended
#' @param scorerole input character for which analytic role the factor score will play. When "predictor"
#' is specified, regression/MAP scores with Croon (2002) correction are computed. When "outcome" is specified,
#' Bartlett scores are computed. No default to prevent accidental specification.
#' @return data frame consisting of data frame specified in datadf with the additional column of factor scores
#' @export
#' @examples
#' dvn = dyadVarNames(dat, xvar="X", sep = ".",distinguish1 = "1", distinguish2 = "2")

scoreit <- function(itemdf, datadf, scorerole = "NULL"){

  #Fit congeneric model
  con.out <- congeneric(df = itemdf)

  if(scorerole == "predictor"){
    map.scores <- as.numeric(lavaan::lavPredict(con.out, method="regression"))
    outdf <- dplyr::mutate(datadf, out.score = map.scores)
    return(outdf)
  }else if (scorerole == "outcome"){
    bart.scores <- as.numeric(lavaan::lavPredict(con.out, method="Bartlett"))
    outdf <- dplyr::mutate(datadf, out.score = bart.scores)
    return(outdf)
  }else if (is.null(scorerole)){
    stop("You need to specify whether this factor score will play the role of predictor or outcome!")
  }

}
