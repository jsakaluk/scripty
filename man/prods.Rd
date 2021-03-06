% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/prods.R
\name{prods}
\alias{prods}
\title{A Function Creates Product Indicators Needed to Script a  MIMIC Model of DIF}
\usage{
prods(df, covname, match.op = FALSE, doubleMC.op = T)
}
\arguments{
\item{df}{input data frame consisting of variables in the measurement model}

\item{covname}{character input of name of covariate in data frame}

\item{match.op}{supplies indprod (default to FALSE)}

\item{doubleMC.op}{supplies inprod (default to TRUE)}
}
\value{
a tibble of the original data frame, covariate, and product indicators
}
\description{
This function uses semTools to create the product-terms needed to specify
the latent interaction needed to test non-uniform DIF in a MIMIC model
}
\examples{
df2 <- prods(df, covname = "age")
}
