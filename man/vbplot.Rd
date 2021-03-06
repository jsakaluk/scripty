% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/vbplot.R
\name{vbplot}
\alias{vbplot}
\title{A Function For Performing and Visualizing a van Bork et al. (2019) Test of Unidimensional Factor Models vs. Simple Network Models}
\usage{
vbplot(df, n, seed = 123)
}
\arguments{
\item{df}{input data frame consisting of variables in the measurement model}

\item{n}{input number of simulated samples to use if specifying latent or network model. If comparing ufm and snm, the same
n should be specified.}

\item{seed}{input number to set random seed for simulations. Defaults to 705.}
}
\description{
This produces an APA-formatted ggplot of the distributions of simulation results under
the UFM and SNM, with a vertical line corresponding to the observed data
}
\examples{
plot <- vbplot(df, n = 10000, seed = 123)
}
