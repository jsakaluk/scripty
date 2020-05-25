
<!-- README.md is generated from README.Rmd. Please edit that file -->

# psyscores

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![CRAN
status](https://www.r-pkg.org/badges/version/psyscores)](https://CRAN.R-project.org/package=psyscores)
<!-- badges: end -->

The goal of psyscores is to help automate the specification and
comparison of different generating psychometric models for indicators of
psychological constructs. The package also aims to help researchers
calculate scores based on their determined psychometric model for use in
subsequent analyses. This package is very early in its development, so
functionality is likely to expand/change dramatically/suddenly.

## Installation

You can install the development version from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("jsakaluk/psyscores")
```

## Examples

``` r
library(dplyr) 
library(psych)
library(psyscores)
data(bfi)

#Fit congeneric factor model to bfi agreeableness items
agree.mod <- congeneric(dplyr::select(bfi, A1:A5))

#Calculate H and Omega indexes of reliability: 
reliability(agree.mod)

#Extract factor score for agreeableness as a predictor
bfi <- scoreit(dplyr::select(bfi, A1:A5), bfi, scorerole = "predictor")

#Preview first few A1:A5 and factor scores
head(select(bfi, A1:A5, out.score))
```
