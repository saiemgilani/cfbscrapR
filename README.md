
<!-- README.md is generated from README.Rmd. Please edit that file -->

<!-- badges: start -->

![Lifecycle:stable](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)

![Travis-CI:
build-status](https://travis-ci.com/saiemgilani/cfbscrapR.svg?token=BxsozfUD3VCvCzzJpdFf&branch=master)
<!-- badges: end -->

# cfbscrapR

A scraping and aggregating package using the CollegeFootballData API

`cfbscrapR` is an R package for working with CFB data. It is an R API
wrapper around <https://collegefootballdata.com/>. It provides users the
capability to get a plethora of endpoints, and supplement that data with
additional information (Expected Points Added/Win Probability added).

**Note:** The API ingests data from ESPN as well as other sources. For
details on those source, please go the website linked above. Sometimes
there are inconsitences in the underlying data itself. Please report
issues here or to <https://collegefootballdata.com/>.

## Installation

You can install `cfbscrapR` from GitHub with:

``` r
# install.packages("devtools")
devtools::install_github("saiemgilani/cfbscrapR")
```

The goal of cfbscrapR is to …

## Installation

You can install the released version of cfbscrapR from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("cfbscrapR")
```

And the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("saiemgilani/cfbscrapR")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(cfbscrapR)
#> Warning: replacing previous import 'mgcv::multinom' by 'nnet::multinom' when
#> loading 'cfbscrapR'
## basic example code
```
