
<!-- README.md is generated from README.Rmd. Please edit that file -->

# cfbpointsR

A scraping and aggregating package using the CollegeFootballData API

`cfbpointsR` is an R package for working with CFB data. It is an R API
wrapper around <https://collegefootballdata.com/>. It provides users the
capability to get a plethora of endpoints, and supplement that data with
additional information (Expected Points Added/Win Probability added).

**Note:** The API ingests data from ESPN as well as other sources. For
details on those source, please go the website linked above. Sometimes
there are inconsitences in the underlying data itself. Please report
issues here or to <https://collegefootballdata.com/>.

## Installation

You can install `cfbpointsR` from GitHub with:

``` r
# install.packages("devtools")
devtools::install_github("saiemgilani/cfbpointsR")
```

<!-- badges: start -->
![Lifecycle:stable](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)

![Travis-CI:
build-status](https://travis-ci.com/saiemgilani/cfbpointsR.svg?token=BxsozfUD3VCvCzzJpdFf&branch=master)
<!-- badges: end -->

The goal of cfbpointsR is to …

## Installation

You can install the released version of cfbpointsR from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("cfbpointsR")
```

And the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("saiemgilani/cfbpointsR")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(cfbpointsR)
## basic example code
```

What is special about using `README.Rmd` instead of just `README.md`?
You can include R chunks like so:

``` r
summary(cars)
#>      speed           dist       
#>  Min.   : 4.0   Min.   :  2.00  
#>  1st Qu.:12.0   1st Qu.: 26.00  
#>  Median :15.0   Median : 36.00  
#>  Mean   :15.4   Mean   : 42.98  
#>  3rd Qu.:19.0   3rd Qu.: 56.00  
#>  Max.   :25.0   Max.   :120.00
```

You’ll still need to render `README.Rmd` regularly, to keep `README.md`
up-to-date.

You can also embed plots, for example:

<img src="man/figures/README-pressure-1.png" width="100%" />

In that case, don’t forget to commit and push the resulting figure
files, so they display on GitHub\!
