
<!-- README.md is generated from README.Rmd. Please edit that file -->

<!-- badges: start -->

![Lifecycle:maturing](https://img.shields.io/badge/lifecycle-maturing-blue.svg)
![Travis-CI:
build-status](https://travis-ci.com/saiemgilani/cfbscrapR.svg?token=BxsozfUD3VCvCzzJpdFf&branch=master)
[![Twitter
Follow](https://img.shields.io/twitter/follow/cfbscrapR?style=social)](https://twitter.com/cfbscrapR)
<!-- badges: end -->

# cfbscrapR

![cfbscrapr-tile-1-300.png](https://raw.githubusercontent.com/saiemgilani/cfbscrapR/master/man/figures/cfbscrapr-tile-1-300.png)

A scraping and aggregating package using the CollegeFootballData API

`cfbscrapR` is an R package for working with CFB data. It is an R API
wrapper around <https://collegefootballdata.com/>. It provides users the
capability to retrieve data from a plethora of endpoints and supplement
that data with additional information (Expected Points Added/Win
Probability added).

**Note:** The API ingests data from ESPN as well as other sources. For
details on those source, please go the website linked above. Sometimes
there are inconsistencies in the underlying data itself. Please report
issues here or to <https://collegefootballdata.com/>.

## Installation

You can install `cfbscrapR` from
[GitHub](https://github.com/saiemgilani/cfbscrapR) with:

``` r
# Then can install using the devtools package from either of the following:
devtools::install_github(repo = "saiemgilani/cfbscrapR")
# or the following (these are the exact same packages):
devtools::install_github(repo = "meysubb/cfbscrapR")
```

## Documentation

For more information on the package and function reference, please see
the `cfbscrapR`
[documentation](https://saiemgilani.github.io/cfbscrapR/).

## Expected Points and Win Probability models

If you would like to learn more about the Expected Points and Win
Probability models, please refer to the `cfbscrapR`
[tutorials](https://saiemgilani.github.io/cfbscrapR/articles/index.html)
or for the code repository where the models are built, [click
here](https://github.com/meysubb/cfbscrapR-MISC)

#### Expected Points model calibration plots (~~1.31%~~ ~~1.15%~~ 0.94% Calibration Error)

![ep\_fg\_cv\_loso\_calibration\_results.png](https://github.com/saiemgilani/cfbscrapR/blob/master/man/figures/ep_fg_cv_loso_calibration_results.png?raw=true)

#### Win Probability model calibration plots (~~0.89%~~ ~~0.787%~~ 0.669% Calibration Error)

![wp\_cv\_loso\_calibration\_results.png](https://github.com/saiemgilani/cfbscrapR/blob/master/man/figures/wp_cv_loso_calibration_results.png?raw=true)

# cfbscrapR 1.0.3

  - Updated expected points models and win probability models
  - Add player and yardage columns to `cfb_pbp_data()` pull thanks to a
    great deal of help from @NickTice
  - Add spread values to the `cfb_pbp_data()` pull
  - Add drive detailed result with attempts at creating more accurate
    drive result labels
  - Added series and first down variables
  - Added argumentation to allow for San Jose State to be entered
    without accent into `cfb_pbp_data()` function `team` argument.
