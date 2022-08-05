
<!-- README.md is generated from README.Rmd. Please edit that file -->

# GSDCovAdj

<!-- badges: start -->
<!-- badges: end -->

The goal of GSDCovAdj is to combine group sequential,
information-adaptive designs with covariate adjustment

Kelly Van Lancker, Josh Betz and Michael Rosenblum

## Installation

Before installation of the development version of **GSDCovAdj**, we
recommend installing the package simul from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("nt-williams/simul")
```

You can install the development version of GSDCovAdj from
[GitHub](https://github.com/) with:

``` r
devtools::install_github("kelvlanc/GSDCovAdj")
```

## Scope

**GSDCovAdj** implements the methods proposed by Van Lancker, Betz and
Rosenblum (2022) to combine group sequential, information-adaptive
designs with covariate adjustment. The approach is implemented for a
G-computation and targeted maximum likelihood estimator for continuous
and binary outcomes, and for the estimator proposed by Díaz et
al. (2019) for time-to-event outcomes.

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(GSDCovAdj)
#> Warning: replacing previous import 'betareg::predict' by 'stats::predict' when
#> loading 'GSDCovAdj'
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
up-to-date. `devtools::build_readme()` is handy for this. You could also
use GitHub Actions to re-render `README.Rmd` every time you push. An
example workflow can be found here:
<https://github.com/r-lib/actions/tree/v1/examples>.

You can also embed plots, for example:

<img src="man/figures/README-pressure-1.png" width="100%" />

In that case, don’t forget to commit and push the resulting figure
files, so they display on GitHub and CRAN.
