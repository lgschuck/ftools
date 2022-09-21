
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ftools

<!-- badges: start -->
<!-- badges: end -->

The goal of ftools is to provide tools for data manipulation and other
things…

## Installation

You can install the development version of ftools from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("lgschuck/ftools")
```

## Loading the package

``` r
library(ftools)
```

## Function dif_month

This function calculates the difference in months between two dates.

``` r
x1 <- c(Sys.Date()+10, Sys.Date()+5)
x2 <- c(Sys.Date()+100, Sys.Date()+58)

x1
#> [1] "2022-10-01" "2022-09-26"
x2
#> [1] "2022-12-30" "2022-11-18"
dif_month(x2,x1)
#> [1] 2 2

x2 <- c(Sys.Date()+100, Sys.Date()+358)

x1
#> [1] "2022-10-01" "2022-09-26"
x2
#> [1] "2022-12-30" "2023-09-14"

dif_month(x2,x1)
#> [1]  2 12

dif_month(x1,x2)
#> [1]  -2 -12
```
