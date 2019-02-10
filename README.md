
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ringo

<!-- badges: start -->

<!-- badges: end -->

The goal of ringo is to parse STAR files like those produced by Relion.

## Installation

Install from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("jameswagstaff/ringo")
```

## Example

Use ringo::starr to read in a star file:

``` r
library(ringo)
ringo::starr("file.star")
```
