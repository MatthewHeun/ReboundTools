
<!-- README.md is generated from README.Rmd. Please edit README.Rmd -->
<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/ReboundTools)](https://cran.r-project.org/package=ReboundTools)
[![lifecycle](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![Project Status: Active – The project has reached a stable, usable
state and is being actively
developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![R-CMD-check](https://github.com/MatthewHeun/ReboundTools/workflows/R-CMD-check/badge.svg)](https://github.com/MatthewHeun/ReboundTools/actions)
[![codecov](https://codecov.io/gh/MatthewHeun/ReboundTools/branch/main/graph/badge.svg?token=20kytlPnGF)](https://codecov.io/gh/MatthewHeun/ReboundTools)
<!-- badges: end -->

<!-- This is the badge code from GitHub. The badge above is from usethis::use_github_action_check_standard() -->
<!-- [![R-CMD-check](https://github.com/MatthewHeun/ReboundTools/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/MatthewHeun/ReboundTools/actions/workflows/R-CMD-check.yaml) -->

# ReboundTools

`ReboundTools` is an `R` package that provides functions to analyze
energy rebound,  
the unanticipated reduction of the benefits of energy efficiency due to
behavior change and economy-wide effects. Many functions perform
analysis calculations to move from known parameters to rebound
estimates. Graphing functions (described in the `Graphs` vignette)
create rebound path graphs in energy, expenditure, and preference
spaces. Other graphing functions create sensitivity graphs. The
functions in this package were used for the analyses and graphs in the
paper \*\*\*\*\*\*\*\*\*\*.

## Installation

<!-- You can install `ReboundTools` from CRAN with: -->
<!-- ```{r CRAN-installation, eval = FALSE} -->
<!-- install.packages("ReboundTools") -->
<!-- ``` -->
<!-- You can install a recent development version of `ReboundTools` from github with: -->

You can install `IEATools` from github with:

``` r
# install devtools if not already installed
# install.packages("devtools")
devtools::install_github("MatthewHeun/ReboundTools")
# To build vignettes locally, use
devtools::install_github("MatthewHeun/ReboundTools", build_vignettes = TRUE)
```

## History

The functions in this package were used in the paper
\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*

## More Information

Find more information, including vignettes and function documentation,
at <https://MatthewHeun.github.io/ReboundTools/>.

## References
