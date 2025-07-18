
<!-- README.md is generated from README.Rmd. Please edit README.Rmd -->

<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/ReboundTools)](https://cran.r-project.org/package=ReboundTools)
[![Lifecycle:
stable](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html#stable)
[![Project Status: Active – The project has reached a stable, usable
state and is being actively
developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![R-CMD-check](https://github.com/MatthewHeun/ReboundTools/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/MatthewHeun/ReboundTools/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/MatthewHeun/ReboundTools/graph/badge.svg)](https://app.codecov.io/gh/MatthewHeun/ReboundTools)
[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.4999846.svg)](https://doi.org/10.5281/zenodo.4999846)
<!-- badges: end -->

# ReboundTools

`ReboundTools` is an `R` package that provides functions to analyze
energy rebound, the unanticipated reduction of the benefits of energy
efficiency due to behavior change and economy-wide effects. Many
functions perform analysis calculations to move from known parameters to
rebound estimates. Graphing functions (described in the `Graphs`
vignette) create rebound path graphs in energy, expenditure, and
consumption spaces. Other graphing functions create sensitivity graphs.
The functions in this package were used for the analyses and graphs in
the following two papers:

- *Energetic and economic aspects of rebound, Part I: Foundations of a
  rigorous analytical framework* (Heun, Semieniuk, and Brockway 2025a)
  and
- *Energetic and economic aspects of rebound, Part II: Applications of
  the framework* (Heun, Semieniuk, and Brockway 2025b).

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

## More Information

Find more information, including vignettes and function documentation,
at <https://MatthewHeun.github.io/ReboundTools/>.

## References

<div id="refs" class="references csl-bib-body hanging-indent"
entry-spacing="0">

<div id="ref-Heun:2025-1" class="csl-entry">

Heun, Matthew Kuperus, Gregor Semieniuk, and Paul E. Brockway. 2025a.
“Energetic and Economic Aspects of Rebound, Part I: Foundations of a
Rigorous Analytical Framework.” *The Energy Journal*, July, 1–63.
<https://doi.org/10.1177/01956574251331969>.

</div>

<div id="ref-Heun:2025-2" class="csl-entry">

———. 2025b. “Energetic and Economic Aspects of Rebound, Part II:
Applications of the Framework.” *The Energy Journal*, July, 1–42.
<https://doi.org/10.1177/01956574251331966>.

</div>

</div>
