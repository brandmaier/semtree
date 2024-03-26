# semtree

<!-- badges: start -->

[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.1116294.svg)](https://doi.org/10.5281/zenodo.1116294)
[![cran
version](http://www.r-pkg.org/badges/version/semtree)](https://cran.r-project.org/package=semtree)
[![rstudio mirror
downloads](http://cranlogs.r-pkg.org/badges/semtree)](https://github.com/r-hub/cranlogs.app)
[![R-CMD-check](https://github.com/brandmaier/semtree/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/brandmaier/semtree/actions/workflows/R-CMD-check.yaml)
![Code
size](https://img.shields.io/github/languages/code-size/brandmaier/semtree.svg)
![Downloads](https://cranlogs.r-pkg.org/badges/grand-total/semtree)
<!-- badges: end -->
![contributions](https://img.shields.io/badge/contributions-welcome-brightgreen.svg?style=flat)
[![License: GPL
v3](https://img.shields.io/badge/License-GPLv3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)

## What is this?

An R package for estimating Structural Equation Model (SEM) Trees and
Forests. They are a fusion of SEM and decision trees, or SEM and random
forests respectively. While SEM is a confirmatory modeling technique,
SEM trees and forests allow to explore whether there are predictors that
provide further information about an initial, theory-based model.
Potential use cases are the search for potential predictors that explain
individual differences, finding omitted variables in a model, or
exploring measurement invariance over a large set of predictors. A
recent overview is in our latest book chapter in the SEM handbook
(Brandmaier & Jacobucci, 2023).

## Install

Install the latest stable version from CRAN:

    install.packages("semtree")

To install the latest semtree package directly from GitHub, copy the
following line into R:

    library(devtools)
    devtools::install_github("brandmaier/semtree")

    # even better: install with package vignette (extra documentation)
    devtools::install_github("brandmaier/semtree",force=TRUE, build_opts = c())

## Usage

Package documentation and use-cases with runnable R code can be found on
our github pages: <https://brandmaier.github.io/semtree/>.

Package vignettes (shipped with the package) contain documentation on
how to use the package. Simply type this in R once you have loaded the
package:

    browseVignettes("semtree")

## References

Theory and method:

-   Brandmaier, A. M., & Jacobucci, R. C. (2023). Machine-learning
    approaches to structural equation modeling. In R. H. Hoyle (Ed.),
    Handbook of structural equation modeling (2nd rev. ed.,
    pp. 722–739). Guilford Press.

-   Arnold, M., Voelkle, M.C., and Brandmaier, A.M. (2021). Score-guided
    structural equation model trees. *Frontiers in psychology*, 11,
    564403.

-   Brandmaier, A. M., Driver, C., & Voelkle, M. C. (2019). Recursive
    partitioning in continuous time analysis. In K. van Montfort, J.
    Oud, & M. C. Voelkle (Eds.), Continuous time modeling in the
    behavioral and related sciences. New York: Springer.

-   Brandmaier, A. M., Prindle, J. J., McArdle, J. J., &
    Lindenberger, U. (2016). Theory-guided exploration with structural
    equation model forests. *Psychological Methods*, 21, 566-582.

-   Brandmaier, A. M., von Oertzen, T., McArdle, J. J., &
    Lindenberger, U. (2014). Exploratory data mining with structural
    equation model trees. In J. J. McArdle & G. Ritschard (Eds.),
    Contemporary issues in exploratory data mining in the behavioral
    sciences (pp. 96-127). New York: Routledge.

-   Brandmaier, A. M., von Oertzen, T., McArdle, J. J., &
    Lindenberger, U. (2013). Structural equation model trees.
    *Psychological Methods*, 18, 71-86.

Applied examples (there are many more):

Brandmaier, A. M., Ram, N., Wagner, G. G., & Gerstorf, D. (2017).
Terminal decline in well-being: The role of multi-indicator
constellations of physical health and psychosocial correlates.
Developmental Psychology.
