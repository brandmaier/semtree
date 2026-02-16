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

## Example

First, we load the `birthwt` data frame from the `MASS` package, which
contains risk factors associated with low infant birth weight. We
extract the variables presence of uterine irritability (ui), history of
hypertension (ht), smoke status during pregnancy (smoke) and birth
weight (btw):

     library(semtree)
    #> Lade nötiges Paket: OpenMx

      bw <- with(MASS::birthwt, {
      ui <- factor(ui, labels=c("no","yes"))
      ht <- factor(ht, labels=c("no","yes"))
      smoke <- factor(smoke, labels=c("no","yes"))
      data.frame( bwt, uterine_irritability=ui, hypertension=ht, smoke, num_premature_labours=ptl, physician_visits=ftv )
     })

Next, we set up a simple univariate `model` that estimates mean and
variance of birth weight

     model <- lavaan::lavaan("bwt~~bwt; bwt~1")

Last, we estimate a SEM tree with a maximum depth of two, a significance
criterion of 1% and splits based on score-based tests, and plot the
resulting tree:

    ctrl <- semtree_control(method="score", max.depth = 2, alpha = 0.01)

    tree <- semtree(model = model, data = bw, control = ctrl)
    plot(tree)
     

    #> ✖ Variable num_premature_labours is numeric but has only few unique values. Consider recoding as ordered factor.
    #> ✖ Variable physician_visits is numeric but has only few unique values. Consider recoding as ordered factor.
    #> ✔ Tree construction finished [took less than a second].

![](man/figures/birthwt-1.png)

![](man/figures/birthwt-1.png)

## Robustness checklist for end users

To make analyses with `semtree` more robust and reproducible, we
recommend the following:

1.  **Set a seed in `semtree_control(seed = ...)`** before tree growth
    so randomized split procedures are reproducible.
2.  **Set explicit stopping rules** (`min.N`, optionally `min.bucket`,
    and `max.depth`) instead of relying on defaults.
3.  **Choose split method intentionally** (`naive`, `fair`, or `score`)
    and report the choice in manuscripts.
4.  **Use conservative split testing where appropriate** with
    `bonferroni = TRUE` when many predictors are screened.
5.  **Inspect convergence behavior** by keeping
    `check.convergence = TRUE` and increasing `report.level` for
    diagnostics.
6.  **Handle missing split variables explicitly** (`missing`, `use.all`)
    and document your choice.
7.  **Pick and report likelihood computation** (`loglik = "model"` vs
    `"mvn"`) especially when comparing engines.
8.  **Use pruning for interpretation** to avoid over-reading deep,
    unstable branches (`prune(tree, max.depth = ...)`).
9.  **Validate findings with forests** (`semforest`) to check whether
    important predictors recur across trees.
10. **Archive session metadata** (at the very least document
    `sessionInfo()`, package versions, and control settings) alongside
    analysis outputs, or use packages such as `reproducibleRchunks` or
    `repro` to improve reproducibility chances.

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
