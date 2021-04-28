semtree
=======

[![DOI](http://zenodo.org/badge/76649538.svg)](https://zenodo.org/badge/latestdoi/76649538)
[![cran
version](http://www.r-pkg.org/badges/version/semtree)](https://cran.r-project.org/package=semtree)
[![rstudio mirror
downloads](http://cranlogs.r-pkg.org/badges/semtree)](https://github.com/r-hub/cranlogs.app)
[![Build
Status](https://travis-ci.com/brandmaier/semtree.svg?branch=master)](https://travis-ci.com/brandmaier/semtree)
[![License: GPL
v3](https://img.shields.io/badge/License-GPLv3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)

What is this?
-------------

An R package for estimating Structural Equation Model Trees and Forests.

Install
-------

Install the latest stable version from CRAN:

    install.packages("semtree")

To install the latest semtree package directly from GitHub, copy the
following line into R:

    library(devtools)
    devtools::install_github("semtree/brandmaier")

    # even better: install with package vignette (extra documentation)
    devtools::install_github("brandmaier/semtree",force=TRUE, build_opts = c())

Usage
-----

Package documentation and use-cases with runnable R code can be found on
our github pages: <https://brandmaier.github.io/semtree/>.

You may also want to visit the semtree website:
<https://brandmaier.de/semtree>

Package vignettes (shipped with the package) contain documentation on
how to use the package. Simply type this in R once you have loaded the
package:

    browseVignettes("semtree")

References
----------

Theory and method:

Brandmaier, A. M., Driver, C., & Voelkle, M. C. (2019). Recursive
partitioning in continuous time analysis. In K. van Montfort, J. Oud, &
M. C. Voelkle (Eds.), Continuous time modeling in the behavioral and
related sciences. New York: Springer.

Brandmaier, A. M., Prindle, J. J., McArdle, J. J., & Lindenberger, U.
(2016). Theory-guided exploration with structural equation model
forests. Psychological Methods, 21, 566-582.

Brandmaier, A. M., von Oertzen, T., McArdle, J. J., & Lindenberger, U.
(2014). Exploratory data mining with structural equation model trees. In
J. J. McArdle & G. Ritschard (Eds.), Contemporary issues in exploratory
data mining in the behavioral sciences (pp. 96-127). New York:
Routledge.

Brandmaier, A. M., von Oertzen, T., McArdle, J. J., & Lindenberger, U.
(2013). Structural equation model trees. Psychological Methods, 18,
71-86.

Applied examples:

Brandmaier, A. M., Ram, N., Wagner, G. G., & Gerstorf, D. (2017).
Terminal decline in well-being: The role of multi-indicator
constellations of physical health and psychosocial correlates.
Developmental Psychology.
