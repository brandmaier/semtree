# semtree 0.10.0 (2025)

- semtree now supports focus parameters in lavaan (but not for "fair" rule, yet)
- default construction method for forests changed from "fair" to maxLR-based computation
- bugfix in toTable() which would sometimes not show the decisions properly; also introduced default rounding of estimates
- fixed bug in counting depth of a tree
- convergence warnings in OpenMx models with focus parameter are now suppressed
- the feature global invariance was removed because it was just a wrapper to fix parameters at their ML estimate
- trees now store the number of comparisons per node (for later pruning)
- removed deprecated folds argument in semtree.control()
- removed unused fold option from semtree.control()
- make semtree_control() the standard constructor instead of semtree.control()
- variable importance now automatically chooses the right permutation scheme based on the presence or absence of focus parameters
- aggregateVarimp() is now exported as a function to aggregate variable importance estimates across a forest
- prune() supports pruning of non-converged daugther nodes
- added thinOut() function
- plot semtree has option to display a nonconvergence warning
- various safety checks and better warnings/errors
- ggplot2 deprecated aes_string(), so we now require version 3.3.0 to use the replacement
- removed duplicate labels in semtree plot() when lavaan models have equality constraints (thanks to Manuel Arnold)
- removed outdated information from semtree.control print function (thanks to Manuel Arnold)

# semtree 0.9.23 (2025)

- In semtree control object, the option 'use.maxlm' was renamed to 'use.maxlr' for clarity because it refers to the maxLR statistic as proposed by Arnold et al. (2021)
- bug fixes in score function: ensured symmetric labeling in the S matrix — this prevents issues with S_deriv creation; corrected labeling in the M matrix to allow proper subsetting and avoid duplicated mean labels; contributed by Moritz John
- removed `naive.bonferroni.correction` in `semtree.control` object. This used to be an option to choose how the number of test was computed. This is now always the number of variables. Bonferroni correction is now also numerically more stable
- moved code for Bonferroni correction outside the stopping rule code.
- new vignettes to demonstrate parallel processing and Bonferroni correction

# semtree 0.9.22 (2025)

- added multivariate density to evaluate loglikelihood of lavaan models (thanks to Yves Rosseel for support). New option in semtree.control allows to choose between different approaches for lavaan models. New default ist multivariate normal computation for lavaan models as it fixes bug with likelihood evaluation when the number of cases is small
- now shipping a preliminary boruta implementation

# semtree 0.9.21 (2024)

- added merge functions for varimp objects
- fixed error in to.table() which convert everything to strings
- fixed issue in OpenMx models with unnamed parameters in matrices
- fixed a bug in toLatex() of trees, where edge labels were undefined

# semtree 0.9.20 (2024)

- added an error handler for score-based tests when the vcov matrix cannot be computed (e.g., models with Heywood cases)
- leaner package imports: removed dependency on bitops and stringr package
- prefer `semforest_control()` over `semforest.control()` and `semtree_control()` over `semtree.control()`
- added heuristics for choosing `mtry` in forests (if `NULL`) and for choosing `min.N` and `min.bucket` (if `NULL`)
- moved dependency on `ctsemOMX` to suggested package

# semtree 0.9.19 (2023)

- changed default behavior of print function of `varimp`, such that na.omit=TRUE, which is consistent with other packages like party or partykit
- fixed issues with `toTable()`-command, by default, all parameters are shown now, also fixed a bug with score-based tests and toTable()
- fixed problem with focus-parameters and variable importance
- bugfix in score-based tests that sometimes did not respect min.N constraints
- new functionality for parameter contribution evaluation
- more verbose vignettes
- removed dependency on set, plotrix and digest package to make package imports leaner

# semtree 0.9.18 (2022)

- happily welcoming Caspar van Lissa to the developer team
- more efficient implementation of partial dependence and proximity
- new predict() functions
- bugfix in handling factors with non-numeric levels
- removed crossvalidation splitting after deprecation in v0.9.15
- exporting strip() function for lighter SEM trees in forests
- introducing new classed tree_stripped and forest_stripped

# semtree 0.9.16 (2021)

- removed all code based on parallel-package and replace by future-equivalent. Thanks to Michael Krause!
- restored semtree.print() generic function that was accidentally dropped in 0.9.15
- fix to score-based tests to support definition variables in OpenMx. Thanks to Manuel Arnold!

# semtree 0.9.15 (2021)

- roll-out of score-based tests - use method "score" to enable 
- use of method "cross-validation" is now deprecated and discouraged
- added variable importance for focus parameters
- added new vignettes
- fixed compatibility with new OpenMx version

# semtree 0.9.14 (2020)

- added requirement rpart.plot > 3.0.6 to avoid error w.r.t. to missing round.int
- Manuel Arnold joined the team to work on a score-test implementation
- fixed behavior of min.N in semtree.control()
- semtree captions uses signif for truncating decision thresholds to three significant digits
- fixed class type checks to be compatible with upcoming R 4.0.0

# semtree 0.9.13 (2018)

- bugfix in invariance testing 
- fix to remove warning from rpart plot (version 3.0.0 and above)

# semtree 0.9.12 (2018)

- bugfix in split evaluation procedure for cases where variables are named 'c', which crashed semtree
- bugfix in varimpConvergencePlot: na.omit command logic was inverted

# semtree 0.9.11 (2017)

- semforests support for lavaan models including variable importance and partial dependence
- crossvalidated likelihood supported with lavaan
- plots of semtrees show correct #df when using focus parameters
- added diversity functions

# semtree 0.9.10 (2017)

- added NEWS.md file
- bugfix to partial dependence on variables of type factor
- bugfixes to plotting of partial dependence
- deprecated partialDependencePlot and introduced partialDependence() function with S3 plotting method
- added parallel computation option to partialDependence
- added new demo scripts
- added extra.legend parameter to varimpConvergencePlot
- bugfix in traverse() that led to underestimations of variable importance in some cases
- added error message when trying to use lavaan and global constraints
