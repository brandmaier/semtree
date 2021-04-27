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
- added extra.legend paramter to varimpConvergencePlot
- bugfix in traverse() that led to underestimations of variable importance in some cases
- added error message when trying to use lavaan and global constraints