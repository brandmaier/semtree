# semtree 0.9.11

- semforests support for lavaan models including variable importance and partial dependence
- crossvalidated likelihood supported with lavaan

# semtree 0.9.10

- added NEWS.md file
- bugfix to partial dependence on variables of type factor
- bugfixes to plotting of partial dependence
- deprecated partialDependencePlot and introduced partialDependence() function with S3 plotting method
- added parallel computation option to partialDependence
- added new demo scripts
- added extra.legend paramter to varimpConvergencePlot
- bugfix in traverse() that led to underestimations of variable importance in some cases
- added error message when trying to use lavaan and global constraints
