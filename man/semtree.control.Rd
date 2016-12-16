\name{semtree.control}
\alias{semtree.control}
\alias{print.semtree.control}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  SEM Tree Control Object
}
\description{
  A \code{semtree.control} object contains parameters that determine the tree growing process. These parameters include choices of different split candidate selection procedures and hyperparameters of those. Calling the constructor without parameters creates a default control object. A number of tree growing methods are included in with this package: 1. "naive" splitting takes the best split value of all possible splits on each covariate. 2. "fair" selection is so called because it tests all splits on half of the data, then tests the best split value for each covariate on the other half of the data. The equal footing of each covariate in this two phase test removes bias from testing variables with many possible splits compared to those with few. 3. "fair3" does the phases described above, with an additional step of retesting all of the split values on the best covariate found in the second phase. Variations in the sample from subsetting are removed and bias in split selection further reduced. 4. "crossvalidation" partitions the data for maximizing splits on each variable, then comparing maximum splits across each variable on the rest of the data. 
}
\usage{
  semtree.control(method="naive", min.N = 20, max.depth=NA, alpha=.05, 
  alpha.invariance=NA, folds=5, exclude.heywood=TRUE, progress.bar=TRUE,
   verbose=FALSE, bonferroni=FALSE, use.all=FALSE, seed = NA, 
   custom.stopping.rule=NA, mtry=NA, report.level=0, exclude.code=NA  )
}
% print.semtree.control(x, ...)
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{method}{
  Default: "naive". One out of \code{c("fair","fair3","naive","cv")} for either an unbiased two-step selection algorithm, three-step fair algorithm, a naive take-the-best, or a cross-validation scheme.
}
  \item{min.N}{
  Default: 10. Minimum sample size per a node, used to determine whether to continue splitting a tree or establish a terminal node.
}
  \item{max.depth}{
  Default: NA. Maximum levels per a branch. Parameter for limiting tree growth.
}
  \item{alpha}{
  Default: 0.05. Significance level for splitting at a given node.
}
  \item{alpha.invariance}{
  Default: NA. Significance level for invariance tests. If NA, the value of alpha is used.
}
  \item{folds}{
  Default: 5. Defines the number of folds for the \code{"cv"} method.
}
  \item{exclude.heywood}{
  Default: TRUE. Reports whether there is an identification problem in the covariance structure of an SEM tested.
}
  \item{progress.bar}{
  Default: NA. Option to disable the progress bar for tree growth. 
}
  \item{verbose}{
  Default: FALSE. Option to turn on or off \emph{all} model messages during tree growth. 
}
  \item{bonferroni}{
  Default: FALSE. Correct for multiple tests with Bonferroni type correction.
}
  \item{seed}{
  Default: NA. Set a random number seed for repeating random fold generation in tree analysis.
	}
  \item{custom.stopping.rule}{
  Default: NA. Otherwise, this can be a boolean function with a custom stopping rule for tree growing.
}
  \item{exclude.code}{
  Default: NA. NPSOL error code for exclusion from model fit evaluations when finding best split. Default: Models with errors during fitting are retained.
}
  \item{mtry}{
  Default: NA. Number of sample columns to use in SEMforest analysis.
}
  \item{report.level}{
  Default: 0. Values up to 99 can be used to increase the number of onscreen reports for semtree analysis.
}
  \item{use.all}{
  Treatment of missing variables. By default, missing values stay in a decision node. If TRUE, cases are
  distributed according to a maximum likelihood principle to the child nodes.
  }

}
\value{
  A control object containing a list of the above parameters.
}
\references{
  Brandmaier, A.M., Oertzen, T. v., McArdle, J.J., & Lindenberger, U. (2013). Structural equation model trees. \emph{Psychological Methods}, 18(1), 71-86.
}
\author{
  Andreas M. Brandmaier, John J. Prindle
}
\seealso{
  \code{\link{semtree}}
}
\examples{

	# create a control object with an alpha level of 1%
	my.control <- semtree.control(alpha=0.01)

	# set the minimum number of cases per node to ten
	my.control$min.N <- 10
	
	# print contents of the control object
	print(my.control)

}
