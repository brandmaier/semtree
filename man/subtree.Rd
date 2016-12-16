\name{subtree}
\alias{subtree}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  SEMtree Partitioning Tool
}
\description{
  The \code{subtree} function returns a tree from a selected node of the \code{\link{semtree}} returned tree. 
}
\usage{
  subtree(tree, startNode=NULL,level = 0, foundNode = FALSE)
}
\arguments{
  \item{tree}{A SEMtree object obtained from \code{\link{semtree}}}
  \item{startNode}{Node id, which will be future root node (0 to max node number of \code{tree)}}
  \item{level}{Ignore. Only used internally.}
  \item{foundNode}{Ignore. Only used internally.}
}
\details{
  The row names of the resulting data frame correspond to internal node ids and the column names correspond to standard errors in the SEM. Standard errors of the estimates can be obtained from \code{\link{se}}.
}
\value{
  Returns a \code{\link{semtree}} object which is a partitioned tree from the input \code{semtree}. 
}
\references{
  Brandmaier, A.M., Oertzen, T. v., McArdle, J.J., & Lindenberger, U. (2013). Structural equation model trees. \emph{Psychological Methods}, 18(1), 71-86.
}
\author{
  Andreas M. Brandmaier, John J. Prindle
}
%\note{
%%  ~~further notes~~
%}

\seealso{
  \code{\link{semtree}},
  \code{\link{semtree.control}}
}
%\examples{
%  # load an example dataset
%  data("semtree_example1_dataset")
%  # load an example SEM "growthCurveModel"
%  data("semtree_example1_model")
%
%  # set data
%  growthCurveModel@data <- mxData(dataset,type="raw")
%
%  # grow a tree and print parameter standard errors by node
%  tree <- semtree(growthCurveModel)
%  subtree(tree, startNode=2)
%}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
