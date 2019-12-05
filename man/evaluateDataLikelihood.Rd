\name{evaluateDataLikelihood}
\alias{evaluateDataLikelihood}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Evaluate Data Likelihood
}
\description{
Evaluate Fitted Model Likelihood for Dataset. This helper function is used in the \code{\link{semforest}} \code{\link{varimp}} and \code{\link{proximity}} aggregate functions.
}
\usage{
evaluateDataLikelihood(model, data, data_type = "raw")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{model}{
A \code{\link{OpenMx}} model as used in \code{\link{semtree}} and \code{\link{semforest}}.
}
  \item{data}{
Data set to apply to a fitted model.
}
  \item{data_type}{
Type of data ("raw", "cov", "cor")
}
}

\value{
Returns a -2LL model fit for the model 
}
\references{
  Brandmaier, A.M., Oertzen, T. v., McArdle, J.J., & Lindenberger, U. (2013). Structural equation model trees. \emph{Psychological Methods}, 18(1), 71-86.
}
\author{
  Andreas M. Brandmaier, John J. Prindle
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{semtree}}, \code{\link{semforest}}
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (model, data, data_type = "raw") 
{
    model <- omxSetParameters(model, labels = names(omxGetParameters(model)), 
        free = F)
    if (data_type == "raw") {
        data <- full_mxdata <- mxData(observed = data, type = "raw")
    }
    else if (data_type == "cov") {
        data <- full_mxdata <- mxData(observed = cov(data), type = "cov", 
            numObs = dim(data)[1])
    }
    else if (data_type == "cor") {
        data <- full_mxdata <- mxData(observed = cor(data), type = "cor", 
            numObs = dim(data)[1])
    }
    else {
        warning("data type is not supported!")
        return(NULL)
    }
    model <- setData(model, data)
    run <- OpenMx::mxRun(model, silent = T, useOptimizer = F, 
        suppressWarnings = T)
    result <- OpenMx::mxEval(objective, run)
    return(result)
  }
}

