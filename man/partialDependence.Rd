\name{partialDependence}
\alias{partialDependence}
\alias{plot.partialDependence}
\alias{partialDependencePlot}
\title{Partial Dependence Plot}
% partialDependencePlot(forest, reference.var, reference.param,
%	 support=10, xlab=NULL, ylab=NULL,...) 
\usage{


partialDependence(forest, reference.var, reference.param, support=NULL, cluster=NULL) 


	 
}
\description{
Partial dependence plot for the effect of an indepedent variable in the dataset on the dependent outcome parameter selected.
Returns a partialDependence object that can be plotted via generic plot command.
}
\arguments{
\item{forest}{A SEM forest}
\item{reference.var}{Label of the (independent) reference variable for which partial dependence is plotted}
\item{reference.param}{Label of the (dependent) model parameter for which partial dependence is plotted}
\item{support}{Number of grid points for interpolating the reference.var}
\item{cluster}{A reference to a cluster from parallel package for parallel execution. Defaults to NULL for sequential computation. }
%\item{xlab}{X-axis label}
%\item{ylab}{Y-axis label}
%\item{\dots}{Optional parameters for plotting}
}

\author{Andreas M. Brandmaier}