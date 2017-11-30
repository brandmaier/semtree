\name{diversityMatrix}
\alias{diversityMatrix}
\title{Diversity Matrix}
\description{Computes a diversity matrix using a distance function between trees}
\usage{
 diversityMatrix(forest, divergence, showProgressBar=TRUE)
}
\arguments{
\item{forest}{A SEM forest}
\item{divergence}{A divergence function such as hellinger or klsym}
\item{showProgressBar}{Boolean. Show a progress bar.}
}