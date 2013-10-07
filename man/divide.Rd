\name{divide}
\alias{divide}
\title{Divide transaction stream into distinct continguous clusters}
\arguments{
  \item{event}{An Event object}

  \item{method}{Passed to hclust to specify the clustering
  algorithm}

  \item{levels}{Number of levels to use when cutting the
  tree}

  \item{plot}{Specify whether a plot be generated}

  \item{\dots}{Additional arguments to pass to
  \code{partition}}
}
\description{
  This method is less susceptible to noise than a direct
  clustering approach using only interarrival rates.
}
\examples{
d <- Sys.Date() + cumsum(round(c(rnorm(20,15,6), rnorm(20,25,10))))
e <- Event(d, abs(rnorm(length(d))))
divide(e, plot=FALSE)
}
\author{
  Brian Lee Yung Rowe
}
\keyword{cluster}

