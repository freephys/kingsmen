\name{divide}
\alias{divide}
\title{Divide a stream of events into distinct continguous clusters}
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
\value{
  The result of divide is an EventGroup object. This can be
  summarized using the standard \code{summary} function.
}
\description{
  This method is less susceptible to noise than a direct
  clustering approach using only interarrival rates.
}
\section{Usage}{
  divide %::% Event : . : numeric : logical : ... :
  EventGroup divide(event, method='complete', levels=1,
  plot=TRUE, ...)
}

\section{Details}{
  Irregular time series are challenging to work with since
  many statistical approaches require regular observation
  intervals.
}
\examples{
d <- Sys.Date() + cumsum(round(c(rnorm(20,15,6), rnorm(20,25,10))))
e <- Event(d, abs(rnorm(length(d))))
g <- divide(e, plot=FALSE)
summary(g)
}
\author{
  Brian Lee Yung Rowe
}
\keyword{cluster}

