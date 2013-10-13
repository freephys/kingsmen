\name{event_group}
\alias{event_group}
\title{Group interarrival rates into clusters}
\arguments{
  \item{event}{An Event object that represents the events
  to cluster}

  \item{part}{A partition table to aid the clustering}

  \item{method}{The cluster method}

  \item{levels}{The level of the tree to cut at to define
  the clusters}
}
\value{
  The result of the function is an EventGroup object. This
  can be passed to \code{summary} and \code{plot} for
  further analysis.
}
\description{
  Use hierarchical clustering to group interarrival rates
  together.
}
\section{Usage}{
  An EventGroup constructed based on the kingsmen technique
  event_group(event, part, method, levels)

  This EventGruop acts as the control event_group(event,
  method='complete', levels=1)
}

\section{Details}{
  This function controls the clustering of the actual event
  data.
}
\examples{
d <- Sys.Date() + cumsum(round(c(runif(20,5,10), runif(20,25,30))))
e <- Event(d, abs(rnorm(length(d))))
g <- event_group(e)
}
\author{
  Brian Lee Yung Rowe
}
\keyword{cluster}

