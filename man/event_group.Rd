\name{event_group}
\alias{event_group}
\title{Group interarrival rates into clusters}
\arguments{
  \item{event}{}

  \item{part}{}

  \item{method}{}

  \item{levels}{}
}
\description{
  Use hierarchical clustering to group interarrival rates
  together.
}
\section{Usage}{

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

