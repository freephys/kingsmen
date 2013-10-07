\name{interarrival}
\alias{interarrival}
\title{Calculate interarrival times}
\arguments{
  \item{dates}{A sequence of dates}
}
\description{
  Calculate interarrival times
}
\examples{
d <- Sys.Date() + cumsum(round(c(runif(20,5,10), runif(20,25,30))))
interarrival(d)
}
\author{
  Brian Lee Yung Rowe
}
\keyword{cluster}

