\name{Event}
\alias{Event}
\title{Represents a sequence of events}
\description{
  An ordered sequence of events. This is essentially a list
  of tuples containing a date and an amount (some arbitrary
  value associated with the date).
}
\examples{
d <- Sys.Date() + cumsum(round(c(runif(20,5,10), runif(20,25,30))))
e <- Event(d, abs(rnorm(length(d))))
}
\author{
  Brian Lee Yung Rowe
}
\keyword{cluster}

