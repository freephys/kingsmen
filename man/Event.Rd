\name{Event}
\alias{Event}
\title{Represents a sequence of events}
\arguments{
  \item{date}{A date vector representing the dates of each
  event}

  \item{amount}{A numeric vector representing the actual
  event}

  \item{df}{A data.frame that contains a date and amount
  columns}
}
\value{
  As this is a type constructor, the return is an
  EventGroup object.
}
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

