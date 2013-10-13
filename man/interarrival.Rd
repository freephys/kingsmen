\name{interarrival}
\alias{interarrival}
\title{Calculate interarrival rates}
\arguments{
  \item{dates}{A sequence of dates}
}
\value{
  A sequence of interarrival rates are returned. The length
  will always be 1 less than the length of the input dates.
  Any names associated with the date sequence are copied
  over to the return sequence with the first name dropped.
  This makes sense since the first interarrival rate is
  only available once the event associated with the second
  element arrives.
}
\description{
  Given a sequence of dates, compute the interarrival
  rates.
}
\section{Usage}{
  interarrival %::% character : difftime
  interarrival(dates)

  interarrival %::% Date : difftime interarrival(dates)
}

\section{Details}{
  This function is a convenience wrapper around difftime to
  calculate interarrival rates for irregular time series.
}
\examples{
d <- Sys.Date() + cumsum(round(c(runif(20,5,10), runif(20,25,30))))
interarrival(d)
}
\author{
  Brian Lee Yung Rowe
}
\keyword{cluster}

