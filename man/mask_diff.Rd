\name{mask_diff}
\alias{mask_diff}
\title{Get differences omitting masked results}
\arguments{
  \item{x}{A vector}

  \item{mask}{The value(s) to remove from the diff results}
}
\value{
  The names of a sequence with masked values removed
}
\description{
  This is a specialized function that gets the names
  associated with a vector after removing a set of masked
  values from the sequence.
}
\section{Usage}{
  mask_diff(x, mask=0)
}

\section{Details}{
  This is a convenience function and primarily used
  internally.
}
\examples{
a <- round(runif(20,0,4))
mask_diff(a)
}
\author{
  Brian Lee Yung Rowe
}
\keyword{manip}

