\name{mask_diff}
\alias{mask_diff}
\title{Get differences omitting masked results}
\arguments{
  \item{x}{A vector}

  \item{mask}{The value(s) to remove from the diff results}
}
\description{
  Get differences omitting masked results
}
\examples{
a <- round(runif(20,0,4))
mask_diff(a)
}
\author{
  Brian Lee Yung Rowe
}
\keyword{manip}

