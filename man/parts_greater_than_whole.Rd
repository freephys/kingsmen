\name{parts_greater_than_whole}
\alias{parts_greater_than_whole}
\title{Determine whether the sum of the parts is greater than the whole}
\arguments{
  \item{parts}{A vector of values}

  \item{whole}{A scalar representing the collective value
  of the parts}
}
\description{
  This is to find non-linear behavior where adding up the
  parts is more than the value of the whole.
}
\examples{
x <- rnorm(20)
parts_greater_than_whole(x, sum(x))
}
\author{
  Brian Lee Yung Rowe
}
\keyword{array}

