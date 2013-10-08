\name{kl.dist}
\alias{kl.dist}
\title{Compute the Kullback-Leibler distance between two probability
distributions.}
\usage{
  kl.dist(p, q)
}
\arguments{
  \item{p}{A probability density or mass function}

  \item{q}{A probability density or mass function}
}
\description{
  Compute the Kullback-Leibler distance between two
  probability distributions.
}
\examples{
x <- dnorm(dnorm(seq(-10,10, 0.2)), 1, 2)
y <- dnorm(dnorm(seq(-10,10, 0.2)), 0, 3)
d <- kl.dist(x,y)
}
\author{
  Brian Lee Yung Rowe
}
\keyword{distribution}

