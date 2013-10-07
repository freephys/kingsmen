\name{is_valid_cluster}
\alias{is_valid_cluster}
\title{Determine whether a cluster is valid}
\arguments{
  \item{z}{An n x 2 matrix of points}

  \item{groups}{A vector of group associations}
}
\description{
  If a cluster minimizes distance between points, then it
  is considered to be a valid cluster.
}
\details{
  With hierarchical clustering techniques it is difficult
  to know whether the set of clusters produced are valid.
  Typically this is left to interpretation and must be
  'eye-balled' to choose the cutoff point as well as decide
  whether the cluster boundaries make sense.

  For an automated system, such a manual decision point is
  undesirable and must be replaced by automatic process.
  Since this data is multidimensional, one approach is to
  use a distance metric or other mathematical property as a
  heuristic. This function uses accepts a group of clusters
  if the sum of the variances of the distance within each
  cluster is less than the variance of the distance as a
  single cluster.
}
\author{
  Brian Lee Yung Rowe
}
\keyword{cluster}

