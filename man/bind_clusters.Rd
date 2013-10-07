\name{bind_clusters}
\alias{bind_clusters}
\title{Bind dates to cluster information}
\arguments{
  \item{groups}{}

  \item{dates}{}
}
\description{
  Pad cluster group information and attach dates.
}
\details{
  Since the group information is based on interarrival
  data, which is then partitioned, the effective length of
  the data set shrinks. This function expands the group
  data so that it matches the length of the input data.
}
\author{
  Brian Lee Yung Rowe
}
\keyword{cluster}

