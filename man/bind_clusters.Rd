\name{bind_clusters}
\alias{bind_clusters}
\title{Bind dates to cluster information}
\arguments{
  \item{groups}{A sequence of group information}

  \item{dates}{The original dates}
}
\value{
  A padded
}
\description{
  This function pads cluster group information to match the
  length of the original data and attaches dates.
}
\section{Usage}{
  bind_clusters(groups, dates)
}

\section{Details}{
  Since the group information is based on interarrival
  data, which is then partitioned, the effective length of
  the data set shrinks. This function expands the group
  data so that it matches the length of the input data.
}
\author{
  Brian Lee Yung Rowe
}
\keyword{cluster}

