\name{EventGroup}
\alias{EventGroup}
\title{Represents a cluster of transaction events}
\arguments{
  \item{event}{An Event object representing the events to
  divide}

  \item{group}{A sequence of group assignments}

  \item{part}{The partition table}

  \item{height}{The height of the tree}
}
\value{
  As this is a type constructor, the return is an
  EventGroup object.
}
\description{
  An EventGroup object contains the result of a clustering
  operation on a stream of events.
}
\author{
  Brian Lee Yung Rowe
}
\keyword{cluster}

