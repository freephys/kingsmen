\docType{package}
\name{kingsmen-package}
\alias{kingsmen}
\alias{kingsmen-package}
\title{A technique for detecing regime change in irregular time series}
\description{
  In time series that are irregular, different processes
  can be dominant at different points in time.
}
\details{
  \tabular{ll}{ Package: \tab kingsmen\cr Type: \tab
  Package\cr Version: \tab 1.0.0\cr Date: \tab
  2013-10-09\cr License: \tab LGPL-3\cr LazyLoad: \tab
  yes\cr }
}
\examples{
\dontrun{
d <- Sys.Date() + cumsum(round(c(rnorm(20,15,6), rnorm(20,25,10))))
e <- Event(d, abs(rnorm(length(d))))
divide(e)
}
}
\author{
  Brian Lee Yung Rowe <r@zatonovo.com>
}
\seealso{
  \code{\link{divide}}
}
\keyword{cluster}
\keyword{package}

