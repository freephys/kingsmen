# :vim set filetype=R
#' A technique for detecing regime change in irregular time series
#'
#' In time series that are irregular, different processes can be
#' dominant at different points in time.
#'
#' \tabular{ll}{
#' Package: \tab kingsmen\cr
#' Type: \tab Package\cr
#' Version: \tab 1.0.0\cr
#' Date: \tab 2013-10-09\cr
#' License: \tab LGPL-3\cr
#' LazyLoad: \tab yes\cr
#' }
#' 
#' @name kingsmen-package
#' @aliases kingsmen-package kingsmen
#' @docType package
#' @exportPattern "^[^\\.]"
#' @import lambda.r lambda.tools futile.logger plyr
#' @author Brian Lee Yung Rowe <r@@zatonovo.com>
#' @seealso \code{\link{divide}}
#' @keywords package cluster
#' @examples
#' \dontrun{
#' d <- Sys.Date() + cumsum(round(c(rnorm(20,15,6), rnorm(20,25,10))))
#' e <- Event(d, abs(rnorm(length(d))))
#' divide(e)
#' }
NULL
