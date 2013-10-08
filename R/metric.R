# :vim set filetype=R
#' Compute the Kullback-Leibler distance between two probability
#' distributions.
#'
#' @name kl.dist
#' @param p A probability density or mass function
#' @param q A probability density or mass function
#' @author Brian Lee Yung Rowe
#' @keywords distribution
#' @examples
#' x <- dnorm(dnorm(seq(-10,10, 0.2)), 1, 2)
#' y <- dnorm(dnorm(seq(-10,10, 0.2)), 0, 3)
#' d <- kl.dist(x,y)
kl.dist <- function(p,q) {
  sum(ifelse(p == 0 & q == 0, 0,
    ifelse(p == 0 | q == 0, max(p,q), p * log(p/q))))
}

print("Environment for kl.dist is\n")
print(environment(kl.dist))
print("\n")

print("Enclosing environment for kl.dist is\n")
print(parent.env(environment(kl.dist)))
print("\n")

#' Compute the Hellinger distance between two probability
#' distributions.
#'
#' @name hellinger.dist
#' @param p A probability density or mass function
#' @param q A probability density or mass function
#' @author Brian Lee Yung Rowe
#' @keywords distribution
#' @examples
#' x <- dnorm(dnorm(seq(-10,10, 0.2)), 1, 2)
#' y <- dnorm(dnorm(seq(-10,10, 0.2)), 0, 3)
#' d <- hellinger.dist(x,y)
hellinger.dist <- function(p,q) {
  sqrt(sum((sqrt(p) - sqrt(q))^2)) / sqrt(2)
}

#' Determine whether the sum of the parts is greater than the whole
#'
#' This is to find non-linear behavior where adding up the parts
#' is more than the value of the whole.
#'
#' @name parts_greater_than_whole
#' @param parts A vector of values
#' @param whole A scalar representing the collective value of the parts
#' @author Brian Lee Yung Rowe
#' @keywords array
#' @examples
#' x <- rnorm(20)
#' parts_greater_than_whole(x, sum(x))
parts_greater_than_whole(NULL, groups) %as% FALSE

parts_greater_than_whole(parts, whole) %::% numeric : numeric : logical
parts_greater_than_whole(parts, whole) %when% {
  any(is.na(parts)) || is.na(whole)
} %as% FALSE

parts_greater_than_whole(parts, whole) %as% { sum(parts) > whole }


