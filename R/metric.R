# :vim set filetype=R
kl.dist <- function(p,q) {
  sum(ifelse(p == 0 & q == 0, 0,
    ifelse(p == 0 | q == 0, max(p,q), p * log(p/q))))
}

hellinger.dist <- function(p,q) {
  sqrt(sum((sqrt(p) - sqrt(q))^2)) / sqrt(2)
}

parts_greater_than_whole(parts, whole) %::% numeric : numeric : logical
parts_greater_than_whole(parts, whole) %when% {
  any(is.na(parts)) || is.na(whole)
} %as% FALSE

parts_greater_than_whole(parts, whole) %as% { sum(parts) > whole }


