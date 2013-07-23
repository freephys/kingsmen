# :vim set filetype=R

#' Similar to partion where radius=2 and there is no metric
segment(x) %as% cbind(x[1:(length(x)-1)], x[2:length(x)])

#' Partition a sequence into coordinate pairs based on adjacent windows
partition(x, metric=median, radius=10) %as% {
  f <- function(x,i) {
    c(metric(x[max(1,i-radius):i]), metric(x[(i+1):min(length(x),i+1+radius)]))
  }
  m <- t(sapply(1:(length(x)-1), function(i) f(x,i)))
  colnames(m) <- c('left','right')
  m
}

#' Split a sequence based on an expression
slice(x, expression) %as%
{
  left <- x[expression,]
  right <- x[!expression,]
  list(left, right)
}


