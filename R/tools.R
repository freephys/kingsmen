# :vim set filetype=R

#' Similar to partion where radius=2 and there is no metric
segment(x) %as% data.frame(a=x[1:(length(x)-1)], b=x[2:length(x)])

#' Split a sequence based on an expression
slice(x, expression) %as%
{
  left <- x[expression,]
  right <- x[!expression,]
  list(left, right)
}

chomp(x, head=1, tail=1) %when% {
  is.null(dim(x))
} %as% {
  x[(1+head):(length(x)-tail)]
}

chomp(x, head=1, tail=1) %as% {
  x[(1+head):(length(x)-tail), ]
}


#' Partition a sequence into coordinate pairs based on adjacent windows
partition(x, metric=median, radius=10) %as% {
  f <- function(x,i) {
    c(left=metric(x[max(1,i-radius):i]), 
      right=metric(x[(i+1):min(length(x),i+1+radius)]))
  }
  t(sapply(1:(length(x)-1), function(i) f(x,i)))
}


