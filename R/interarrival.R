# :vim set filetype=R
# Journal of the Operational Research Society
# Foresight: The International Journal of Applied Forecasting
# Applied Mathematics and Computation - Elsevier
# European Journal of Operational Research - Elsevier
# Computers & Operations Research - Elsevier
# International Journal of Production Economics
# 

#' Divide transaction stream into distinct continguous clusters
#'
#' This method is less susceptible to noise than a direct 
#' clustering approach using only interarrival rates.
#'
#' @name divide
#' @param event An Event object
#' @param method Passed to hclust to specify the clustering algorithm
#' @param levels Number of levels to use when cutting the tree
#' @param plot Specify whether a plot be generated
#' @param \dots Additional arguments to pass to \code{partition}
#' @author Brian Lee Yung Rowe
#' @keywords cluster
#' @examples
#' d <- Sys.Date() + cumsum(round(c(rnorm(20,15,6), rnorm(20,25,10))))
#' e <- Event(d, abs(rnorm(length(d))))
#' divide(e, plot=FALSE)
#
# Some others to try
# d <- Sys.Date() + cumsum(round(c(runif(20,5,10), runif(20,25,30))))
# d <- Sys.Date() + cumsum(round(c(rnorm(20,10,4), rnorm(20,25,10))))
# d <- Sys.Date() + cumsum(round(c(rnorm(20,5,1), rnorm(20,15,3), rnorm(20,45,5))))
divide(event, method='complete', levels=1, plot=TRUE, ...) %when% {
  nrow(event) > 5
} %as% {
  z <- partition(interarrival(event$date), ...)
  #z <- z[2:(nrow(z)-1),]
  if (any(is.na(z))) return(NA)

  egroup <- event_group(event, z, method, levels)
  if (plot) plot(egroup)
  summary(egroup)
}

#' Determine whether a cluster is valid
#'
#' If a cluster minimizes distance between points, then it is 
#' considered to be a valid cluster.
#'
#' With hierarchical clustering techniques it is difficult to know
#' whether the set of clusters produced are valid. Typically this
#' is left to interpretation and must be 'eye-balled' to choose
#' the cutoff point as well as decide whether the cluster boundaries
#' make sense.
#'
#' For an automated system, such a manual decision point is 
#' undesirable and must be replaced by automatic process.
#' Since this data is multidimensional, one approach is to use a 
#' distance metric or other mathematical property as a heuristic.
#' This function uses accepts a group of clusters if the sum
#' of the variances of the distance within each cluster is less
#' than the variance of the distance as a single cluster.
#' 
#' @name is_valid_cluster
#' @param z An n x 2 matrix of points
#' @param groups A vector of group associations
#' @author Brian Lee Yung Rowe
#' @keywords cluster
is_valid_cluster(NULL, groups) %as% TRUE

is_valid_cluster(z, groups) %::% matrix : integer : logical
is_valid_cluster(z, groups) %as% {
  vz <- var(dist(z))
  vs <- sapply(unique(groups), function(g) var(dist(z[groups==g,])) )
  flog.info("var(dz) = %s, var(groups) = { %s }", vz, paste(vs, collapse=', '))
  ! parts_greater_than_whole(vs, vz)
}


#' Bind dates to cluster information
#'
#' Pad cluster group information and attach dates.
#'
#' Since the group information is based on interarrival data,
#' which is then partitioned, the effective length of the data set
#' shrinks. This function expands the group data so that it matches
#' the length of the input data.
#'
#' @name bind_clusters
#' @param groups
#' @param dates
#' @author Brian Lee Yung Rowe
#' @keywords cluster
bind_clusters(groups, dates) %as% {
  # Pad cluster information to fit original data
  padded <- c(rep(groups[1],2), groups, rep(tail(groups,1),1))
  names(padded) <- dates[order(dates)]
  padded
}

#' Get differences omitting masked results
#'
#' @name mask_diff
#' @param x A vector
#' @param mask The value(s) to remove from the diff results
#' @author Brian Lee Yung Rowe
#' @keywords manip
#' @examples
#' a <- round(runif(20,0,4))
#' mask_diff(a)
mask_diff(x, mask=0) %as% {
  y <- diff(x)
  names(y[y != mask])
}

#' Return the value of regime or NA
#'
#' @name active_regime
#' @param regime The current regime
#' @param .lambda.r_1 Boolean to control switching
#' @author Brian Lee Yung Rowe
#' @keywords manip
active_regime(regime, FALSE) %as% NA
active_regime(regime, TRUE) %as% regime

#' Calculate interarrival times
#'
#' @name interarrival
#' @param dates A sequence of dates
#' @author Brian Lee Yung Rowe
#' @keywords cluster
#' @examples
#' d <- Sys.Date() + cumsum(round(c(runif(20,5,10), runif(20,25,30))))
#' interarrival(d)
interarrival(dates) %::% character : difftime
interarrival(dates) %as% interarrival(as.Date(dates))

interarrival(dates) %::% Date : difftime
interarrival(dates) %as% {
  dates <- unique(dates[order(dates)])
  i <- diff(dates)
  names(i) <- dates[2:length(dates)]
  i
}


#' Group interarrival rates into clusters
#'
#' Use hierarchical clustering to group interarrival rates together.
#'
#' @section Usage:
#'
#' @name event_group
#' @param event
#' @param part
#' @param method
#' @param levels
#' @author Brian Lee Yung Rowe
#' @keywords cluster
#' @examples
#' d <- Sys.Date() + cumsum(round(c(runif(20,5,10), runif(20,25,30))))
#' e <- Event(d, abs(rnorm(length(d))))
#' g <- event_group(e)
event_group(event, part, method, levels) %as% {
  dz <- dist(part)
  h <- hclust(dz, method=method)
  grouping <- cutree(h, h=tail(h$height,levels)[1]-1)
  group <- bind_clusters(grouping, event$date)
  EventGroup(event, group, part, h$height)
}

# This acts as the control
event_group(event, method='complete', levels=1) %as% {
  dz <- dist(interarrival(event$date))
  h <- hclust(dz, method=method)
  grouping <- cutree(h, h=tail(h$height,levels)[1]-1)
  group <- bind_clusters(grouping, event$date)
  EventGroup(event, group, NULL, h$height)
}

#' Represents a sequence of events
#'
#' An ordered sequence of events. This is essentially a list of tuples
#' containing a date and an amount (some arbitrary value associated
#' with the date).
#'
#' @name Event
#' @author Brian Lee Yung Rowe
#' @keywords cluster
#' @examples
#' d <- Sys.Date() + cumsum(round(c(runif(20,5,10), runif(20,25,30))))
#' e <- Event(d, abs(rnorm(length(d))))
Event(date, amount) %as% data.frame(date=date, amount=amount)

Event(df) %when% {
  df %hasa% date
  df %hasa% amount
} %as% df


#' Represents a cluster of transaction events
#'
#' Used internally by \code{event_group}.
#'
#' @name EventGroup
#' @param event
#' @param group
#' @param part
#' @param height
#' @author Brian Lee Yung Rowe
#' @keywords cluster
EventGroup(event, group, part, height) %as%
  list(event=event,
    group=group,
    part=part,
    height=height,
    unique=unique(group))


#' Plot an EventGroup
#'
#' @name plot.Event
#' @author Brian Lee Yung Rowe
#' @keywords cluster
plot.EventGroup(egroup, main='Event stream') %when% {
  is.null(egroup$part)
} %as% {
  attach(egroup)
  on.exit(detach('egroup'))
  colors <- rainbow(length(egroup$unique))
  opar <- par(mfrow=c(1,1))
  f <- function(g) {
    event <- event[event$date %in% as.Date(names(group[group==g])),]
    lines(event$date, event$amount, type='h', col=colors[g])
  }
  bounds <- range(event$date)
  plot(bounds, c(min(event$amount), max(event$amount)),
    type='h', col='white',
  xlab='date', ylab='magnitude',
    main=main)
  sapply(egroup$unique, f)
  par(opar)
}

plot.EventGroup(egroup, main='Event stream') %as% {
  attach(egroup)
  on.exit(detach('egroup'))
  colors <- rainbow(length(egroup$unique))
  opar <- par(mfrow=c(1,2))
  f <- function(g) {
    event <- event[event$date %in% as.Date(names(group[group==g])),]
    lines(event$date, event$amount, type='h', col=colors[g])
  }
  bounds <- range(event$date)
  plot(bounds, c(min(event$amount), max(event$amount)),
    type='h', col='white',
  xlab='date', ylab='magnitude',
    main=main)
  sapply(egroup$unique, f)
  #abline(v=bounds[2]-30, col='black')

  plot(part, col='white', main='Interarrival Clusters')
  sapply(egroup$unique,
    function(g) points(part[chomp(group,2,2)==g,], col=colors[g]))
  par(opar)
}

#' Provide a summary of an EventGroup
#'
#' @name summary.EventGroup
#' @author Brian Lee Yung Rowe
#' @keywords cluster
summary.EventGroup(egroup) %as% {
  attach(egroup)
  on.exit(detach('egroup'))
  delta <- mask_diff(group)
  flog.info("Transition points: %s", paste(delta, collapse=','))

  bounds <- range(event$date)
  intervals <- segment(c(bounds[1], as.Date(delta), bounds[2]))
  f <- function(g) {
    days <- as.numeric(diff(as.Date(g))) + 1
    events <- nrow(event[event$date >= g[1] & event$date <= g[2],])
    regime <- group[g[1]]
    data.frame(regime, days, events)
  }
  ps <- apply(intervals, 1, f)
  ps <- ddply(do.call(rbind,ps), .(regime),
    function(x) c(days=sum(x$days), events=sum(x$events)))

  valid <- is_valid_cluster(part, chomp(group,2,2))
  ps@active <- active_regime(tail(group,1), valid)
  ps@height <- height
  ps
}


