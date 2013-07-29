# :vim set filetype=R
# Journal of the Operational Research Society
# Foresight: The International Journal of Applied Forecasting
# Applied Mathematics and Computation - Elsevier
# European Journal of Operational Research - Elsevier
# Computers & Operations Research - Elsevier
# International Journal of Production Economics
# 

is_valid_cluster(z, groups) %::% matrix : integer : logical
is_valid_cluster(z, groups) %as% {
  vz <- var(dist(z))
  vs <- sapply(unique(groups), function(g) var(dist(z[groups==g,])) )
  flog.info("var(dz) = %s, var(groups) = { %s }", vz, paste(vs, collapse=', '))
  ! parts_greater_than_whole(vs, vz)
}


bind_clusters(groups, dates) %as% {
  # Pad cluster information to fit original data
  padded <- c(rep(groups[1],2), groups, rep(tail(groups,1),1))
  names(padded) <- dates[order(dates)]
  padded
}

non_zero_diff(x) %as% {
  y <- diff(x)
  names(y[y != 0])
}

event_group(event, part, method, levels) %as% {
  dz <- dist(part)
  h <- hclust(dz, method=method)
  grouping <- cutree(h, h=tail(h$height,levels)[1]-1)
  group <- bind_clusters(grouping, event$date)
  EventGroup(event, group, part, h$height)
}

#' This acts as the control
#' @example
#' d <- Sys.Date() + cumsum(round(c(runif(20,5,10), runif(20,25,30))))
#' e <- Event(data.frame(date=d, amount=abs(rnorm(length(d)))))
#' g <- event_group(e)
event_group(event, method='complete', levels=1) %as% {
  dz <- dist(interarrival(event$date))
  h <- hclust(dz, method=method)
  grouping <- cutree(h, h=tail(h$height,levels)[1]-1)
  group <- bind_clusters(grouping, event$date)
  EventGroup(event, group, NULL, h$height)
}

#' Represents a sequence of events
Event(df) %when% {
  df %hasa% date
  df %hasa% amount
} %as% df

#' Represents a cluster of transaction events
EventGroup(event, group, part, height) %as%
  list(event=event,
    group=group,
    part=part,
    height=height,
    unique=unique(group))


#' Plot an EventGroup
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
summary.EventGroup(egroup) %as% {
  attach(egroup)
  on.exit(detach('egroup'))
  delta <- non_zero_diff(group)
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


active_regime(regime, FALSE) %as% NA
active_regime(regime, TRUE) %as% regime

#' Calculates interarrival times
interarrival(dates) %::% character : difftime
interarrival(dates) %as% interarrival(as.Date(dates))

interarrival(dates) %::% Date : difftime
interarrival(dates) %as% {
  dates <- unique(dates[order(dates)])
  #i <- dates[2:length(dates)] - dates[1:(length(dates)-1)]
  i <- diff(dates)
  names(i) <- dates[2:length(dates)]
  i
}

#' Divide transaction stream into distinct continguous clusters
#' @examples
#' d <- Sys.Date() + cumsum(round(c(runif(20,5,10), runif(20,25,30))))
#' d <- Sys.Date() + cumsum(round(c(rnorm(20,10,4), rnorm(20,25,10))))
#' d <- Sys.Date() + cumsum(round(c(rnorm(20,5,1), rnorm(20,15,3), rnorm(20,45,5))))
#' e <- Event(data.frame(date=d, amount=abs(rnorm(length(d)))))
#' divide(e)
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

