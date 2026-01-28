squish <- function(x, new_min = min_bound, new_max = max_bound){   
  (x - min(x))/(max(x)-min(x)) * (new_max - new_min) + new_min 
}

rqpois <- function(n, mu, theta) {
  rnbinom(n = n, mu = mu, size = mu/(theta-1))
}