ts.qexp <- function (x,q) {
  if (q == 1) {
    y <- exp(x)
  } else {
    y <- numeric(length(x))
    y[1+(1-q)*x > 0] <- (1 + (1-q)*x)^(1/(1-q))
    y[1+(1-q)*x <= 0] <- 0^(1/(1-q))
  }
  return(y)
}


pts.qexp <- function(q, shape, rate, lower.tail = TRUE) {
  shape1 <- 1/(2-shape)
  prob <- 1 - ts.qexp(-rate*q/shape1, shape1)
  if(!lower.tail) {
    prob <- 1 - prob
  }
  return(prob)
}

dts.qexp <- function(x, shape, rate) {
    den <- (2-shape)*rate*ts.qexp(-rate*x, shape)
  return(den)
}



