adcov.test <- function(x, y, R = 499) {

  n <- dim(x)[1]
  stat <- estats::adcov(x, y, bc = TRUE)
  pstat <- numeric(R)
  for ( i in 1:R ) {
    id <- Rfast2::Sample.int(n, n)
    pstat[i] <- estats::adcov(x, y[id, ], bc = TRUE)
  }
  p.value <- ( sum(pstat > stat) + 1 ) / (R + 1)
  res <- c(stat, p.value)
  names(res) <- c("dcov", "p-value")
  res
}
