dcov.test <- function(x, y, R = 1) {

  if ( R == 1 ) {
    if ( is.vector(x) ) {
      n <- length(x)[1]
      stat <- dcov::dcor2d(x, y, type = "U")
    } else {
      n <- dim(x)[1]
      stat <- Rfast::dcor(x, y, bc = TRUE)$dcor
    }
    p.value <- pchisq(n * stat + 1, 1, lower.tail = FALSE)
    res <- c(stat, p.value)
    names(res) <- c("dcor", "p-value")

  } else {

    if ( is.vector(x) ) {
      stat <- dcov::dcov2d(x, y, type = "U")
      y <- Rfast::rep_col(y, R)
      y <- Rfast::colShuffle(y)
      pstat <- dcov::mdcov(x, y, type = "U")
    } else {
      n <- dim(x)[1]
      stat <- Rfast::dcov(x, y, bc = TRUE)
      pstat <- numeric(R)
      for ( i in 1:R ) {
        id <- Rfast2::Sample.int(n, n)
        pstat[i] <- Rfast::dcov(x, y[id, ], bc = TRUE)
      }
    }
    p.value <- ( sum(pstat > stat) + 1 ) / (R + 1)
    res <- c(stat, p.value)
    names(res) <- c("dcov", "p-value")
  }

  res
}
