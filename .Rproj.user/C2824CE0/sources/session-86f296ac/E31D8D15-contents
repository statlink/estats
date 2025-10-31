\name{Partial distance correlation}
\alias{pdcor}
\title{
Partial distance correlation
}

\description{
Partial distance correlation.
}

\usage{
pdcor(x, y, z)
}

\arguments{
\item{x}{
A numerical vector or matrix.
}
\item{y}{
A numerical vector or  matrix.
}
\item{z}{
A numerical vector or matrix.
}
}

\details{
The unbiased partial distance correlation between x and y conditioning on z is computed. \bold{Note:} currently, ony two cases are supported, all x, y, and z are vectors or they are all matrices with the same dimensions.
}

\value{
The unbiased partial distance correlation.
}

\references{
Szekely G. J. and Rizzo M. L. (2014). Partial Distance Correlation with Methods for Dissimilarities.
The Annals of Statistics, 42(6): 2382--2412.

Szekely G. J. and Rizzo M. L. (2023). The Energy of Data and Distance Correlation.
Chapman and Hall/CRC.

Tsagris M. and Papadakis M. (2025). Fast and light-weight energy statistics using the R package Rfast.
https://arxiv.org/abs/2501.02849

Kontemeniotis N., Vargiakakis R. and Tsagris M. (2025). On independence testing using the (partial) distance correlation. https://arxiv.org/abs/2506.15659v1
}

\author{
Michail Tsagris.

R implementation and documentation: Michail Tsagris \email{mtsagris@uoc.gr}.
}

\seealso{
\code{ \link{pdcor.test}, \link{mpdcor}
}
}

\examples{
x <- iris[, 1]
y <- iris[, 2]
z <- iris[, 3]
pdcor(x, y, z)
}

