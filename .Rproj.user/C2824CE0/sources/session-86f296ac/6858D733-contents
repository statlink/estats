\name{Many pPartial distance correlations}
\alias{mpdcor}
\title{
Many partial distance correlations
}

\description{
Many partial distance correlations.
}

\usage{
mpdcor(y, x, z)
}

\arguments{
\item{y}{
A numerical vector.
}
\item{x}{
A numerical matrix.
}
\item{z}{
A numerical vector.
}
}

\details{
This computes the unbiased pdcor between y and each column of x, conditional on the vector z.
}

\value{
A vector with many unbiased partial distance correlations.
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
\code{ \link{pdcor}, \link{mpdcor.test}
}
}

\examples{
y <- iris[, 1]
x <- matrix( rnorm(150 * 10), ncol = 10 )
z <- iris[, 2]
mpdcor(y, x, z)
pdcor(y, x[, 1], z)
}

