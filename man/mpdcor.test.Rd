\name{Hypothesis testing for many partial distance correlations}
\alias{mpdcor.test}
\title{
Hypothesis testing for many partial distance correlations
}

\description{
Hypothesis testing for many partial distance correlations.
}

\usage{
mpdcor.test(y, x, z, R = 500)
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
\item{R}{
The number of permutations to implement. If R = 1, the the asymptotic p-value is returned only.
}
}

\details{
Hypothesis testing between y and each column of x, conditional on z is performed.
}

\value{
A matrix with three columns: the unbiased partial distance correlation, the permutation based p-value and the asymptotic p-value as proposed by Shen, Panda and Vogelstein (2022).
}

\references{
Szekely G. J. and Rizzo M. L. (2014). Partial Distance Correlation with Methods for Dissimilarities.
The Annals of Statistics, 42(6): 2382--2412.

Shen C., Panda S. and Vogelstein J. T. (2022). The Chi-Square Test of Distance Correlation.
Journal of Computational and Graphical Statistics, 31(1): 254--262.

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
\code{ \link{mpdcor},  \link{pdcor.test}
}
}

\examples{
y <- iris[, 1]
x <- matrix( rnorm(150 * 10), ncol = 10 )
z <- iris[, 2]
mpdcor.test(y, x, z)
}

