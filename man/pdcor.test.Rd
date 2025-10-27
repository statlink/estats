\name{Hypothesis testing for the partial distance correlation}
\alias{pdcor.test}
\title{
Hypothesis testing for the partial distance correlation
}

\description{
Hypothesis testing for the partial distance correlation.
}

\usage{
pdcor.test(x, y, z, type = 1, R = 500)
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
\item{type}{
In case that all x, y, and z are vectors the user may select the type = 2 which is even faster,
but at the expense of requiring more memory.
}
\item{R}{
The number of permutations to implement. If R = 1, the the asymptotic p-value is returned only.
}
}

\details{
Hypothesis testing using the unbiased partial distance correlation between x and y conditioning on z is computed. \bold{Note:} currently, ony two cases are supported, all x, y, and z are vectors or they are all matrices with the same dimensions.
}

\value{
A vector with the unbiased partial distance correlation, the permutation based p-value and the asymptotic p-value as proposed by Shen, Panda and Vogelstein (2022).
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
Michail Tsagris and Nikolaos Kontemeniotis .

R implementation and documentation: Michail Tsagris \email{mtsagris@uoc.gr} and Nikolaos Kontemeniotis \email{kontemeniotisn@gmail.com}.
}

\seealso{
\code{ \link{pdcor}
}
}

\examples{
x <- iris[, 1]
y <- iris[, 2]
z <- iris[, 3]
pdcor.test(x, y, z)
}

