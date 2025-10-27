\name{Permutation based and asymptotic distance covariance hypothesis test}
\alias{dcov.test}
\title{
Permutation based and asymptotic distance covariance hypothesis test
}

\description{
Permutation based and asymptotic distance covariance hypothesis test.
}

\usage{
dcov.test(x, y, R = 1)
}

\arguments{
\item{x}{
A numerical matrix or a vector.
}
\item{y}{
A numerical matrix (of the same dimensions) or a vector.
}
\item{R}{
If R=1, the asymptotic p-value of Shen, Panda and Vogelstein (2022) is returned. If R > 1, the permutation based p-value is computed.
}
}

\details{
The bias corrected distance correlation is used. The hypothesis test is whether the two matrices are independent or not. If R=1, the test is based on the distance correlation. If R > 1 the test is based upon the distance covariance.
}

\value{
A vector with 2 elements, the bias corrected distance correlation or covariance, and the associated permutation or asymptotic based p-value.
}

\references{
Shen C., Panda S. and Vogelstein J. T. (2022). The Chi-Square Test of Distance Correlation.
Journal of Computational and Graphical Statistics, 31(1): 254--262.

G.J. Szekely, M.L. Rizzo and N. K. Bakirov (2007). Measuring and Testing Independence
by Correlation of Distances. Annals of Statistics, 35(6): 2769--2794.

Szekely G. J. and Rizzo M. L. (2023). The Energy of Data and Distance Correlation.
Chapman and Hall/CRC.
}

\author{
Manos Papadakis

R implementation and documentation: Michail Tsagris <mtsagris@uoc.gr> and Manos Papadakis <papadakm95@gmail.com>.
}
%\note{
%%  ~~further notes~~
%}

\seealso{
\code{ \link{dcov}, \link{edist}
}
}

\examples{
x <- as.matrix(iris[1:50, 1:4])
y <- as.matrix(iris[51:100, 1:4])
res <- dcov.test(x, y)
}

